//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;
use std::fmt;

use crate::cycles::{
    calculate_8086_unaligned_access, get_total_clocks, get_total_clocks_str, print_inst_clock_debug,
};
use crate::decode::{
    get_ip_absolute, AddTo, ExecuteSettings, InstType, ModRmDataType, OpCodeType, RegName,
    WidthType,
};

pub const MEMORY_SIZE: usize = 1024 * 1024;

#[derive(Debug, Default)]
pub struct CpuStateType {
    /// The instruction pointer (IP) register
    pub ip: u16,
    // MGH TODO: Make reg_file an array, and map RegNames to an index via a
    // match statement. Compare performance!
    reg_file: BTreeMap<RegName, u16>,
    flags_reg: FlagsRegType,
    pub memory: Vec<u8>,
    total_cycles: u64,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
/// This struct contains all the flag bits in the flags register. Each flag bit
/// is tracked by a boolean field.
/// See Figure 2-9 on page 2-7 of the manual.
/// The order of the flags is from LSB to MSB, and that is also the order in
/// which the flags will be printed during display.
/// The first 6 bits/flags are status flags, and the last 3 bits/flags are
/// control flags.
struct FlagsRegType {
    /// If set, there has been a carry out of the high-order bit of the result
    /// for an addition or a borrow into the high-order.bit of the result for
    /// a subtraction.
    /// NOTE: A signed carry is carry != overflow.
    /// carry can be used to detect an unsigned overflow.
    carry: bool,
    /// If set, the result has even parity (an even number of 1-bits).
    parity: bool,
    /// True if an addition results in a carry out of the low order half-byte
    /// (nibble) of the result.
    /// True if a subtraction results in a borrow into the low order half-byte
    /// (nibble) of the result.
    auxiliary_carry: bool,
    /// If set, the result of the operation is 0
    zero: bool,
    /// If set, the high order bit of the result is a 1.
    sign: bool,
    /// Indicates *signed* arithmetic overflow - if the result is too large or
    /// small to fit in dest without modifying the sign bit.
    /// If set, an arithmetic overflow has occurred (the significant digit has
    /// been lost because the size of the result exceeded the capacity of its
    /// destination location).
    /// This flag can be ignored on unsigned arithmetic
    overflow: bool,
    /// Setting this allows the CPU to recognize external (maskable) interrupt
    /// requests. Clearing IF disables these interrupts. This has no affect on
    /// either non-maskable external or internally-generated interrupts.
    interrupt_enable: bool,
    /// Setting this causes string instructions to auto-decrement.
    direction: bool,
    /// Setting this puts the processor into single-step mode for debugging.
    trap: bool,
}

/// Print out the flags reg from LSB to MSB.
impl fmt::Display for FlagsRegType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.carry {
            write!(f, "C")?;
        }
        if self.parity {
            write!(f, "P")?;
        }
        if self.auxiliary_carry {
            write!(f, "A")?;
        }
        if self.zero {
            write!(f, "Z")?;
        }
        if self.sign {
            write!(f, "S")?;
        }
        if self.overflow {
            write!(f, "O")?;
        }
        if self.interrupt_enable {
            write!(f, "I")?;
        }
        if self.direction {
            write!(f, "D")?;
        }
        if self.trap {
            write!(f, "T")?;
        }
        Ok(())
    }
}

/// Move the given vector containing the program into the CPU state and use it
/// as memory during execution.
/// Initialize the SP and IP registers according to the given values.
pub fn init_state(
    program_bytes: Vec<u8>,
    init_ip: Option<u16>,
    init_sp: Option<u16>,
) -> CpuStateType {
    let ip = match init_ip {
        Some(x) => x,
        None => 0,
    };
    let mut state = CpuStateType {
        // Initialize the memory array to 1 MB
        memory: program_bytes,
        ip: ip,
        ..Default::default()
    };

    // Initialize register values, as needed
    match init_sp {
        Some(sp) => {
            state.reg_file.insert(RegName::Sp, sp);
        }
        _ => {}
    };

    state
}

/// An enum representing where to place the destination value in.
#[derive(Eq, PartialEq)]
enum DestTarget {
    /// Store the result at the given memory address
    MemAddress(usize),
    /// Store the result in the given register
    RegisterName(RegName),
    /// Store the result in the flags register
    FlagsReg,
}

/// Execute the given instruction and modify the passed in CPU state. Return a
/// string summarizing the change in state that occurred, and return a bool that
/// halts CPU execution if it's true.
///
/// no_ip: If true, do NOT add IP changes or the final state of IP to the output
/// string.
pub fn execute(
    inst: &mut InstType,
    state: &mut CpuStateType,
    settings: &ExecuteSettings,
) -> (String, bool) {
    let mut effect = "".to_string();
    let op_type = match inst.op_type {
        Some(op_type) => op_type,
        None => {
            println!("Bad instruction object: {:?}", inst,);
            return (effect, true);
        }
    };

    // Handle stop on ret right away
    if op_type == OpCodeType::Ret && settings.stop_on_ret {
        effect.push_str(&format!(
            "STOPONRET: Return encountered at address {}.",
            state.ip
        ));
        return (effect, true);
    }

    // Save copy of old flags
    let old_flags = state.flags_reg.clone();

    // The destination value
    let mut new_val = None;
    let mut dest_target;
    // Set this var if we should set the flags reg at the end
    let mut modify_flags = false;
    let mut print_flags = false;
    let mut new_val_overflowed = false;
    let mut new_val_carry = false;
    let mut new_val_aux_carry = false;
    // This lets the flag setting code know whether it should work on the entire
    // word of new_val, the top byte, or the bottom byte. Default to looking at
    // the entire word.
    let mut new_val_width = WidthType::Word;
    let current_ip = state.ip;
    let mut jumped = false;
    let mut shift_count = None;
    let mut old_sp = None;
    let mut new_sp = None;
    // If true, this instruction accesses memory via the stack at this instruction
    let mut stack_mem_addr = None;
    // The number of stack pushes or pops an instruction does
    let mut stack_mem_count = 0;
    // If this instruction has a destination memory address, then marks two
    // transfers instead of one
    let mut double_mem_dest = false;
    // Accumulate a string of register changes as needed, if not covered by
    // new_val + dest_target
    let mut reg_change_str = None;

    // "While an instruction is executing, IP refers to the next instruction."
    // BYU RTOS Website, 8086InstructionSet.html
    // So, always advance the IP before executing each instruction so that it
    // points to the next instruction.
    advance_ip_reg(inst, state);

    // Handle emulator hooks before doing any "real" instructions
    match (op_type, inst.data_value) {
        (OpCodeType::Int, Some(int_num @ (0x0 | 0x10 | 0x1B | 0x21))) => {
            let ax = *state.reg_file.get(&RegName::Ax).unwrap();
            let ah = (ax >> 8) as u8;
            let al = ax as i8;
            // Handle various emulator hooks
            match (int_num, ah) {
                (0x0, _) => unimplemented!("int 0x{int_num:x} (divide by 0)"),
                (0x10, _) => unimplemented!("int 0x{int_num:x} function 0x{ah:x} (BIOS interrupt)"),
                (0x1B, _) => unimplemented!("int 0x{int_num:x} function 0x{ah:x} (simptris)"),
                (0x21, 0x02 | 0x09) => unimplemented!("int 0x{int_num:x} function 0x{ah:x}"),
                (0x21, 0x40) => {
                    let string_start = *state.reg_file.get(&RegName::Dx).unwrap() as usize;
                    let string_len = *state.reg_file.get(&RegName::Cx).unwrap() as usize;
                    let string_end = string_start + string_len;
                    let _file_number = *state.reg_file.get(&RegName::Bx).unwrap();
                    // Get the string to print from memory
                    println!("printString(): {string_start}..{string_end} ({string_len})");
                    let str_slice = &state.memory[string_start..string_end];
                    let str_to_print = match std::str::from_utf8(str_slice) {
                        Ok(str_utf8) => str_utf8,
                        Err(e) => {
                            effect.push_str(&format!("ERROR: Invalid UTF-8 sequence: {e}"));
                            return (effect, true);
                        }
                    };
                    println!("PROGRAM: {str_to_print}");
                }
                // See https://en.wikipedia.org/wiki/COM_file
                // See https://github.com/hintron/8086-toolchain/blob/27a3148651bd/emulator/Bios.cpp#L105-L150
                // Register AH defines the specific "function" or "service"
                // under int 0x21 to run. E.g. 0x21 function 0x4C specifies
                // program termination, according to the COM file spec.
                (0x21, 0x4C) => {
                    println!("Exiting emulator w/ exit code {al}!");
                    effect.push_str(&format!(
                        "EXIT {al}: int 0x21 func 0x4c - emulator exit interrupt"
                    ));
                    // MGH TODO: Return the error code somehow
                    return (effect, true);
                }
                _ => {}
            }
            // Now that the emulator hook has completed, go to next instruction
            return (effect, false);
        }
        _ => {}
    };

    // Get mem address for implicit stack mem access so we can account for any
    // cycle penalties
    match op_type {
        OpCodeType::Push
        | OpCodeType::Pushf
        | OpCodeType::Call
        | OpCodeType::Int
        | OpCodeType::Int3 => {
            (old_sp, new_sp) = decrement_sp(2, &mut state.reg_file);
            stack_mem_addr = new_sp;
        }
        OpCodeType::Pop | OpCodeType::Popf | OpCodeType::Ret => {
            stack_mem_addr = Some(get_sp(&state.reg_file));
        }
        _ => {}
    }
    match op_type {
        OpCodeType::Push
        | OpCodeType::Pushf
        | OpCodeType::Call
        | OpCodeType::Pop
        | OpCodeType::Popf
        | OpCodeType::Ret => stack_mem_count = 1,
        OpCodeType::Int | OpCodeType::Int3 => stack_mem_count = 5,
        _ => {}
    }

    // Account for implicit double destination mem accesses
    match op_type {
        OpCodeType::Dec
        | OpCodeType::Inc
        | OpCodeType::Add
        | OpCodeType::Sub
        | OpCodeType::And
        | OpCodeType::Test
        | OpCodeType::Xor
        | OpCodeType::Shl
        | OpCodeType::Shr => double_mem_dest = true,
        _ => {}
    }

    // Figure out destination width, which will dictate how wide the data
    // transfer is for this instruction
    let dest_width = match (
        inst.dest_reg,
        inst.dest_width,
        inst.source_reg,
        inst.source_width,
    ) {
        (Some(dest_reg), _, _, _) => dest_reg.width,
        (_, Some(dest_width), _, _) => dest_width,
        (_, _, Some(source_reg), _) => source_reg.width,
        (_, _, _, Some(source_width)) => source_width,
        // Just default to word width
        _ => WidthType::Word,
    };

    // Is it a byte that is going from src to dst, or a word?
    let transfer_width = match dest_width {
        WidthType::Word => WidthType::Word,
        WidthType::Hi8 | WidthType::Byte => WidthType::Byte,
    };

    // Get the final memory addresses for source and dest, if applicable
    let (mut mem_addr_src, mem_addr_dst) = get_inst_mem_addrs(inst, state);

    // lea doesn't actually access memory, so clear mem_addr_src to avoid memory
    // cycle counting and unnecessary source calculations
    if op_type == OpCodeType::Lea {
        new_val = mem_addr_src;
        mem_addr_src = None;
    }

    // Now that we have the final memory address, if any, we can check it to see
    // if there are any unaligned word mem access penalties for the 8086
    inst.mem_access_word_unaligned = match calculate_8086_unaligned_access(
        mem_addr_src,
        mem_addr_dst,
        stack_mem_addr,
        stack_mem_count,
        double_mem_dest,
        transfer_width,
        inst.transfers,
    ) {
        Some(transfers) => transfers,
        None => {
            println!("inst: {inst:#?}");
            unimplemented!("Bad cycle timing for instruction");
        }
    };

    // Print this instruction's clock debug info now that all clock data is set
    print_inst_clock_debug(inst);

    let (dest_val, dest_val_target) = get_dest_val(inst, state, mem_addr_dst);
    dest_target = dest_val_target;

    let (source_val, source_width) = get_source_val(inst, state, mem_addr_src, transfer_width);

    // SUB/CMP: The source operand is subtracted from the destination operand,
    // and the result replaces the destination operand.

    // ADD: The source operand is added to the destination operand,
    // and the result replaces the destination operand.

    // SUB/CMP/ADD updates flags AF, CF, OF, PF, SF, and ZF.
    // MOV does not update any flags.

    match op_type {
        OpCodeType::Lea => {}
        OpCodeType::Cbw => {
            let ax = *state.reg_file.get(&RegName::Ax).unwrap_or(&0);
            let ax = ax as u8 as i8 as i16 as u16;
            new_val = Some(ax);
            dest_target = Some(DestTarget::RegisterName(RegName::Ax));
        }
        OpCodeType::Cwd => {
            let ax = *state.reg_file.get(&RegName::Ax).unwrap_or(&0);
            let dxax = ax as i16 as i32 as u32;
            let dx = (dxax >> 16) as u16;
            new_val = Some(dx);
            dest_target = Some(DestTarget::RegisterName(RegName::Dx));
            // We don't need to touch ax
        }
        OpCodeType::Cli => {
            state.flags_reg.interrupt_enable = false;
            print_flags = true;
        }
        OpCodeType::Sti => {
            state.flags_reg.interrupt_enable = true;
            print_flags = true;
        }
        OpCodeType::Clc => {
            state.flags_reg.carry = false;
            print_flags = true;
        }
        OpCodeType::Stc => {
            state.flags_reg.carry = true;
            print_flags = true;
        }
        OpCodeType::Cmc => {
            state.flags_reg.carry = !state.flags_reg.carry;
            print_flags = true;
        }
        OpCodeType::Cld => {
            state.flags_reg.direction = false;
            print_flags = true;
        }
        OpCodeType::Std => {
            state.flags_reg.direction = true;
            print_flags = true;
        }
        jump_op @ (OpCodeType::Ja
        | OpCodeType::Jnb
        | OpCodeType::Jb
        | OpCodeType::Jbe
        | OpCodeType::Je
        | OpCodeType::Jg
        | OpCodeType::Jnl
        | OpCodeType::Jl
        | OpCodeType::Jle
        | OpCodeType::Jne
        | OpCodeType::Jno
        | OpCodeType::Jnp
        | OpCodeType::Jns
        | OpCodeType::Jo
        | OpCodeType::Jp
        | OpCodeType::Js
        | OpCodeType::Jmp) => {
            jumped = handle_jmp_variants(jump_op, state, inst.ip_inc, inst.ip_abs);
        }
        jump_op @ (OpCodeType::Loopnz | OpCodeType::Loopz | OpCodeType::Loop) => {
            // pg 2-45 - 2-46
            // Decrement cx by 1 and jump if cx != 0
            let cx = state.reg_file.get(&RegName::Cx).unwrap() - 1;
            new_val = Some(cx);
            dest_target = Some(DestTarget::RegisterName(RegName::Cx));
            println!("loop: cx is now {cx}");
            // NOTE: We do NOT modify flags when modifying cs in loops
            if cx != 0 {
                jumped = handle_jmp_variants(jump_op, state, inst.ip_inc, inst.ip_abs);
            }
        }
        OpCodeType::Call => {
            // Push current IP onto the stack (memory access)
            stack_push(state.ip, &state.reg_file, &mut state.memory);
            // Change current IP to call target
            jumped = handle_jmp_variants(OpCodeType::Call, state, inst.ip_inc, inst.ip_abs);
        }
        OpCodeType::Int | OpCodeType::Int3 => {
            // Get the interrupt number
            let int_num = match (op_type, source_val) {
                (OpCodeType::Int3, _) => 3,
                (_, Some(immed8)) => immed8,
                _ => unreachable!(),
            };

            // Push flags
            let flags = flags_to_u16(&state.flags_reg);
            stack_push(flags, &state.reg_file, &mut state.memory);
            // Push CS
            (_, _) = decrement_sp(2, &mut state.reg_file);
            let cs = state.reg_file.get(&RegName::Cs).unwrap_or(&0);
            stack_push(*cs, &state.reg_file, &mut state.memory);
            // Push IP
            (_, new_sp) = decrement_sp(2, &mut state.reg_file);
            stack_push(state.ip, &state.reg_file, &mut state.memory);
            // Clear IF and TF
            state.flags_reg.interrupt_enable = false;
            state.flags_reg.trap = false;
            print_flags = true;
            // Get the new IP and CS from the interrupt vector table in memory
            let int_address_ip = int_num * 4;
            let int_address_cs = int_address_ip + 2;
            let int_ip = load_u16_from_mem(&state.memory, int_address_ip);
            let int_cs = load_u16_from_mem(&state.memory, int_address_cs);
            // Create the new absolute IP address to jump to
            let ip_abs = get_ip_absolute(Some(int_ip), Some(int_cs));
            // Jump to ip_abs
            jumped = handle_jmp_variants(op_type, state, None, ip_abs);
        }
        OpCodeType::Ret => {
            // The IP value to return to, stored at the top of the stack
            let ret_ip_addr = Some(stack_pop(&state.reg_file, &mut state.memory));
            // If ret has an optional pop, just add that to ret_ip_addr
            (old_sp, new_sp) = match source_val {
                // Ret addr + pop val
                Some(pop_val) => increment_sp(2 + pop_val, &mut state.reg_file),
                // Just increment for ret addr
                _ => increment_sp(2, &mut state.reg_file),
            };
            jumped = handle_jmp_variants(OpCodeType::Ret, state, None, ret_ip_addr);
        }
        OpCodeType::Push => {
            let (source_val, source_width) = match (source_val, source_width) {
                (Some(val), Some(width)) => (val, width),
                (None, _) => unreachable!("Push doesn't have a source!"),
                (_, None) => unreachable!("Push doesn't have a source width!"),
            };
            let source_val_sized = execute_mov(0xFFFF, source_val, WidthType::Word, source_width);
            stack_push(source_val_sized, &state.reg_file, &mut state.memory);
        }
        OpCodeType::Pushf => {
            let source_val = flags_to_u16(&state.flags_reg);
            // No need to resize source_val - word to word transfer
            stack_push(source_val, &state.reg_file, &mut state.memory);
        }
        OpCodeType::Pop => {
            let dest_val = match dest_val {
                Some(x) => x,
                None => unreachable!("Pop doesn't have a dest!"),
            };
            let popped_val = stack_pop(&state.reg_file, &mut state.memory);
            let popped_val_sized = execute_mov(dest_val, popped_val, dest_width, WidthType::Word);
            new_val = Some(popped_val_sized);
            (old_sp, new_sp) = increment_sp(2, &mut state.reg_file);
        }
        OpCodeType::Popf => {
            let popped_val = stack_pop(&state.reg_file, &mut state.memory);
            // No need to resize popped_val - word to word transfer
            new_val = Some(popped_val);
            dest_target = Some(DestTarget::FlagsReg);
            (old_sp, new_sp) = increment_sp(2, &mut state.reg_file);
        }
        OpCodeType::Imul => {
            // dest_val is really the single explicit source operand
            // AX/AL is an implicit operand
            let ax_val = *(state.reg_file.get(&RegName::Ax).unwrap_or(&0));
            // If the upper half of the result is not the sign extension of the
            // lower half of the result, CF and OF are set; otherwise they are
            // cleared.
            let (new_dx, new_ax, overflowed) = match (dest_val, dest_width) {
                (Some(val), WidthType::Word) => {
                    // Multiply AX with word-sized operand and store in DX:AX
                    let result = ((val as i16 as i32) * (ax_val as i16 as i32)) as u32;
                    let dx = (result >> 16) as u16;
                    let ax = result as u16;
                    let overflowed = (dx & 0x8000) != (ax & 0x8000);
                    (Some(dx), ax, overflowed)
                }
                (Some(val), WidthType::Hi8) => {
                    // Multiply AL with byte-sized operand and store in AX
                    let ax = (((val as i16) >> 8) * (ax_val as u8 as i8 as i16)) as u16;
                    let overflowed = (ax >> 8) & 0x80 != (ax & 0x80);
                    (None, ax, overflowed)
                }
                (Some(val), WidthType::Byte) => {
                    // Multiply AL with byte-sized operand and store in AX
                    let ax = ((val as u8 as i8 as i16) * (ax_val as u8 as i8 as i16)) as u16;
                    let overflowed = (ax >> 8) & 0x80 != (ax & 0x80);
                    (None, ax, overflowed)
                }
                (None, _) => unimplemented!("{op_type} is missing operand"),
            };

            // Store the new dx and ax values, as needed
            let mut temp = String::new();
            let old_ax = state.reg_file.insert(RegName::Ax, new_ax).unwrap_or(0);
            if old_ax != new_ax {
                temp.push_str(&format!(" {}:0x{:x}->0x{:x}", RegName::Ax, old_ax, new_ax));
            }
            match new_dx {
                Some(new_dx) => {
                    let old_dx = state.reg_file.insert(RegName::Dx, new_dx).unwrap_or(0);
                    if old_dx != new_dx {
                        temp.push_str(&format!(" {}:0x{:x}->0x{:x}", RegName::Dx, old_dx, new_dx));
                    }
                }
                _ => {}
            }
            if !temp.is_empty() {
                reg_change_str = Some(temp);
            }

            state.flags_reg.overflow = overflowed;
            // Carry is the same as overflow for imul
            state.flags_reg.carry = overflowed;
            print_flags = true;
        }
        // Handle all other non-special purpose ops here
        op @ _ => {
            // These all expect source and destinations to be set
            let dest_val = match dest_val {
                Some(val) => val,
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!(
                        "{} has no dest val: `{}`",
                        op_type,
                        inst.text.as_ref().unwrap()
                    );
                }
            };

            let (source_val, source_width) = match (source_val, source_width) {
                (Some(val), Some(width)) => (val, width),
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!(
                        "{} has no source val or width: `{}`",
                        op_type,
                        inst.text.as_ref().unwrap()
                    );
                }
            };

            // Figure out what part of the source value to put where, and which
            // bytes of the dest register to replace
            new_val = Some(match op {
                OpCodeType::Mov => execute_mov(dest_val, source_val, dest_width, source_width),
                op @ (OpCodeType::Add | OpCodeType::Sub | OpCodeType::Cmp) => {
                    let (result, overflowed, carry, aux_carry) =
                        execute_op_arith_flags(dest_val, source_val, dest_width, source_width, op);
                    new_val_overflowed = overflowed;
                    new_val_carry = carry;
                    new_val_aux_carry = aux_carry;
                    result
                }
                op @ (OpCodeType::Inc | OpCodeType::Dec) => {
                    let (result, overflowed, _, aux_carry) =
                        execute_op_arith_flags(dest_val, source_val, dest_width, source_width, op);
                    new_val_overflowed = overflowed;
                    new_val_aux_carry = aux_carry;
                    result
                }
                op @ (OpCodeType::And | OpCodeType::Test | OpCodeType::Xor) => {
                    execute_op(dest_val, source_val, dest_width, source_width, op)
                }
                op @ (OpCodeType::Shl | OpCodeType::Shr) => {
                    let (result, bit_shift_cnt) =
                        execute_shift(dest_val, source_val, dest_width, source_width, op);
                    shift_count = Some(bit_shift_cnt);
                    result
                }
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!(
                        "Execution of instruction `{}` is unimplemented",
                        inst.text.as_ref().unwrap()
                    );
                }
            });

            new_val_width = dest_width;

            // CMP and TEST do not store the result
            match op {
                OpCodeType::Cmp | OpCodeType::Test => dest_target = None,
                _ => {}
            }

            // Some insts should not modify flags
            match op {
                OpCodeType::Mov
                | OpCodeType::Push
                | OpCodeType::Pushf
                | OpCodeType::Pop
                | OpCodeType::Popf => {}
                _ => {
                    modify_flags = true;
                }
            }
        }
    }

    effect.push_str(inst.text.as_ref().unwrap());
    effect.push_str(" ;");

    // Print cycle estimation, if requested
    match settings.cycle_model {
        Some(cpu_type) => {
            if inst.clocks_base == 0 {
                println!("inst debug: {:#?}", inst);
                unimplemented!("Inst is missing cycles!: {}", inst.text.as_ref().unwrap());
            }
            let inst_total = get_total_clocks(inst, cpu_type, jumped, shift_count);
            state.total_cycles += inst_total;
            effect.push_str(&format!(" Clocks: +{inst_total} = {}", state.total_cycles));
            match get_total_clocks_str(inst, cpu_type, jumped, shift_count) {
                Some(str) => effect.push_str(&format!(" ({})", str)),
                None => {}
            }

            effect.push_str(" |");
        }
        _ => {} // Do nothing if not modeling cycles
    };

    match (modify_flags, new_val) {
        (true, Some(new_val)) => {
            // Set parity if even number of ones *in the bottom byte only*
            // https://open.substack.com/pub/computerenhance/p/simulating-add-jmp-and-cmp?r=leu8y&utm_campaign=comment-list-share-cta&utm_medium=web&comments=true&commentId=14205872
            state.flags_reg.parity = match new_val_width {
                WidthType::Hi8 => get_parity_u8(((new_val & 0xFF00) >> 8) as u8),
                // If not set explicitly, fall back to getting parity of u16
                _ => get_parity(new_val & 0xFF),
            };
            state.flags_reg.zero = match new_val_width {
                WidthType::Byte => (new_val & 0xFF) == 0,
                WidthType::Hi8 => (new_val & 0xFF00) == 0,
                _ => new_val == 0,
            };
            state.flags_reg.sign = match new_val_width {
                WidthType::Byte => (new_val & 0x80) == 0x80,
                // This works for u16 and hi u8
                _ => (new_val & 0x8000) == 0x8000,
            };
            state.flags_reg.overflow = new_val_overflowed;
            state.flags_reg.carry = new_val_carry;
            state.flags_reg.auxiliary_carry = new_val_aux_carry;
        }
        // No new val to process
        _ => {}
    }

    // Append reg changes
    match reg_change_str {
        Some(string) => effect.push_str(&string),
        _ => {}
    }

    // Store the new value somewhere
    match (&dest_target, new_val) {
        (Some(DestTarget::RegisterName(reg_name)), Some(new_val)) => {
            // Store new val in the dest register
            let old_val = state.reg_file.insert(*reg_name, new_val).unwrap_or(0);
            if old_val != new_val {
                effect.push_str(&format!(" {}:0x{:x}->0x{:x}", reg_name, old_val, new_val));
            }
        }
        (Some(DestTarget::MemAddress(addr)), Some(new_val)) => {
            store_u16_in_mem(&mut state.memory, *addr, new_val);
            // Don't print out memory changes (yet)
        }
        (Some(DestTarget::FlagsReg), Some(new_val)) => {
            state.flags_reg = u16_to_flags(new_val);
            print_flags = true;
        }
        (None, _) => {} // Nothing is stored back into destination
        _ => {}
    }

    match (old_sp, new_sp) {
        (Some(old_val), Some(new_val)) => {
            effect.push_str(&format!(" sp:0x{:x}->0x{:x}", old_val, new_val))
        }
        _ => {}
    }

    if !settings.no_ip {
        // Tack on the IP change to the instruction effect string
        effect.push_str(&format!(" ip:0x{:x}->0x{:x}", current_ip, state.ip));
    }

    // Print change in flags register, if needed
    if (modify_flags || print_flags) && (old_flags != state.flags_reg) {
        effect.push_str(&format!(" flags:{}->{}", old_flags, state.flags_reg));
    }

    return (effect, false);
}

// Push an item onto the stack (decrement of the stack is assumed to have
// happened before this call).
fn stack_push(val: u16, reg_file: &BTreeMap<RegName, u16>, memory: &mut Vec<u8>) {
    let sp_val = match reg_file.get(&RegName::Sp) {
        Some(x) => *x,
        None => 0,
    };
    store_u16_in_mem(memory, sp_val as usize, val);
    println!("Push onto stack: 0x{val:x} at addr 0x{sp_val:x}");
}

// Pop an item off the stack (increment of the stack is assumed to happen after
// this call).
fn stack_pop(reg_file: &BTreeMap<RegName, u16>, memory: &mut Vec<u8>) -> u16 {
    let sp_val = match reg_file.get(&RegName::Sp) {
        Some(x) => *x,
        None => 0,
    };
    let val = load_u16_from_mem(memory, sp_val);
    println!("Pop off stack: 0x{val:x} from addr 0x{sp_val:x}");
    val
}

/// Decrement SP by 2, then return the old and new SP values
fn get_sp(reg_file: &BTreeMap<RegName, u16>) -> u16 {
    match reg_file.get(&RegName::Sp) {
        Some(x) => *x,
        None => 0,
    }
}

/// Decrement SP, then return the old and new SP values
fn decrement_sp(count: u16, reg_file: &mut BTreeMap<RegName, u16>) -> (Option<u16>, Option<u16>) {
    let old_sp = match reg_file.get(&RegName::Sp) {
        Some(x) => *x,
        None => 0,
    };
    let (new_sp, overflowed) = old_sp.overflowing_sub(count);
    println!("Decrementing SP by {count}: 0x{old_sp:x} -> 0x{new_sp:x}");
    if overflowed {
        println!("Overflow!")
    }
    reg_file.insert(RegName::Sp, new_sp).unwrap_or(0);
    (Some(old_sp), Some(new_sp))
}

/// Increment SP, then return the old and new SP values
fn increment_sp(count: u16, reg_file: &mut BTreeMap<RegName, u16>) -> (Option<u16>, Option<u16>) {
    let old_sp = match reg_file.get(&RegName::Sp) {
        Some(x) => *x,
        None => 0,
    };
    let (new_sp, overflowed) = old_sp.overflowing_add(count);
    println!("Incrementing SP by {count}: 0x{old_sp:x} -> 0x{new_sp:x}");
    if overflowed {
        println!("Overflow!")
    }
    reg_file.insert(RegName::Sp, new_sp).unwrap_or(0);
    println!("Old val: {old_sp}");
    (Some(old_sp), Some(new_sp))
}

/// Get the final source value for most instructions
fn get_source_val(
    inst: &InstType,
    state: &CpuStateType,
    mem_addr_src: Option<u16>,
    transfer_width: WidthType,
) -> (Option<u16>, Option<WidthType>) {
    // Get the op's source val either from a source reg or an immediate
    match (
        inst.source_reg,
        mem_addr_src,
        inst.add_data_to,
        inst.source_hardcoded,
    ) {
        (_, Some(address), _, _) => {
            let source_val = load_u16_from_mem(&state.memory, address);
            let source_width = transfer_width;
            (Some(source_val), Some(source_width))
        }
        // Handle source reg to dest reg
        (Some(source_reg), _, _, _) => {
            // Get the value of the source register
            let source_val = match state.reg_file.get(&source_reg.name) {
                Some(x) => *x,
                None => 0,
            };
            (Some(source_val), Some(source_reg.width))
        }
        (_, _, Some(AddTo::Source), _) => {
            // Use data as an immediate source value
            let source_width = transfer_width;
            (Some(inst.data_value.unwrap()), Some(source_width))
        }
        (_, _, _, Some(source_hardcoded_val)) => {
            // Use the hardcoded source value
            (Some(source_hardcoded_val), Some(transfer_width))
        }
        _ => (None, None),
    }
}

/// Get the final destination value for most instructions
fn get_dest_val(
    inst: &InstType,
    state: &CpuStateType,
    mem_addr_dst: Option<u16>,
) -> (Option<u16>, Option<DestTarget>) {
    // Get the op's destination reg and its current value
    match (inst.dest_reg, mem_addr_dst) {
        (Some(dest_reg), None) => {
            println!("Dest is reg!");
            // Note: This also currently covers ModRmDataType::Reg(_)
            // Get the value of the dest register
            let dest_val = match state.reg_file.get(&dest_reg.name) {
                Some(x) => *x,
                None => 0,
            };
            let dest_target = DestTarget::RegisterName(dest_reg.name);
            (Some(dest_val), Some(dest_target))
        }
        (_, Some(address)) => {
            println!("Dest is mem addr!");
            let dest_target = DestTarget::MemAddress(address as usize);
            let dest_val = load_u16_from_mem(&state.memory, address);
            (Some(dest_val), Some(dest_target))
        }
        // Immediate values can't be a destination
        _ => (None, None),
    }
}

/// Convert the flags reg into a single u16 value
fn flags_to_u16(flags_reg: &FlagsRegType) -> u16 {
    ((flags_reg.carry as u16) << 0)
        | ((flags_reg.parity as u16) << 2)
        | ((flags_reg.auxiliary_carry as u16) << 4)
        | ((flags_reg.zero as u16) << 6)
        | ((flags_reg.sign as u16) << 7)
        | ((flags_reg.trap as u16) << 8)
        | ((flags_reg.interrupt_enable as u16) << 9)
        | ((flags_reg.direction as u16) << 10)
        | ((flags_reg.overflow as u16) << 11)
}

/// Convert a u16 into a FlagsRegType
fn u16_to_flags(flags_reg_val: u16) -> FlagsRegType {
    FlagsRegType {
        carry: (flags_reg_val & (0x1 << 0)) != 0,
        parity: (flags_reg_val & (0x1 << 2)) != 0,
        auxiliary_carry: (flags_reg_val & (0x1 << 4)) != 0,
        zero: (flags_reg_val & (0x1 << 6)) != 0,
        sign: (flags_reg_val & (0x1 << 7)) != 0,
        trap: (flags_reg_val & (0x1 << 8)) != 0,
        interrupt_enable: (flags_reg_val & (0x1 << 9)) != 0,
        direction: (flags_reg_val & (0x1 << 10)) != 0,
        overflow: (flags_reg_val & (0x1 << 11)) != 0,
    }
}

/// Determine if this byte should have the parity flag set
fn get_parity_u8(val: u8) -> bool {
    (val.count_ones() & 0x1) == 0x0
}

/// Determine if this word should have the parity flag set
fn get_parity(val: u16) -> bool {
    (val.count_ones() & 0x1) == 0x0
}

/// Get the source and memory addresses for this instruction (if any)
fn get_inst_mem_addrs(inst: &InstType, state: &CpuStateType) -> (Option<u16>, Option<u16>) {
    let (mut mem_addr_src, mut mem_addr_dst) = (None, None);

    match (inst.mem_access, inst.add_data_to) {
        (true, Some(AddTo::Source)) => mem_addr_src = inst.data_value,
        (true, Some(AddTo::Dest)) => mem_addr_dst = inst.data_value,
        _ => {}
    }

    // Consolidate parts of the inst into a single memory address
    let ea = match inst.mod_rm_data {
        Some(mod_rm_data) => get_effective_addr(mod_rm_data, inst.disp_value, &state.reg_file),
        _ => None,
    };

    match (ea, inst.add_mod_rm_mem_to) {
        (Some(_), Some(AddTo::Source)) => {
            if mem_addr_src.is_some() {
                unreachable!("Src mem addr was already set! Something is wrong...")
            }
            mem_addr_src = ea
        }
        (Some(_), Some(AddTo::Dest)) => {
            if mem_addr_dst.is_some() {
                unreachable!("Dest mem addr was already set! Something is wrong...")
            }
            mem_addr_dst = ea
        }
        (Some(_), _) => unimplemented!("No add_mod_rm_mem_to set!"),
        _ => {}
    }

    (mem_addr_src, mem_addr_dst)
}

/// Convert the mod_rm and displacement bytes into a memory address (the single
/// effective address) and also return the cycles taken to calculate the
/// address. The EA address calculation is the same for both 8086 and 8088. See
/// table 2-20 for clock information.
/// TODO: Add 2 clocks for segment override when segments are implemented
/// See Decode::get_mod_rm_addr_str()
fn get_effective_addr(
    mod_rm_data: ModRmDataType,
    disp: Option<i16>,
    reg_file: &BTreeMap<RegName, u16>,
) -> Option<u16> {
    let (result, overflowed) = match (mod_rm_data, disp) {
        (ModRmDataType::MemDirectAddr, Some(address)) => (address as u16, false),
        (ModRmDataType::MemReg(reg), _) => {
            // Get reg value and return
            (*reg_file.get(&reg.name).unwrap_or(&0), false)
        }
        (ModRmDataType::MemRegReg(reg1, reg2), _) => {
            // i.e. base + index
            let val1 = *reg_file.get(&reg1.name).unwrap_or(&0);
            let val2 = *reg_file.get(&reg2.name).unwrap_or(&0);
            val1.overflowing_add(val2)
        }
        (ModRmDataType::MemRegDisp(_), None) => {
            unreachable!("ERROR: No displacement found for MemRegDisp")
        }
        (ModRmDataType::MemRegDisp(reg), Some(disp)) => {
            let val = *reg_file.get(&reg.name).unwrap_or(&0);
            // Disp is signed, so do signed addition to not trigger overflow warning
            let (result, overflowed) = (val as i16).overflowing_add(disp);
            (result as u16, overflowed)
        }
        (ModRmDataType::MemRegRegDisp(_, _), None) => {
            unreachable!("ERROR: No displacement found for MemRegRegDisp")
        }
        (ModRmDataType::MemRegRegDisp(reg1, reg2), Some(disp)) => {
            // Get reg 1 + 2, add together with disp, and return
            let val1 = *reg_file.get(&reg1.name).unwrap_or(&0);
            let val2 = *reg_file.get(&reg2.name).unwrap_or(&0);
            let (temp, overflowed) = val1.overflowing_add(val2);
            // Disp is signed, so do signed addition to not trigger overflow warning
            let (temp2, overflowed2) = (temp as i16).overflowing_add(disp);
            (temp2 as u16, overflowed || overflowed2)
        }
        // Type Reg isn't a mem access, so this will be handled via source and
        // dest registers
        (ModRmDataType::Reg(_), _) => return None,
        _ => unreachable!(),
    };
    if overflowed {
        println!(
            "Get effective addr calculation overflowed! {:?}, {:?}, result={result}(0x{result:x})",
            mod_rm_data, disp
        );
    }
    Some(result)
}

/// Given the memory array and a 16-bit address, return a 16-bit value
fn load_u16_from_mem(memory: &Vec<u8>, address: u16) -> u16 {
    // Get the value from memory. Remember, it's little endian!
    // Cast the individual u8 bytes to u16
    let lower = memory[address as usize] as u16;
    let upper = memory[(address + 1) as usize] as u16;
    // Combine into a single u16 value
    let val = lower | (upper << 8);
    println!("Loaded 0x{val:x} from 0x{address:x}");
    val
}

/// Given the memory array and a 16-bit address, return a 16-bit value
fn store_u16_in_mem(memory: &mut Vec<u8>, address: usize, new_val: u16) {
    // Store 16-bit value in little endian order
    memory[address] = (new_val & 0x00FF) as u8;
    memory[address + 1] = ((new_val & 0xFF00) >> 8) as u8;
    println!("Stored 0x{new_val:x} at 0x{address:x}");
}

/// Handle the logic for the given jump op code. Modify the IP register in the
/// CPU state. If the jump op jumped, then return true. Otherwise, return false.
fn handle_jmp_variants(
    jump_op: OpCodeType,
    state: &mut CpuStateType,
    ip_inc: Option<i16>,
    ip_abs: Option<u16>,
) -> bool {
    println!("state.ip before: {}", state.ip);

    // See table 2-15 on pg. 2-46 of the 8086 documentation
    let jump = match jump_op {
        OpCodeType::Jne => !state.flags_reg.zero,
        // je/jz - jump equal/zero
        OpCodeType::Je => state.flags_reg.zero,
        // jb/jnae/jc - jump below/not above or equal/carry
        OpCodeType::Jb => state.flags_reg.carry,
        OpCodeType::Jbe => !state.flags_reg.carry,
        // NOTE: For loops, cx was already checked to be != 0
        OpCodeType::Jg => {
            (state.flags_reg.sign ^ state.flags_reg.overflow) || state.flags_reg.zero == false
        }
        OpCodeType::Jnl => (state.flags_reg.sign ^ state.flags_reg.overflow) == false,
        OpCodeType::Jl => state.flags_reg.sign ^ state.flags_reg.overflow,
        OpCodeType::Jle => {
            (state.flags_reg.sign ^ state.flags_reg.overflow) || state.flags_reg.zero
        }
        OpCodeType::Jno => !state.flags_reg.overflow,
        OpCodeType::Jnp => !state.flags_reg.parity,
        OpCodeType::Jns => !state.flags_reg.sign,
        OpCodeType::Jo => state.flags_reg.overflow,
        OpCodeType::Js => state.flags_reg.sign,
        OpCodeType::Loopnz => !state.flags_reg.zero,
        OpCodeType::Loopz => state.flags_reg.zero,
        OpCodeType::Loop => true, // Only cx != 0
        OpCodeType::Jp => state.flags_reg.parity,
        OpCodeType::Jmp => true,
        OpCodeType::Call => true,
        OpCodeType::Ret => true,
        OpCodeType::Int => true,
        OpCodeType::Int3 => true,
        x @ _ => unimplemented!("Unimplemented jump variant {x}"),
    };
    let overflowed = match (jump, ip_inc, ip_abs) {
        (true, Some(ip_inc), _) => {
            // Add the IP increment value to IP
            let (result, overflowed) = (state.ip as i16).overflowing_add(ip_inc);
            state.ip = result as u16;
            overflowed
        }
        (true, _, Some(ip_abs)) => {
            // Replace the IP entirely. No overflow possible.
            state.ip = ip_abs;
            false
        }
        _ => false,
    };
    println!("state.ip after: {}", state.ip);
    if overflowed {
        println!("Warning: IP register overflowed!: IP: {}", state.ip);
    }
    jump
}

/// Advance the IP by modifying ip in the CPU state object according to the
/// instruction length.
///
/// inst: The instruction that was executed.
/// cpu_state: The CPU state object, which contains the IP.
///
/// Returns a string indicating the change in IP.
fn advance_ip_reg(inst: &InstType, cpu_state: &mut CpuStateType) {
    // Get executed instruction's length
    let inst_length = inst.processed_bytes.len() as u16;
    // Get the current value for IP
    let current_ip = cpu_state.ip;
    // Advance IP according to the length of this instruction
    cpu_state.ip = current_ip + inst_length;
}

/// Print the final CPU state.
/// If `no_ip` is specified, don't print out the IP register.
pub fn print_final_state(state: &CpuStateType, no_ip: bool) -> Vec<String> {
    let mut lines = vec![];
    let ax_val = state.reg_file.get(&RegName::Ax).unwrap_or(&0);
    let bx_val = state.reg_file.get(&RegName::Bx).unwrap_or(&0);
    let cx_val = state.reg_file.get(&RegName::Cx).unwrap_or(&0);
    let dx_val = state.reg_file.get(&RegName::Dx).unwrap_or(&0);
    let sp_val = state.reg_file.get(&RegName::Sp).unwrap_or(&0);
    let bp_val = state.reg_file.get(&RegName::Bp).unwrap_or(&0);
    let si_val = state.reg_file.get(&RegName::Si).unwrap_or(&0);
    let di_val = state.reg_file.get(&RegName::Di).unwrap_or(&0);
    let es_val = state.reg_file.get(&RegName::Es).unwrap_or(&0);
    let ss_val = state.reg_file.get(&RegName::Ss).unwrap_or(&0);
    let ds_val = state.reg_file.get(&RegName::Ds).unwrap_or(&0);

    lines.push(format!(""));
    lines.push(format!("Final registers:"));
    if *ax_val != 0 {
        lines.push(format!("      ax: 0x{:04x} ({})", ax_val, ax_val));
    }
    if *bx_val != 0 {
        lines.push(format!("      bx: 0x{:04x} ({})", bx_val, bx_val));
    }
    if *cx_val != 0 {
        lines.push(format!("      cx: 0x{:04x} ({})", cx_val, cx_val));
    }
    if *dx_val != 0 {
        lines.push(format!("      dx: 0x{:04x} ({})", dx_val, dx_val));
    }
    if *sp_val != 0 {
        lines.push(format!("      sp: 0x{:04x} ({})", sp_val, sp_val));
    }
    if *bp_val != 0 {
        lines.push(format!("      bp: 0x{:04x} ({})", bp_val, bp_val));
    }
    if *si_val != 0 {
        lines.push(format!("      si: 0x{:04x} ({})", si_val, si_val));
    }
    if *di_val != 0 {
        lines.push(format!("      di: 0x{:04x} ({})", di_val, di_val));
    }
    if *es_val != 0 {
        lines.push(format!("      es: 0x{:04x} ({})", es_val, es_val));
    }
    if *ss_val != 0 {
        lines.push(format!("      ss: 0x{:04x} ({})", ss_val, ss_val));
    }
    if *ds_val != 0 {
        lines.push(format!("      ds: 0x{:04x} ({})", ds_val, ds_val));
    }
    if state.ip != 0 && !no_ip {
        lines.push(format!("      ip: 0x{:04x} ({})", state.ip, state.ip));
    }
    let default_flags = FlagsRegType {
        ..Default::default()
    };
    if state.flags_reg != default_flags {
        lines.push(format!("   flags: {}", state.flags_reg));
    }
    lines.push(format!(""));
    lines
}

fn execute_mov(
    dest_val: u16,
    source_val: u16,
    dest_width: WidthType,
    source_width: WidthType,
) -> u16 {
    match (dest_width, source_width) {
        (WidthType::Word, WidthType::Word) => source_val,
        (WidthType::Byte, WidthType::Byte) => (dest_val & 0xFF00) | (source_val & 0xFF),
        (WidthType::Byte, WidthType::Hi8) => (dest_val & 0xFF00) | ((source_val & 0xFF00) >> 8),
        (WidthType::Hi8, WidthType::Byte) => (dest_val & 0xFF) | ((source_val & 0xFF) << 8),
        (WidthType::Hi8, WidthType::Hi8) => (dest_val & 0xFF00) | (source_val & 0xFF),
        _ => unimplemented!(),
    }
}

/// Execute a shift OpType. Return the number of bit shifts executed.
/// The shift count will just be whatever the src value is, whether it's a
/// hardcoded value of 1 or the CL register.
fn execute_shift(
    dst: u16,
    src: u16,
    dst_width: WidthType,
    src_width: WidthType,
    op: OpCodeType,
) -> (u16, u16) {
    let result = execute_op(dst, src, dst_width, src_width, op);
    let shift_count = src & 0xFF;
    (result, shift_count)
}

/// Execute an OpCodeType and return the result.
fn execute_op(
    dst: u16,
    src: u16,
    dst_width: WidthType,
    src_width: WidthType,
    op: OpCodeType,
) -> u16 {
    // The current assumption is that src and dst are sized the same
    match dst_width {
        // Operate on two words
        WidthType::Word => {
            // We are discarding the overflow result because that is not the same as the
            // 8086 overflow flag. Rust's overflow result seems to only care about
            // unsigned wrapping from 0x0000 to 0xFFFF or vice versa, and not if the
            // sign value wraps from negative to positive.
            let result = match op {
                OpCodeType::Add | OpCodeType::Inc => {
                    let (result, _) = dst.overflowing_add(src);
                    result
                }
                OpCodeType::Sub | OpCodeType::Cmp | OpCodeType::Dec => {
                    let (result, _) = dst.overflowing_sub(src);
                    result
                }
                OpCodeType::And | OpCodeType::Test => dst & src,
                OpCodeType::Xor => dst ^ src,
                OpCodeType::Shl => dst << src,
                OpCodeType::Shr => dst >> src,
                _ => unimplemented!(),
            };
            println!(
                "WORD {op:?}: dest={dst} (0x{dst:x}), src={src} (0x{src:x}), result={result} (0x{result:04x})"
            );
            result
        }
        // Operate on two bytes
        WidthType::Byte | WidthType::Hi8 => {
            // Get the bytes from src and dst that we want to operate on
            let dst_u8 = match dst_width {
                WidthType::Byte => (dst & 0xFF) as u8,
                WidthType::Hi8 => ((dst & 0xFF00) >> 8) as u8,
                _ => unreachable!(),
            };
            let src_u8 = match src_width {
                WidthType::Byte => (src & 0xFF) as u8,
                WidthType::Hi8 => ((src & 0xFF00) >> 8) as u8,
                _ => unreachable!(),
            };

            let result = match op {
                OpCodeType::Add | OpCodeType::Inc => {
                    let (result, _) = dst_u8.overflowing_add(src_u8);
                    result
                }
                OpCodeType::Sub | OpCodeType::Cmp => {
                    let (result, _) = dst_u8.overflowing_sub(src_u8);
                    result
                }
                OpCodeType::And | OpCodeType::Test => dst_u8 & src_u8,
                OpCodeType::Xor => dst_u8 ^ src_u8,
                OpCodeType::Shl => dst_u8 << src_u8,
                OpCodeType::Shr => dst_u8 >> src_u8,
                _ => unimplemented!(),
            };

            let merged_result = match dst_width {
                WidthType::Byte => result as u16 | (dst & 0xFF00),
                WidthType::Hi8 => (result as u16) << 8 | (dst & 0xFF),
                _ => unreachable!(),
            };

            println!("BYTE {op:?}: dest={dst_u8} (0x{dst_u8:x}), src={src_u8} (0x{src_u8:x}), result={merged_result} (0x{merged_result:04x})");
            // Merge the result back into the bottom byte of the destination
            merged_result
        }
    }
}

/// Do an arithmetic operation between a src and dst.
///
/// If the operation is to be done between single bytes of the u16 word, then
/// the byte location is specified with dst_width and src_width.
///
/// 8086 defines add overflow as the sign bit of the destination changing.
/// This is true with 0x7FFF + 0x0001, but also true with 0xFFFF + 0x0001.
/// The overflow flag will still be set even if the user is intending to
/// do unsigned arithmetic. The bits are the same. See [FlagsRegType::overflow].
fn execute_op_arith_flags(
    dst: u16,
    src: u16,
    dst_width: WidthType,
    src_width: WidthType,
    op: OpCodeType,
) -> (u16, bool, bool, bool) {
    println!("Dest: 0x{dst:X}:{dst_width:?}; Source: 0x{src:X}:{src_width:?}");
    let dst_sign_bit = match dst_width {
        WidthType::Word | WidthType::Hi8 => (dst & 0x8000) != 0,
        WidthType::Byte => (dst & 0x80) != 0,
    };
    let src_sign_bit = match src_width {
        WidthType::Word | WidthType::Hi8 => (src & 0x8000) != 0,
        WidthType::Byte => (src & 0x80) != 0,
    };

    let result = execute_op(dst, src, dst_width, src_width, op);
    let result_sign_bit = match dst_width {
        WidthType::Word | WidthType::Hi8 => (result & 0x8000) != 0,
        WidthType::Byte => (result & 0x80) != 0,
    };

    let overflow = match op {
        // Add overflow cannot occur when the sign bits differ between operands.
        // If the two operands have the same sign bit, then overflow occurs if
        // the result does not have that same sign bit.
        // See "Overflow Rule for addition" in https://www.doc.ic.ac.uk/~eedwards/compsys/arithmetic/index.html
        OpCodeType::Add | OpCodeType::Inc => {
            (dst_sign_bit == src_sign_bit) && (dst_sign_bit != result_sign_bit)
        }
        // Since we're subtracting, left and right sign must be opposite for
        // overflow to occur.
        OpCodeType::Sub | OpCodeType::Cmp | OpCodeType::Dec => {
            (dst_sign_bit != src_sign_bit) && (dst_sign_bit != result_sign_bit)
        }
        _ => unimplemented!(),
    };
    let carry = match op {
        OpCodeType::Add | OpCodeType::Inc => calc_carry_add(dst, src, dst_width, src_width),
        OpCodeType::Sub | OpCodeType::Cmp | OpCodeType::Dec => {
            calc_carry_sub(dst, src, dst_width, src_width)
        }
        _ => unimplemented!(),
    };
    let aux_carry = match op {
        OpCodeType::Add | OpCodeType::Inc => calc_aux_carry_add(dst, src, dst_width, src_width),
        OpCodeType::Sub | OpCodeType::Cmp | OpCodeType::Dec => {
            calc_aux_carry_sub(dst, src, dst_width, src_width)
        }
        _ => unimplemented!(),
    };
    if overflow {
        println!("overflow!")
    }
    if carry {
        println!("carry!")
    }
    if aux_carry {
        println!("aux_carry!")
    }
    (result, overflow, carry, aux_carry)
}

/// If an addition results in a carry out of the high-order bit of the result,
/// then CF is set.
const fn calc_carry_add(dst: u16, src: u16, dst_width: WidthType, src_width: WidthType) -> bool {
    match (dst_width, src_width) {
        (WidthType::Word, _) => (dst as u32) + (src as u32) > 0xFFFF,
        (WidthType::Byte, WidthType::Byte) => (dst & 0xFF) + (src & 0xFF) > 0xFF,
        (WidthType::Byte, WidthType::Hi8) => (dst & 0xFF) + (src >> 8) > 0xFF,
        (WidthType::Hi8, WidthType::Byte) => (dst >> 8) + (src & 0xFF) > 0xFF,
        (WidthType::Hi8, WidthType::Hi8) => (dst >> 8) + (src >> 8) > 0xFF,
        _ => unreachable!(),
    }
}

/// If an addition results in a carry out of the low-order half-byte of the
/// result, then AF is set.
const fn calc_aux_carry_add(
    dst: u16,
    src: u16,
    dst_width: WidthType,
    src_width: WidthType,
) -> bool {
    match (dst_width, src_width) {
        (WidthType::Word | WidthType::Byte, WidthType::Word | WidthType::Byte) => {
            (dst & 0xF) + (src & 0xF) > 0xF
        }
        (WidthType::Byte, WidthType::Hi8) => (dst & 0xF) + ((src & 0x0F00) >> 8) > 0xF,
        (WidthType::Hi8, WidthType::Byte) => ((dst & 0x0F00) >> 8) + (src & 0xF) > 0xF,
        (WidthType::Hi8, WidthType::Hi8) => (dst & 0x0F00) + (src & 0x0F00) > 0x0F00,
        _ => unreachable!(),
    }
}

/// If a subtraction results in a borrow into the high-order bit of the result,
/// then CF is set.
const fn calc_carry_sub(dst: u16, src: u16, dst_width: WidthType, src_width: WidthType) -> bool {
    match (dst_width, src_width) {
        (WidthType::Word, _) => src > dst,
        (WidthType::Byte, WidthType::Byte) => src & 0xFF > dst & 0xFF,
        (WidthType::Byte, WidthType::Hi8) => ((src & 0xFF00) >> 8) > dst & 0xFF,
        (WidthType::Hi8, WidthType::Byte) => src & 0xFF > ((dst & 0xFF00) >> 8),
        (WidthType::Hi8, WidthType::Hi8) => src & 0xFF00 > dst & 0xFF00,
        _ => unreachable!(),
    }
}

/// If a subtraction results in a borrow into the low-order half-byte of the
/// result, then AF is set.
const fn calc_aux_carry_sub(
    dst: u16,
    src: u16,
    dst_width: WidthType,
    src_width: WidthType,
) -> bool {
    match (dst_width, src_width) {
        (WidthType::Word | WidthType::Byte, WidthType::Word | WidthType::Byte) => {
            (src & 0xF) > (dst & 0xF)
        }
        (WidthType::Byte, WidthType::Hi8) => ((src & 0x0F00) >> 8) > (dst & 0xF),
        (WidthType::Hi8, WidthType::Byte) => (src & 0xF) > ((dst & 0x0F00) >> 8),
        (WidthType::Hi8, WidthType::Hi8) => (src & 0x0F00) > (dst & 0x0F00),
        _ => unreachable!(),
    }
}
