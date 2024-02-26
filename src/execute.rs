//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;
use std::fmt;

use crate::cycles::{
    calculate_8086_unaligned_access, get_total_clocks, get_total_clocks_str, print_inst_clock_debug,
};
use crate::decode::{
    AddTo, ExecuteSettings, InstType, ModRmDataType, OpCodeType, RegName, WidthType,
};

const MEMORY_SIZE: usize = 1024 * 1024;

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

pub fn init_state(init_ip: Option<u16>) -> CpuStateType {
    let ip = match init_ip {
        Some(x) => x,
        None => 0,
    };
    CpuStateType {
        // Initialize the memory array to 1 MB
        memory: vec![0; MEMORY_SIZE],
        ip: ip,
        ..Default::default()
    }
}

/// An enum representing either an immediate, an address, or a register.
/// Note: an immediate value can't be a dest, so always assume that the data
/// bytes for a destination are always for an address.
enum Target {
    MemAddress(usize),
    RegisterName(RegName),
    /// No target - don't store the result at dest or load from this source.
    None,
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
    let new_val;
    let mut dest_target;
    // Set this var if we should set the flags reg at the end
    let mut modify_flags = false;
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

    // "While an instruction is executing, IP refers to the next instruction."
    // BYU RTOS Website, 8086InstructionSet.html
    // So, always advance the IP before executing each instruction so that it
    // points to the next instruction.
    advance_ip_reg(inst, state);

    // Figure out destination width, which will dictate how wide the data
    // transfer is for this instruction
    let dest_width = match (
        inst.dest_reg,
        inst.dest_width,
        inst.source_reg,
        inst.source_width,
    ) {
        (Some(dest_reg), _, _, _) => Some(dest_reg.width),
        (_, Some(dest_width), _, _) => Some(dest_width),
        (_, _, Some(source_reg), _) => Some(source_reg.width),
        (_, _, _, Some(source_width)) => Some(source_width),
        _ => None,
    };

    // Is it a byte that is going from src to dst, or a word?
    let transfer_width = match dest_width {
        Some(WidthType::Word) | None => WidthType::Word,
        Some(WidthType::Hi8 | WidthType::Byte) => WidthType::Byte,
    };

    // Get the final memory address, if applicable, whether it's an effective
    // address or straight address literal.
    let (mem_addr, add_mem_to) = get_inst_mem_addr(inst, state);

    // Now that we have the final memory address, if any, we can check it to see
    // if there are any unaligned word mem access penalties for the 8086
    inst.mem_access_word_unaligned =
        calculate_8086_unaligned_access(mem_addr, transfer_width, inst.transfers);

    // Print this instruction's clock debug info now that all clock data is set
    print_inst_clock_debug(inst);

    // SUB/CMP: The source operand is subtracted from the destination operand,
    // and the result replaces the destination operand.

    // ADD: The source operand is added to the destination operand,
    // and the result replaces the destination operand.

    // SUB/CMP/ADD updates flags AF, CF, OF, PF, SF, and ZF.
    // MOV does not update any flags.

    match op_type {
        jump_op @ (OpCodeType::Jne | OpCodeType::Je | OpCodeType::Jb | OpCodeType::Jp) => {
            new_val = None;
            dest_target = Target::None;
            jumped = handle_jmp_variants(jump_op, inst, state);
        }
        jump_op @ (OpCodeType::Loopnz | OpCodeType::Loopz | OpCodeType::Loop) => {
            // pg 2-45 - 2-46
            // Decrement cx by 1 and jump if cx != 0
            let cx = state.reg_file.get(&RegName::Cx).unwrap() - 1;
            new_val = Some(cx);
            dest_target = Target::RegisterName(RegName::Cx);
            println!("loop: cx is now {cx}");
            // NOTE: We do NOT modify flags when modifying cs in loops
            if cx != 0 {
                jumped = handle_jmp_variants(jump_op, inst, state);
            }
        }
        OpCodeType::Ret => {
            unimplemented!("The Ret instruction isn't yet implemented",);
        }
        // Handle all other non-special purpose ops here
        op @ _ => {
            let dest_width = match dest_width {
                Some(x) => x,
                None => unimplemented!("No dest reg, dest width, source reg, or source width"),
            };
            let dest_val;

            // Get the op's destination reg and its current value
            match (inst.dest_reg, add_mem_to) {
                (Some(dest_reg), _) => {
                    // Note: This also currently covers ModRmDataType::Reg(_)
                    // Get the value of the dest register
                    dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    dest_target = Target::RegisterName(dest_reg.name);
                }
                (_, Some(AddTo::Dest)) => {
                    let address = mem_addr.unwrap();
                    dest_target = Target::MemAddress(address as usize);
                    dest_val = load_u16_from_mem(&state.memory, address);
                }
                // Immediate values can't be a destination
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!("{op} has no dest: `{}`", inst.text.as_ref().unwrap())
                }
            };

            // Get the op's source val either from a source reg or an immediate
            let (source_val, source_width) = match (
                inst.source_reg,
                add_mem_to,
                inst.add_data_to,
                inst.source_hardcoded,
            ) {
                // Handle source reg to dest reg
                (Some(source_reg), _, _, _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    (source_val, source_reg.width)
                }
                (_, Some(AddTo::Source), _, _) => {
                    let source_val = load_u16_from_mem(&state.memory, mem_addr.unwrap());
                    let source_width = transfer_width;
                    (source_val, source_width)
                }
                (_, _, Some(AddTo::Source), _) => {
                    // Use data as an immediate source value
                    let source_width = transfer_width;
                    (inst.data_value.unwrap(), source_width)
                }
                (_, _, _, Some(source_hardcoded_val)) => {
                    // Use the hardcoded source value
                    (source_hardcoded_val, transfer_width)
                }
                _ => {
                    // No source is found!
                    println!("inst debug: {:#?}", inst);
                    unimplemented!("{op} has no source: `{}`", inst.text.as_ref().unwrap());
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
                OpCodeType::Cmp | OpCodeType::Test => dest_target = Target::None,
                _ => {}
            }

            // MOV does not modify flags, but the others do
            if op != OpCodeType::Mov {
                modify_flags = true;
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

    // Store the new value somewhere
    match (dest_target, new_val) {
        (Target::RegisterName(reg_name), Some(new_val)) => {
            // Store new val in the dest register
            let old_val = state.reg_file.insert(reg_name, new_val).unwrap_or(0);
            if old_val != new_val {
                effect.push_str(&format!(" {}:0x{:x}->0x{:x}", reg_name, old_val, new_val));
            }
        }
        (Target::MemAddress(addr), Some(new_val)) => {
            store_u16_in_mem(&mut state.memory, addr, new_val);
            // Don't print out memory changes (yet)
        }
        (Target::None, _) => {} // Nothing is stored back into destination
        _ => {}
    }

    if !settings.no_ip {
        // Tack on the IP change to the instruction effect string
        effect.push_str(&format!(" ip:0x{:x}->0x{:x}", current_ip, state.ip));
    }

    // Print change in flags register, if needed
    if modify_flags && (old_flags != state.flags_reg) {
        effect.push_str(&format!(" flags:{}->{}", old_flags, state.flags_reg));
    }

    return (effect, false);
}

/// Determine if this byte should have the parity flag set
fn get_parity_u8(val: u8) -> bool {
    (val.count_ones() & 0x1) == 0x0
}

/// Determine if this word should have the parity flag set
fn get_parity(val: u16) -> bool {
    (val.count_ones() & 0x1) == 0x0
}

/// Get the memory address for this instruction (if any) and whether it's for
// the source or the dest operand
fn get_inst_mem_addr(inst: &InstType, state: &CpuStateType) -> (Option<u16>, Option<AddTo>) {
    // Consolidate parts of the inst into a single memory address
    match (inst.mem_access, inst.mod_rm_data) {
        (true, _) => (Some(inst.data_value.unwrap()), inst.add_data_to),
        (_, Some(mod_rm_data)) => {
            let ea = get_effective_addr(mod_rm_data, inst.disp_value, &state.reg_file);
            (ea, inst.add_mod_rm_mem_to)
        }
        _ => (None, None),
    }
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
    match (mod_rm_data, disp) {
        (ModRmDataType::MemDirectAddr, Some(address)) => Some(address as u16),
        (ModRmDataType::MemReg(reg), _) => {
            // Get reg value and return
            Some(*reg_file.get(&reg.name).unwrap_or(&0))
        }
        (ModRmDataType::MemRegReg(reg1, reg2), _) => {
            // i.e. base + index
            let val1 = *reg_file.get(&reg1.name).unwrap_or(&0);
            let val2 = *reg_file.get(&reg2.name).unwrap_or(&0);
            Some(val1 + val2)
        }
        (ModRmDataType::MemRegDisp(_), None) => {
            unreachable!("ERROR: No displacement found for MemRegDisp")
        }
        (ModRmDataType::MemRegDisp(reg), Some(disp)) => {
            let val = *reg_file.get(&reg.name).unwrap_or(&0);
            Some(val + disp as u16)
        }
        (ModRmDataType::MemRegRegDisp(_, _), None) => {
            unreachable!("ERROR: No displacement found for MemRegRegDisp")
        }
        (ModRmDataType::MemRegRegDisp(reg1, reg2), Some(disp)) => {
            // Get reg 1 + 2, add together with disp, and return
            let val1 = *reg_file.get(&reg1.name).unwrap_or(&0);
            let val2 = *reg_file.get(&reg2.name).unwrap_or(&0);
            Some(val1 + val2 + disp as u16)
        }
        // Type Reg isn't a mem access, so this will be handled via source and
        // dest registers
        (ModRmDataType::Reg(_), _) => None,
        _ => unreachable!(),
    }
}

/// Given the memory array and a 16-bit address, return a 16-bit value
fn load_u16_from_mem(memory: &Vec<u8>, address: u16) -> u16 {
    // Get the value from memory. Remember, it's little endian!
    // Cast the individual u8 bytes to u16
    let lower = memory[address as usize] as u16;
    let upper = memory[(address + 1) as usize] as u16;
    // Combine into a single u16 value
    let val = lower | (upper << 8);
    println!("Loaded {val} from {address}");
    val
}

/// Given the memory array and a 16-bit address, return a 16-bit value
fn store_u16_in_mem(memory: &mut Vec<u8>, address: usize, new_val: u16) {
    // Store 16-bit value in little endian order
    memory[address] = (new_val & 0x00FF) as u8;
    memory[address + 1] = ((new_val & 0xFF00) >> 8) as u8;
    println!("Stored {new_val} at {address}");
}

/// Handle the logic for the given jump op code. Modify the IP register in the
/// CPU state. If the jump op jumped, then return true. Otherwise, return false.
fn handle_jmp_variants(jump_op: OpCodeType, inst: &InstType, state: &mut CpuStateType) -> bool {
    let jmp_value = match inst.jmp_value {
        Some(jmp_value) => jmp_value,
        _ => {
            println!("inst debug: {:#?}", inst);
            unimplemented!(
                "Jump variant {jump_op} is missing a jmp_value: {}",
                inst.text.as_ref().unwrap()
            );
        }
    };

    let ip_inc_8 = (jmp_value as i8) as i16;
    println!("ip_inc_8: {}", ip_inc_8);
    println!("state.ip before: {}", state.ip);

    // See table 2-15 on pg. 2-46 of the 8086 documentation
    let jump = match jump_op {
        OpCodeType::Jne => !state.flags_reg.zero,
        // je/jz - jump equal/zero
        OpCodeType::Je => state.flags_reg.zero,
        // jb/jnae/jc - jump below/not above or equal/carry
        OpCodeType::Jb => state.flags_reg.carry,
        // NOTE: For loops, cx was already checked to be != 0
        OpCodeType::Loopnz => !state.flags_reg.zero,
        OpCodeType::Loopz => state.flags_reg.zero,
        OpCodeType::Loop => true, // Only cx != 0
        OpCodeType::Jp => state.flags_reg.parity,
        x @ _ => {
            println!("inst debug: {:#?}", inst);
            unimplemented!("Unimplemented jump op {x} in handle_jmp_variants()")
        }
    };
    // If jump, add the IP increment value to IP and return true
    if jump {
        let (result, overflowed) = (state.ip as i16).overflowing_add(ip_inc_8);
        if overflowed {
            println!("Warning: IP register overflowed!: IP: {}", result);
        }
        state.ip = result as u16;
    }
    println!("state.ip after: {}", state.ip);
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
