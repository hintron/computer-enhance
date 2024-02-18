//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;
use std::fmt;
use std::fs::File;
use std::io::Write;

use crate::decode::{AddTo, InstType, ModRmDataType, OpCodeType, RegName, RegWidth};

// Third-party imports
use minifb::{Window, WindowOptions};

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

pub fn init_state() -> CpuStateType {
    CpuStateType {
        // Initialize the memory array to 1 MB
        memory: vec![0; MEMORY_SIZE],
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
/// string summarizing the change in state that occurred.
///
/// no_ip: If true, do NOT add IP changes or the final state of IP to the output
/// string.
pub fn execute(inst: &mut InstType, state: &mut CpuStateType, no_ip: bool) -> String {
    let mut effect = "".to_string();
    let op_type = match inst.op_type {
        Some(op_type) => op_type,
        None => {
            println!("Bad instruction object: {:?}", inst,);
            return effect;
        }
    };

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
    let current_ip = state.ip;

    // "While an instruction is executing, IP refers to the next instruction."
    // BYU RTOS Website, 8086InstructionSet.html
    // So, always advance the IP before executing each instruction so that it
    // points to the next instruction.
    advance_ip_reg(inst, state);

    // SUB/CMP: The source operand is subtracted from the destination operand,
    // and the result replaces the destination operand.

    // ADD: The source operand is added to the destination operand,
    // and the result replaces the destination operand.

    // SUB/CMP/ADD updates flags AF, CF, OF, PF, SF, and ZF.
    // MOV does not update any flags.

    match op_type {
        op @ (OpCodeType::Mov | OpCodeType::Sub | OpCodeType::Cmp | OpCodeType::Add) => {
            let dest_width;
            let dest_val;

            // Get the op's destination reg and its current value
            match (inst.dest_reg, inst.add_data_to, inst.mod_rm_data) {
                (Some(dest_reg), _, _) => {
                    // Note: This also currently covers ModRmDataType::Reg(_)
                    // Get the value of the dest register
                    dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    dest_target = Target::RegisterName(dest_reg.name);
                }
                (_, Some(AddTo::Dest), _) => {
                    // For now, we can assume that mem_access is true when
                    // data_value_dest exists. You can't store into an
                    // immediate value! I.e. we know it's [dest_val] as dest
                    let address = inst.data_value.unwrap();
                    dest_target = Target::MemAddress(address as usize);
                    dest_val = load_u16_from_mem(&state.memory, address);
                }
                (_, _, Some(mod_rm_data)) => {
                    let address =
                        mod_rm_to_addr(mod_rm_data, inst.disp_value, &state.reg_file).unwrap();
                    dest_target = Target::MemAddress(address as usize);
                    dest_val = load_u16_from_mem(&state.memory, address);
                }
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!("{op} has no dest: `{}`", inst.text.as_ref().unwrap())
                }
            };

            // Figure out destination width
            dest_width = match (
                inst.dest_reg,
                inst.dest_width,
                inst.source_reg,
                inst.source_width,
            ) {
                (Some(dest_reg), _, _, _) => dest_reg.width,
                (_, Some(dest_width), _, _) => dest_width,
                (_, _, Some(source_reg), _) => source_reg.width,
                (_, _, _, Some(source_width)) => source_width,
                _ => {
                    unimplemented!("No dest reg, dest width, source reg, or source width")
                }
            };

            // Get the op's source val either from a source reg or an immediate
            let source_val = match (inst.source_reg, inst.add_data_to, inst.mod_rm_data) {
                // Handle source reg to dest reg
                (Some(source_reg), _, _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out which bytes to get from the source
                    let source_val_sized = match source_reg.width {
                        RegWidth::Byte => source_val & 0xFF,
                        RegWidth::Hi8 => (source_val & 0xFF00) >> 8,
                        RegWidth::Word => source_val,
                    };
                    source_val_sized
                }
                (_, Some(AddTo::Source), _) => {
                    if inst.mem_access {
                        // If mem_access, use data as address into mem
                        load_u16_from_mem(&state.memory, inst.data_value.unwrap())
                    } else {
                        // Otherwise, use data as just an immediate value
                        inst.data_value.unwrap()
                    }
                }
                (_, _, Some(mod_rm_data)) => {
                    let address =
                        mod_rm_to_addr(mod_rm_data, inst.disp_value, &state.reg_file).unwrap();
                    load_u16_from_mem(&state.memory, address)
                }
                _ => {
                    println!("inst debug: {:#?}", inst);
                    unimplemented!("{op} has no source: `{}`", inst.text.as_ref().unwrap());
                }
            };

            // Figure out what part of the source value to put where, and which
            // bytes of the dest register to replace
            new_val = Some(match (op, dest_width) {
                (OpCodeType::Mov, RegWidth::Byte) => (dest_val & 0xFF00) | (source_val & 0xFF),
                (OpCodeType::Add, RegWidth::Byte) => (dest_val & 0xFF00) + (source_val & 0xFF),
                (OpCodeType::Sub | OpCodeType::Cmp, RegWidth::Byte) => {
                    (dest_val & 0xFF00) - (source_val & 0xFF)
                }
                (OpCodeType::Mov, RegWidth::Hi8) => (dest_val & 0x00FF) | (source_val << 8),
                (OpCodeType::Add, RegWidth::Hi8) => (dest_val & 0x00FF) + (source_val << 8),
                (OpCodeType::Sub | OpCodeType::Cmp, RegWidth::Hi8) => {
                    (dest_val & 0x00FF) - (source_val << 8)
                }
                (OpCodeType::Mov, RegWidth::Word) => source_val,
                (OpCodeType::Add, RegWidth::Word) => {
                    let (result, overflowed, carry, aux_carry) =
                        add_with_overflow(dest_val, source_val);
                    new_val_overflowed = overflowed;
                    new_val_carry = carry;
                    new_val_aux_carry = aux_carry;
                    result
                }
                (OpCodeType::Sub | OpCodeType::Cmp, RegWidth::Word) => {
                    let (result, overflowed, carry, aux_carry) =
                        sub_with_overflow(dest_val, source_val);
                    new_val_overflowed = overflowed;
                    new_val_carry = carry;
                    new_val_aux_carry = aux_carry;
                    result
                }
                _ => unreachable!(),
            });

            // CMP does not store the result, but the others do
            if op == OpCodeType::Cmp {
                dest_target = Target::None
            };

            // MOV does not modify flags, but the others do
            if op != OpCodeType::Mov {
                modify_flags = true;
            }
        }
        jump_op @ (OpCodeType::Jne | OpCodeType::Je | OpCodeType::Jb | OpCodeType::Jp) => {
            new_val = None;
            dest_target = Target::None;
            handle_jmp_variants(jump_op, inst, state);
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
                handle_jmp_variants(jump_op, inst, state);
            }
        }
        _ => {
            println!("inst debug: {:#?}", inst);
            unimplemented!(
                "Execution of instruction `{}` is unimplemented",
                inst.text.as_ref().unwrap()
            );
        }
    }

    effect.push_str(inst.text.as_ref().unwrap());
    effect.push_str(" ;");

    match (modify_flags, new_val) {
        (true, Some(new_val)) => {
            // Set parity if even number of ones *in the bottom byte only*
            // https://open.substack.com/pub/computerenhance/p/simulating-add-jmp-and-cmp?r=leu8y&utm_campaign=comment-list-share-cta&utm_medium=web&comments=true&commentId=14205872
            state.flags_reg.parity = ((new_val & 0xFF).count_ones() & 0x1) == 0x0;
            state.flags_reg.zero = new_val == 0;
            state.flags_reg.sign = (new_val & 0x8000) == 0x8000;
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

    if !no_ip {
        // Tack on the IP change to the instruction effect string
        effect.push_str(&format!(" ip:0x{:x}->0x{:x}", current_ip, state.ip));
    }

    // Print change in flags register, if needed
    if modify_flags && (old_flags != state.flags_reg) {
        effect.push_str(&format!(" flags:{}->{}", old_flags, state.flags_reg));
    }

    return effect;
}

/// Convert the mod_rm and displacement bytes into a memory address.
/// See Decode::get_mod_rm_addr_str()
fn mod_rm_to_addr(
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
    lower | (upper << 8)
}

/// Given the memory array and a 16-bit address, return a 16-bit value
fn store_u16_in_mem(memory: &mut Vec<u8>, address: usize, new_val: u16) {
    // Store 16-bit value in little endian order
    memory[address] = (new_val & 0x00FF) as u8;
    memory[address + 1] = (new_val & 0xFF00) as u8;
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
        // jb/jnae - jump below/not above or equal
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

/// Write the CPU memory array to a file.
pub fn memory_to_file(memory: &Vec<u8>, output_file: &str) {
    let mut file = File::create(output_file).expect("Failed to create file {output_file}");
    let all_zeros = memory.iter().all(|&x| x == 0);
    if all_zeros {
        file.write_all("Memory was empty".as_bytes())
            .expect("Failed to write to file");
    } else {
        file.write_all(memory).expect("Failed to write to file");
    }
    file.flush().expect("Failed to flush file");
}

/// Display a 64x64 image from program memory, starting at location 0
/// The output image is 4 bytes per pixel (RGBA). minifb expects 32 bits (4
/// bytes) per pixel.
pub fn display_memory(memory: &mut Vec<u8>) {
    // Image dimensions
    const WIDTH: usize = 64;
    const HEIGHT: usize = 64;

    let window_result = Window::new(
        "Memory Display - Computer Enhance Simulator",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    );
    let mut window = match window_result {
        Ok(x) => x,
        Err(e) => {
            println!("ERROR: Unable to display memory in window: {e}");
            return;
        }
    };

    // Ensure that the length memory is divisible by 4
    assert_eq!(memory.len() % 4, 0);

    // Reinterpret u8 memory vector as a u32 memory slice
    let memory_u32: &mut [u32] = unsafe {
        std::slice::from_raw_parts_mut(memory.as_mut_ptr() as *mut u32, memory.len() / 4)
    };

    let mut update = true;
    // Main loop: draw the image until the window is closed
    while window.is_open() && !window.is_key_down(minifb::Key::Escape) {
        if !update {
            // Update the window with the image data
            window
                .update_with_buffer(&memory_u32[..], WIDTH, HEIGHT)
                .unwrap();
            update = false;
        }
    }
}

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

/// Add two 16-bit numbers together. If the sign bit of the lhs changes, set
/// the overflow flag. Return the result, whether there was a signed arithmetic
/// overflow, and whether there was an auxilliary carry.
///
/// 8086 defines overflow as the sign bit of the left hand side (destination)
/// changing. This is true with 0x7FFF + 0x0001, but also true with 0xFFFF +
/// 0x0001. The overflow flag will still be set even if the user is intending to
/// do unsigned arithmetic. The bits are the same. See [FlagsRegType::overflow].
fn add_with_overflow(lhs: u16, rhs: u16) -> (u16, bool, bool, bool) {
    let left_sign_bit = lhs & 0x8000;
    let right_sign_bit = rhs & 0x8000;
    // We are discarding the overflow result because that is not the same as the
    // 8086 overflow flag. Rust's overflow result seems to only care about
    // unsigned wrapping from 0x0000 to 0xFFFF or vice versa, and not if the
    // sign value wraps from negative to positive.
    let (result, _) = lhs.overflowing_add(rhs);
    let result_sign_bit = result & 0x8000;

    // Overflow cannot occur when the sign bits differ between operands
    // If the two operands have the same sign bit, then overflow occurs if
    // the result does not have that same sign bit.
    // See "Overflow Rule for addition" in https://www.doc.ic.ac.uk/~eedwards/compsys/arithmetic/index.html
    let overflow = (left_sign_bit == right_sign_bit) && (left_sign_bit != result_sign_bit);
    let carry = (rhs as u32) + (lhs as u32) > 0xFFFF;
    let aux_carry = calc_aux_carry_add(lhs, rhs);
    println!("{lhs} (0x{lhs:x}) + {rhs} (0x{rhs:x}) = {result} (0x{result:x})");
    if overflow {
        println!("Addition overflowed!")
    }
    if carry {
        println!("Addition carry!")
    }
    if aux_carry {
        println!("Addition aux_carry!")
    }
    (result, overflow, carry, aux_carry)
}

/// Subtract two 16 bit numbers (rhs from lhs) and return the result, whether
/// there was a signed arithmetic overflow, and whether there was an auxilliary
/// carry.
fn sub_with_overflow(lhs: u16, rhs: u16) -> (u16, bool, bool, bool) {
    let left_sign_bit = lhs & 0x8000;
    let right_sign_bit = rhs & 0x8000;
    let (result, _) = lhs.overflowing_sub(rhs);
    let result_sign_bit = result & 0x8000;
    // Since we're subtracting, left and right sign must be opposite for
    // overflow to occur.
    let overflow = (left_sign_bit != right_sign_bit) && (left_sign_bit != result_sign_bit);
    let carry = rhs > lhs;
    let aux_carry = calc_aux_carry_sub(lhs, rhs);
    println!("{lhs} (0x{lhs:x}) - {rhs} (0x{rhs:x}) = {result} (0x{result:x})");
    if overflow {
        println!("Subtract overflowed!")
    }
    if carry {
        println!("Subtract carry!")
    }
    if aux_carry {
        println!("Subtract aux_carry!")
    }
    (result, overflow, carry, aux_carry)
}

const fn calc_aux_carry_add(lhs: u16, rhs: u16) -> bool {
    ((lhs & 0xF) + (rhs & 0xF)) > 0xF
}

const fn calc_aux_carry_sub(lhs: u16, rhs: u16) -> bool {
    (rhs & 0xF) > (lhs & 0xF)
}
