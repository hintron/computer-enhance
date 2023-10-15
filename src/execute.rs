//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;
use std::fmt;

use crate::decode::{InstType, OpCodeType, RegName, RegWidth};

#[derive(Debug, Default)]
pub struct CpuStateType {
    // MGH TODO: Make reg_file an array, and map RegNames to an index via a
    // match statement. Compare performance!
    reg_file: BTreeMap<RegName, u16>,
    flags_reg: FlagsRegType,
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
    /// If set, there has been a carry out of, or a borrow into, the high-order
    /// bit of the result.
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
        ..Default::default()
    }
}

pub fn execute(inst: &mut InstType, state: &mut CpuStateType) -> String {
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
    // The final register to store new_val into. If None, throw it away (cmp)
    let dest_name;
    // Set this var if we should set the flags reg at the end
    let mut modify_flags = false;
    let mut new_val_overflowed = false;
    let mut new_val_aux_carry = false;

    match op_type {
        // Handle all movs
        OpCodeType::Mov => {
            match (inst.dest_reg, inst.source_reg, inst.immediate_value) {
                // Handle immediate to dest reg movs
                (Some(dest_reg), _, Some(immediate)) => {
                    let old_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out what part of the immediate value to put where
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (old_val & 0xFF00) | (immediate & 0xFF),
                        RegWidth::Hi8 => (old_val & 0x00FF) | (immediate << 8),
                        RegWidth::Word => immediate,
                    });
                    dest_name = Some(dest_reg.name);
                }
                // Handle source reg to dest reg
                (Some(dest_reg), Some(source_reg), _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Get the value of the dest register
                    let dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out which bytes to get from the source
                    let source_val_sized = match source_reg.width {
                        RegWidth::Byte => source_val & 0xFF,
                        RegWidth::Hi8 => (source_val & 0xFF00) >> 8,
                        RegWidth::Word => source_val,
                    };
                    // Figure out which bytes to replace in dest
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (dest_val & 0xFF00) | (source_val_sized & 0xFF),
                        RegWidth::Hi8 => (dest_val & 0xFF) | (source_val_sized << 8),
                        RegWidth::Word => source_val_sized,
                    });
                    dest_name = Some(dest_reg.name);
                }
                _ => {
                    unimplemented!(
                        "Unimplemented mov variant: `{}`",
                        inst.text.as_ref().unwrap()
                    );
                }
            }
        }
        // The source operand is subtracted from the destination operand, and
        // the result replaces the destination operand. SUB updates flags AF,
        // CF, OF, PF, SF, and ZF.
        OpCodeType::Sub => {
            match (inst.dest_reg, inst.source_reg, inst.immediate_value) {
                // Handle immediate to dest reg movs
                (Some(dest_reg), _, Some(immediate)) => {
                    let old_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out what part of the immediate value to put where
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (old_val & 0xFF00) - (immediate & 0xFF),
                        RegWidth::Hi8 => (old_val & 0x00FF) - (immediate << 8),
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                sub_with_overflow(old_val, immediate);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = Some(dest_reg.name);
                    modify_flags = true;
                }
                // Handle source reg to dest reg
                (Some(dest_reg), Some(source_reg), _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Get the value of the dest register
                    let dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out which bytes to get from the source
                    let source_val_sized = match source_reg.width {
                        RegWidth::Byte => source_val & 0xFF,
                        RegWidth::Hi8 => (source_val & 0xFF00) >> 8,
                        RegWidth::Word => source_val,
                    };
                    // Figure out which bytes to replace in dest
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => {
                            println!("sub byte");
                            (dest_val & 0xFF00) - (source_val_sized & 0xFF)
                        }
                        RegWidth::Hi8 => {
                            println!("sub hi8");
                            (dest_val & 0xFF) - (source_val_sized << 8)
                        }
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                sub_with_overflow(dest_val, source_val_sized);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = Some(dest_reg.name);
                    modify_flags = true;
                }
                _ => {
                    unimplemented!(
                        "Unimplemented sub variant: `{}`",
                        inst.text.as_ref().unwrap()
                    );
                }
            }
        }
        // Same as Sub, but throw away new value and keep old
        OpCodeType::Cmp => {
            match (inst.dest_reg, inst.source_reg, inst.immediate_value) {
                // Handle immediate to dest reg movs
                (Some(dest_reg), _, Some(immediate)) => {
                    let old_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out what part of the immediate value to put where
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (old_val & 0xFF00) - (immediate & 0xFF),
                        RegWidth::Hi8 => (old_val & 0x00FF) - (immediate << 8),
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                sub_with_overflow(old_val, immediate);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = None;
                    modify_flags = true;
                }
                // Handle source reg to dest reg
                (Some(dest_reg), Some(source_reg), _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Get the value of the dest register
                    let dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out which bytes to get from the source
                    let source_val_sized = match source_reg.width {
                        RegWidth::Byte => source_val & 0xFF,
                        RegWidth::Hi8 => (source_val & 0xFF00) >> 8,
                        RegWidth::Word => source_val,
                    };
                    // Figure out which bytes to replace in dest
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (dest_val & 0xFF00) - (source_val_sized & 0xFF),
                        RegWidth::Hi8 => (dest_val & 0xFF) - (source_val_sized << 8),
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                sub_with_overflow(dest_val, source_val_sized);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = None;
                    modify_flags = true;
                }
                _ => {
                    unimplemented!(
                        "Unimplemented sub variant: `{}`",
                        inst.text.as_ref().unwrap()
                    );
                }
            }
        }
        // The source operand is added to the destination operand, and
        // the result replaces the destination operand. ADD updates flags AF,
        // CF, OF, PF, SF, and ZF.
        OpCodeType::Add => {
            match (inst.dest_reg, inst.source_reg, inst.immediate_value) {
                // Handle immediate to dest reg movs
                (Some(dest_reg), _, Some(immediate)) => {
                    let old_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out what part of the immediate value to put where
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => {
                            println!("Add byte");
                            (old_val & 0xFF00) + (immediate & 0xFF)
                        }
                        RegWidth::Hi8 => {
                            println!("Add hi8");
                            (old_val & 0x00FF) + (immediate << 8)
                        }
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                add_with_overflow(old_val, immediate);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = Some(dest_reg.name);
                    modify_flags = true;
                }
                // Handle source reg to dest reg
                (Some(dest_reg), Some(source_reg), _) => {
                    // Get the value of the source register
                    let source_val = match state.reg_file.get(&source_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Get the value of the dest register
                    let dest_val = match state.reg_file.get(&dest_reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out which bytes to get from the source
                    let source_val_sized = match source_reg.width {
                        RegWidth::Byte => source_val & 0xFF,
                        RegWidth::Hi8 => (source_val & 0xFF00) >> 8,
                        RegWidth::Word => source_val,
                    };
                    // Figure out which bytes to replace in dest
                    new_val = Some(match dest_reg.width {
                        RegWidth::Byte => (dest_val & 0xFF00) + (source_val_sized & 0xFF),
                        RegWidth::Hi8 => (dest_val & 0xFF) + (source_val_sized << 8),
                        RegWidth::Word => {
                            let (result, overflowed, aux_carry) =
                                add_with_overflow(dest_val, source_val_sized);
                            new_val_overflowed = overflowed;
                            new_val_aux_carry = aux_carry;
                            result
                        }
                    });
                    dest_name = Some(dest_reg.name);
                    modify_flags = true;
                }
                _ => {
                    unimplemented!(
                        "Unimplemented add variant: `{}`",
                        inst.text.as_ref().unwrap()
                    );
                }
            }
        }
        _ => {
            unimplemented!(
                "Execution of instruction `{}` is unimplemented",
                inst.text.as_ref().unwrap()
            );
        }
    }

    let new_val = new_val.unwrap();

    effect.push_str(inst.text.as_ref().unwrap());
    effect.push_str(" ;");

    if modify_flags {
        // Set parity if even number of ones *in the bottom byte only*
        // https://open.substack.com/pub/computerenhance/p/simulating-add-jmp-and-cmp?r=leu8y&utm_campaign=comment-list-share-cta&utm_medium=web&comments=true&commentId=14205872
        state.flags_reg.parity = ((new_val & 0xFF).count_ones() & 0x1) == 0x0;
        state.flags_reg.zero = new_val == 0;
        state.flags_reg.sign = (new_val & 0x8000) == 0x8000;
        state.flags_reg.overflow = new_val_overflowed;
        state.flags_reg.auxiliary_carry = new_val_aux_carry;
    }

    match dest_name {
        Some(dest_name) => {
            // Store new val in the dest register
            let old_val = state.reg_file.insert(dest_name, new_val).unwrap_or(0);
            effect.push_str(&format!(" {}:0x{:x}->0x{:x}", dest_name, old_val, new_val));
        }
        // Nothing is stored back into destination
        None => {}
    }

    // Print change in flags register, if needed
    if modify_flags && (old_flags != state.flags_reg) {
        effect.push_str(&format!(" flags:{}->{}", old_flags, state.flags_reg));
    }

    return effect;
}

pub fn print_final_state(state: &CpuStateType, lines: &mut Vec<String>) {
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
    lines.push(format!("      ax: 0x{:04x} ({})", ax_val, ax_val));
    lines.push(format!("      bx: 0x{:04x} ({})", bx_val, bx_val));
    lines.push(format!("      cx: 0x{:04x} ({})", cx_val, cx_val));
    lines.push(format!("      dx: 0x{:04x} ({})", dx_val, dx_val));
    lines.push(format!("      sp: 0x{:04x} ({})", sp_val, sp_val));
    lines.push(format!("      bp: 0x{:04x} ({})", bp_val, bp_val));
    lines.push(format!("      si: 0x{:04x} ({})", si_val, si_val));
    lines.push(format!("      di: 0x{:04x} ({})", di_val, di_val));
    lines.push(format!("      es: 0x{:04x} ({})", es_val, es_val));
    lines.push(format!("      ss: 0x{:04x} ({})", ss_val, ss_val));
    lines.push(format!("      ds: 0x{:04x} ({})", ds_val, ds_val));
    lines.push(format!("   flags: {}", state.flags_reg));
    lines.push(format!(""));
}

/// Add two 16-bit numbers together. If the sign bit of the lhs changes, set
/// the overflow flag. Return the result, whether there was a signed arithmetic
/// overflow., and whether there was an auxilliary carry.
///
/// 8086 defines overflow as the sign bit of the left hand side (destination)
/// changing. This is true with 0x7FFF + 0x0001, but also true with 0xFFFF +
/// 0x0001. The overflow flag will still be set even if the user is intending to
/// do unsigned arithmetic. The bits are the same. See [FlagsRegType::overflow].
fn add_with_overflow(lhs: u16, rhs: u16) -> (u16, bool, bool) {
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
    let overflow = (left_sign_bit == right_sign_bit) && (left_sign_bit != result_sign_bit);
    let aux_carry = calc_aux_carry_add(lhs, rhs);
    println!("{lhs} (0x{lhs:x}) + {rhs} (0x{rhs:x}) = {result} (0x{result:x})");
    if overflow {
        println!("Addition overflowed!")
    }
    if aux_carry {
        println!("Addition aux_carry!")
    }
    (result, overflow, aux_carry)
}

/// Subtract two 16 bit numbers (rhs from lhs) and return the result, whether
/// there was a signed arithmetic overflow., and whether there was an auxilliary
/// carry.
fn sub_with_overflow(lhs: u16, rhs: u16) -> (u16, bool, bool) {
    let left_sign_bit = lhs & 0x8000;
    let right_sign_bit = rhs & 0x8000;
    let (result, _) = lhs.overflowing_sub(rhs);
    let result_sign_bit = result & 0x8000;
    // Since we're subtracting, left and right sign must be opposite for
    // overflow to occur.
    let overflow = (left_sign_bit != right_sign_bit) && (left_sign_bit != result_sign_bit);
    let aux_carry = calc_aux_carry_sub(lhs, rhs);
    println!("{lhs} (0x{lhs:x}) - {rhs} (0x{rhs:x}) = {result} (0x{result:x})");
    if overflow {
        println!("Subtract overflowed!")
    }
    if aux_carry {
        println!("Subtract aux_carry!")
    }
    (result, overflow, aux_carry)
}

const fn calc_aux_carry_add(lhs: u16, rhs: u16) -> bool {
    ((lhs & 0xF) + (rhs & 0xF)) > 0xF
}

const fn calc_aux_carry_sub(lhs: u16, rhs: u16) -> bool {
    (rhs & 0xF) > (lhs & 0xF)
}
