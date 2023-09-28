//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;

use crate::decode::{InstType, OpCodeType, RegName, RegWidth};

#[derive(Debug, Default)]
pub struct CpuStateType {
    reg_file: BTreeMap<RegName, u16>,
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

    match op_type {
        // Handle all movs
        OpCodeType::Mov => {
            match (inst.dest_reg, inst.source_reg, inst.immediate_value) {
                // Handle immediate to dest reg movs
                (Some(reg), _, Some(immediate)) => {
                    let old_val = match state.reg_file.get(&reg.name) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Figure out what part of the immediate value to put where
                    let new_val = match reg.width {
                        RegWidth::Byte => (old_val & 0xFF00) | (immediate & 0xFF),
                        RegWidth::Hi8 => (old_val & 0x00FF) | (immediate << 8),
                        RegWidth::Word => immediate,
                    };
                    // Check the dest register
                    let old_val = state.reg_file.insert(reg.name, new_val).unwrap_or(0);
                    effect = format!(
                        "{} ; {}:0x{:X}->0x{:X}",
                        inst.text.as_ref().unwrap(),
                        reg.name,
                        old_val,
                        new_val
                    );
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
                    let new_val = match dest_reg.width {
                        RegWidth::Byte => (dest_val & 0xFF00) | (source_val_sized & 0xFF),
                        RegWidth::Hi8 => (dest_val & 0xFF) | (source_val_sized << 8),
                        RegWidth::Word => source_val_sized,
                    };
                    // Store new val in the dest register
                    let old_val = state.reg_file.insert(dest_reg.name, new_val).unwrap_or(0);
                    assert!(old_val == dest_val);
                    effect = format!(
                        "{} ; {}:0x{:X}->0x{:X}",
                        inst.text.as_ref().unwrap(),
                        dest_reg.name,
                        old_val,
                        new_val
                    );
                }
                _ => {
                    println!(
                        "Unimplemented mov variant: `{}`",
                        inst.text.as_ref().unwrap()
                    );
                    println!("dest_reg: {:?}", inst.dest_reg);
                    println!("source_reg: {:?}", inst.source_reg);
                    println!("immediate_value: {:?}", inst.immediate_value);
                }
            }
        }
        _ => {
            println!(
                "Execution of instruction `{}` is unimplemented",
                inst.text.as_ref().unwrap()
            );
        }
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
    lines.push(format!(""));
}
