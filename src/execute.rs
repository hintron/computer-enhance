//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use std::collections::BTreeMap;

use crate::decode::{InstType, OpCodeType, RegType};

#[derive(Debug, Default)]
pub struct CpuStateType {
    reg_file: BTreeMap<RegType, u16>,
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
                (Some(reg), _, Some(new_val)) => {
                    // Check the dest register
                    let old_val = state.reg_file.insert(reg, new_val).unwrap_or(0);
                    effect = format!(
                        "{} ; {}:0x{}->0x{}",
                        inst.text.as_ref().unwrap(),
                        reg,
                        old_val,
                        new_val
                    );
                }
                // Handle source reg to dest reg
                (Some(dest_reg), Some(source_reg), _) => {
                    // Get the value of the source register
                    let new_val = match state.reg_file.get(&source_reg) {
                        Some(x) => *x,
                        None => 0,
                    };
                    // Copy it to the dest register
                    let old_val = state.reg_file.insert(dest_reg, new_val).unwrap_or(0);
                    effect = format!(
                        "{} ; {}:0x{}->0x{}",
                        inst.text.as_ref().unwrap(),
                        dest_reg,
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
    let ax_val = state.reg_file.get(&RegType::Ax).unwrap_or(&0);
    let bx_val = state.reg_file.get(&RegType::Bx).unwrap_or(&0);
    let cx_val = state.reg_file.get(&RegType::Cx).unwrap_or(&0);
    let dx_val = state.reg_file.get(&RegType::Dx).unwrap_or(&0);
    let sp_val = state.reg_file.get(&RegType::Sp).unwrap_or(&0);
    let bp_val = state.reg_file.get(&RegType::Bp).unwrap_or(&0);
    let si_val = state.reg_file.get(&RegType::Si).unwrap_or(&0);
    let di_val = state.reg_file.get(&RegType::Di).unwrap_or(&0);

    lines.push(format!(""));
    lines.push(format!("Final registers:"));
    lines.push(format!("      ax: 0x00{:02x} ({})", ax_val, ax_val));
    lines.push(format!("      bx: 0x00{:02x} ({})", bx_val, bx_val));
    lines.push(format!("      cx: 0x00{:02x} ({})", cx_val, cx_val));
    lines.push(format!("      dx: 0x00{:02x} ({})", dx_val, dx_val));
    lines.push(format!("      sp: 0x00{:02x} ({})", sp_val, sp_val));
    lines.push(format!("      bp: 0x00{:02x} ({})", bp_val, bp_val));
    lines.push(format!("      si: 0x00{:02x} ({})", si_val, si_val));
    lines.push(format!("      di: 0x00{:02x} ({})", di_val, di_val));
    lines.push(format!(""));
}
