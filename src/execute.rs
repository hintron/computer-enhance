//! The module implements the execution/simulation logic. All functions take in
//! a CPU state struct and modify it according to the requested operation.

use crate::decode::{InstType, OpCodeType};

#[derive(Copy, Clone, Debug, Default)]
pub struct CpuStateType {
    instrs_executed: u64,
    reg_ax: u8,
    reg_bx: u8,
    reg_cx: u8,
    reg_dx: u8,
    reg_sp: u8,
    reg_bp: u8,
    reg_si: u8,
    reg_di: u8,
}

pub fn init_state() -> CpuStateType {
    CpuStateType {
        ..Default::default()
    }
}

pub fn execute(inst: &mut InstType, state: &mut CpuStateType) {
    let op_type = match inst.op_type {
        Some(op_type) => op_type,
        None => {
            println!(
                "{}: Op type not found in instruction object",
                state.instrs_executed,
            );
            return;
        }
    };

    match op_type {
        OpCodeType::Mov => {
            // Check the dest register
            // match &inst.dest_text[..] {
            //     _ => {}
            // }
        }
        _ => {
            unimplemented!()
        }
    }

    state.instrs_executed += 1;
    println!(
        "{}: Executed {}",
        state.instrs_executed,
        inst.text.as_ref().unwrap()
    );
}
