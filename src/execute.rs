//! The module implements the execution logic

use crate::decode::InstType;

#[derive(Copy, Clone, Debug, Default)]
pub struct CpuStateType {
    instrs_executed: u64,
}

pub fn init_state() -> CpuStateType {
    CpuStateType {
        ..Default::default()
    }
}

pub fn execute(inst: &mut InstType, state: &mut CpuStateType) {
    state.instrs_executed += 1;
    println!(
        "{}: Executed {}",
        state.instrs_executed,
        inst.text.as_ref().unwrap()
    )
}
