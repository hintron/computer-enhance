//! This module implements the 8086 and 8088 cycles logic
//!
//! Much of this file is dedicated to holding data from table 2-20 and 2-21.
//! NOTE: "cycles" and "clocks" are used interchangeably.
//!

use crate::decode::{CpuType, InstType, ModRmDataType, OpCodeType, RegName};

/// This in combination with the instruction's op code type index into table
/// 2-21 to get clocks data for each instruction.
/// NOTE: The table shows the instruction types as DST, SRC. These types are in
/// the same order, to make things easier.
#[derive(Copy, Clone, Debug)]
pub enum OperandsType {
    MemAcc,
    AccMem,
    RegReg,
    RegMem,
    MemReg,
    RegImm,
    MemImm,
    SegReg,
    SegMem,
    RegSeg,
    MemSeg,
    AccImm,
}

/// Now that the instruction is all decoded, fill in clock information
/// This contains all the clock information from table 2-21.
///
/// Regarding 8086 and 8088-specific clock penalties:
/// From pg. 2-50: "For instructions executing on an 8086, four clocks should be
/// added for each instruction reference to a word operand located at an odd
/// memory address to reflect any additional operand bus cycles required.
/// Similarly for instructions executing on an 8088, four clocks should be added
/// to each instruction reference to a 16-bit memory operand; this includes all
/// stack operations."
/// Table 2-21 lists the required number of memory transfers in an instruction.
/// Whether there is a 4-clock transfer penalty depends on if it is an unaligned
/// memory access (in the 8086) or any 16-bit memory access (in the 8088).
pub fn calculate_inst_clocks(inst: &mut InstType) {
    match (inst.op_type, inst.operands_type) {
        // TODO: Implement seg reg moves
        (_, Some(OperandsType::SegReg)) => todo!(),
        (_, Some(OperandsType::SegMem)) => todo!(),
        (_, Some(OperandsType::RegSeg)) => todo!(),
        (_, Some(OperandsType::MemSeg)) => todo!(),
        // Arithmetic instructions that all share the same cycle timing
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::RegReg),
        ) => inst.clocks_base = 3,
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::RegMem),
        ) => inst.clocks_base = 9,
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::MemReg),
        ) => inst.clocks_base = 16,
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::RegImm),
        ) => inst.clocks_base = 4,
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::MemImm),
        ) => inst.clocks_base = 17,
        (
            Some(
                OpCodeType::Adc
                | OpCodeType::Add
                | OpCodeType::And
                | OpCodeType::Or
                | OpCodeType::Sub
                | OpCodeType::Sbb
                | OpCodeType::Xor,
            ),
            Some(OperandsType::AccImm),
        ) => inst.clocks_base = 4,
        // Cmp
        (Some(OpCodeType::Cmp), Some(OperandsType::RegReg)) => inst.clocks_base = 3,
        (Some(OpCodeType::Cmp), Some(OperandsType::RegMem)) => inst.clocks_base = 9,
        (Some(OpCodeType::Cmp), Some(OperandsType::MemReg)) => inst.clocks_base = 9,
        (Some(OpCodeType::Cmp), Some(OperandsType::RegImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Cmp), Some(OperandsType::MemImm)) => inst.clocks_base = 10,
        (Some(OpCodeType::Cmp), Some(OperandsType::AccImm)) => inst.clocks_base = 4,
        // Lds
        (Some(OpCodeType::Lds), _) => inst.clocks_base = 16,
        // Lea
        (Some(OpCodeType::Lea), _) => inst.clocks_base = 2,
        // Les
        (Some(OpCodeType::Les), _) => inst.clocks_base = 16,
        // Mov
        (Some(OpCodeType::Mov), Some(OperandsType::AccMem)) => inst.clocks_base = 10,
        (Some(OpCodeType::Mov), Some(OperandsType::MemAcc)) => inst.clocks_base = 10,
        (Some(OpCodeType::Mov), Some(OperandsType::RegReg)) => inst.clocks_base = 2,
        (Some(OpCodeType::Mov), Some(OperandsType::RegMem)) => inst.clocks_base = 8,
        (Some(OpCodeType::Mov), Some(OperandsType::MemReg)) => inst.clocks_base = 9,
        (Some(OpCodeType::Mov), Some(OperandsType::RegImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Mov), Some(OperandsType::MemImm)) => inst.clocks_base = 10,
        // Test
        (Some(OpCodeType::Test), Some(OperandsType::RegReg)) => inst.clocks_base = 3,
        (Some(OpCodeType::Test), Some(OperandsType::RegMem)) => inst.clocks_base = 9,
        (Some(OpCodeType::Test), Some(OperandsType::AccImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Test), Some(OperandsType::RegImm)) => inst.clocks_base = 5,
        (Some(OpCodeType::Test), Some(OperandsType::MemImm)) => inst.clocks_base = 11,

        _ => {} // (_, None) => {},
                // (_, _) => {
                //     println!("inst debug: {:#?}", inst);
                //     unimplemented!("Unimplemented inst + operands type: {}", inst.text.as_ref().unwrap())
                // }
    }

    println!(
        "clocks: OpType={:?}, base={}",
        inst.operands_type, inst.clocks_base
    );
}

/// Return the clock penalty (if any) for the given combination of base + index
/// registers within an effective address calculation. Either 1 or 0.
/// (bp + si) or (bx + di) has a 1-cycle penalty
/// (bx + si) or (bp + di) has a 0-cycle penalty
/// No other registers can be used as base or index registers.
/// See table 2-20
fn get_reg_clock_penalty(reg1: RegName, reg2: RegName) -> u64 {
    match (reg1, reg2) {
        (RegName::Bp, RegName::Di)
        | (RegName::Di, RegName::Bp)
        | (RegName::Bx, RegName::Si)
        | (RegName::Si, RegName::Bx) => 0,
        (RegName::Bp, RegName::Si)
        | (RegName::Si, RegName::Bp)
        | (RegName::Bx, RegName::Di)
        | (RegName::Di, RegName::Bx) => 1,
        _ => {
            unreachable!("Invalid registers for base + index: {reg1} {reg2}")
        }
    }
}

/// Get the clocks associated with the effective address calculation.
/// See table 2-20.
pub fn get_effective_addr_clocks(
    mod_rm_data: ModRmDataType,
    disp_value: Option<i16>,
) -> Option<u64> {
    // When disp is 0, there is no disp penalty! So reduce by 4
    let disp_penalty = match disp_value {
        None | Some(0) => 0,
        Some(_) => 4,
    };
    match mod_rm_data {
        // displacement only
        ModRmDataType::MemDirectAddr => Some(6),
        // base or index only
        ModRmDataType::MemReg(_) => Some(5),
        // displacement + base or index
        ModRmDataType::MemRegDisp(_) => Some(5 + disp_penalty),
        // base + index
        ModRmDataType::MemRegReg(reg1, reg2) => {
            let penalty = get_reg_clock_penalty(reg1.name, reg2.name);
            Some(7 + penalty)
        }
        // displacement + base + index
        ModRmDataType::MemRegRegDisp(reg1, reg2) => {
            let penalty = get_reg_clock_penalty(reg1.name, reg2.name);
            Some(7 + disp_penalty + penalty)
        }
        _ => None,
    }
}

/// Return a string showing the clocks in parts. If the
/// instruction only has base clocks or no clocks set, return None.
pub fn get_total_clocks_str(inst: &InstType, cpu_type: CpuType) -> Option<String> {
    let print_clocks = inst.clocks_ea.is_some()
        || (inst.mem_access_word_unaligned > 0 && cpu_type == CpuType::Intel8086)
        || (inst.mem_access_word > 0 && cpu_type == CpuType::Intel8088);
    if !print_clocks {
        return None;
    }

    // Account for 8086 and 8088-specific clocks
    let clocks_transfer = if cpu_type == CpuType::Intel8086 {
        inst.mem_access_word_unaligned * 4
    } else {
        inst.mem_access_word * 4
    };

    let clocks_str = match (inst.clocks_base, inst.clocks_ea, clocks_transfer) {
        (inst, Some(ea), 0) => format!("{inst} + {ea}ea"),
        (inst, Some(ea), t_penalty) => format!("{inst} + {ea}ea + {t_penalty}p"),
        (inst, None, 0) => format!("{inst}"),
        (inst, None, t_penalty) => format!("{inst} + {t_penalty}p"),
    };
    Some(clocks_str)
}

/// Add up clocks_base, ea clocks, and transfer penalty clocks and return it
/// as the total clocks for this instruction.
pub fn get_total_clocks(inst: &InstType, cpu_type: CpuType) -> u64 {
    let mut total = inst.clocks_base;
    match inst.clocks_ea {
        Some(x) => total += x,
        _ => {}
    };
    // Account for 8086 and 8088-specific clocks
    if cpu_type == CpuType::Intel8086 {
        total += inst.mem_access_word_unaligned * 4;
    } else {
        total += inst.mem_access_word * 4;
    };
    total
}

/// Print out whether this run is an 8086 or 8088, and print out a notice about
/// cycle estimation accuracy.
pub fn print_cycle_header(cycle_type: Option<CpuType>) -> Vec<String> {
    let mut lines = vec![];
    if cycle_type.is_some() {
        lines.push("**************".to_string());
        lines.push(format!("**** {} ****", cycle_type.unwrap()));
        lines.push("**************".to_string());
        lines.push("".to_string());
        lines.push(
            "WARNING: Clocks reported by this utility are strictly from the 8086 manual."
                .to_string(),
        );
        lines.push(
            "They will be inaccurate, both because the manual clocks are estimates, and because"
                .to_string(),
        );
        lines.push(
            "some of the entries in the manual look highly suspicious and are probably typos."
                .to_string(),
        );
        lines.push("".to_string());
    }
    lines
}
