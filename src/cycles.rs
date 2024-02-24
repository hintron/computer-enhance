//! This module implements the 8086 and 8088 cycles logic
//!
//! Much of this file is dedicated to holding data from table 2-20 and 2-21.
//! NOTE: "cycles" and "clocks" are used interchangeably.
//!

use crate::decode::{CpuType, InstType, ModRmDataType, OpCodeType, RegName, WidthType};

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
    Reg8,
    Reg16,
    Mem,
}

/// Now that the instruction is all decoded, fill in clock information
///
/// Regarding 8086 and 8088-specific clock penalties:
/// From pg. 2-50: "For instructions executing on an 8086, four clocks should be
/// added for each instruction reference to a word operand located at an odd
/// memory address to reflect any additional operand bus cycles required.
/// Similarly for instructions executing on an 8088, four clocks should be added
/// to each instruction reference to a 16-bit memory operand; this includes all
/// stack operations."
pub fn calculate_inst_clocks(inst: &mut InstType) {
    calculate_base_clocks_transfers(inst);

    // Calculate effective address clocks
    inst.clocks_ea = get_effective_addr_clocks(inst.mod_rm_data, inst.disp_value);

    // NOTE: Calculate 8086 clocks at the beginning of execution

    // Calculate 8088 clock penalties
    calculate_8088_clocks(inst);
}

pub fn print_inst_clock_debug(inst: &InstType) {
    println!(
        "clocks: OpType={:?}, base={}, ea={:?}, transfers={}, mem_word={}, mem_word_odd={}",
        inst.operands_type,
        inst.clocks_base,
        inst.clocks_ea,
        inst.transfers,
        inst.mem_access_word,
        inst.mem_access_word_unaligned
    );
}

/// Return the number of 8086 unaligned word memory access penalties for an
/// instruction.
///
/// Returns the value of the instruction's transfers if the memory address is
/// unaligned - i.e. it's an odd address. If the mem_addr does not exist, or the
/// instructions do not have any transfers, or addr is aligned/even, then return
/// 0.
///
/// `mem_addr` is the final memory address for the instruction, if it exists.
/// `transfers` is the number of times this instruction either loads or stores
/// at the memory address. E.g. `add [addr], 1` accesses memory two times, while
/// `mov [addr], 1` only accesses memory once.
///
/// NOTE: This must be called in execute, because mem_addr could be derived from
/// an effective address, which needs register values from the CPU state.
pub fn calculate_8086_unaligned_access(mem_addr: Option<u16>, transfers: u64) -> u64 {
    // If an instruction has a mem addr, it should also have transfers. If not,
    // then the transfers value was probably not set properly.
    assert!(mem_addr.is_some() == (transfers > 0));
    if transfers == 0 {
        return 0;
    }
    match mem_addr {
        Some(addr) => {
            if addr & 0x1 == 1 {
                transfers
            } else {
                0
            }
        }
        _ => 0,
    }
}

// Calculate 8088 word transfer penalties if not already set for the inst
fn calculate_8088_clocks(inst: &mut InstType) {
    if inst.transfers > 0 {
        // Current assumption is that both of these can't be set
        // TODO: Create one width field, and use AddTo, to enforce this
        assert!(inst.dest_width.is_none() || inst.source_width.is_none());

        match inst.dest_width {
            Some(WidthType::Word) => inst.mem_access_word += inst.transfers,
            _ => {}
        }

        match inst.source_width {
            Some(WidthType::Word) => inst.mem_access_word += inst.transfers,
            _ => {}
        }

        // TODO: We need to track all memory accesses where the other side
        // is a register with an implicit width. These cases aren't covered
        // by dest_width or source_width.
        let is_memory_inst = is_memory_inst(inst);
        match (is_memory_inst, inst.dest_reg, inst.source_reg) {
            (true, Some(dest_reg), _) => {
                if dest_reg.width == WidthType::Word {
                    inst.mem_access_word += inst.transfers;
                }
            }
            (true, _, Some(src_reg)) => {
                if src_reg.width == WidthType::Word {
                    inst.mem_access_word += inst.transfers;
                }
            }
            _ => {}
        }
    }
}

/// This contains all the clock information from table 2-21.
///
/// Table 2-21 lists the required number of memory transfers in an instruction.
/// Whether there is a 4-clock transfer penalty depends on if it is an unaligned
/// memory access (in the 8086) or any 16-bit memory access (in the 8088).
///
/// Regarding transfers, remember that an `add` is different than a `mov`.
/// For example, consider the difference bewteen these two instructions:
///     mov \[0x1000], word 1
///     add \[0x1000], word 1
///
/// `mov` simply moves 1 into the memory address 0x1000. However, `add`
/// does `DEST = DEST + SOURCE`. So it must first read out `[0x1000]` into some
/// temporary storage location on-chip, add 1 to that value, and then send the
/// result back to `[0x1000]`. This means there are *two* memory accesses in
/// this instruction! So in the case of the 8088, there will be 2 word mem
/// access penalties instead of one for an `add` of this form.
///
/// Typos in table 2-21:
/// * TEST, pg. 2-67: There should be 1 transfer instead of - for operand type
/// 'memory, immediate'. (Any instruction with a memory access and EA should
/// have at least one memory transfer.)
pub fn calculate_base_clocks_transfers(inst: &mut InstType) {
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
        ) => {
            inst.clocks_base = 9;
            inst.transfers = 1;
        }
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
        ) => {
            inst.clocks_base = 16;
            inst.transfers = 2;
        }
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
        ) => {
            inst.clocks_base = 17;
            inst.transfers = 2;
        }
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
        (Some(OpCodeType::Cmp), Some(OperandsType::RegMem)) => {
            inst.clocks_base = 9;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Cmp), Some(OperandsType::MemReg)) => {
            inst.clocks_base = 9;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Cmp), Some(OperandsType::RegImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Cmp), Some(OperandsType::MemImm)) => {
            inst.clocks_base = 10;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Cmp), Some(OperandsType::AccImm)) => inst.clocks_base = 4,
        (
            Some(
                OpCodeType::Ja
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
                | OpCodeType::Js,
            ),
            _,
        ) => {
            inst.clocks_base = 4;
            inst.clocks_jump = Some(12);
        }
        (Some(OpCodeType::Inc), Some(OperandsType::Reg8)) => {
            inst.clocks_base = 3;
        }
        (Some(OpCodeType::Inc), Some(OperandsType::Reg16)) => {
            inst.clocks_base = 2;
        }
        (Some(OpCodeType::Inc), Some(OperandsType::Mem)) => {
            inst.clocks_base = 15;
            inst.transfers = 2;
        }
        (Some(OpCodeType::Jcxz | OpCodeType::Loopz), _) => {
            inst.clocks_base = 6;
            inst.clocks_jump = Some(12);
        }
        (Some(OpCodeType::Loop), _) => {
            inst.clocks_base = 5;
            inst.clocks_jump = Some(12);
        }
        (Some(OpCodeType::Loopnz), _) => {
            inst.clocks_base = 5;
            inst.clocks_jump = Some(14);
        }
        // Lds
        (Some(OpCodeType::Lds), _) => {
            inst.clocks_base = 16;
            inst.transfers = 2;
        }
        // Lea
        (Some(OpCodeType::Lea), _) => inst.clocks_base = 2,
        // Les
        (Some(OpCodeType::Les), _) => {
            inst.clocks_base = 16;
            inst.transfers = 2;
        }
        // Mov
        (Some(OpCodeType::Mov), Some(OperandsType::MemAcc)) => {
            inst.clocks_base = 10;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Mov), Some(OperandsType::AccMem)) => {
            inst.clocks_base = 10;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Mov), Some(OperandsType::RegReg)) => inst.clocks_base = 2,
        (Some(OpCodeType::Mov), Some(OperandsType::RegMem)) => {
            inst.clocks_base = 8;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Mov), Some(OperandsType::MemReg)) => {
            inst.clocks_base = 9;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Mov), Some(OperandsType::RegImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Mov), Some(OperandsType::MemImm)) => {
            inst.clocks_base = 10;
            inst.transfers = 1;
        }
        // Test
        (Some(OpCodeType::Test), Some(OperandsType::RegReg)) => inst.clocks_base = 3,
        (Some(OpCodeType::Test), Some(OperandsType::RegMem)) => {
            inst.clocks_base = 9;
            inst.transfers = 1;
        }
        (Some(OpCodeType::Test), Some(OperandsType::AccImm)) => inst.clocks_base = 4,
        (Some(OpCodeType::Test), Some(OperandsType::RegImm)) => inst.clocks_base = 5,
        (Some(OpCodeType::Test), Some(OperandsType::MemImm)) => {
            inst.clocks_base = 11;
            // NOTE: This is not in the docs, but I believe it is a typo
            inst.transfers = 1;
        }
        _ => {}
    }
}

/// Determine if this instruction is a memory instruction
fn is_memory_inst(inst: &InstType) -> bool {
    match (inst.mod_rm_data, inst.operands_type) {
        // This is a reg to reg operation - no mem
        (Some(ModRmDataType::Reg(_)), _) => false,
        // The other ModRmDataTypes are all memory accesses
        (Some(_), _) => true,
        (
            _,
            Some(
                OperandsType::AccMem
                | OperandsType::MemImm
                | OperandsType::MemReg
                | OperandsType::MemSeg
                | OperandsType::RegMem
                | OperandsType::SegMem,
            ),
        ) => {
            println!("NOTE: OperandsType used to determine this inst was mem!");
            true
        }
        _ => false,
    }
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
    mod_rm_data: Option<ModRmDataType>,
    disp_value: Option<i16>,
) -> Option<u64> {
    // When disp is 0, there is no disp penalty! So reduce by 4
    let disp_penalty = match disp_value {
        None | Some(0) => 0,
        Some(_) => 4,
    };
    match mod_rm_data {
        // displacement only
        Some(ModRmDataType::MemDirectAddr) => Some(6),
        // base or index only
        Some(ModRmDataType::MemReg(_)) => Some(5),
        // displacement + base or index
        Some(ModRmDataType::MemRegDisp(_)) => Some(5 + disp_penalty),
        // base + index
        Some(ModRmDataType::MemRegReg(reg1, reg2)) => {
            let penalty = get_reg_clock_penalty(reg1.name, reg2.name);
            Some(7 + penalty)
        }
        // displacement + base + index
        Some(ModRmDataType::MemRegRegDisp(reg1, reg2)) => {
            let penalty = get_reg_clock_penalty(reg1.name, reg2.name);
            Some(7 + disp_penalty + penalty)
        }
        _ => None,
    }
}

/// Return a string showing the clocks in parts. If the
/// instruction only has base clocks or no clocks set, return None.
pub fn get_total_clocks_str(inst: &InstType, cpu_type: CpuType, jumped: bool) -> Option<String> {
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

    let mut base = inst.clocks_base;
    if jumped {
        base += inst.clocks_jump.unwrap()
    };

    let clocks_str = match (base, inst.clocks_ea, clocks_transfer) {
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
