//! This module implements the 8086 decoding logic, and also calls into the
//! execute module as needed to simulate the code being decoded..
//!
//! It will be difficult to follow along with what is going on in this file
//! without referencing the 8086 manual (available in this code repo at
//! docs/8086_family_Users_Manual_1_.pdf, or found online at various places).
//! The manual describes instruction encoding and decoding from pages 4-18 to
//! 4-36.
//!
//! There are a few typos/errors in the manual, including:
//! * table 4-11, pg. 4-21: (DISP-LO) says that the disp byte is unsigned. In
//! effect, it is signed. I can't reconcile how it can be unsigned.
//! * table 4-12, pg. 4-22: Both the 'Segment register' variants of push and pop
//! should show `sr` but instead show `reg`.
//! * table 4-12, pg. 4-24: Immediate with accumulator variant of cmp does not
//! show the high data byte.
//! * table 4-12, pg. 4-24: Immediate with register/memory variant of cmp should
//! show s:w=01 for the high data byte.
//! * table 4-12, pg. 4-24: AAM and AAD should not have DISP-LO or DISP-HI.
//! * table 4-12, pg. 4-25: TEST register/memory and register variant
//! erroneously shows the opcode byte starting with 0b 0001 00, which is
//! actually instruction adc. It should be 0b 1000 01 instead. This is
//! corroborated by table 4-13.
//! * table 4-12, pg. 4-25: XOR immediate to register/memory's second byte
//! should say "mod 1 1 0 r/m" instead of "data". This is corroborated by table
//! 4-14, and the data bytes are already at the end.
//! * table 4-12, pg. 4-25: STOS is misspelled as STDS. Also, Stor -> Store
//! and AL/A -> AL/AX.

use std::fmt;

use crate::cycles::{calculate_inst_clocks, OperandsType};
use crate::execute::{execute, init_state, CpuStateType};

/// The four types of modes in the mod field of "mod r/m" bytes
#[derive(Copy, Clone, Debug)]
enum ModType {
    MemoryMode0,
    MemoryMode8,
    MemoryMode16,
    RegisterMode,
}

/// The possible types of 'mod r/m' bytes, which is the second byte in many
/// instructions.
///
/// `ModRegRm` is a standalone 'mod r/m' byte that is relatively
/// straight-forward. The other "mod r/m" bytes are more complicated and
/// factor in to what the overall op code of the instruction is.
///
/// See table 4-14 on pg 4-36 for the four official types of 'mod op r/m' bytes:
///
/// ```text
/// | mod □ rm | 000 | 001 | 010 | 011 | 100 | 101 | 110 | 111 |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | Immed    | ADD | OR  | ADC | SBB | AND | SUB | XOR | CMP |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | Shift    | ROL | ROR | RCL | RCR |SHL/SAL|SHR|-----| SAR |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | Grp 1    |TEST |-----| NOT | NEG | MUL |IMUL | DIV |IDIV |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | Grp 2    | INC | DEC |CALL |CALL | JMP | JMP | PUSH|-----|
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
///
/// ```
/// The following are not official 'mod op rm' bytes in the docs, but are
/// groupings I created to track instructions that have a similar 'mod op rm'
/// structure:
///
/// ```text
/// | mod □ rm | 000 | 001 | 010 | 011 | 100 | 101 | 110 | 111 |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | ModMovRm | mov |-----|-----|-----|-----|-----|-----|-----|
/// | ModPopRm | pop |-----|-----|-----|-----|-----|-----|-----|
/// ```
///
/// Each group has a first byte that starts with the following bits:
/// ```text
/// Immed:    1000 00
/// Shift:    1101 00
/// Grp 1:    1111 011
/// Grp 2:    1111 111
/// ModMovRm: 1100 011
/// ModPopRm: 1000 1111 (0x8F)
/// ```
///
#[derive(Copy, Clone, Debug)]
enum ModRmByteType {
    ModRegRm,
    ModMovRm,
    ModPopRm,
    ModImmedRm,
    ModShiftRm,
    ModGrp1Rm,
    ModGrp2Rm,
    ModSrRm,
}

/// The possible types of immediate bytes that can be included in an instruction
/// other than the first byte and the mod r/m byte. Immediate bytes include
/// displacement and data bytes used to construct immediate values.
#[derive(Copy, Clone, Debug)]
enum ImmBytesType {
    /// An 8-bit unsigned value.
    Data8,
    /// An 8-bit signed value, optionally extended by DataHi
    DataLo,
    DataHi,
    DispLo,
    DispHi,
    /// An 8-bit signed increment offset to the instruction pointer
    IpInc8,
    IpIncLo,
    IpIncHi,
    IpLo,
    IpHi,
    CsLo,
    CsHi,
    /// The following byte can be ignored (e.g. the second byte of aam/aad
    /// apparently provides no additional data)
    DoNotCare,
}

/// Indicates whether to apply some data to the source or the destination
#[derive(Copy, Clone, Debug)]
pub enum AddTo {
    Dest,
    Source,
}

/// OpCode types containing a static string mapping
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpCodeType {
    Aaa,
    Aad,
    Aam,
    Aas,
    Adc,
    Add,
    And,
    Call,
    /// Convert byte to word
    Cbw,
    /// Clear carry flag
    Clc,
    /// Clear direction flag
    Cld,
    /// Clear interrupt flag (disable interrupts)
    Cli,
    Cmc,
    Cmp,
    Cmps,
    /// Convert word to double word
    Cwd,
    Daa,
    Das,
    Dec,
    Div,
    Hlt,
    Idiv,
    Imul,
    In,
    Inc,
    Int,
    Int3,
    Into,
    Iret,
    /// ja/jnbe - jump if above/not below or equal
    Ja,
    /// jb/jnae/jc - jump if below/not above or equal/on carry
    Jb,
    /// jbe/jna - jump if below or equal/not above
    Jbe,
    /// Jcxz - jump if cx is zero
    Jcxz,
    /// je/jz - jump if equal/zero
    Je,
    /// jg/jnle - jump if greater/not less than or equal
    Jg,
    /// jl/jnge - jump if less than/not greater or equal
    Jl,
    /// jle/jng - jump if less than or equal/not greater
    Jle,
    /// jmp - unconditional jump
    Jmp,
    /// jnb/jae/jnc - jump if not below/above or equal/on not carry
    Jnb,
    /// jne/jnz - jump if not equal/zero
    Jne,
    /// jnl/jge - jump if not less/greater or equal
    Jnl,
    /// Jno - jump on not overflow
    Jno,
    /// jnp/jpo - jump if not par/par odd
    Jnp,
    /// jns - jump on not overflow
    Jns,
    /// jump on overflow
    Jo,
    /// jp/jpe - jump on parity/parity even
    Jp,
    /// js - jump on sign
    Js,
    Lahf,
    Lds,
    /// Load effective address to register
    Lea,
    Les,
    Lods,
    Loop,
    /// loopnz/loopne - loop while not zero/equal
    Loopnz,
    /// loopz/loope - loop while zero/equal
    Loopz,
    Mov,
    Movs,
    Mul,
    Neg,
    Not,
    Or,
    Out,
    Pop,
    Popf,
    Push,
    Pushf,
    /// rcl - rotate left through carry
    Rcl,
    /// rcr - rotate right through carry
    Rcr,
    Ret,
    Retf,
    /// rol - rotate left
    Rol,
    /// ror - rotate right
    Ror,
    Sahf,
    /// sar - shift right - arithmetic (sign extend)
    Sar,
    Sbb,
    Scas,
    /// shl/sal - shift left (same for arithmetic and logical shifts)
    Shl,
    /// shr - shift right - logical (zero fill, don't sign extend)
    Shr,
    /// Set carry flag
    Stc,
    /// Set direction flag
    Std,
    /// Set interrupt flag (enable interrupts)
    Sti,
    Stos,
    Sub,
    Test,
    Wait,
    Xchg,
    Xlat,
    Xor,
}

/// Implement OpCodeType enum to string mapping, so OpCodeType can be printed
impl fmt::Display for OpCodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpCodeType::Aaa => write!(f, "aaa"),
            OpCodeType::Aad => write!(f, "aad"),
            OpCodeType::Aam => write!(f, "aam"),
            OpCodeType::Aas => write!(f, "aas"),
            OpCodeType::Adc => write!(f, "adc"),
            OpCodeType::Add => write!(f, "add"),
            OpCodeType::And => write!(f, "and"),
            OpCodeType::Call => write!(f, "call"),
            OpCodeType::Cbw => write!(f, "cbw"),
            OpCodeType::Clc => write!(f, "clc"),
            OpCodeType::Cld => write!(f, "cld"),
            OpCodeType::Cli => write!(f, "cli"),
            OpCodeType::Cmc => write!(f, "cmc"),
            OpCodeType::Cmp => write!(f, "cmp"),
            OpCodeType::Cmps => write!(f, "cmps"),
            OpCodeType::Cwd => write!(f, "cwd"),
            OpCodeType::Daa => write!(f, "daa"),
            OpCodeType::Das => write!(f, "das"),
            OpCodeType::Dec => write!(f, "dec"),
            OpCodeType::Div => write!(f, "div"),
            OpCodeType::Hlt => write!(f, "hlt"),
            OpCodeType::Idiv => write!(f, "idiv"),
            OpCodeType::Imul => write!(f, "imul"),
            OpCodeType::Inc => write!(f, "inc"),
            OpCodeType::Int3 => write!(f, "int3"),
            OpCodeType::Into => write!(f, "into"),
            OpCodeType::Int => write!(f, "int"),
            OpCodeType::In => write!(f, "in"),
            OpCodeType::Iret => write!(f, "iret"),
            OpCodeType::Ja => write!(f, "ja"),
            OpCodeType::Jbe => write!(f, "jbe"),
            OpCodeType::Jb => write!(f, "jb"),
            OpCodeType::Jcxz => write!(f, "jcxz"),
            OpCodeType::Je => write!(f, "je"),
            OpCodeType::Jg => write!(f, "jg"),
            OpCodeType::Jle => write!(f, "jle"),
            OpCodeType::Jl => write!(f, "jl"),
            OpCodeType::Jmp => write!(f, "jmp"),
            OpCodeType::Jnb => write!(f, "jnb"),
            OpCodeType::Jne => write!(f, "jne"),
            OpCodeType::Jnl => write!(f, "jnl"),
            OpCodeType::Jno => write!(f, "jno"),
            OpCodeType::Jnp => write!(f, "jnp"),
            OpCodeType::Jns => write!(f, "jns"),
            OpCodeType::Jo => write!(f, "jo"),
            OpCodeType::Jp => write!(f, "jp"),
            OpCodeType::Js => write!(f, "js"),
            OpCodeType::Lahf => write!(f, "lahf"),
            OpCodeType::Lds => write!(f, "lds"),
            OpCodeType::Lea => write!(f, "lea"),
            OpCodeType::Les => write!(f, "les"),
            OpCodeType::Lods => write!(f, "lods"),
            OpCodeType::Loopnz => write!(f, "loopnz"),
            OpCodeType::Loop => write!(f, "loop"),
            OpCodeType::Loopz => write!(f, "loopz"),
            OpCodeType::Mov => write!(f, "mov"),
            OpCodeType::Movs => write!(f, "movs"),
            OpCodeType::Mul => write!(f, "mul"),
            OpCodeType::Neg => write!(f, "neg"),
            OpCodeType::Not => write!(f, "not"),
            OpCodeType::Or => write!(f, "or"),
            OpCodeType::Out => write!(f, "out"),
            OpCodeType::Popf => write!(f, "popf"),
            OpCodeType::Pop => write!(f, "pop"),
            OpCodeType::Pushf => write!(f, "pushf"),
            OpCodeType::Push => write!(f, "push"),
            OpCodeType::Rcl => write!(f, "rcl"),
            OpCodeType::Rcr => write!(f, "rcr"),
            OpCodeType::Retf => write!(f, "retf"),
            OpCodeType::Ret => write!(f, "ret"),
            OpCodeType::Rol => write!(f, "rol"),
            OpCodeType::Ror => write!(f, "ror"),
            OpCodeType::Sahf => write!(f, "sahf"),
            OpCodeType::Sar => write!(f, "sar"),
            OpCodeType::Sbb => write!(f, "sbb"),
            OpCodeType::Scas => write!(f, "scas"),
            OpCodeType::Shl => write!(f, "shl"),
            OpCodeType::Shr => write!(f, "shr"),
            OpCodeType::Stc => write!(f, "stc"),
            OpCodeType::Std => write!(f, "std"),
            OpCodeType::Sti => write!(f, "sti"),
            OpCodeType::Stos => write!(f, "stos"),
            OpCodeType::Sub => write!(f, "sub"),
            OpCodeType::Test => write!(f, "test"),
            OpCodeType::Wait => write!(f, "wait"),
            OpCodeType::Xchg => write!(f, "xchg"),
            OpCodeType::Xlat => write!(f, "xlat"),
            OpCodeType::Xor => write!(f, "xor"),
        }
    }
}

/// Register type uniquely identifying an addressable (or unaddressable)
/// register. Derive Ord so we can create a map with RegName keys.
/// NOTE: The IP reg will NOT be part of the reg file, since it can't be written
/// to directly and it's easier to track it separately from the other registers.
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum RegName {
    Ax,
    Bx,
    Cx,
    Dx,
    Sp,
    Bp,
    Si,
    Di,
    /// Segment register es
    Es,
    /// Segment register cs
    Cs,
    /// Segment register ss
    Ss,
    /// Segment register ds
    Ds,
}

impl fmt::Display for RegName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegName::Ax => write!(f, "ax"),
            RegName::Bx => write!(f, "bx"),
            RegName::Cx => write!(f, "cx"),
            RegName::Dx => write!(f, "dx"),
            RegName::Sp => write!(f, "sp"),
            RegName::Bp => write!(f, "bp"),
            RegName::Si => write!(f, "si"),
            RegName::Di => write!(f, "di"),
            RegName::Es => write!(f, "es"),
            RegName::Cs => write!(f, "cs"),
            RegName::Ss => write!(f, "ss"),
            RegName::Ds => write!(f, "ds"),
        }
    }
}

/// The width of something. Could be the size of an operand (whether it's a
/// register, immediate, or memory location) or the width of a data transfer.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WidthType {
    // 8 bits/low half of word
    Byte,
    // Upper 8 bits/high half of word
    Hi8,
    // 16 bits,
    Word,
    // 32 bits
    // Dword,
    // // 64 bits
    // Qword,
    // // 128 bits
    // Dqword,
}

impl WidthType {
    /// Return a string + space to convert this WidthType to a source or dest
    /// prefix string
    fn to_width_prefix_str(&self) -> &'static str {
        match self {
            WidthType::Byte | WidthType::Hi8 => "byte ",
            WidthType::Word => "word ",
        }
    }
}

/// Identifies a register and the size
#[derive(Copy, Clone, Debug)]
pub struct RegType {
    pub name: RegName,
    pub width: WidthType,
}

impl fmt::Display for RegType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.name, self.width) {
            (RegName::Ax, WidthType::Byte) => write!(f, "al"),
            (RegName::Ax, WidthType::Hi8) => write!(f, "ah"),
            (RegName::Ax, WidthType::Word) => write!(f, "ax"),
            (RegName::Bx, WidthType::Byte) => write!(f, "bl"),
            (RegName::Bx, WidthType::Hi8) => write!(f, "bh"),
            (RegName::Bx, WidthType::Word) => write!(f, "bx"),
            (RegName::Cx, WidthType::Byte) => write!(f, "cl"),
            (RegName::Cx, WidthType::Hi8) => write!(f, "ch"),
            (RegName::Cx, WidthType::Word) => write!(f, "cx"),
            (RegName::Dx, WidthType::Byte) => write!(f, "dl"),
            (RegName::Dx, WidthType::Hi8) => write!(f, "dh"),
            (RegName::Dx, WidthType::Word) => write!(f, "dx"),
            (RegName::Sp, WidthType::Word) => write!(f, "sp"),
            (RegName::Bp, WidthType::Word) => write!(f, "bp"),
            (RegName::Si, WidthType::Word) => write!(f, "si"),
            (RegName::Di, WidthType::Word) => write!(f, "di"),
            (RegName::Es, WidthType::Word) => write!(f, "es"),
            (RegName::Cs, WidthType::Word) => write!(f, "cs"),
            (RegName::Ss, WidthType::Word) => write!(f, "ss"),
            (RegName::Ds, WidthType::Word) => write!(f, "ds"),
            (name, width) => unreachable!("Could not print RegType {{{:?} {:?}}}", name, width),
        }
    }
}

/// ModRmDataType records the result of decoding the Mod field and the RM field.
/// This is basically table 4-10 on page 4-20. There are 40 possible
/// combinations from 8 values of R/M * 5 modes - mod 00, mod 01, mod 10, mod 11
/// w0, and mod 11 w1.
#[derive(Copy, Clone, Debug)]
pub enum ModRmDataType {
    /// Just a single reg (i.e. mod 11, w={0,1}, rm={000-111})
    Reg(RegType),
    /// \[reg] (i.e. mod 0 rm {100, 101, 111})
    MemReg(RegType),
    /// \[reg1 + reg2] (i.e. mod 00 rm {000, 001, 010, 011})
    MemRegReg(RegType, RegType),
    /// \[DIRECT ADDRESS] (i.e. mod 00 rm 110)
    MemDirectAddr,
    /// \[reg + disp]. disp is u8/u16, depending on disp bytes
    MemRegDisp(RegType),
    /// \[reg1 + reg2 + disp]. disp is u8/u16, depending on disp bytes
    MemRegRegDisp(RegType, RegType),
}

/// A type to determine if this execution run is for an 8086 or an 8088. If
/// specified, it means we care about cycles and want to print out cycle
/// estimates accordingly.
///
/// Cycle estimates will be different, depending on if this is an 8086 or an
/// 8088. The 8086 has a 16-bit bus, while the 8088 has an 8-bit bus. The 8088
/// was designed to be easier and cheaper to make, but with slightly worse
/// latencies.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum CpuType {
    Intel8086,
    Intel8088,
}

impl fmt::Display for CpuType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Intel8086 => write!(f, "8086")?,
            Self::Intel8088 => write!(f, "8088")?,
        }
        Ok(())
    }
}

/// Decode-specific settings
pub struct DecodeSettings {
    pub print: bool,
    pub verbose: bool,
}

/// Execution-specific settings
pub struct ExecuteSettings {
    pub init_ip: Option<u16>,
    pub init_sp: Option<u16>,
    pub no_ip: bool,
    pub cycle_model: Option<CpuType>,
    pub stop_on_ret: bool,
}

/// A struct holding all the decoded data of a given instruction
/// All public fields will be used in the execute module or printed out to the
/// user.
#[derive(Default, Debug)]
pub struct InstType {
    /// If true, the REG field is the destination, otherwise it is the source.
    /// See table 4-7 on pg. 4-19.
    d_field: Option<bool>,
    w_field: Option<bool>,
    /// If true, shift/rotate count is specified in the CL register. Otherwise,
    /// it is 1.
    v_field: Option<bool>,
    /// Sign extend field. If true, sign extend 8-bit immediate as needed
    s_field: Option<bool>,
    /// If true, repeat/loop while zero flag is set, otherwise repeat while zero
    /// flag is clear.
    z_field: Option<bool>,
    /// A LOCK prefix was part of the instruction
    lock_prefix: Option<bool>,
    /// A string of all prefixes concatenated together in the order parsed
    prefixes: Option<Vec<&'static str>>,
    mod_field: Option<ModType>,
    rm_field: Option<u8>,
    /// This is "reg" in the ModRegRm byte. This will be copied into source_reg
    /// or dest_reg, depending on d_field.
    reg_field: Option<RegType>,
    /// This is the segment register field, used with "segment register"
    /// variants of push and pop, as well as for segment override prefixes.
    sr_field: Option<RegType>,
    /// The op code type
    pub op_type: Option<OpCodeType>,
    /// A suffix string to append to the opcode, like `b` for `movsb`
    op_type_suffix: Option<&'static str>,
    /// A list of all bytes processed for this instruction
    pub processed_bytes: Vec<u8>,
    /// Processed data from the mod rm byte
    pub mod_rm_data: Option<ModRmDataType>,
    mod_rm_byte: Option<ModRmByteType>,
    /// If true, then the first byte was a REP and there is a second string
    /// manipulation byte to follow.
    has_string_byte: Option<bool>,
    /// The actual data for the immediate bytes
    data_8: Option<u8>,
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    /// Displacement value. Signed. Can come from either one or two displacement
    /// bytes. See add_disp_to to see whether disp is applied to source or
    /// destination.
    pub disp_value: Option<i16>,
    ip_inc8: Option<u8>,
    ip_inc_lohi: Option<u16>,
    ip_lohi: Option<u16>,
    cs_lohi: Option<u16>,
    /// Whether disp_value should be applied to the source or destination.
    pub add_disp_to: Option<AddTo>,
    /// Whether data_value should be applied to the source or destination.
    pub add_data_to: Option<AddTo>,
    /// The actual value of the data immediate bytes (either data_8 or data_hi +
    /// data_lo). It's stored as a u16, even if it's only a u8.
    pub data_value: Option<u16>,
    /// A relative value to add to the current IP when jumping
    pub ip_inc: Option<i16>,
    /// An absolute address to set the current IP to when jumping, including any
    /// segment register offsets.
    pub ip_abs: Option<u16>,
    /// If true, then use the data bytes as part of a memory reference, like
    /// \[DATA_BYTES]. Use add_data_to to determine if it's the src or dest.
    pub mem_access: bool,
    // If mod rm encodes a mem operation, indicate whether it's for src or dest.
    pub add_mod_rm_mem_to: Option<AddTo>,
    /// If true, then this instruction is an indirect intersegment/far variant
    /// and needs an explicit `far` keyword.
    pub far_prefix: bool,
    /// If true, sign extend the value in data_lo to be 2 bytes/16 bits wide.
    sign_extend_data_lo: bool,
    /// The expected immediate byte types to parse after we parse the 1st byte
    /// and the mod/rm byte (if it exists).
    immediate_bytes: Vec<ImmBytesType>,
    /// The source register, if the source is a register
    pub source_reg: Option<RegType>,
    /// Explicitly indicate the source width. Needed when coming from memory.
    pub source_width: Option<WidthType>,
    /// The value of the source operand, if it's a hardcoded value
    pub source_hardcoded: Option<u16>,
    /// If true, do NOT print it out when printing the instruction
    pub source_hardcoded_implicit: bool,
    /// The destination register, if the destination is a register
    pub dest_reg: Option<RegType>,
    /// Explicitly indicate the destination width. Needed when going to memory.
    pub dest_width: Option<WidthType>,
    /// The final instruction representation
    pub text: Option<String>,
    /// The type of operands supplied to the instruction. Used to figure out
    /// cycle information for the instruction.
    pub operands_type: Option<OperandsType>,
    /// The base clocks that this instruction takes
    pub clocks_base: u64,
    /// The clocks needed to calculate the effective address, if any
    pub clocks_ea: Option<u64>,
    /// The additional clock penalty this instruction has if it jumps
    pub clocks_jump: Option<u64>,
    /// For shift instructions, this is the additional clock penalty for each
    /// bit shift executed.
    pub clocks_per_bit: Option<u16>,
    /// The number of memory accesses that this instruction performs.
    /// E.g. `mov [ADDR], 1` only accesses memory once, while `add [ADDR], 1`
    /// accesses memory twice - once to load dest in before adding 1 to it, and
    /// once to store the result back into ADDR. See table 2-21.
    pub transfers: u64,
    /// The number of unaligned 16-bit (word) accesses. There is a transfer
    /// penalty of 4 clocks per each unaligned access on the 8086.
    pub mem_access_word_unaligned: u64,
    /// The number of 16-bit (word) memory operands. There is a transfer penalty
    /// of 4 clocks per 16-bit memory access on the 8088.
    pub mem_access_word: u64,
}

/// Decode and execute an 8086 program. This will decode and execute whatever
/// the IP points to and supports branches and jumps.
///
/// no_ip: If true, do NOT print out IP changes or the final state of IP
///
/// Returns a vector of instructions executed, as well as the final CPU state.
pub fn decode_execute(
    program_bytes: Vec<u8>,
    program_length: u64,
    decode_settings: &DecodeSettings,
    exec_settings: &ExecuteSettings,
) -> (Vec<String>, CpuStateType) {
    let mut output_text_lines = vec![];
    let mut cpu_state = init_state(program_bytes, exec_settings.init_ip, exec_settings.init_sp);

    // MGH idea: Create a decoded instruction cache. Take the 16-byte window and
    // see if the first n bytes match any decoded instructions. If so, skip
    // decode, use that InstType, and advance the IP. Use the # of
    // processed bytes in InstType to advance the IP.
    loop {
        // If the IP surpassed the original program length, then exit, or else
        // we may start decoding zeroes as instructions since the vector was
        // resized to be 1 MB. This check works for most code, but could start
        // being a problem if we are executing self-modifying code that creates
        // and executes instructions past the end of the original program.
        if (cpu_state.ip as u64) >= program_length {
            println!("IP surpassed original program length. Halting CPU.");
            break;
        }
        let inst_byte_window = match get_inst_window(cpu_state.ip as usize, &cpu_state.memory) {
            Some(x) => x,
            None => break,
        };
        // Decode one (possibly multi-byte) instruction at a time
        match decode_single(inst_byte_window, decode_settings.verbose) {
            Some(mut inst) => {
                if decode_settings.print {
                    println!("{}", inst.text.as_ref().unwrap());
                }
                // Execute the instruction
                let (text, halt) = execute(&mut inst, &mut cpu_state, exec_settings);
                output_text_lines.push(text);
                if halt {
                    break;
                }
                // On to the next instruction...
            }
            // Done with the instruction stream
            None => break,
        };
    }

    (output_text_lines, cpu_state)
}

/// Decode an 8086 instruction stream. This is a dumb line-by-line decode of an
/// instruction stream and does not take branches or do any simulation
/// whatsoever. It prints processed bytes, prints the decoded instruction, and
/// returns a vector of instructions.
pub fn decode(inst_stream: Vec<u8>, decode_settings: &DecodeSettings) -> Vec<InstType> {
    let mut insts = vec![];
    // Simply track the IP in this variable instead of in CpuState, since we
    // aren't actually executing the code. The IP is just how we iterate through
    // the instruction stream.
    let mut ip = 0;
    loop {
        let inst_byte_window = match get_inst_window(ip, &inst_stream) {
            Some(x) => x,
            None => break,
        };
        // Decode one (possibly multi-byte) instruction at a time
        match decode_single(inst_byte_window, decode_settings.verbose) {
            Some(inst) => {
                if decode_settings.print {
                    println!("{}", inst.text.as_ref().unwrap());
                }
                ip = ip + inst.processed_bytes.len();
                insts.push(inst);
                // On to the next instruction...
            }
            // Done with the instruction stream
            None => break,
        };
    }
    insts
}

/// Decode a single instruction from a given slice of 16 bytes
///
/// # Approach:
/// An instruction stream is parsed byte by byte, as 8086 has variable-length
/// instructions that require parsing the first and sometimes the second byte
/// before knowing how many total bytes to parse.
///
/// As the instruction is decoded, a new `InstType` struct is filled out with
/// all the decoded fields. This struct is passed to different stages, which
/// update it as needed. The final decoded text is created using the data from
/// this struct. The struct also holds some partially-built values as needed.
///
/// Instructions are decoded in the following stages:
///     1) The first byte is decoded
///     2) If needed, the "mod rm" byte is decoded next
///     3) Any displacement or immediate/data bytes are decoded.
///     4) The final output text is assembled from the values in the `InstType`
///        struct.
///     5) The output text is printed to stdout.
fn decode_single(inst_byte_window: &[u8], debug: bool) -> Option<InstType> {
    let mut inst = InstType {
        ..Default::default()
    };

    let mut iter = inst_byte_window.iter().peekable();
    let mut byte = iter.next().unwrap();
    if debug {
        debug_byte(byte);
    }
    inst.processed_bytes.push(*byte);

    // Decode any prefix bytes
    let mut decode_prefix = decode_prefix_bytes(*byte, &mut inst);
    while decode_prefix {
        // The first byte was a prefix. Are there more?
        byte = iter.next().unwrap();
        if debug {
            debug_byte(byte);
        }
        inst.processed_bytes.push(*byte);
        decode_prefix = decode_prefix_bytes(*byte, &mut inst);
    }

    // Decode first non-prefix byte
    if decode_first_byte(*byte, &mut inst) == false {
        println!("Unknown instruction");
        return None;
    }

    // Process mod rm byte, if it exists
    if inst.mod_rm_byte.is_some() {
        if iter.peek().is_none() {
            println!("Unexpected end of instruction stream");
            return None;
        };
        // Get the next (mod r/m) byte in the stream
        let byte = iter.next().unwrap();
        if debug {
            debug_byte(byte);
        }
        inst.processed_bytes.push(*byte);
        decode_mod_rm_byte(*byte, &mut inst);
    }

    // Get immediate bytes and store in inst
    for byte_type in &inst.immediate_bytes {
        if iter.peek().is_none() {
            println!("Unexpected end of instruction stream");
            return None;
        };

        let byte = iter.next().unwrap();
        if debug {
            debug_byte(byte);
        }
        inst.processed_bytes.push(*byte);

        match byte_type {
            ImmBytesType::DispLo => inst.disp_lo = Some(*byte),
            ImmBytesType::DispHi => inst.disp_hi = Some(*byte),
            ImmBytesType::DataLo => inst.data_lo = Some(*byte),
            ImmBytesType::Data8 => inst.data_8 = Some(*byte),
            ImmBytesType::DataHi => inst.data_hi = Some(*byte),
            ImmBytesType::IpInc8 => inst.ip_inc8 = Some(*byte),
            ImmBytesType::IpIncLo => inst.ip_inc_lohi = Some(*byte as u16),
            ImmBytesType::IpIncHi => {
                inst.ip_inc_lohi = match inst.ip_inc_lohi {
                    Some(val) => Some((*byte as u16) << 8 | val),
                    None => unimplemented!("Hit IpIncHi without preceding InIncLo!"),
                }
            }
            ImmBytesType::IpLo => inst.ip_lohi = Some(*byte as u16),
            ImmBytesType::IpHi => {
                inst.ip_lohi = match inst.ip_lohi {
                    Some(val) => Some((*byte as u16) << 8 | val),
                    None => unimplemented!("Hit IpHi without preceding IpLo!"),
                }
            }
            ImmBytesType::CsLo => inst.cs_lohi = Some(*byte as u16),
            ImmBytesType::CsHi => {
                inst.cs_lohi = match inst.cs_lohi {
                    Some(val) => Some((*byte as u16) << 8 | val),
                    None => unimplemented!("Hit CsHi without preceding CsLo!"),
                }
            }
            ImmBytesType::DoNotCare => {}
        }
    }

    // Now that we have all the data decoded, massage and set any additional
    // info that we want to pass to the execution side.
    calculate_execution_values(&mut inst);

    let inst_test = build_inst_string(&inst);

    // Print out instruction to log, for debug
    if debug {
        println!("inst: {inst_test}");
    }

    inst.text = Some(inst_test);

    // Now that the inst is fully decoded, calculate clock values
    calculate_inst_clocks(&mut inst);

    return Some(inst);
}

/// Set additional public info in the instruction to pass to the execution side.
fn calculate_execution_values(inst: &mut InstType) {
    // Calculate the ip increment value, if it exists
    inst.ip_inc = get_ip_increment(inst.ip_inc8.as_ref(), inst.ip_inc_lohi.as_ref());
    // Calculate the ip absolute value, if it exists
    inst.ip_abs = get_ip_absolute(inst.ip_lohi, inst.cs_lohi);

    // Get the actual u16 value of the data immediate bytes
    if inst.add_data_to.is_some() {
        inst.data_value = Some(get_data_value(
            inst.data_lo.as_ref(),
            inst.data_hi.as_ref(),
            inst.data_8.as_ref(),
            inst.sign_extend_data_lo,
        ));
    }

    if inst.add_disp_to.is_some() {
        // Get the actual i16 value of the disp immediate bytes
        inst.disp_value = get_disp_value(inst.disp_lo.as_ref(), inst.disp_hi.as_ref());
    }

    // NOTE: We can't calculate the effective address until we get into execute,
    // since we need to access register values from the reg file, which is part
    // of the CPU state.
}

fn build_source_dest_strings(inst: &InstType) -> (String, String) {
    // Build source and dest strings
    let mut source_text = String::new();
    let mut dest_text = String::new();

    let mod_rm_addr = get_mod_rm_addr_str(inst.mod_rm_data, inst.disp_value, inst.sr_field);
    match (mod_rm_addr, inst.d_field) {
        (None, _) => {}
        (Some(mod_rm_addr), None | Some(false)) => {
            if inst.far_prefix {
                dest_text.push_str("far ");
            }
            // Dest is rm field
            dest_text.push_str(&mod_rm_addr);
        }
        (Some(mod_rm_addr), Some(true)) => {
            if inst.far_prefix {
                source_text.push_str("far ");
            }
            // Source is rm field
            source_text.push_str(&mod_rm_addr);
        }
    }

    if inst.add_data_to.is_some() {
        let data_bytes_text = process_data_bytes(
            inst.data_lo.as_ref(),
            inst.data_hi.as_ref(),
            inst.data_8.as_ref(),
            inst.sign_extend_data_lo,
        );

        // If this is a mem access, use data bytes for the mem access
        if inst.mem_access {
            match inst.add_data_to {
                Some(AddTo::Source) => {
                    source_text.push_str(&format!("[{}]", data_bytes_text));
                }
                Some(AddTo::Dest) => {
                    dest_text.push_str(&format!("[{}]", data_bytes_text));
                }
                None => unreachable!(),
            };
        } else {
            match inst.add_data_to {
                Some(AddTo::Source) => source_text.push_str(&data_bytes_text),
                Some(AddTo::Dest) => dest_text.push_str(&data_bytes_text),
                None => unreachable!(),
            }
        }
    }

    let ip_str = get_ip_change_str(
        inst.ip_inc8.as_ref(),
        inst.ip_inc_lohi.as_ref(),
        inst.ip_lohi.as_ref(),
        inst.cs_lohi.as_ref(),
        inst.processed_bytes.len(),
    );
    match ip_str {
        Some(str) => dest_text.push_str(&str),
        None => {}
    }

    // Move dest_reg into dest_text if dest_text hasn't been set yet
    match inst.dest_reg {
        Some(dest_reg) => dest_text.push_str(&format!("{dest_reg}")),
        _ => {}
    }

    match (
        inst.source_reg,
        inst.source_hardcoded,
        inst.source_hardcoded_implicit,
    ) {
        // Move source_reg into source_text if source_text hasn't been set yet
        (Some(source_reg), _, _) => source_text.push_str(&format!("{source_reg}")),
        // Move hardcoded source value into source_text
        (_, Some(source_val), false) => source_text.push_str(&format!("{source_val}")),
        _ => {}
    }

    (source_text, dest_text)
}

/// Take all the data in an instruction and build the final instruction string.
fn build_inst_string(inst: &InstType) -> String {
    // Build the source and dest texts
    // TODO: Build source and dest text in order and append to inst_text, to
    // avoid extra String allocations.
    let (source_text, dest_text) = build_source_dest_strings(&inst);

    // Build the final instruction string from left to right
    let mut inst_text = "".to_string();
    /////////////////////////////////////////////////////////
    // Op Code
    /////////////////////////////////////////////////////////
    if inst.prefixes.is_some() {
        for prefix in inst.prefixes.as_ref().unwrap() {
            inst_text.push_str(prefix);
        }
    }
    inst_text.push_str(&inst.op_type.unwrap().to_string());
    if inst.op_type_suffix.is_some() {
        inst_text.push_str(inst.op_type_suffix.unwrap());
    }

    if !dest_text.is_empty() || !source_text.is_empty() {
        inst_text.push(' ');
    }

    /////////////////////////////////////////////////////////
    // Destination
    /////////////////////////////////////////////////////////
    match inst.dest_width {
        Some(width) => inst_text.push_str(width.to_width_prefix_str()),
        _ => {}
    }
    if !dest_text.is_empty() {
        inst_text.push_str(&dest_text);
    }
    if !dest_text.is_empty() && !source_text.is_empty() {
        inst_text.push_str(", ");
    }
    /////////////////////////////////////////////////////////
    // Source
    /////////////////////////////////////////////////////////
    match inst.source_width {
        Some(width) => inst_text.push_str(width.to_width_prefix_str()),
        _ => {}
    }
    if !source_text.is_empty() {
        inst_text.push_str(&source_text);
    }

    inst_text
}

/// Decode any "prefix" bytes to a given instruction, like LOCK.
/// Return true if a prefix byte is found in this byte, and false otherwise.
/// Since we know these are prefixes, we can always add a trailing space.
fn decode_prefix_bytes(byte: u8, inst: &mut InstType) -> bool {
    let prefix = match byte {
        // lock
        0xF0 => {
            inst.lock_prefix = Some(true);
            "lock "
        }
        // rep - string repeat
        0xF2..=0xF3 => {
            inst.has_string_byte = Some(true);
            inst.z_field = Some((byte & 0x1) == 1);
            "rep "
        }
        // Segment register override prefixes
        0x26 | 0x2E | 0x36 | 0x3E => {
            let sr_field = decode_sr_field((byte & 0b000_11_000) >> 3);
            inst.sr_field = Some(sr_field);
            // Have effective address logic deal with printing the sr_field
            return true;
        }
        // This byte isn't a prefix...
        _ => return false,
    };
    match &mut inst.prefixes {
        None => inst.prefixes = Some(vec![prefix]),
        Some(prefixes) => prefixes.push(prefix),
    }
    true
}

/// Decode the first byte of an 8086 instruction.
///
/// Returns true if there are more bytes left in the instruction, and false if
/// not.
fn decode_first_byte(byte: u8, inst: &mut InstType) -> bool {
    // Decode the first byte of the instruction.
    // For 8086 decoding help, see pg. 4-18 through 4-36.
    match byte {
        // add - Reg/memory with register to either
        0x00..=0x03 => {
            inst.op_type = Some(OpCodeType::Add);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // Immediate to reg/mem for add, or, adc, sbb, and, sub, cmp
        0x80..=0x83 => {
            // We don't know the op code yet - it's contained in the second byte
            inst.w_field = Some((byte & 0x1) == 1);
            // TODO: OR, AND, XOR do not use this s_field, so don't set it
            inst.s_field = Some(((byte & 0x2) >> 1) == 1);
            // d field is hard coded to 0: dest is rm and source is immediate
            inst.mod_rm_byte = Some(ModRmByteType::ModImmedRm);
        }
        // add - Immediate to accumulator
        0x04..=0x05 => {
            inst.op_type = Some(OpCodeType::Add);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // adc (add w/ carry) - Reg/mem with register to either
        0x10..=0x13 => {
            inst.op_type = Some(OpCodeType::Adc);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // adc - Immediate to accumulator
        0x14..=0x15 => {
            inst.op_type = Some(OpCodeType::Adc);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // inc - register
        0x40..=0x47 => {
            inst.op_type = Some(OpCodeType::Inc);
            let w_field = true;
            let reg_field = decode_reg_field(byte & 0b111, Some(w_field));
            inst.dest_reg = Some(reg_field);
            inst.reg_field = Some(reg_field);
            if w_field {
                inst.operands_type = Some(OperandsType::Reg16);
            } else {
                inst.operands_type = Some(OperandsType::Reg8);
            }
            inst.w_field = Some(w_field);
            inst.source_hardcoded = Some(1);
            inst.source_hardcoded_implicit = true;
        }
        // aaa - ASCII adjust for add
        0x37 => {
            inst.op_type = Some(OpCodeType::Aaa);
        }
        // aaa - Decimal adjust for add
        0x27 => {
            inst.op_type = Some(OpCodeType::Daa);
        }
        // aas - ASCII adjust for subtract
        0x3F => {
            inst.op_type = Some(OpCodeType::Aas);
        }
        // das - Decimal adjust for subtract
        0x2F => {
            inst.op_type = Some(OpCodeType::Das);
        }
        // aam - ASCII adjust for multiply
        0xD4 => {
            inst.op_type = Some(OpCodeType::Aam);
            // The second byte doesn't add anything. Ignore it for now.
            inst.immediate_bytes.push(ImmBytesType::DoNotCare)
        }
        // aad - ASCII adjust for divide
        0xD5 => {
            inst.op_type = Some(OpCodeType::Aad);
            // The second byte doesn't add anything. Ignore it for now.
            inst.immediate_bytes.push(ImmBytesType::DoNotCare)
        }
        // cbw - Convert byte to word
        0x98 => {
            inst.op_type = Some(OpCodeType::Cbw);
        }
        // cwd - Convert word to double word
        0x99 => {
            inst.op_type = Some(OpCodeType::Cwd);
        }
        // rol, ror, rcl, rcr, shl/sal, shr, sar
        0xD0..=0xD3 => {
            inst.w_field = Some((byte & 0x1) == 1);
            inst.v_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModShiftRm);
        }
        // and - reg/mem with register to either
        0x20..=0x23 => {
            inst.op_type = Some(OpCodeType::And);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // and - Immediate to accumulator
        0x24..=0x25 => {
            inst.op_type = Some(OpCodeType::And);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // or - reg/mem with register to either
        0x08..=0x0B => {
            inst.op_type = Some(OpCodeType::Or);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // or - Immediate to accumulator
        0x0C..=0x0D => {
            inst.op_type = Some(OpCodeType::Or);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // xor - reg/mem with register to either
        0x30..=0x33 => {
            inst.op_type = Some(OpCodeType::Xor);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // xor - Immediate to accumulator
        0x34..=0x35 => {
            inst.op_type = Some(OpCodeType::Xor);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // mov - Register/memory to/from register
        0x88..=0x8B => {
            inst.op_type = Some(OpCodeType::Mov);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
            // We need to see what mod is before we know what is the source
            // and what is the destination
        }
        // mov - Immediate to register/memory
        0xC6..=0xC7 => {
            inst.op_type = Some(OpCodeType::Mov);
            inst.w_field = Some((byte & 0x1) == 1);
            // In effect, the d field is hard coded to 0: the destination is
            // rm and the source is an immediate (which replaced reg from
            // the above mov variant)
            inst.mod_rm_byte = Some(ModRmByteType::ModMovRm);
        }
        // mov - Immediate to register
        0xB0..=0xBF => {
            inst.op_type = Some(OpCodeType::Mov);
            inst.w_field = Some(((byte & 0b1000) >> 3) == 1);
            let reg_field = decode_reg_field(byte & 0b111, inst.w_field);
            inst.reg_field = Some(reg_field.clone());
            inst.dest_reg = Some(reg_field);
            // No mod rm byte for this mov variant!
            // Indicate that there are source data bytes after this byte
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            match inst.w_field {
                Some(true) => {
                    inst.immediate_bytes.push(ImmBytesType::DataHi);
                },
                _ => {},
            }
            inst.operands_type = Some(OperandsType::RegImm);
        }
        // mov - Memory to accumulator or accumulator to memory
        0xA0..=0xA3 => {
            inst.op_type = Some(OpCodeType::Mov);
            let w_field = (byte & 0x1) == 1;
            let accumulator = Some(RegType{name: RegName::Ax, width: WidthType::Word});
            inst.mem_access = true;
            match ((byte & 0x2) >> 1) == 1 {
                false => {
                    inst.dest_reg = accumulator;
                    inst.add_data_to = Some(AddTo::Source);
                    inst.operands_type = Some(OperandsType::AccMem);
                }
                true => {
                    inst.source_reg = accumulator;
                    inst.add_data_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemAcc);
                }
            };
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            }
            inst.w_field = Some(w_field);
        }
        // mov - Register/memory to segment register
        // mov - Segment register to register/memory
        // The docs have these as two separate instructions, but they are really
        // one instruction with a d field as bit 2
        0x8C | 0x8E => {
            inst.op_type = Some(OpCodeType::Mov);
            inst.mod_rm_byte = Some(ModRmByteType::ModSrRm);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            // Hard code w to 1, since no w field in instruction
            inst.w_field = Some(true);
        }
        // inc/dec/call/push - Register/memory
        // jmp - indirect within segment and indirect intersegment
        0xFE..=0xFF => {
            inst.mod_rm_byte = Some(ModRmByteType::ModGrp2Rm);
            // NOTE: The first byte hardcodes w to 1 for pushes, pops, and jmps,
            // since the operand is always 16 bits.
            inst.w_field = Some((byte & 0x1) == 1);
        }
        // jmp - Direct within segment
        0xE9 => {
            inst.op_type = Some(OpCodeType::Jmp);
            inst.immediate_bytes.push(ImmBytesType::IpIncLo);
            inst.immediate_bytes.push(ImmBytesType::IpIncHi);
        }
        // jmp - Direct within segment-short
        0xEB => {
            inst.op_type = Some(OpCodeType::Jmp);
            inst.immediate_bytes.push(ImmBytesType::IpInc8);
        }
        // jmp - Direct intersegment
        0xEA => {
            inst.op_type = Some(OpCodeType::Jmp);
            inst.immediate_bytes.push(ImmBytesType::IpLo);
            inst.immediate_bytes.push(ImmBytesType::IpHi);
            inst.immediate_bytes.push(ImmBytesType::CsLo);
            inst.immediate_bytes.push(ImmBytesType::CsHi);
        }
        // call - Direct within segment (intrasegment/near)
        0xE8 => {
            inst.op_type = Some(OpCodeType::Call);
            inst.immediate_bytes.push(ImmBytesType::IpIncLo);
            inst.immediate_bytes.push(ImmBytesType::IpIncHi);
            inst.operands_type = Some(OperandsType::NearProc)
        }
        // call - Direct intersegment
        0x9A => {
            inst.op_type = Some(OpCodeType::Call);
            inst.immediate_bytes.push(ImmBytesType::IpLo);
            inst.immediate_bytes.push(ImmBytesType::IpHi);
            inst.immediate_bytes.push(ImmBytesType::CsLo);
            inst.immediate_bytes.push(ImmBytesType::CsHi);
            inst.operands_type = Some(OperandsType::FarProc)
        }
        // ret - Within segment
        0xC3 => {
            inst.op_type = Some(OpCodeType::Ret);
            inst.operands_type = Some(OperandsType::NoPop)
        }
        // ret - Within segment adding immediate to SP
        0xC2 => {
            inst.op_type = Some(OpCodeType::Ret);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            inst.immediate_bytes.push(ImmBytesType::DataHi);
            inst.add_data_to = Some(AddTo::Source);
            inst.operands_type = Some(OperandsType::Pop)
        }
        // retf - Intersegment ret
        0xCB => {
            inst.op_type = Some(OpCodeType::Retf);
            inst.operands_type = Some(OperandsType::NoPop)
        }
        // retf - Intersegment ret adding immediate to SP
        0xCA => {
            inst.op_type = Some(OpCodeType::Retf);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            inst.immediate_bytes.push(ImmBytesType::DataHi);
            inst.add_data_to = Some(AddTo::Source);
            inst.operands_type = Some(OperandsType::Pop)
        }
        // push - Register
        0x50..=0x57 => {
            inst.op_type = Some(OpCodeType::Push);
            // Hardcode registers to 16-bit widths
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field.clone());
            inst.source_reg = Some(reg_field);
            inst.operands_type = Some(OperandsType::Reg16);
            // Dest is implicit (stack)
        }
        // push - segment register - 0x06,0x0E,0x16,0x1E
        0b000_00_110 | 0b000_01_110 | 0b000_10_110 | 0b000_11_110 => {
            inst.op_type = Some(OpCodeType::Push);
            let sr_field = decode_sr_field((byte & 0b000_11_000) >> 3);
            inst.sr_field = Some(sr_field.clone());
            inst.source_reg = Some(sr_field);
            inst.operands_type = Some(OperandsType::Seg);
            // Dest is implicit (stack)
        }
        // pop - Register/memory
        0x8F => {
            inst.op_type = Some(OpCodeType::Pop);
            inst.mod_rm_byte = Some(ModRmByteType::ModPopRm);
            // Source is implicit (stack)
        }
        // pop - Register
        0x58..=0x5F => {
            inst.op_type = Some(OpCodeType::Pop);
            // Hardcode registers to 16-bit widths
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field.clone());
            inst.dest_reg = Some(reg_field);
            inst.operands_type = Some(OperandsType::Reg16);
            // Source is implicit (stack)
        }
        // pop - segment register - 0x07,~0x0F~,0x17,0x1F
        // pop CS (0x0f) is apparently not a thing in 8086
        0b000_00_111 /*| 0b000_01_111*/ | 0b000_10_111 | 0b000_11_111 => {
            inst.op_type = Some(OpCodeType::Pop);
            let sr_field = decode_sr_field((byte & 0b000_11_000) >> 3);
            inst.sr_field = Some(sr_field.clone());
            inst.dest_reg = Some(sr_field);
            inst.operands_type = Some(OperandsType::Reg16);
            // Source is implicit (stack)
        }
        // xchg - Reg/memory with register
        0x86..=0x87 => {
            inst.op_type = Some(OpCodeType::Xchg);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(true);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // xchg - Register with accumulator
        0x90..=0x97 => {
            inst.op_type = Some(OpCodeType::Xchg);
            inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field);
            inst.source_reg = Some(reg_field);
            inst.operands_type = Some(OperandsType::AccImm);
        }
        // in - fixed port
        0xE4..=0xE5 => {
            inst.op_type = Some(OpCodeType::In);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.immediate_bytes.push(ImmBytesType::Data8);
            inst.add_data_to = Some(AddTo::Source);
            inst.w_field = Some(w_field);
        }
        // in - variable port
        0xEC..=0xED => {
            inst.op_type = Some(OpCodeType::In);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.source_reg = Some(RegType{name: RegName::Dx, width: WidthType::Word});
            inst.w_field = Some(w_field);
        }
        // out - fixed port
        0xE6..=0xE7 => {
            inst.op_type = Some(OpCodeType::Out);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.source_reg = Some(RegType { name: RegName::Ax, width: WidthType::Word });
            } else {
                inst.source_reg = Some(RegType { name: RegName::Ax, width:WidthType::Byte });
            }
            inst.immediate_bytes.push(ImmBytesType::Data8);
            inst.add_data_to = Some(AddTo::Dest);
            inst.w_field = Some(w_field);
        }
        // out - variable port
        0xEE..=0xEF => {
            inst.op_type = Some(OpCodeType::Out);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.source_reg = Some(RegType { name: RegName::Ax, width: WidthType::Word });
            } else {
                inst.source_reg = Some(RegType { name: RegName::Ax, width: WidthType::Byte });
            }
            inst.dest_reg = Some(RegType { name: RegName::Dx, width: WidthType::Word });
            inst.w_field = Some(w_field);
        }
        // xlat
        0xD7 => {
            // "xlat (translate) replaces a byte in the AL register with a byte
            // from a 256-byte, user-coded translation tablet (source table).
            // Register BX is assumed to point to the beginning of the table.
            // The byte in AL is used as an index into the table and is replaced
            // by the byte as the offset in the table corresponding to AL's
            // binary value"
            inst.op_type = Some(OpCodeType::Xlat);
        }
        // lea - Load EA (effective address) to register
        0x8D => {
            inst.op_type = Some(OpCodeType::Lea);
            // Hardcode registers to 16 bits, and REG as destination
            inst.w_field = Some(true);
            inst.d_field = Some(true);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // lds - Load 32-bit pointer to DS (high 16 bits) and REG (low 16 bits)
        0xC5 => {
            inst.op_type = Some(OpCodeType::Lds);
            // Hardcode registers to 16 bits, and REG as destination
            inst.w_field = Some(true);
            inst.d_field = Some(true);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // lds - Load 32-bit pointer to ES (high 16 bits) and REG (low 16 bits)
        0xC4 => {
            inst.op_type = Some(OpCodeType::Les);
            // Hardcode registers to 16 bits, and REG as destination
            inst.w_field = Some(true);
            inst.d_field = Some(true);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // lahf - Load AH with flags
        0x9F => {
            inst.op_type = Some(OpCodeType::Lahf);
        }
        // sahf - Store AH into flags
        0x9E => {
            inst.op_type = Some(OpCodeType::Sahf);
        }
        // pushf - Push flags
        0x9C => {
            inst.op_type = Some(OpCodeType::Pushf);
            // Flags reg source and dest are implicit
        }
        // popf - Pop flags
        0x9D => {
            inst.op_type = Some(OpCodeType::Popf);
            // Flags reg src and dest are implicit
        }
        // sub - Reg/memory and register to either
        0x28..=0x2B => {
            inst.op_type = Some(OpCodeType::Sub);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // sub - Immediate from accumulator
        0x2C..=0x2D => {
            inst.op_type = Some(OpCodeType::Sub);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // sbb (subtract with borrow) - Reg/memory and register to either
        0x18..=0x1B => {
            inst.op_type = Some(OpCodeType::Sbb);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // sbb - Immediate from accumulator
        0x1C..=0x1D => {
            inst.op_type = Some(OpCodeType::Sbb);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        // dec - register
        0x48..=0x4F => {
            inst.op_type = Some(OpCodeType::Dec);
            let w_field = true;
            let reg_field = decode_reg_field(byte & 0b111, Some(w_field));
            inst.dest_reg = Some(reg_field);
            inst.reg_field = Some(reg_field);
            if w_field {
                inst.operands_type = Some(OperandsType::Reg16);
            } else {
                inst.operands_type = Some(OperandsType::Reg8);
            }
            inst.w_field = Some(w_field);
            inst.source_hardcoded = Some(1);
            inst.source_hardcoded_implicit = true;
        }
        // test,not,neg,mul,imul,div,idiv - immediate data and register/memory
        0xF6..=0xF7 => {
            inst.mod_rm_byte = Some(ModRmByteType::ModGrp1Rm);
            inst.w_field = Some((byte & 0x1) == 1);
        }
        // cmp - Register/memory and register
        0x38..=0x3B => {
            inst.op_type = Some(OpCodeType::Cmp);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // cmp - Immediate with accumulator
        0x3C..=0x3D => {
            inst.op_type = Some(OpCodeType::Cmp);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.operands_type = Some(OperandsType::AccImm);
            inst.w_field = Some(w_field);
        }
        0x70..=0x7F | 0xE0..=0xE3 => {
            inst.immediate_bytes.push(ImmBytesType::IpInc8);
            match byte {
                0x74 => inst.op_type = Some(OpCodeType::Je),
                0x7C => inst.op_type = Some(OpCodeType::Jl),
                0x7E => inst.op_type = Some(OpCodeType::Jle),
                0x72 => inst.op_type = Some(OpCodeType::Jb),
                0x76 => inst.op_type = Some(OpCodeType::Jbe),
                0x7A => inst.op_type = Some(OpCodeType::Jp),
                0x70 => inst.op_type = Some(OpCodeType::Jo),
                0x78 => inst.op_type = Some(OpCodeType::Js),
                0x75 => inst.op_type = Some(OpCodeType::Jne),
                0x7D => inst.op_type = Some(OpCodeType::Jnl),
                0x7F => inst.op_type = Some(OpCodeType::Jg),
                0x73 => inst.op_type = Some(OpCodeType::Jnb),
                0x77 => inst.op_type = Some(OpCodeType::Ja),
                0x7B => inst.op_type = Some(OpCodeType::Jnp),
                0x71 => inst.op_type = Some(OpCodeType::Jno),
                0x79 => inst.op_type = Some(OpCodeType::Jns),
                0xE2 => inst.op_type = Some(OpCodeType::Loop),
                0xE1 => inst.op_type = Some(OpCodeType::Loopz),
                0xE0 => inst.op_type = Some(OpCodeType::Loopnz),
                0xE3 => inst.op_type = Some(OpCodeType::Jcxz),
                _ => unreachable!(),
            }
        }
        // test - Register/memory and register
        0x84..=0x87 => {
            inst.op_type = Some(OpCodeType::Test);
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // test - Immediate to accumulator
        0xA8..=0xA9 => {
            inst.op_type = Some(OpCodeType::Test);
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            if w_field {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Word});
                inst.immediate_bytes.push(ImmBytesType::DataHi);
            } else {
                inst.dest_reg = Some(RegType{name: RegName::Ax, width:WidthType::Byte});
            }
            inst.w_field = Some(w_field);
            inst.operands_type = Some(OperandsType::AccImm);
        }
        // int - Type specified
        0xCD => {
            inst.op_type = Some(OpCodeType::Int);
            inst.add_data_to = Some(AddTo::Source);
            inst.immediate_bytes.push(ImmBytesType::Data8);
        }
        // int - Type 3
        0xCC => {
            inst.op_type = Some(OpCodeType::Int3);
        }
        // into - Interrupt on overflow
        0xCE => {
            inst.op_type = Some(OpCodeType::Into);
        }
        // iret - Interrupt return
        0xCF => {
            inst.op_type = Some(OpCodeType::Iret);
        }
        // clc - Clear carry
        0xF8 => {
            inst.op_type = Some(OpCodeType::Clc);
        }
        // cmc - Complement carry
        0xF5 => {
            inst.op_type = Some(OpCodeType::Cmc);
        }
        // stc - Set carry
        0xF9 => {
            inst.op_type = Some(OpCodeType::Stc);
        }
        // cld - Clear direction
        0xFC => {
            inst.op_type = Some(OpCodeType::Cld);
        }
        // std - Set direction
        0xFD => {
            inst.op_type = Some(OpCodeType::Std);
        }
        // cli - Clear interrupt
        0xFA => {
            inst.op_type = Some(OpCodeType::Cli);
        }
        // sti - Set interrupt
        0xFB => {
            inst.op_type = Some(OpCodeType::Sti);
        }
        // hlt - Halt
        0xF4 => {
            inst.op_type = Some(OpCodeType::Hlt);
        }
        // wait
        0x9B => {
            inst.op_type = Some(OpCodeType::Wait);
        }
        0xA4..=0xA5 => {
            assert!(inst.has_string_byte.is_some());
            let w_field = (byte & 0x1) == 1;
            inst.op_type = Some(OpCodeType::Movs);
            inst.op_type_suffix = if w_field { Some("w") } else { Some("b") };
            inst.w_field = Some(w_field);
        }
        0xA6..=0xA7 => {
            assert!(inst.has_string_byte.is_some());
            let w_field = (byte & 0x1) == 1;
            inst.op_type = Some(OpCodeType::Cmps);
            inst.op_type_suffix = if w_field { Some("w") } else { Some("b") };
            inst.w_field = Some(w_field);
        }
        0xAE..=0xAF => {
            assert!(inst.has_string_byte.is_some());
            let w_field = (byte & 0x1) == 1;
            inst.op_type = Some(OpCodeType::Scas);
            inst.op_type_suffix = if w_field { Some("w") } else { Some("b") };
            inst.w_field = Some(w_field);
        }
        0xAC..=0xAD => {
            assert!(inst.has_string_byte.is_some());
            let w_field = (byte & 0x1) == 1;
            inst.op_type = Some(OpCodeType::Lods);
            inst.op_type_suffix = if w_field { Some("w") } else { Some("b") };
            inst.w_field = Some(w_field);
        }
        0xAA..=0xAB => {
            assert!(inst.has_string_byte.is_some());
            let w_field = (byte & 0x1) == 1;
            inst.op_type = Some(OpCodeType::Stos);
            inst.op_type_suffix = if w_field { Some("w") } else { Some("b") };
            inst.w_field = Some(w_field);
        }
        _ => {
            return false;
        }
    }
    true
}

/// Decode the "mod rm" byte of an 8086 instruction.
///
/// The mod rm byte, if it exists, is the second byte of an 8086 instruction
/// that specifies further bytes and addressing modes.
fn decode_mod_rm_byte(byte: u8, inst: &mut InstType) {
    // Decode the second byte of the instruction.
    // Get the upper two bits
    let mode = decode_mod_field((byte & 0b11000000) >> 6);
    let rm_field = byte & 0b00000111;
    let mod_rm_data = decode_rm_field(rm_field, mode, inst.w_field);

    // Indicate that there are displacement bytes to process next
    // Displacement bytes come before immediate/data bytes
    let has_disp = match (mode, mod_rm_data) {
        (ModType::MemoryMode8, _) => {
            inst.immediate_bytes.push(ImmBytesType::DispLo);
            true
        }
        (ModType::MemoryMode16, _) => {
            inst.immediate_bytes.push(ImmBytesType::DispLo);
            inst.immediate_bytes.push(ImmBytesType::DispHi);
            true
        }
        (ModType::MemoryMode0, ModRmDataType::MemDirectAddr) => {
            inst.immediate_bytes.push(ImmBytesType::DispLo);
            inst.immediate_bytes.push(ImmBytesType::DispHi);
            true
        }
        _ => false,
    };

    if has_disp {
        // Indicate what displacement should be added to: src or dest
        match inst.d_field {
            None | Some(false) => inst.add_disp_to = Some(AddTo::Dest),
            Some(true) => inst.add_disp_to = Some(AddTo::Source),
        }
    }

    // If mod rm is a straight reg, set the source or dest reg
    match (mod_rm_data, inst.d_field) {
        (ModRmDataType::Reg(reg), Some(false) | None) => inst.dest_reg = Some(reg),
        (ModRmDataType::Reg(reg), Some(true)) => inst.source_reg = Some(reg),
        _ => {}
    };

    // Process the middle part of the mod rm byte
    match inst.mod_rm_byte {
        Some(ModRmByteType::ModRegRm) => {
            let reg_field = decode_reg_field((byte & 0b00111000) >> 3, inst.w_field);
            // See if reg is specified as source or dest
            match inst.d_field {
                None | Some(false) => {
                    // Source is REG field
                    inst.source_reg = Some(reg_field);
                }
                Some(true) => {
                    // Destination is REG field
                    inst.dest_reg = Some(reg_field);
                }
            };
            inst.reg_field = Some(reg_field);
            // We need no byte/word prefix for ModRegRm, since there is always
            // a register source/dest to indicate size

            // Set operand types and memory address direction
            match (mode, inst.d_field) {
                (ModType::RegisterMode, _) => inst.operands_type = Some(OperandsType::RegReg),
                (_, Some(true)) => {
                    inst.add_mod_rm_mem_to = Some(AddTo::Source);
                    inst.operands_type = Some(OperandsType::RegMem)
                }
                (_, _) => {
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemReg)
                }
            }
        }
        Some(ModRmByteType::ModMovRm) => {
            inst.immediate_bytes.push(ImmBytesType::DataLo);
            match inst.w_field {
                Some(true) => {
                    inst.immediate_bytes.push(ImmBytesType::DataHi);
                }
                _ => {}
            }
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                (_, Some(false)) => inst.source_width = Some(WidthType::Byte),
                (_, Some(true)) => inst.source_width = Some(WidthType::Word),
                _ => {}
            }

            // Set operand types and memory address direction
            match mode {
                ModType::RegisterMode => inst.operands_type = Some(OperandsType::RegImm),
                _ => {
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemImm);
                }
            }
        }
        Some(ModRmByteType::ModImmedRm) => {
            inst.op_type = Some(decode_immed_op((byte & 0b00111000) >> 3));
            match (inst.w_field, inst.s_field) {
                (None, _) => {
                    panic!("This ModImmedRm inst is missing the w field!")
                }
                (Some(false), _) => {
                    inst.immediate_bytes.push(ImmBytesType::DataLo);
                }
                (Some(true), Some(true)) => {
                    inst.immediate_bytes.push(ImmBytesType::DataLo);
                    inst.sign_extend_data_lo = true;
                    // The sign_extend_data_lo will sign extend DataLo as a
                    // 16-bit value
                }
                (Some(true), None | Some(false)) => {
                    inst.immediate_bytes.push(ImmBytesType::DataLo);
                    inst.immediate_bytes.push(ImmBytesType::DataHi);
                }
            }
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                (_, Some(false)) => inst.source_width = Some(WidthType::Byte),
                (_, Some(true)) => inst.source_width = Some(WidthType::Word),
                _ => {}
            }
            // Set operand types and memory address direction
            match mode {
                ModType::RegisterMode => inst.operands_type = Some(OperandsType::RegImm),
                _ => {
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemImm);
                }
            }
        }
        Some(ModRmByteType::ModShiftRm) => {
            inst.op_type = Some(decode_shift_op((byte & 0b00111000) >> 3));
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                // Set destination width for shifts in memory
                (_, Some(false)) => inst.dest_width = Some(WidthType::Byte),
                (_, Some(true)) => inst.dest_width = Some(WidthType::Word),
                (_, None) => unreachable!(),
            }
            // Set operand types and memory address direction
            match (mode, inst.v_field) {
                (ModType::RegisterMode, Some(true)) => {
                    inst.source_reg = Some(RegType {
                        name: RegName::Cx,
                        width: WidthType::Byte,
                    });
                    inst.operands_type = Some(OperandsType::RegReg)
                }
                (ModType::RegisterMode, _) => {
                    // Create a hardcoded immediate value of 1
                    inst.source_hardcoded = Some(1);
                    inst.operands_type = Some(OperandsType::RegImm)
                }
                (_, Some(true)) => {
                    inst.source_reg = Some(RegType {
                        name: RegName::Cx,
                        width: WidthType::Byte,
                    });
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemReg);
                }
                (_, _) => {
                    // Create a hardcoded immediate value of 1
                    inst.source_hardcoded = Some(1);
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                    inst.operands_type = Some(OperandsType::MemImm);
                }
            }
        }
        Some(ModRmByteType::ModGrp1Rm) => {
            let op_type = decode_grp1_op((byte & 0b00111000) >> 3);
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                // ModGrp1Rm instructions are 1-operand, so add prefix to dest
                (_, Some(false)) => inst.dest_width = Some(WidthType::Byte),
                (_, Some(true)) => inst.dest_width = Some(WidthType::Word),
                (_, None) => unreachable!(),
            }
            match op_type {
                OpCodeType::Test => {
                    inst.add_data_to = Some(AddTo::Source);
                    inst.immediate_bytes.push(ImmBytesType::DataLo);
                    match inst.w_field {
                        Some(true) => inst.immediate_bytes.push(ImmBytesType::DataHi),
                        _ => {}
                    }
                    // Set cycles info
                    match mode {
                        ModType::RegisterMode => inst.operands_type = Some(OperandsType::RegImm),
                        _ => inst.operands_type = Some(OperandsType::MemImm),
                    }
                }
                OpCodeType::Imul => {
                    // Set cycles info
                    match (mode, inst.w_field) {
                        (ModType::RegisterMode, Some(true)) => {
                            inst.operands_type = Some(OperandsType::Reg16)
                        }
                        (ModType::RegisterMode, _) => inst.operands_type = Some(OperandsType::Reg8),
                        (_, Some(true)) => inst.operands_type = Some(OperandsType::Mem16),
                        _ => inst.operands_type = Some(OperandsType::Mem8),
                    }
                }
                _ => {}
            }
            inst.op_type = Some(op_type);
        }
        Some(ModRmByteType::ModGrp2Rm) => {
            let (op_type, is_intersegment) = decode_grp2_op((byte & 0b00111000) >> 3);
            inst.op_type = Some(op_type);
            inst.far_prefix = is_intersegment;
            match (mode, op_type) {
                (ModType::RegisterMode, _) => {}
                (_, OpCodeType::Push) => inst.add_mod_rm_mem_to = Some(AddTo::Source),
                (_, OpCodeType::Inc | OpCodeType::Dec) => {
                    inst.source_hardcoded = Some(1);
                    inst.source_hardcoded_implicit = true;
                    inst.add_mod_rm_mem_to = Some(AddTo::Dest);
                }
                (_, _) => inst.add_mod_rm_mem_to = Some(AddTo::Dest),
            }
            match (op_type, mode, inst.w_field) {
                // We know the size if Register Mode
                (_, ModType::RegisterMode, Some(w_field)) => {
                    if w_field {
                        inst.operands_type = Some(OperandsType::Reg16);
                    } else {
                        inst.operands_type = Some(OperandsType::Reg8);
                    }
                }
                (OpCodeType::Jmp | OpCodeType::Call, _, _) => {
                    if is_intersegment {
                        inst.operands_type = Some(OperandsType::MemPtr32);
                    } else {
                        inst.operands_type = Some(OperandsType::MemPtr16);
                    }
                }
                // ModGrp2Rm instructions are 1-operand, so add prefix to dest
                (_, _, Some(false)) => {
                    inst.dest_width = Some(WidthType::Byte);
                    inst.operands_type = Some(OperandsType::Mem);
                }
                (_, _, Some(true)) => {
                    inst.dest_width = Some(WidthType::Word);
                    inst.operands_type = Some(OperandsType::Mem);
                }
                _ => unimplemented!(),
            }
        }
        Some(ModRmByteType::ModPopRm) => {
            match mode {
                ModType::RegisterMode => {}
                _ => inst.dest_width = Some(WidthType::Word),
            }
            match mode {
                // We know the size if Register Mode
                ModType::RegisterMode => {
                    inst.operands_type = Some(OperandsType::Reg16);
                }
                _ => {
                    inst.operands_type = Some(OperandsType::Mem);
                }
            }
        }
        // Similar to ModRegRm
        Some(ModRmByteType::ModSrRm) => {
            let sr_reg = decode_sr_field((byte & 0b00011000) >> 3);
            match inst.d_field {
                Some(false) => inst.source_reg = Some(sr_reg),
                Some(true) => inst.dest_reg = Some(sr_reg),
                _ => unreachable!(),
            };
            inst.reg_field = Some(sr_reg);
            // We need no byte/word prefix for ModSrRm, since there is always
            // a register source/dest to indicate size
        }
        None => unreachable!(),
    }

    inst.mod_rm_data = Some(mod_rm_data);
    inst.mod_field = Some(mode);
    inst.rm_field = Some(rm_field);
}

/// Return the memory address string from mod rm and the displacement.
/// See Execute::mod_rm_to_addr()
fn get_mod_rm_addr_str(
    mod_rm_data: Option<ModRmDataType>,
    disp_value: Option<i16>,
    seg_reg: Option<RegType>,
) -> Option<String> {
    let mod_rm_data = match mod_rm_data {
        // Don't print anything here, since the reg will already have been
        // copied into a source or dest reg and printed via that.
        None | Some(ModRmDataType::Reg(_)) => return None,
        Some(x) => x,
    };

    // Print out segment override, if it exists
    let mut str = match seg_reg {
        Some(seg_reg) => format!("[{seg_reg}:"),
        None => format!("["),
    };

    match (mod_rm_data, disp_value) {
        (ModRmDataType::MemDirectAddr, Some(address)) => str.push_str(&format!("0x{address:04X}]")),
        (ModRmDataType::MemReg(reg), _) => str.push_str(&format!("{reg}]")),
        (ModRmDataType::MemRegReg(reg1, reg2), _) => str.push_str(&format!("{reg1} + {reg2}]")),
        (ModRmDataType::MemRegDisp(_), None) => {
            unreachable!("ERROR: No displacement found for MemRegDisp")
        }
        (ModRmDataType::MemRegDisp(reg), Some(disp)) => {
            let sign = if disp >= 0 { "+" } else { "-" };
            str.push_str(&format!("{reg} {sign} {}]", disp.abs()))
        }
        (ModRmDataType::MemRegRegDisp(_, _), None) => {
            unreachable!("ERROR: No displacement found for MemRegRegDisp")
        }
        (ModRmDataType::MemRegRegDisp(reg1, reg2), Some(disp)) => {
            let sign = if disp >= 0 { "+" } else { "-" };
            str.push_str(&format!("{reg1} + {reg2} {sign} {}]", disp.abs()))
        }
        _ => unreachable!(),
    }
    Some(str)
}

fn get_disp_value(disp_lo: Option<&u8>, disp_hi: Option<&u8>) -> Option<i16> {
    match (disp_lo, disp_hi) {
        (Some(lo), None) => Some(*lo as i8 as i16),
        (Some(lo), Some(hi)) => Some(*lo as i16 + ((*hi as i16) << 8)),
        _ => None,
    }
}

fn get_data_value(
    data_lo: Option<&u8>,
    data_hi: Option<&u8>,
    data_8: Option<&u8>,
    sign_extend_data_lo: bool,
) -> u16 {
    match (data_lo, data_hi, data_8, sign_extend_data_lo) {
        (Some(lo), None, None, false) => *lo as u16,
        (Some(lo), None, None, true) => sign_extend_byte(*lo) as u16,
        (Some(lo), Some(hi), None, _) => *lo as u16 | ((*hi as u16) << 8),
        (None, None, Some(data8), _) => *data8 as u16,
        (None, _, _, true) => unreachable!("ERROR: sign_extend_data_lo set when no data_lo"),
        (None, None, None, _) => unreachable!("ERROR: No data bytes found"),
        (None, Some(_), _, _) => unreachable!("ERROR: Low data byte not set"),
        _ => panic!("Unhandled case in process_data_bytes()"),
    }
}

/// Process the data (immediate) bytes by applying it to the needed fields in
/// the instruction struct
fn process_data_bytes(
    data_lo: Option<&u8>,
    data_hi: Option<&u8>,
    data_8: Option<&u8>,
    sign_extend_data_lo: bool,
) -> String {
    let data_value = get_data_value(data_lo, data_hi, data_8, sign_extend_data_lo);
    // The value is formatted differently depending on what data bytes were set
    match (data_lo, data_hi, data_8, sign_extend_data_lo) {
        (Some(_), None, None, false) => format!("{}", data_value as u8),
        (Some(_), None, None, true) => format!("{}", data_value as i16),
        (Some(_), Some(_), None, _) => {
            format!("{}", data_value)
        }
        (None, None, Some(data8), _) => format!("{}", *data8),
        (None, _, _, true) => unreachable!("ERROR: sign_extend_data_lo set when no data_lo"),
        (None, None, None, _) => unreachable!("ERROR: No data bytes found"),
        (None, Some(_), _, _) => unreachable!("ERROR: Low data byte not set"),
        _ => panic!("Unhandled case in process_data_bytes()"),
    }
}

/// Take in two possible IP increment values and return a single i16 value
fn get_ip_increment(ip_inc8: Option<&u8>, ip_inc_lohi: Option<&u16>) -> Option<i16> {
    match (ip_inc8, ip_inc_lohi) {
        (Some(ip_inc8), _) => Some((*ip_inc8 as i8) as i16),
        (_, Some(ip_inc16)) => Some(*ip_inc16 as i16),
        _ => None,
    }
}

/// Calculate an absolute IP value, given hardcoded IP and CS values
pub fn get_ip_absolute(ip_lohi: Option<u16>, cs_lohi: Option<u16>) -> Option<u16> {
    match (ip_lohi, cs_lohi) {
        (Some(ip_abs), Some(cs)) => Some(ip_abs as u16 + (cs << 4)),
        _ => None,
    }
}

/// Take in ip HiLo and hardcoded CS value and return a string representation
fn get_ip_absolute_str(ip_lohi: Option<&u16>, cs_lohi: Option<&u16>) -> Option<String> {
    match (ip_lohi, cs_lohi) {
        (Some(ip_lohi), Some(cs_lohi)) => Some(format!("{cs_lohi}:{ip_lohi}")),
        _ => None,
    }
}

/// Take in all possible IP-changing sources and return a string representing
/// the new IP value.
///
/// The tricky part is that we can't recreate label text - but all labels are
/// just translated into relative offsets to the IP. So we use `$` in NASM to
/// encode this relative offset. $ refers to the IP of the current assembly
/// line. However, while executing an instruction, the IP always refers to the
/// next instruction. Thus, $ == IP - len. So when a jump instruction does IP =
/// IP + X, that is really IP = ($ + len) + X, which is why we add len.
fn get_ip_change_str(
    ip_inc8: Option<&u8>,
    ip_inc_lohi: Option<&u16>,
    ip_lohi: Option<&u16>,
    cs_lohi: Option<&u16>,
    len: usize,
) -> Option<String> {
    let ip_inc = get_ip_increment(ip_inc8, ip_inc_lohi);
    let ip_abs_str = get_ip_absolute_str(ip_lohi, cs_lohi);
    match (ip_inc, ip_abs_str) {
        (Some(ip_inc), _) => {
            let ip_val = ip_inc + len as i16;
            Some(format!("${:+}", ip_val))
        }
        (_, ip_abs_str) => ip_abs_str,
    }
}

/// Print out the hex and binary of a byte in an assembly comment
fn debug_byte(byte: &u8) {
    println!("Processing byte 0x{byte:02X} (0b{byte:08b})");
}

/// MOD (Mode) Field Encoding
///
/// See table 4-8
fn decode_mod_field(mode: u8) -> ModType {
    match mode {
        0b00 => ModType::MemoryMode0,
        0b01 => ModType::MemoryMode8,
        0b10 => ModType::MemoryMode16,
        0b11 => ModType::RegisterMode,
        _ => unreachable!(),
    }
}

/// REG (Register) Field Encoding
///
/// See table 4-9
fn decode_reg_field(reg: u8, w: Option<bool>) -> RegType {
    match (reg, w) {
        (0b000, None | Some(false)) => RegType {
            name: RegName::Ax,
            width: WidthType::Byte,
        },
        (0b001, None | Some(false)) => RegType {
            name: RegName::Cx,
            width: WidthType::Byte,
        },
        (0b010, None | Some(false)) => RegType {
            name: RegName::Dx,
            width: WidthType::Byte,
        },
        (0b011, None | Some(false)) => RegType {
            name: RegName::Bx,
            width: WidthType::Byte,
        },
        (0b100, None | Some(false)) => RegType {
            name: RegName::Ax,
            width: WidthType::Hi8,
        },
        (0b101, None | Some(false)) => RegType {
            name: RegName::Cx,
            width: WidthType::Hi8,
        },
        (0b110, None | Some(false)) => RegType {
            name: RegName::Dx,
            width: WidthType::Hi8,
        },
        (0b111, None | Some(false)) => RegType {
            name: RegName::Bx,
            width: WidthType::Hi8,
        },
        (0b000, Some(true)) => RegType {
            name: RegName::Ax,
            width: WidthType::Word,
        },
        (0b001, Some(true)) => RegType {
            name: RegName::Cx,
            width: WidthType::Word,
        },
        (0b010, Some(true)) => RegType {
            name: RegName::Dx,
            width: WidthType::Word,
        },
        (0b011, Some(true)) => RegType {
            name: RegName::Bx,
            width: WidthType::Word,
        },
        (0b100, Some(true)) => RegType {
            name: RegName::Sp,
            width: WidthType::Word,
        },
        (0b101, Some(true)) => RegType {
            name: RegName::Bp,
            width: WidthType::Word,
        },
        (0b110, Some(true)) => RegType {
            name: RegName::Si,
            width: WidthType::Word,
        },
        (0b111, Some(true)) => RegType {
            name: RegName::Di,
            width: WidthType::Word,
        },
        _ => unreachable!(),
    }
}

/// SR (Segment Register) Field Encoding
///
/// See table 4-11
fn decode_sr_field(sr: u8) -> RegType {
    match sr {
        0b00 => RegType {
            name: RegName::Es,
            width: WidthType::Word,
        },
        0b01 => RegType {
            name: RegName::Cs,
            width: WidthType::Word,
        },
        0b10 => RegType {
            name: RegName::Ss,
            width: WidthType::Word,
        },
        0b11 => RegType {
            name: RegName::Ds,
            width: WidthType::Word,
        },
        _ => unreachable!(),
    }
}

/// R/M (Register/Memory) Field Encoding
///
/// Return a ModRmDataType object, which encodes the result of table 4-10 on pg
/// 4-20. Any displacement will be processed after the displacement bytes have
/// been parsed.
fn decode_rm_field(rm: u8, mode: ModType, w: Option<bool>) -> ModRmDataType {
    match (rm, mode, w) {
        (0b000, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Ax,
            width: WidthType::Byte,
        }),
        (0b001, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Cx,
            width: WidthType::Byte,
        }),
        (0b010, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Dx,
            width: WidthType::Byte,
        }),
        (0b011, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Bx,
            width: WidthType::Byte,
        }),
        (0b100, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Ax,
            width: WidthType::Hi8,
        }),
        (0b101, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Cx,
            width: WidthType::Hi8,
        }),
        (0b110, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Dx,
            width: WidthType::Hi8,
        }),
        (0b111, ModType::RegisterMode, None | Some(false)) => ModRmDataType::Reg(RegType {
            name: RegName::Bx,
            width: WidthType::Hi8,
        }),
        (0b000, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Ax,
            width: WidthType::Word,
        }),
        (0b001, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Cx,
            width: WidthType::Word,
        }),
        (0b010, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Dx,
            width: WidthType::Word,
        }),
        (0b011, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Bx,
            width: WidthType::Word,
        }),
        (0b100, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Sp,
            width: WidthType::Word,
        }),
        (0b101, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Bp,
            width: WidthType::Word,
        }),
        (0b110, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Si,
            width: WidthType::Word,
        }),
        (0b111, ModType::RegisterMode, Some(true)) => ModRmDataType::Reg(RegType {
            name: RegName::Di,
            width: WidthType::Word,
        }),
        (_, ModType::RegisterMode, _) => unreachable!("ERROR: Unknown RegisterMode condition"),
        (0b000, ModType::MemoryMode0, _) => ModRmDataType::MemRegReg(
            RegType {
                name: RegName::Bx,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Si,
                width: WidthType::Word,
            },
        ),
        (0b001, ModType::MemoryMode0, _) => ModRmDataType::MemRegReg(
            RegType {
                name: RegName::Bx,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Di,
                width: WidthType::Word,
            },
        ),
        (0b010, ModType::MemoryMode0, _) => ModRmDataType::MemRegReg(
            RegType {
                name: RegName::Bp,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Si,
                width: WidthType::Word,
            },
        ),
        (0b011, ModType::MemoryMode0, _) => ModRmDataType::MemRegReg(
            RegType {
                name: RegName::Bp,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Di,
                width: WidthType::Word,
            },
        ),
        (0b100, ModType::MemoryMode0, _) => ModRmDataType::MemReg(RegType {
            name: RegName::Si,
            width: WidthType::Word,
        }),
        (0b101, ModType::MemoryMode0, _) => ModRmDataType::MemReg(RegType {
            name: RegName::Di,
            width: WidthType::Word,
        }),
        // No registers - just a 16-bit immediate address from data lo, data hi
        (0b110, ModType::MemoryMode0, _) => ModRmDataType::MemDirectAddr,
        (0b111, ModType::MemoryMode0, _) => ModRmDataType::MemReg(RegType {
            name: RegName::Bx,
            width: WidthType::Word,
        }),
        (_, ModType::MemoryMode0, _) => unreachable!("ERROR: Unknown MemoryMode0 condition"),
        // For MM8/MM16, all we need to do later after this function is add in
        // a signed disp with +/- sign explicitly printed out.
        (0b000, ModType::MemoryMode8 | ModType::MemoryMode16, _) => ModRmDataType::MemRegRegDisp(
            RegType {
                name: RegName::Bx,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Si,
                width: WidthType::Word,
            },
        ),
        (0b001, ModType::MemoryMode8 | ModType::MemoryMode16, _) => ModRmDataType::MemRegRegDisp(
            RegType {
                name: RegName::Bx,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Di,
                width: WidthType::Word,
            },
        ),
        (0b010, ModType::MemoryMode8 | ModType::MemoryMode16, _) => ModRmDataType::MemRegRegDisp(
            RegType {
                name: RegName::Bp,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Si,
                width: WidthType::Word,
            },
        ),
        (0b011, ModType::MemoryMode8 | ModType::MemoryMode16, _) => ModRmDataType::MemRegRegDisp(
            RegType {
                name: RegName::Bp,
                width: WidthType::Word,
            },
            RegType {
                name: RegName::Di,
                width: WidthType::Word,
            },
        ),
        (0b100, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            ModRmDataType::MemRegDisp(RegType {
                name: RegName::Si,
                width: WidthType::Word,
            })
        }
        (0b101, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            ModRmDataType::MemRegDisp(RegType {
                name: RegName::Di,
                width: WidthType::Word,
            })
        }
        (0b110, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            ModRmDataType::MemRegDisp(RegType {
                name: RegName::Bp,
                width: WidthType::Word,
            })
        }
        (0b111, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            ModRmDataType::MemRegDisp(RegType {
                name: RegName::Bx,
                width: WidthType::Word,
            })
        }
        (_, ModType::MemoryMode8, _) => unreachable!("ERROR: Unknown MemoryMode8 condition"),
        (_, ModType::MemoryMode16, _) => unreachable!("ERROR: Unknown MemoryMode16 condition"),
    }
}

/// Get the op code for an instruction starting with 0b100000. `bits` is the
/// value of the middle 3 'op' bits in the second mod-op-r/m byte.
fn decode_immed_op(bits: u8) -> OpCodeType {
    match bits {
        0b000 => OpCodeType::Add,
        0b001 => OpCodeType::Or,
        // Add with carry
        0b010 => OpCodeType::Adc,
        // Subtract with borrow
        0b011 => OpCodeType::Sbb,
        0b100 => OpCodeType::And,
        0b101 => OpCodeType::Sub,
        0b110 => OpCodeType::Xor,
        // Compare - immediate with register/memory
        0b111 => OpCodeType::Cmp,
        _ => panic!("Bad bits specified in decode_immed_op()"),
    }
}

/// Get the op code an instruction starting with 0b 1101 00. `bits` is the
/// value of the middle 3 'op' bits in the second mod-op-r/m byte.
fn decode_shift_op(bits: u8) -> OpCodeType {
    match bits {
        0b000 => OpCodeType::Rol,
        0b001 => OpCodeType::Ror,
        0b010 => OpCodeType::Rcl,
        0b011 => OpCodeType::Rcr,
        0b100 => OpCodeType::Shl,
        0b101 => OpCodeType::Shr,
        0b110 => panic!("Unused field 0b001 in decode_shift_op()"),
        0b111 => OpCodeType::Sar,
        _ => panic!("Bad bits specified in decode_shift_op()"),
    }
}

/// Get the op code an instruction starting with 0b 1111 011. `bits` is the
/// value of the middle 3 'op' bits in the second mod-op-r/m byte.
fn decode_grp1_op(bits: u8) -> OpCodeType {
    match bits {
        0b000 => OpCodeType::Test,
        0b001 => panic!("Unused field 0b001 in decode_grp1_op()"),
        0b010 => OpCodeType::Not,
        0b011 => OpCodeType::Neg,
        0b100 => OpCodeType::Mul,
        0b101 => OpCodeType::Imul,
        0b110 => OpCodeType::Div,
        0b111 => OpCodeType::Idiv,
        _ => panic!("Bad bits specified in decode_grp1_op()"),
    }
}

/// Get the op code of an instruction starting with 0xFF. `bits` is the value
/// of the middle 3 'op' bits in the second mod-op-r/m byte.
/// Also, in the case of the jmp and call instructions, return true if it's an
/// indirect intersegment, and false if it's indirect within segment
/// (intrasegment).
fn decode_grp2_op(bits: u8) -> (OpCodeType, bool) {
    match bits {
        0b000 => (OpCodeType::Inc, false),
        0b001 => (OpCodeType::Dec, false),
        0b010 => (OpCodeType::Call, false),
        0b011 => (OpCodeType::Call, true),
        0b100 => (OpCodeType::Jmp, false),
        0b101 => (OpCodeType::Jmp, true),
        0b110 => (OpCodeType::Push, false),
        0b111 => panic!("Unused field 0b111 in decode_grp2_op()"),
        _ => panic!("Bad bits specified in decode_grp2_op()"),
    }
}

/// Take a given u8 and return a sign-extended i16
fn sign_extend_byte(byte: u8) -> i16 {
    (byte as i8) as i16
}

/// Get up to a 16-byte window wherever the IP points to in the program code.
/// x86-64 ISA guarantees that all instructions are at most 16 bytes, so
/// a 16-byte window is always enough to decode the next instruction.
fn get_inst_window(cpu_state_ip: usize, program_bytes: &Vec<u8>) -> Option<&[u8]> {
    let window_start = cpu_state_ip;
    let program_len = program_bytes.len();
    if window_start >= program_len {
        // We are trying to execute outside the program! Exit
        println!(
            "IP ran off the end of the program: ip: {}; prog len: {}",
            window_start, program_len
        );
        return None;
    }

    let mut window_end = cpu_state_ip + 16;
    // Cap instruction window so it does not exceed the end of the program
    if window_end > program_len {
        window_end = program_len;
    }

    let sixteen_byte_window = &program_bytes[window_start..window_end];
    Some(sixteen_byte_window)
}
