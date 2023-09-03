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

use core::slice::Iter;
use std::fmt;
use std::iter::Peekable;

use crate::execute::execute;
use crate::execute::init_state;

// Make this type look nicer
type ByteStreamIter<'o> = Peekable<Iter<'o, u8>>;

/// The bits of r/m field that is direct address if mode is MemoryMode0
const DIRECT_ADDR: u8 = 0b110;

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
}

/// An enum representing the possible types of "extra" bytes containing data
/// that come after the first byte and the mod r/m byte. Extra bytes include
/// displacement bytes and "data" bytes for immediate values.
#[derive(Copy, Clone, Debug)]
enum ExtraBytesType {
    /// An 8-bit unsigned value.
    Data8,
    /// An 8-bit signed value, optionally extended by DataHi
    DataLo,
    DataHi,
    /// If w=1 and s=1, then create a ephemeral DataHi by sign-extending DataLo
    DataExtend,
    DispLo,
    DispHi,
    /// An 8-bit signed increment offset to the instruction pointer
    IpInc8,
    IpIncLo,
    IpIncHi,
    /// The following byte can be ignored (e.g. the second byte of aam/aad
    /// apparently provides no additional data)
    DoNotCare,
}

/// Indicates whether to apply some data to the source or the destination
#[derive(Copy, Clone, Debug)]
enum AddTo {
    Dest,
    Source,
}

/// OpCode types containing a static string mapping
#[derive(Copy, Clone, Debug)]
pub enum OpCodeType {
    Aaa,
    Aad,
    Aam,
    Aas,
    Adc,
    Add,
    And,
    Call,
    Cbw,
    Clc,
    Cld,
    Cli,
    Cmc,
    Cmp,
    Cmps,
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
    Ja,
    Jb,
    Jbe,
    Jcxz,
    Je,
    Jg,
    Jl,
    Jle,
    Jmp,
    Jnb,
    Jne,
    Jnl,
    Jno,
    Jnp,
    Jns,
    Jo,
    Jp,
    Js,
    Lahf,
    Lds,
    Lea,
    Les,
    Lods,
    Loop,
    Loopnz,
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
    Rcl,
    Rcr,
    Ret,
    Retf,
    Rol,
    Ror,
    Sahf,
    Sar,
    Sbb,
    Scas,
    Shl,
    Shr,
    Stc,
    Std,
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
    prefixes: Option<String>,
    mod_field: Option<ModType>,
    rm_field: Option<u8>,
    reg_field: Option<String>,
    sr_field: Option<String>,
    /// A string containing the registers but NOT the src/dst
    /// The op code type
    pub op_type: Option<OpCodeType>,
    /// A suffix string to append to the opcode, like `b` for `movsb`
    op_type_suffix: Option<&'static str>,
    /// A string of the full op code plus any prefixes or suffixes
    op_type_str: Option<String>,
    /// A list of all bytes processed for this instruction
    processed_bytes: Vec<u8>,
    mod_rm_byte: Option<ModRmByteType>,
    /// If true, then the first byte was a REP and there is a second string
    /// manipulation byte to follow.
    has_string_byte: Option<bool>,
    /// The actual data for the extra bytes
    data_8: Option<u8>,
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    ip_inc8: Option<u8>,
    ip_inc_lo: Option<u8>,
    ip_inc_hi: Option<u8>,
    /// If set, we expect to add displacement bytes to what AddTo specifies
    add_disp_to: Option<AddTo>,
    /// If set, we expect to add data bytes to what AddTo specifies
    add_data_to: Option<AddTo>,
    /// If true, then the displacement is a direct address instead of added to
    /// any
    disp_direct_address: bool,
    /// The expected "extra" byte types to parse after we parse the 1st byte and
    /// the mod/rm byte (if it exists).
    extra_bytes: Vec<ExtraBytesType>,
    /// The text for the source operand
    source_text: Option<String>,
    source_text_end: Option<String>,
    source_prefix: Option<String>,
    /// The text for the destination operand
    dest_text: Option<String>,
    dest_text_end: Option<String>,
    dest_prefix: Option<String>,
    /// The final instruction representation
    pub text: Option<String>,
}

/// Decode and execute an 8086 instruction stream. This will decode whatever
/// the IP points to and simulates that instruction.
pub fn decode_execute(inst_stream: Vec<u8>) -> Vec<InstType> {
    let mut iter = inst_stream.iter().peekable();
    let mut insts = vec![];
    let mut cpu_state = init_state();

    while iter.peek().is_some() {
        // Decode one (possibly multi-byte) instruction at a time
        match decode_single(&mut iter, false) {
            Some(mut inst) => {
                // Execute the instruction
                execute(&mut inst, &mut cpu_state);
                // Record the instruction
                insts.push(inst);

                // On to the next instruction...
            }
            // Done with the instruction stream
            None => break,
        };
    }
    insts
}

/// Decode an 8086 instruction stream. This is a dumb line-by-line decode of an
/// instruction stream and does not take branches or do any simulation
/// whatsoever. It prints processed bytes, prints the decoded instruction, and
/// returns a vector of instructions.
pub fn decode(inst_stream: Vec<u8>) -> Vec<InstType> {
    let mut iter = inst_stream.iter().peekable();
    let mut insts = vec![];
    while iter.peek().is_some() {
        // Decode one (possibly multi-byte) instruction at a time
        match decode_single(&mut iter, true) {
            Some(inst) => {
                println!("{}", inst.text.as_ref().unwrap());
                insts.push(inst);
                // On to the next instruction...
            }
            // Done with the instruction stream
            None => break,
        };
    }
    insts
}

/// Decode a single instruction, advancing the byte stream iterator as needed.\
/// Return the instruction.
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
fn decode_single(iter: &mut ByteStreamIter, debug: bool) -> Option<InstType> {
    let mut inst = InstType {
        ..Default::default()
    };

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

    // Get extra bytes and store in inst
    for byte_type in &inst.extra_bytes {
        if iter.peek().is_none() {
            println!("Unexpected end of instruction stream");
            return None;
        };

        // If we are sign extending a low data byte, don't actually process
        // a byte.
        match byte_type {
            // Sign extend data_lo into data_hi
            ExtraBytesType::DataExtend => {
                let sign_byte = match inst.data_lo {
                    Some(lo) => {
                        if (lo & 0b10000000) == 0 {
                            0x00
                        } else {
                            0xFF
                        }
                    }
                    None => {
                        panic!("DataExtend byte found no DataLo byte")
                    }
                };
                inst.data_hi = Some(sign_byte);
                continue;
            }
            _ => {}
        }

        let byte = iter.next().unwrap();
        if debug {
            debug_byte(byte);
        }
        inst.processed_bytes.push(*byte);

        match byte_type {
            ExtraBytesType::DispLo => inst.disp_lo = Some(*byte),
            ExtraBytesType::DispHi => inst.disp_hi = Some(*byte),
            ExtraBytesType::DataLo => inst.data_lo = Some(*byte),
            ExtraBytesType::Data8 => inst.data_8 = Some(*byte),
            ExtraBytesType::DataHi => inst.data_hi = Some(*byte),
            ExtraBytesType::IpInc8 => inst.ip_inc8 = Some(*byte),
            ExtraBytesType::IpIncLo => inst.ip_inc_lo = Some(*byte),
            ExtraBytesType::IpIncHi => inst.ip_inc_hi = Some(*byte),
            ExtraBytesType::DoNotCare => {}
            _ => {
                panic!("Unexpected ExtraBytesType!")
            }
        }
    }

    // Process extra bytes
    if inst.add_disp_to.is_some() {
        process_disp_bytes(&mut inst);
    }
    if inst.add_data_to.is_some() {
        process_data_bytes(&mut inst);
    }
    if inst.ip_inc8.is_some() || inst.ip_inc_lo.is_some() {
        process_ip_bytes(&mut inst);
    }

    // Create instruction text
    inst.op_type_str = Some(format!(
        "{}{}",
        inst.op_type.unwrap(),
        inst.op_type_suffix.unwrap_or("")
    ));
    let op_text = concat_texts(&inst.prefixes, &inst.op_type_str);
    let mut dest_text = concat_texts(&inst.dest_text, &inst.dest_text_end);
    let mut source_text = concat_texts(&inst.source_text, &inst.source_text_end);

    // Concatenate any word or byte prefixes
    dest_text = concat_texts(&inst.dest_prefix, &dest_text);
    source_text = concat_texts(&inst.source_prefix, &source_text);

    let inst_text = concat_operands(&op_text, dest_text, source_text);
    inst.text = Some(inst_text);

    return Some(inst);
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
        _ => {
            // This byte isn't a prefix...
            return false;
        }
    };
    match &mut inst.prefixes {
        None => {
            inst.prefixes = Some(prefix.to_string());
        }
        Some(prefixes) => {
            prefixes.push_str(prefix);
        }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // inc - register
        0x40..=0x47 => {
            inst.op_type = Some(OpCodeType::Inc);
            let w_field = Some(true);
            let reg_field = decode_reg_field(byte & 0b111, w_field);
            inst.dest_text = Some(reg_field.clone());
            inst.reg_field = Some(reg_field);
            inst.w_field = w_field;
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
            inst.extra_bytes.push(ExtraBytesType::DoNotCare)
        }
        // aad - ASCII adjust for divide
        0xD5 => {
            inst.op_type = Some(OpCodeType::Aad);
            // The second byte doesn't add anything. Ignore it for now.
            inst.extra_bytes.push(ExtraBytesType::DoNotCare)
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // mov - Register/memory to/from register
        0x88..=0x8C => {
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
            inst.dest_text = Some(reg_field);
            // No mod rm byte for this mov variant!
            // Indicate that there are source data bytes after this byte
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            match inst.w_field {
                Some(true) => inst.extra_bytes.push(ExtraBytesType::DataHi),
                _ => {}
            }
        }
        // mov - Memory to accumulator or accumulator to memory
        0xA0..=0xA3 => {
            inst.op_type = Some(OpCodeType::Mov);
            let w_field = (byte & 0x1) == 1;
            let left_bracket = Some("[".to_string());
            let right_bracket = Some("]".to_string());
            let accumulator = Some("ax".to_string());
            match ((byte & 0x2) >> 1) == 1 {
                false => {
                    inst.dest_text = accumulator;
                    inst.add_data_to = Some(AddTo::Source);
                    inst.source_text = left_bracket;
                    inst.source_text_end = right_bracket;
                }
                true => {
                    inst.source_text = accumulator;
                    inst.add_data_to = Some(AddTo::Dest);
                    inst.dest_text = left_bracket;
                    inst.dest_text_end = right_bracket;
                }
            };
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            }
            inst.w_field = Some(w_field);
        }
        // inc/dec/call/jmp/push - Register/memory
        0xFE..=0xFF => {
            inst.mod_rm_byte = Some(ModRmByteType::ModGrp2Rm);
            // NOTE: The first byte hardcodes w to 1 for pushes and pops, since
            // the operand is always 16 bits.
            inst.w_field = Some((byte & 0x1) == 1);
        }
        // jmp - Direct within segment
        0xE9 => {
            inst.op_type = Some(OpCodeType::Jmp);
            inst.extra_bytes.push(ExtraBytesType::IpIncLo);
            inst.extra_bytes.push(ExtraBytesType::IpIncHi);
        }
        // jmp - Direct within segment-short
        0xEB => {
            inst.op_type = Some(OpCodeType::Jmp);
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
        }
        // call - Direct within segment
        0xE8 => {
            inst.op_type = Some(OpCodeType::Call);
            inst.extra_bytes.push(ExtraBytesType::IpIncLo);
            inst.extra_bytes.push(ExtraBytesType::IpIncHi);
        }
        // ret - Within segment
        0xC3 => {
            inst.op_type = Some(OpCodeType::Ret);
        }
        // ret - Within segment adding immediate to SP
        0xC2 => {
            inst.op_type = Some(OpCodeType::Ret);
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            inst.extra_bytes.push(ExtraBytesType::DataHi);
            inst.add_data_to = Some(AddTo::Source);
        }
        // retf - Intersegment ret
        0xCB => {
            inst.op_type = Some(OpCodeType::Retf);
        }
        // retf - Intersegment ret adding immediate to SP
        0xCA => {
            inst.op_type = Some(OpCodeType::Retf);
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            inst.extra_bytes.push(ExtraBytesType::DataHi);
            inst.add_data_to = Some(AddTo::Source);
        }
        // push - Register
        0x50..=0x57 => {
            inst.op_type = Some(OpCodeType::Push);
            // Hardcode registers to 16-bit widths
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field.clone());
            inst.dest_text = Some(reg_field);
        }
        // push - segment register - 0x06,0x0E,0x86,0x8E
        0b000_00_110 | 0b000_01_110 | 0b000_10_110 | 0b000_11_110 => {
            inst.op_type = Some(OpCodeType::Push);
            let sr_field = decode_sr_field((byte & 0b000_11_000) >> 3);
            inst.sr_field = Some(sr_field.clone());
            inst.dest_text = Some(sr_field);
        }
        // pop - Register/memory
        0x8F => {
            inst.op_type = Some(OpCodeType::Pop);
            inst.mod_rm_byte = Some(ModRmByteType::ModPopRm);
        }
        // pop - Register
        0x58..=0x5F => {
            inst.op_type = Some(OpCodeType::Pop);
            // Hardcode registers to 16-bit widths
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field.clone());
            inst.dest_text = Some(reg_field);
        }
        // push - segment register - 0x06,0x0E,0x86,0x8E
        0b000_00_111 | 0b000_01_111 | 0b000_10_111 | 0b000_11_111 => {
            inst.op_type = Some(OpCodeType::Pop);
            let sr_field = decode_sr_field((byte & 0b000_11_000) >> 3);
            inst.sr_field = Some(sr_field.clone());
            inst.dest_text = Some(sr_field);
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
            inst.dest_text = Some("ax".to_string());
            let reg_field = decode_reg_field(byte & 0b111, Some(true));
            inst.reg_field = Some(reg_field.clone());
            inst.source_text = Some(reg_field);
        }
        // in - fixed port
        0xE4..=0xE5 => {
            inst.op_type = Some(OpCodeType::In);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.dest_text = Some("ax".to_string());
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.extra_bytes.push(ExtraBytesType::Data8);
            inst.add_data_to = Some(AddTo::Source);
            inst.w_field = Some(w_field);
        }
        // in - variable port
        0xEC..=0xED => {
            inst.op_type = Some(OpCodeType::In);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.dest_text = Some("ax".to_string());
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.source_text = Some("dx".to_string());
            inst.w_field = Some(w_field);
        }
        // out - fixed port
        0xE6..=0xE7 => {
            inst.op_type = Some(OpCodeType::Out);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.source_text = Some("ax".to_string());
            } else {
                inst.source_text = Some("al".to_string());
            }
            inst.extra_bytes.push(ExtraBytesType::Data8);
            inst.add_data_to = Some(AddTo::Dest);
            inst.w_field = Some(w_field);
        }
        // out - variable port
        0xEE..=0xEF => {
            inst.op_type = Some(OpCodeType::Out);
            let w_field = (byte & 0x1) == 1;
            if w_field {
                inst.source_text = Some("ax".to_string());
            } else {
                inst.source_text = Some("al".to_string());
            }
            inst.dest_text = Some("dx".to_string());
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
        }
        // popf - Pop flags
        0x9D => {
            inst.op_type = Some(OpCodeType::Popf);
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // dec - register
        0x48..=0x4F => {
            inst.op_type = Some(OpCodeType::Dec);
            let w_field = Some(true);
            let reg_field = decode_reg_field(byte & 0b111, w_field);
            inst.dest_text = Some(reg_field.clone());
            inst.reg_field = Some(reg_field);
            inst.w_field = w_field;
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        0x70..=0x7F | 0xE0..=0xE3 => {
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            match byte {
                // je/jz
                0x74 => inst.op_type = Some(OpCodeType::Je),
                // jl/jnge - jump less/not greater or equal
                0x7C => inst.op_type = Some(OpCodeType::Jl),
                // jle/jng - jump less or equal/not greater
                0x7E => inst.op_type = Some(OpCodeType::Jle),
                // jb/jnae - jump below/not above or equal
                0x72 => inst.op_type = Some(OpCodeType::Jb),
                // jbe/jna - jump below or equal/not above
                0x76 => inst.op_type = Some(OpCodeType::Jbe),
                // jp/jpe - jump on parity/parity even
                0x7A => inst.op_type = Some(OpCodeType::Jp),
                // jo - jump on overflow
                0x70 => inst.op_type = Some(OpCodeType::Jo),
                // js - jump on sign
                0x78 => inst.op_type = Some(OpCodeType::Js),
                // jne/jnz - jump not equal/zero
                0x75 => inst.op_type = Some(OpCodeType::Jne),
                // jnl/jge - jump not less or greater+equal
                0x7D => inst.op_type = Some(OpCodeType::Jnl),
                // jnle/jg - jump greater or not less+equal
                0x7F => inst.op_type = Some(OpCodeType::Jg),
                // jnb/jae - jump on not below or above+equal
                0x73 => inst.op_type = Some(OpCodeType::Jnb),
                // jnbe/ja - jump above or not below+equal
                0x77 => inst.op_type = Some(OpCodeType::Ja),
                // jnp/jpo - jump not par or par odd
                0x7B => inst.op_type = Some(OpCodeType::Jnp),
                // jno - jump on not overflow
                0x71 => inst.op_type = Some(OpCodeType::Jno),
                // jns - jump on not overflow
                0x79 => inst.op_type = Some(OpCodeType::Jns),
                0xE2 => inst.op_type = Some(OpCodeType::Loop),
                // loopz/loope - loop while zero/equal
                0xE1 => inst.op_type = Some(OpCodeType::Loopz),
                // loopnz/loopne - loop while not zero/equal
                0xE0 => inst.op_type = Some(OpCodeType::Loopnz),
                0xE3 => inst.op_type = Some(OpCodeType::Jcxz),
                _ => {
                    unreachable!()
                }
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
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // int - Type specified
        0xCD => {
            inst.op_type = Some(OpCodeType::Int);
            inst.add_data_to = Some(AddTo::Source);
            inst.extra_bytes.push(ExtraBytesType::Data8);
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
    let (rm_text, rm_text_end) = decode_rm_field(rm_field, mode, inst.w_field);
    match inst.d_field {
        None | Some(false) => {
            // Dest is rm field
            inst.dest_text = rm_text;
            inst.dest_text_end = rm_text_end;
        }
        Some(true) => {
            // Source is rm field
            inst.source_text = rm_text;
            inst.source_text_end = rm_text_end;
        }
    }

    // Indicate that there are displacement bytes to process next
    // Displacement bytes come before immediate/data bytes
    match (mode, rm_field) {
        (ModType::MemoryMode8, _) => {
            inst.extra_bytes.push(ExtraBytesType::DispLo);
        }
        (ModType::MemoryMode16, _) => {
            inst.extra_bytes.push(ExtraBytesType::DispLo);
            inst.extra_bytes.push(ExtraBytesType::DispHi);
        }
        (ModType::MemoryMode0, DIRECT_ADDR) => {
            inst.extra_bytes.push(ExtraBytesType::DispLo);
            inst.extra_bytes.push(ExtraBytesType::DispHi);
            inst.disp_direct_address = true;
        }
        _ => {}
    }

    // Process the middle part of the mod rm byte
    match inst.mod_rm_byte {
        Some(ModRmByteType::ModRegRm) => {
            let reg_field = decode_reg_field((byte & 0b00111000) >> 3, inst.w_field);
            // See if reg is specified as source or dest
            match inst.d_field {
                None | Some(false) => {
                    // Source is REG field
                    inst.source_text = Some(reg_field.clone());
                    inst.source_text_end = None;
                }
                Some(true) => {
                    // Destination is REG field
                    inst.dest_text = Some(reg_field.clone());
                    inst.dest_text_end = None;
                }
            };
            inst.reg_field = Some(reg_field);
            // We need no byte/word prefix for ModRegRm, since there is always
            // a register source/dest to indicate size
        }
        Some(ModRmByteType::ModMovRm) => {
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            match inst.w_field {
                Some(true) => inst.extra_bytes.push(ExtraBytesType::DataHi),
                _ => {}
            }
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                (_, Some(false)) => inst.source_prefix = Some("byte ".to_string()),
                (_, Some(true)) => inst.source_prefix = Some("word ".to_string()),
                (_, _) => {}
            }
        }
        Some(ModRmByteType::ModImmedRm) => {
            inst.op_type = Some(decode_immed_op((byte & 0b00111000) >> 3));
            match (inst.w_field, inst.s_field) {
                (None, _) => {
                    panic!("This ModImmedRm inst is missing the w field!")
                }
                (Some(false), _) => {
                    inst.extra_bytes.push(ExtraBytesType::DataLo);
                }
                (Some(true), Some(true)) => {
                    inst.extra_bytes.push(ExtraBytesType::DataLo);
                    inst.extra_bytes.push(ExtraBytesType::DataExtend);
                }
                (Some(true), None | Some(false)) => {
                    inst.extra_bytes.push(ExtraBytesType::DataLo);
                    inst.extra_bytes.push(ExtraBytesType::DataHi);
                }
            }
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                (_, Some(false)) => inst.source_prefix = Some("byte ".to_string()),
                (_, Some(true)) => inst.source_prefix = Some("word ".to_string()),
                (_, _) => {}
            }
        }
        Some(ModRmByteType::ModShiftRm) => {
            inst.op_type = Some(decode_shift_op((byte & 0b00111000) >> 3));
            match inst.v_field {
                Some(false) | None => inst.source_text = Some("1".to_string()),
                Some(true) => inst.source_text = Some("cl".to_string()),
            }

            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                // ModGrp2Rm instructions are 1-operand, so add prefix to dest
                (_, Some(false)) => inst.dest_prefix = Some("byte ".to_string()),
                (_, Some(true)) => inst.dest_prefix = Some("word ".to_string()),
                (_, None) => {
                    unreachable!()
                }
            }
        }
        Some(ModRmByteType::ModGrp1Rm) => {
            let (op_type, is_test_inst) = decode_grp1_op((byte & 0b00111000) >> 3);
            inst.op_type = Some(op_type);
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                // ModGrp1Rm instructions are 1-operand, so add prefix to dest
                (_, Some(false)) => inst.dest_prefix = Some("byte ".to_string()),
                (_, Some(true)) => inst.dest_prefix = Some("word ".to_string()),
                (_, None) => {
                    unreachable!()
                }
            }
            if is_test_inst {
                inst.add_data_to = Some(AddTo::Source);
                inst.extra_bytes.push(ExtraBytesType::DataLo);
                match inst.w_field {
                    Some(true) => inst.extra_bytes.push(ExtraBytesType::DataHi),
                    _ => {}
                }
            }
        }
        Some(ModRmByteType::ModGrp2Rm) => {
            inst.op_type = Some(decode_grp2_op((byte & 0b00111000) >> 3));
            match (mode, inst.w_field) {
                // We know the size if Register Mode
                (ModType::RegisterMode, _) => {}
                // ModGrp2Rm instructions are 1-operand, so add prefix to dest
                (_, Some(false)) => inst.dest_prefix = Some("byte ".to_string()),
                (_, Some(true)) => inst.dest_prefix = Some("word ".to_string()),
                (_, None) => {
                    unreachable!()
                }
            }
        }
        Some(ModRmByteType::ModPopRm) => match mode {
            ModType::RegisterMode => {}
            _ => inst.dest_prefix = Some("word ".to_string()),
        },
        None => {
            unreachable!()
        }
    }
    // Indicate what displacement should be added to: src or dest
    match (inst.d_field, mode, rm_field) {
        (None | Some(false), ModType::MemoryMode8 | ModType::MemoryMode16, _)
        | (None | Some(false), ModType::MemoryMode0, DIRECT_ADDR) => {
            inst.add_disp_to = Some(AddTo::Dest);
        }
        (Some(true), ModType::MemoryMode8 | ModType::MemoryMode16, _)
        | (Some(true), ModType::MemoryMode0, DIRECT_ADDR) => {
            inst.add_disp_to = Some(AddTo::Source);
        }
        (_, _, _) => {}
    }

    inst.mod_field = Some(mode);
    inst.rm_field = Some(rm_field);
}

/// Process the disp bytes by applying it to the needed fields in the
/// instruction struct
fn process_disp_bytes(inst: &mut InstType) {
    let disp_bytes_text = match (inst.disp_lo, inst.disp_hi) {
        (Some(lo), None) => {
            // Print as signed 8 bit
            format!("{:+}", lo as i8)
        }
        (Some(lo), Some(hi)) => {
            match inst.disp_direct_address {
                false => {
                    // If not direct address, print as signed 16 bit
                    let lo_hi = lo as u16 | ((hi as u16) << 8);
                    format!("{:+}", lo_hi as i16)
                }
                true => {
                    // If direct address, print in hex for convenience
                    format!("0x{hi:02X}{lo:02X}")
                }
            }
        }
        (None, None) => {
            unreachable!("ERROR: No disp bytes found")
        }
        (None, Some(_)) => {
            unreachable!("ERROR: Low disp byte not")
        }
    };
    match (&mut inst.dest_text, &mut inst.source_text, inst.add_disp_to) {
        (_, Some(source_text), Some(AddTo::Source)) => {
            source_text.push_str(&disp_bytes_text);
        }
        (Some(dest_text), _, Some(AddTo::Dest)) => {
            dest_text.push_str(&disp_bytes_text);
        }
        (_, _, _) => {
            unreachable!("Unhandled combo for dest_text, source_text, add_disp_to")
        }
    };
}

/// Process the data (immediate) bytes by applying it to the needed fields in
/// the instruction struct
fn process_data_bytes(inst: &mut InstType) {
    let data_bytes_text = match (inst.data_lo, inst.data_hi, inst.data_8) {
        (Some(lo), None, None) => format!("{}", lo as i8),
        (Some(lo), Some(hi), None) => {
            let lo_hi = lo as u16 | ((hi as u16) << 8);
            format!("{}", lo_hi as i16)
        }
        (None, None, Some(data8)) => format!("{data8}"),
        (None, None, None) => {
            unreachable!("ERROR: No data bytes found")
        }
        (None, Some(_), _) => {
            unreachable!("ERROR: Low data byte not set")
        }
        (_, _, _) => {
            panic!("Unhandled case in process_data_bytes()")
        }
    };

    match (&mut inst.dest_text, &mut inst.source_text, inst.add_data_to) {
        (_, Some(source_text), Some(AddTo::Source)) => {
            source_text.push_str(&data_bytes_text);
        }
        (_, None, Some(AddTo::Source)) => {
            inst.source_text = Some(data_bytes_text);
        }
        (Some(dest_text), _, Some(AddTo::Dest)) => {
            dest_text.push_str(&data_bytes_text);
        }
        (None, _, Some(AddTo::Dest)) => {
            inst.dest_text = Some(data_bytes_text);
        }
        (_, _, _) => {
            unreachable!("Unhandled combo for source_text, add_data_to")
        }
    }
}

/// Process any IP offset bytes.
///
/// The tricky part is that we can't recreate label text - but all labels are
/// just translated into relative offsets to the IP. So we use `$` in NASM to
/// encode this relative offset. $ refers to the IP of the current assembly
/// line. However, while executing an instruction, the IP always refers to the
/// next instruction. Thus, $ == IP - 2. So when a jump instruction does IP =
/// IP + X, that is really IP = ($ + 2) + X, which is why we add 2 to ip_inc8
/// below.
fn process_ip_bytes(inst: &mut InstType) {
    inst.dest_text = match (inst.ip_inc8, inst.ip_inc_lo, inst.ip_inc_hi) {
        (Some(ip_inc8), _, _) => Some(format!("${:+}", ip_inc8 as i8 + 2)),
        (None, Some(lo), Some(hi)) => {
            // Combine lo and hi
            let ip_inc = ((hi as i16) << 8) | (lo as i16);
            // TODO: I'm not sure why we need to add three , not two. I think it
            // has to do with the fact that there was a no op byte for
            // alignment.
            Some(format!("${:+}", ip_inc + 3))
        }
        _ => {
            unreachable!()
        }
    };
}

/// Concat op code with optional operands
/// op_type is required
fn concat_operands(
    op_type: &Option<String>,
    dest_text: Option<String>,
    source_text: Option<String>,
) -> String {
    match (op_type, dest_text, source_text) {
        (Some(op), Some(dest), Some(source)) => {
            format!("{op} {dest}, {source}")
        }
        (Some(op), Some(dest), None) => {
            format!("{op} {dest}")
        }
        (Some(op), None, Some(source)) => {
            format!("{op} {source}")
        }
        (Some(op), None, None) => {
            format!("{op}")
        }
        (None, _, _) => {
            unreachable!("Op code text not set!");
        }
    }
}

// MGH TODO: Replace this abominable concat function with something elegant
/// Concatenate two Option Strings
fn concat_texts(a: &Option<String>, b: &Option<String>) -> Option<String> {
    match (a, b) {
        (Some(str_a), Some(str_b)) => Some(format!("{str_a}{str_b}")),
        (Some(str_a), None) => Some(str_a.clone()),
        (None, Some(str_b)) => Some(str_b.clone()),
        (None, None) => None,
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
fn decode_reg_field(reg: u8, w: Option<bool>) -> String {
    match (reg, w) {
        (0b000, None | Some(false)) => "al".to_string(),
        (0b001, None | Some(false)) => "cl".to_string(),
        (0b010, None | Some(false)) => "dl".to_string(),
        (0b011, None | Some(false)) => "bl".to_string(),
        (0b100, None | Some(false)) => "ah".to_string(),
        (0b101, None | Some(false)) => "ch".to_string(),
        (0b110, None | Some(false)) => "dh".to_string(),
        (0b111, None | Some(false)) => "bh".to_string(),
        (0b000, Some(true)) => "ax".to_string(),
        (0b001, Some(true)) => "cx".to_string(),
        (0b010, Some(true)) => "dx".to_string(),
        (0b011, Some(true)) => "bx".to_string(),
        (0b100, Some(true)) => "sp".to_string(),
        (0b101, Some(true)) => "bp".to_string(),
        (0b110, Some(true)) => "si".to_string(),
        (0b111, Some(true)) => "di".to_string(),
        _ => unreachable!(),
    }
}

/// SR (Segment Register) Field Encoding
///
/// See table 4-11
fn decode_sr_field(sr: u8) -> String {
    match sr {
        0b00 => "es".to_string(),
        0b01 => "cs".to_string(),
        0b10 => "ss".to_string(),
        0b11 => "ds".to_string(),
        _ => unreachable!(),
    }
}

/// R/M (Register/Memory) Field Encoding
///
/// See table 4-10
/// Return a tuple of the first part of the text and the last part of the text,
/// so the displacement can be optionally inserted in later. If the last part of
/// the text is None, then there should be no insertion.
fn decode_rm_field(rm: u8, mode: ModType, w: Option<bool>) -> (Option<String>, Option<String>) {
    match (rm, mode, w) {
        (0b000, ModType::RegisterMode, None | Some(false)) => (Some("al".to_string()), None),
        (0b001, ModType::RegisterMode, None | Some(false)) => (Some("cl".to_string()), None),
        (0b010, ModType::RegisterMode, None | Some(false)) => (Some("dl".to_string()), None),
        (0b011, ModType::RegisterMode, None | Some(false)) => (Some("bl".to_string()), None),
        (0b100, ModType::RegisterMode, None | Some(false)) => (Some("ah".to_string()), None),
        (0b101, ModType::RegisterMode, None | Some(false)) => (Some("ch".to_string()), None),
        (0b110, ModType::RegisterMode, None | Some(false)) => (Some("dh".to_string()), None),
        (0b111, ModType::RegisterMode, None | Some(false)) => (Some("bh".to_string()), None),
        (0b000, ModType::RegisterMode, Some(true)) => (Some("ax".to_string()), None),
        (0b001, ModType::RegisterMode, Some(true)) => (Some("cx".to_string()), None),
        (0b010, ModType::RegisterMode, Some(true)) => (Some("dx".to_string()), None),
        (0b011, ModType::RegisterMode, Some(true)) => (Some("bx".to_string()), None),
        (0b100, ModType::RegisterMode, Some(true)) => (Some("sp".to_string()), None),
        (0b101, ModType::RegisterMode, Some(true)) => (Some("bp".to_string()), None),
        (0b110, ModType::RegisterMode, Some(true)) => (Some("si".to_string()), None),
        (0b111, ModType::RegisterMode, Some(true)) => (Some("di".to_string()), None),
        (_, ModType::RegisterMode, _) => unreachable!("ERROR: Unknown RegisterMode condition"),
        (0b000, ModType::MemoryMode0, _) => (Some("[bx + si]".to_string()), None),
        (0b001, ModType::MemoryMode0, _) => (Some("[bx + di]".to_string()), None),
        (0b010, ModType::MemoryMode0, _) => (Some("[bp + si]".to_string()), None),
        (0b011, ModType::MemoryMode0, _) => (Some("[bp + di]".to_string()), None),
        (0b100, ModType::MemoryMode0, _) => (Some("[si]".to_string()), None),
        (0b101, ModType::MemoryMode0, _) => (Some("[di]".to_string()), None),
        // No registers - just a 16-bit immediate address from data lo, data hi
        (0b110, ModType::MemoryMode0, _) => (Some("[".to_string()), Some("]".to_string())),
        (0b111, ModType::MemoryMode0, _) => (Some("[bx]".to_string()), None),
        (_, ModType::MemoryMode0, _) => unreachable!("ERROR: Unknown MemoryMode0 condition"),
        // For MM8/MM16, all we need to do later after this function is add in
        // a signed disp with +/- sign explicitly printed out.
        (0b000, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx + si ".to_string()), Some("]".to_string()))
        }
        (0b001, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx + di ".to_string()), Some("]".to_string()))
        }
        (0b010, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp + si ".to_string()), Some("]".to_string()))
        }
        (0b011, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp + di ".to_string()), Some("]".to_string()))
        }
        (0b100, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[si ".to_string()), Some("]".to_string()))
        }
        (0b101, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[di ".to_string()), Some("]".to_string()))
        }
        (0b110, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp ".to_string()), Some("]".to_string()))
        }
        (0b111, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx ".to_string()), Some("]".to_string()))
        }
        (_, ModType::MemoryMode8, _) => unreachable!("ERROR: Unknown MemoryMode8 condition"),
        (_, ModType::MemoryMode16, _) => unreachable!("ERROR: Unknown MemoryMode16 condition"),
    }
}

/// Get the op code an instruction starting with 0b100000. `bits` is the value
/// of the middle 3 'op' bits in the second mod-op-r/m byte.
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
/// In addition, return true for the second value if the instruction requires
/// a data/immediate operand (i.e. the test instruction)
fn decode_grp1_op(bits: u8) -> (OpCodeType, bool) {
    match bits {
        0b000 => (OpCodeType::Test, true),
        0b001 => panic!("Unused field 0b001 in decode_grp1_op()"),
        0b010 => (OpCodeType::Not, false),
        0b011 => (OpCodeType::Neg, false),
        0b100 => (OpCodeType::Mul, false),
        0b101 => (OpCodeType::Imul, false),
        0b110 => (OpCodeType::Div, false),
        0b111 => (OpCodeType::Idiv, false),
        _ => panic!("Bad bits specified in decode_grp1_op()"),
    }
}

/// Get the op code of an instruction starting with 0xFF. `bits` is the value
/// of the middle 3 'op' bits in the second mod-op-r/m byte.
fn decode_grp2_op(bits: u8) -> OpCodeType {
    match bits {
        0b000 => OpCodeType::Inc,
        0b001 => OpCodeType::Dec,
        0b010 => OpCodeType::Call,
        0b011 => OpCodeType::Call,
        0b100 => OpCodeType::Jmp,
        0b101 => OpCodeType::Jmp,
        0b110 => OpCodeType::Push,
        0b111 => panic!("Unused field 0b111 in decode_grp2_op()"),
        _ => panic!("Bad bits specified in decode_grp2_op()"),
    }
}
