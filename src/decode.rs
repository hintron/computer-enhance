//! This module implements the 8086 decoding logic.
//!
//! It will be difficult to follow along what is going on in this file without
//! referencing the 8086 manual (available in this code repo at
//! docs/8086_family_Users_Manual_1_.pdf, or found online at various places).
//! The manual describes instruction encoding and decoding from pages 4-18 to
//! 4-36.
//!
//! There are a few typos/errors in the manual, including:
//! * table 4-11, pg. 4-21: (DISP-LO) says that the disp byte is unsigned. In
//! effect, it is signed. I can't reconcile how it can be unsigned.
//! * table 4-12, pg. 4-24: Immediate with accumulator variant of cmp does not
//! show the high data byte.
//! * table 4-12, pg. 4-24: Immediate with register/memory variant of cmp should
//! show s:w=01 for the high data byte.
//! * table 4-12, pg. 4-25: STOS is misspelled as STDS.

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

/// The possible types of "mod r/m" bytes, which is the second byte in many
/// instructions.
///
/// See table 4-14 on pg 4-36 for the 4 types of mod r/m bytes. This shows the
/// four types of 'mod op rm' bytes:
///
/// | mod X rm | 000 | 001 | 010 | 011 | 100 | 101 | 110 | 111 |
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
/// Each of these groups have instructions starting with the following bits:
/// Immed: 1000 00
/// Shift: 1101 00
/// Grp 1: 1111 011
/// Grp 2: 1111 111
///
/// `ModMovRm` corresponds to a 'mod op r/m' byte that only applies to the
/// mov instruction starting with 0b 1100 011, and would look like this if added
/// to the table above:
///
/// | mod X rm | 000 | 001 | 010 | 011 | 100 | 101 | 110 | 111 |
/// |----------+-----+-----+-----+-----+-----+-----+-----+-----+
/// | ImmedMov | mov |-----|-----|-----|-----|-----|-----|-----|
///
#[derive(Copy, Clone, Debug)]
enum ModRmByteType {
    ModRegRm,
    ModMovRm,
    ModImmedRm,
    // ModShiftRm,
    // ModGrp1Rm,
    // ModGrp2Rm,
}

/// An enum representing the possible types of "extra" bytes containing data
/// that come after the first byte and the mod r/m byte. Extra bytes include
/// displacement bytes and "data" bytes for immediate values.
#[derive(Copy, Clone, Debug)]
enum ExtraBytesType {
    DataLo,
    DataHi,
    /// If w=1 and s=1, then create a ephemeral DataHi by sign-extending DataLo
    DataExtend,
    DispLo,
    DispHi,
    /// An 8-bit signed increment offset to the instruction pointer
    IpInc8,
}

/// Indicates whether to apply some data to the source or the destination
#[derive(Copy, Clone, Debug)]
enum AddTo {
    Dest,
    Source,
}

/// A struct holding all the decoded data of a given instruction
#[derive(Default, Debug)]
pub struct InstType {
    d_field: Option<bool>,
    w_field: Option<bool>,
    /// Sign extend field. If true, sign extend 8-bit immediate as needed
    s_field: Option<bool>,
    mod_field: Option<ModType>,
    rm_field: Option<u8>,
    reg_field: Option<String>,
    /// A string containing the registers but NOT the src/dst
    op_type: Option<String>,
    /// A list of all bytes processed for this instruction
    processed_bytes: Vec<u8>,
    mod_rm_byte: Option<ModRmByteType>,
    /// The actual data for the extra bytes
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    ip_inc8: Option<u8>,
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
    /// The text for the destination operand
    dest_text: Option<String>,
    dest_text_end: Option<String>,
    /// If true, then we need to add a 'word' or 'byte' prefix in front of an
    /// immediate source (data).
    data_needs_size: bool,
    /// The final instruction representation
    pub text: Option<String>,
}

/// Decode an 8086 instruction stream
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
pub fn decode(inst_stream: Vec<u8>) -> Vec<InstType> {
    let mut iter = inst_stream.iter().peekable();
    let mut insts = vec![];
    while iter.peek().is_some() {
        let mut inst = InstType {
            // Print size prefix by default unless we detect size is implied
            // by e.g. a register destination.
            data_needs_size: true,
            ..Default::default()
        };
        let byte = iter.next().unwrap();
        debug_byte(byte);
        inst.processed_bytes.push(*byte);

        if decode_first_byte(*byte, &mut inst) == false {
            println!("Unknown instruction");
            break;
        }

        // Process mod rm byte, if it exists
        if inst.mod_rm_byte.is_some() {
            if iter.peek().is_none() {
                println!("Unexpected end of instruction stream");
                break;
            };
            // Get the next (mod r/m) byte in the stream
            let byte = iter.next().unwrap();
            debug_byte(byte);
            inst.processed_bytes.push(*byte);
            decode_mod_rm_byte(*byte, &mut inst);
        }

        // Get extra bytes and store in inst
        for byte_type in &inst.extra_bytes {
            if iter.peek().is_none() {
                println!("Unexpected end of instruction stream");
                continue;
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
            debug_byte(byte);
            inst.processed_bytes.push(*byte);

            match byte_type {
                ExtraBytesType::DispLo => inst.disp_lo = Some(*byte),
                ExtraBytesType::DispHi => inst.disp_hi = Some(*byte),
                ExtraBytesType::DataLo => inst.data_lo = Some(*byte),
                ExtraBytesType::DataHi => inst.data_hi = Some(*byte),
                ExtraBytesType::IpInc8 => inst.ip_inc8 = Some(*byte),
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
        if inst.ip_inc8.is_some() {
            process_ip_bytes(&mut inst);
        }

        // Create instruction text
        let dest_text = concat_texts(&inst.dest_text, &inst.dest_text_end);
        let source_text = concat_texts(&inst.source_text, &inst.source_text_end);
        let inst_text = concat_operands(&inst.op_type, dest_text, source_text);

        println!("{}", inst_text);
        inst.text = Some(inst_text);
        insts.push(inst);
        // On to the next instruction...
    }
    insts
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
            inst.op_type = Some("add".to_string());
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
            inst.op_type = Some("add".to_string());
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            inst.data_needs_size = false;
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
            inst.op_type = Some("mov".to_string());
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
            // We need to see what mod is before we know what is the source
            // and what is the destination
        }
        // mov - Immediate to register/memory
        0xC6..=0xC7 => {
            inst.op_type = Some("mov".to_string());
            inst.w_field = Some((byte & 0x1) == 1);
            // In effect, the d field is hard coded to 0: the destination is
            // rm and the source is an immediate (which replaced reg from
            // the above mov variant)
            inst.mod_rm_byte = Some(ModRmByteType::ModMovRm);
        }
        // mov - Immediate to register
        0xB0..=0xBF => {
            inst.op_type = Some("mov".to_string());
            inst.w_field = Some(((byte & 0b1000) >> 3) == 1);
            let reg_field = decode_reg_field(byte & 0b111, inst.w_field);
            inst.reg_field = Some(reg_field.clone());
            inst.dest_text = Some(reg_field);
            inst.data_needs_size = false;
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
            inst.op_type = Some("mov".to_string());
            let w_field = (byte & 0x1) == 1;
            inst.data_needs_size = false;
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
        // sub - Reg/memory and register to either
        0x28..=0x2B => {
            inst.op_type = Some("sub".to_string());
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // sub - Immediate from accumulator
        0x2C..=0x2D => {
            inst.op_type = Some("sub".to_string());
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            inst.data_needs_size = false;
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // cmp - Register/memory and register
        0x38..=0x3B => {
            inst.op_type = Some("cmp".to_string());
            inst.w_field = Some((byte & 0x1) == 1);
            inst.d_field = Some(((byte & 0x2) >> 1) == 1);
            inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
        }
        // cmp - Immediate with accumulator
        0x3C..=0x3D => {
            inst.op_type = Some("cmp".to_string());
            let w_field = (byte & 0x1) == 1;
            inst.add_data_to = Some(AddTo::Source);
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            inst.data_needs_size = false;
            if w_field {
                inst.dest_text = Some("ax".to_string());
                inst.extra_bytes.push(ExtraBytesType::DataHi);
            } else {
                inst.dest_text = Some("al".to_string());
            }
            inst.w_field = Some(w_field);
        }
        // je/jz
        0x74 => {
            inst.op_type = Some("je".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jl/jnge - jump less/not greater or equal
        0x7C => {
            inst.op_type = Some("jl".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jle/jng - jump less or equal/not greater
        0x7E => {
            inst.op_type = Some("jle".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jb/jnae - jump below/not above or equal
        0x72 => {
            inst.op_type = Some("jb".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jbe/jna - jump below or equal/not above
        0x76 => {
            inst.op_type = Some("jbe".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jp/jpe - jump on parity/parity even
        0x7A => {
            inst.op_type = Some("jp".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jo - jump on overflow
        0x70 => {
            inst.op_type = Some("jo".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // js - jump on sign
        0x78 => {
            inst.op_type = Some("js".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jne/jnz - jump not equal/zero
        0x75 => {
            inst.op_type = Some("jne".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jnl/jge - jump not less or greater+equal
        0x7D => {
            inst.op_type = Some("jnl".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jg/jnle - jump greater or not less+equal
        0x7F => {
            inst.op_type = Some("jg".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jnb/jae - jump on not below or above+equal
        0x73 => {
            inst.op_type = Some("jnb".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // ja/jnbe - jump above or not below+equal
        0x77 => {
            inst.op_type = Some("ja".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
        }
        // jnp/jpo - jump not par or par odd
        0x7B => {
            inst.op_type = Some("jnp".to_string());
            inst.extra_bytes.push(ExtraBytesType::IpInc8);
            inst.data_needs_size = false;
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
    if rm_field == DIRECT_ADDR {
        inst.disp_direct_address = true;
    }
    let (rm_text, rm_text_end) = decode_rm_field(rm_field, mode, inst.w_field);
    match inst.d_field {
        None | Some(false) => {
            // Dest is rm field
            inst.dest_text = rm_text;
            inst.dest_text_end = rm_text_end;
            match inst.mod_field {
                // Register dest implies a size, so size prefix
                // isn't needed
                Some(ModType::RegisterMode) => inst.data_needs_size = false,
                _ => {}
            }
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
        (ModType::MemoryMode16, _) | (ModType::MemoryMode0, DIRECT_ADDR) => {
            inst.extra_bytes.push(ExtraBytesType::DispLo);
            inst.extra_bytes.push(ExtraBytesType::DispHi);
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
        }
        Some(ModRmByteType::ModMovRm) => {
            inst.extra_bytes.push(ExtraBytesType::DataLo);
            match inst.w_field {
                Some(true) => inst.extra_bytes.push(ExtraBytesType::DataHi),
                _ => {}
            }
            inst.add_data_to = Some(AddTo::Source);
            // source_text will be filled in later
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
        }
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
    // Add in data/immediate bytes to the source or dest text
    // Add a size prefix to an immediate source if dest is not a reg
    let size_prefix = match (inst.data_needs_size, inst.data_hi) {
        (false, _) => "",
        (true, Some(_)) => "word ",
        (true, None) => "byte ",
    };
    let data_bytes_text = match (inst.data_lo, inst.data_hi) {
        (Some(lo), None) => format!("{size_prefix}{}", lo as i8),
        (Some(lo), Some(hi)) => {
            let lo_hi = lo as u16 | ((hi as u16) << 8);
            format!("{size_prefix}{}", lo_hi as i16)
        }
        (None, None) => {
            unreachable!("ERROR: No data bytes found")
        }
        (None, Some(_)) => {
            unreachable!("ERROR: Low data byte not set")
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
    inst.dest_text = match inst.ip_inc8 {
        Some(ip_inc8) => Some(format!("${:+}", ip_inc8 as i8 + 2)),
        None => {
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
fn decode_immed_op(bits: u8) -> String {
    match bits {
        0b000 => "add".to_string(),
        0b001 => "or".to_string(),
        // Add with carry
        0b010 => "adc".to_string(),
        // Subtract with borrow
        0b011 => "sbb".to_string(),
        0b100 => "and".to_string(),
        0b101 => "sub".to_string(),
        0b110 => panic!("Unknown/unused op in decode_immed_op()"),
        // Compare - immediate with register/memory
        0b111 => "cmp".to_string(),
        _ => panic!("Bad bits specified in decode_immed_op()"),
    }
}
