use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum ModType {
    MemoryMode0,
    MemoryMode8,
    MemoryMode16,
    RegisterMode,
}

#[derive(Copy, Clone, Debug)]
enum ModRmByteType {
    ModRegRm,
    ModOpRm,
}

#[derive(Copy, Clone, Debug)]
enum DataBytesType {
    DataLo,
    DataHi,
    DispLo,
    DispHi,
}

// This tells us whether to add the displacement to the source or destination
#[derive(Debug)]
enum AddTo {
    Dest,
    Source,
}

#[derive(Debug)]
struct InstType {
    d_field: bool,
    w_field: bool,
    mod_field: Option<ModType>,
    reg_field: Option<String>,
    /// A string containing the registers but NOT the src/dst
    op_type: Option<String>,
    /// A list of all bytes processed for this instruction
    processed_bytes: Vec<u8>,
    mod_rm_byte: Option<ModRmByteType>,
    /// The actual data for the data bytes
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    /// If set, we expect to add displacement bytes to what AddTo specifies
    add_disp_to: Option<AddTo>,
    /// If set, we expect to add data bytes to what AddTo specifies
    add_data_to: Option<AddTo>,
    /// The expected byte types to parse after we parse the 1st byte and the
    /// mod/rm byte (if it exists).
    data_bytes: Vec<DataBytesType>,
    /// The text for the source operand
    source_text: Option<String>,
    source_text_end: Option<String>,
    /// The text for the destination operand
    dest_text: Option<String>,
    dest_text_end: Option<String>,
    /// If true, then we know the destination is a register and thus has a fixed
    /// size, so we don't need to put a 'word' or 'byte' prefix in front of an
    /// immediate source. If false, then an immediate will need a size prefix.
    is_dest_reg: bool,
    /// The final instruction representation
    text: Option<String>,
}

fn main() -> io::Result<()> {
    println!("; Welcome to the Computer Enhance homework program!");

    let args: Vec<String> = env::args().collect();

    let mut inst_stream: Vec<u8> = vec![];
    if args.len() < 2 {
        println!("; Decoding a test instruction stream...");
        // Create a test stream of instructions to decode
        // A mov instr
        inst_stream.push(0x89);
        inst_stream.push(0xD9);
        // Add bogus instr
        inst_stream.push(0x00);
        // Another mov instr
        inst_stream.push(0x88);
        inst_stream.push(0xD9);
    } else {
        // Get the instruction stream from a file.
        println!("; Decoding instructions from file '{}'...", &args[1]);
        let mut file = File::open(&args[1])?;
        file.read_to_end(&mut inst_stream)?;
    }

    println!("bits 16");
    decode(inst_stream);

    Ok(())
}

fn decode(inst_stream: Vec<u8>) {
    let mut iter = inst_stream.iter().peekable();
    while iter.peek().is_some() {
        let mut inst = InstType {
            d_field: false,
            w_field: false,
            mod_field: None,
            reg_field: None,
            op_type: None,
            processed_bytes: vec![],
            mod_rm_byte: None,
            data_lo: None,
            data_hi: None,
            add_data_to: None,
            disp_lo: None,
            disp_hi: None,
            add_disp_to: None,
            data_bytes: vec![],
            // source_type: None,
            source_text: None,
            source_text_end: None,
            // dest_type: None,
            dest_text: None,
            dest_text_end: None,
            is_dest_reg: false,
            text: None,
        };
        let byte = iter.next().unwrap();
        debug_byte(byte);
        inst.processed_bytes.push(*byte);

        // Decode the first byte of the instruction.
        // For 8086 decoding help, see pg. 4-18 through 4-36.
        match byte {
            // mov - Register/memory to/from register
            0x88..=0x8C => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = (byte & 0x1) == 1;
                inst.d_field = ((byte & 0x2) >> 1) == 1;
                inst.mod_rm_byte = Some(ModRmByteType::ModRegRm);
                // We need to see what mod is before we know what is the source
                // and what is the destination
            }
            // mov - Immediate to register/memory
            0xC6..=0xC7 => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = (byte & 0x1) == 1;
                // In effect, the d field is hard coded to 0: the destination is
                // rm and the source is an immediate (which replaced reg from
                // the above mov variant)
                inst.mod_rm_byte = Some(ModRmByteType::ModOpRm);
            }
            // mov - Immediate to register
            0xB0..=0xBF => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = ((byte & 0b1000) >> 3) == 1;
                let reg_field = decode_reg_field(byte & 0b111, inst.w_field);
                inst.reg_field = Some(reg_field.clone());
                inst.dest_text = Some(reg_field);
                inst.is_dest_reg = true;
                // No mod rm byte for this mov variant!
                // Indicate that there are source data bytes after this byte
                inst.add_data_to = Some(AddTo::Source);
                // source_text will be filled in later
                inst.data_bytes.push(DataBytesType::DataLo);
                if inst.w_field {
                    inst.data_bytes.push(DataBytesType::DataHi);
                }
            }
            // TODO: Handle other mov variants:
            // 0x8E | 0xA0..=0xA3 | 0xC6..=0xC7
            _ => {
                inst.op_type = Some("; unknown instruction".to_string());
                inst.text = inst.op_type.clone();
                println!("{}", inst.text.unwrap());
                continue;
            }
        }

        if iter.peek().is_none() {
            println!("; End of instruction stream");
            break;
        };

        // Process mod rm byte, if it exists
        match &inst.mod_rm_byte {
            Some(mod_rm_byte) => {
                // Get the next (mod rm) byte in the stream
                let byte = iter.next().unwrap();
                debug_byte(byte);
                inst.processed_bytes.push(*byte);
                // Decode the second byte of the instruction.
                // Get the upper two bits
                let mode = decode_mod_field((byte & 0b11000000) >> 6);
                let (rm_text, rm_text_end) =
                    decode_rm_field(byte & 0b00000111, &mode, inst.w_field);
                match inst.d_field {
                    false => {
                        // Dest is rm field
                        inst.dest_text = rm_text;
                        inst.dest_text_end = rm_text_end;
                        match inst.mod_field {
                            // Mark dest as a reg to add size prefix to imm sources
                            Some(ModType::RegisterMode) => inst.is_dest_reg = true,
                            _ => {}
                        }
                    }
                    true => {
                        // Source is rm field
                        inst.source_text = rm_text;
                        inst.source_text_end = rm_text_end;
                    }
                }

                // Indicate that there are displacement bytes to process next
                // Displacement bytes come before immediate/data bytes
                match &mode {
                    ModType::MemoryMode8 => {
                        inst.data_bytes.push(DataBytesType::DispLo);
                    }
                    ModType::MemoryMode16 => {
                        inst.data_bytes.push(DataBytesType::DispLo);
                        inst.data_bytes.push(DataBytesType::DispHi);
                    }
                    _ => {}
                }

                // Process the middle part of the mod rm byte
                match mod_rm_byte {
                    ModRmByteType::ModRegRm => {
                        let reg_field = decode_reg_field((byte & 0b00111000) >> 3, inst.w_field);
                        // See if reg is specified as source or dest
                        match inst.d_field {
                            false => {
                                // Source is REG field
                                inst.source_text = Some(reg_field.clone());
                                inst.source_text_end = None;
                            }
                            true => {
                                // Destination is REG field
                                inst.dest_text = Some(reg_field.clone());
                                inst.dest_text_end = None;
                            }
                        };
                        inst.reg_field = Some(reg_field);
                    }
                    ModRmByteType::ModOpRm => {
                        match inst.w_field {
                            false => {
                                inst.data_bytes.push(DataBytesType::DataLo);
                            }
                            true => {
                                inst.data_bytes.push(DataBytesType::DataLo);
                                inst.data_bytes.push(DataBytesType::DataHi);
                            }
                        }
                        inst.add_data_to = Some(AddTo::Source);
                        // source_text will be filled in later
                    }
                }
                // Indicate what displacement should be added to: src or dest
                match (inst.d_field, &mode) {
                    (false, ModType::MemoryMode8 | ModType::MemoryMode16) => {
                        inst.add_disp_to = Some(AddTo::Dest);
                    }
                    (true, ModType::MemoryMode8 | ModType::MemoryMode16) => {
                        inst.add_disp_to = Some(AddTo::Source);
                    }
                    (_, _) => {}
                }

                inst.mod_field = Some(mode);
            }
            None => {
                // No mod rm byte for this instruction. Continue
            }
        }

        // Process data bytes
        for byte_type in &inst.data_bytes {
            if iter.peek().is_none() {
                println!("; End of instruction stream");
                return;
            };
            let byte = iter.next().unwrap();
            debug_byte(byte);
            inst.processed_bytes.push(*byte);

            match byte_type {
                DataBytesType::DispLo => inst.disp_lo = Some(*byte),
                DataBytesType::DispHi => inst.disp_hi = Some(*byte),
                DataBytesType::DataLo => inst.data_lo = Some(*byte),
                DataBytesType::DataHi => inst.data_hi = Some(*byte),
            }
        }

        // Add in displacement bytes to the source or dest text
        if inst.add_disp_to.is_some() {
            let disp_bytes_text = match (inst.disp_lo, inst.disp_hi) {
                (Some(lo), None) => format!("0x{lo:X}"),
                (Some(lo), Some(hi)) => format!("0x{hi:X}{lo:X}"),
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

        // Add in data/immediate bytes to the source or dest text
        if inst.add_data_to.is_some() {
            // Add a size prefix to an immediate source if dest is not a reg
            let size_prefix = match (inst.is_dest_reg, inst.data_hi) {
                (false, Some(_)) => "word ",
                (false, None) => "byte ",
                // If dest is reg, size is implicit in reg size
                (true, _) => "",
            };
            let data_bytes_text = match (inst.data_lo, inst.data_hi) {
                (Some(lo), None) => format!("{size_prefix}0x{lo:X}"),
                (Some(lo), Some(hi)) => {
                    format!("{size_prefix}0x{hi:X}{lo:X}")
                }
                (None, None) => {
                    unreachable!("ERROR: No data bytes found")
                }
                (None, Some(_)) => {
                    unreachable!("ERROR: Low data byte not set")
                }
            };

            match (&mut inst.source_text, inst.add_data_to) {
                (Some(source_text), Some(AddTo::Source)) => {
                    source_text.push_str(&data_bytes_text);
                }
                (None, Some(AddTo::Source)) => {
                    inst.source_text = Some(data_bytes_text);
                }
                (_, _) => {
                    unreachable!("Unhandled combo for source_text, add_data_to")
                }
            }
        };

        let dest_text = concat_texts(inst.dest_text, inst.dest_text_end);
        let source_text = concat_texts(inst.source_text, inst.source_text_end);
        let inst_text = match (inst.op_type, dest_text, source_text) {
            (Some(op), Some(dest_text), Some(source_text)) => {
                format!("{op} {dest_text}, {source_text}")
            }
            (Some(op), Some(dest_text), None) => {
                format!("{op} {dest_text}")
            }
            (Some(op), None, Some(source_text)) => {
                format!("{op} {source_text}")
            }
            (Some(op), None, None) => {
                format!("{op}")
            }
            (None, _, _) => {
                unreachable!("Op code text not set!");
            }
        };
        println!("{}", inst_text);
        inst.text = Some(inst_text);
        // TODO: Record instruction
        // On to the next instruction...
    }
}

/// Concatenate two Option Strings
fn concat_texts(a: Option<String>, b: Option<String>) -> Option<String> {
    match (a, b) {
        (Some(str_a), Some(str_b)) => Some(format!("{str_a}{str_b}")),
        (Some(str_a), None) => Some(str_a),
        (None, Some(str_b)) => Some(str_b),
        (None, None) => None,
    }
}

fn debug_byte(byte: &u8) {
    println!("; processing byte 0x{byte:02X} (0b{byte:08b})");
}

/// MOD (Mode) Field Encoding
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

// REG (Register) Field Encoding
// See table 4-9
fn decode_reg_field(reg: u8, w: bool) -> String {
    match (reg, w) {
        (0b000, false) => "al".to_string(),
        (0b001, false) => "cl".to_string(),
        (0b010, false) => "dl".to_string(),
        (0b011, false) => "bl".to_string(),
        (0b100, false) => "ah".to_string(),
        (0b101, false) => "ch".to_string(),
        (0b110, false) => "dh".to_string(),
        (0b111, false) => "bh".to_string(),
        (0b000, true) => "ax".to_string(),
        (0b001, true) => "cx".to_string(),
        (0b010, true) => "dx".to_string(),
        (0b011, true) => "bx".to_string(),
        (0b100, true) => "sp".to_string(),
        (0b101, true) => "bp".to_string(),
        (0b110, true) => "si".to_string(),
        (0b111, true) => "di".to_string(),
        _ => unreachable!(),
    }
}

// R/M (Register/Memory) Field Encoding
// See table 4-10
/// Return a tuple of the first part of the text and the last part of the text,
/// so the displacement can be optionally inserted in later. If the last part of
/// the text is None, then there should be no insertion.
fn decode_rm_field(rm: u8, mode: &ModType, w: bool) -> (Option<String>, Option<String>) {
    match (rm, mode, w) {
        (0b000, ModType::RegisterMode, false) => (Some("al".to_string()), None),
        (0b001, ModType::RegisterMode, false) => (Some("cl".to_string()), None),
        (0b010, ModType::RegisterMode, false) => (Some("dl".to_string()), None),
        (0b011, ModType::RegisterMode, false) => (Some("bl".to_string()), None),
        (0b100, ModType::RegisterMode, false) => (Some("ah".to_string()), None),
        (0b101, ModType::RegisterMode, false) => (Some("ch".to_string()), None),
        (0b110, ModType::RegisterMode, false) => (Some("dh".to_string()), None),
        (0b111, ModType::RegisterMode, false) => (Some("bh".to_string()), None),
        (0b000, ModType::RegisterMode, true) => (Some("ax".to_string()), None),
        (0b001, ModType::RegisterMode, true) => (Some("cx".to_string()), None),
        (0b010, ModType::RegisterMode, true) => (Some("dx".to_string()), None),
        (0b011, ModType::RegisterMode, true) => (Some("bx".to_string()), None),
        (0b100, ModType::RegisterMode, true) => (Some("sp".to_string()), None),
        (0b101, ModType::RegisterMode, true) => (Some("bp".to_string()), None),
        (0b110, ModType::RegisterMode, true) => (Some("si".to_string()), None),
        (0b111, ModType::RegisterMode, true) => (Some("di".to_string()), None),
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
        (0b000, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx + si + ".to_string()), Some("]".to_string()))
        }
        (0b001, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx + di + ".to_string()), Some("]".to_string()))
        }
        (0b010, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp + si + ".to_string()), Some("]".to_string()))
        }
        (0b011, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp + di + ".to_string()), Some("]".to_string()))
        }
        (0b100, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[si + ".to_string()), Some("]".to_string()))
        }
        (0b101, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[di + ".to_string()), Some("]".to_string()))
        }
        (0b110, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bp + ".to_string()), Some("]".to_string()))
        }
        (0b111, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            (Some("[bx + ".to_string()), Some("]".to_string()))
        }
        (_, ModType::MemoryMode8, _) => unreachable!("ERROR: Unknown MemoryMode8 condition"),
        (_, ModType::MemoryMode16, _) => unreachable!("ERROR: Unknown MemoryMode16 condition"),
    }
}
