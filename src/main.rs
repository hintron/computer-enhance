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
enum ModRmBytes {
    ModRegRm,
    // Mod000Rm,
}

#[derive(Copy, Clone, Debug)]
enum DataBytesType {
    DataLo,
    DataHi,
    DispLo,
    DispHi,
}

#[derive(Debug)]
enum SrcDstType {
    Reg,
    Data8,
    Data16,
    Disp8,
    Disp16,
    DirectAddress16,
}

#[derive(Debug)]
struct InstType {
    d_field: bool,
    w_field: bool,
    mod_field: Option<ModType>,
    reg_field: Option<String>,
    // A string containing the registers but NOT the src/dst
    rm_field: Option<String>,
    op_type: Option<String>,
    // A list of all bytes processed for this instruction
    processed_bytes: Vec<u8>,
    mod_rm_byte: Option<ModRmBytes>,
    /// The actual data for the data bytes
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    /// The expected byte types to parse after we parse the 1st byte and the
    /// mod/rm byte (if it exists).
    data_bytes: Vec<DataBytesType>,
    /// The bytes from data_bytes that will be used for the source
    source_type: Option<SrcDstType>,
    // The text
    source_text: Option<String>,
    /// The bytes from data_bytes that will be used for the dest
    dest_type: Option<SrcDstType>,
    dest_text: Option<String>,
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
            rm_field: None,
            op_type: None,
            processed_bytes: vec![],
            mod_rm_byte: None,
            data_lo: None,
            data_hi: None,
            disp_lo: None,
            disp_hi: None,
            data_bytes: vec![],
            source_type: None,
            source_text: None,
            dest_type: None,
            dest_text: None,
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
                inst.d_field = (byte & 0x2) == 1;
                inst.mod_rm_byte = Some(ModRmBytes::ModRegRm);
                // We need to see what mod is before we know what is the source
                // and what is the destination
            }
            // mov - Immediate to register
            0xB0..=0xBF => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = (byte & 0b1000) == 1;
                let reg_field = decode_reg_field(byte & 0b111, inst.w_field);
                inst.dest_text = Some(reg_field);
                inst.reg_field = Some(reg_field);
                // No mod rm byte for this mov variant!
                // Indicate that there are source data bytes after this byte
                inst.data_bytes.push(DataBytesType::DataLo);
                if inst.w_field {
                    inst.data_bytes.push(DataBytesType::DataHi);
                    inst.source_type = Some(SrcDstType::Data16);
                } else {
                    inst.source_type = Some(SrcDstType::Data8);
                }
                inst.dest_type = Some(SrcDstType::Reg);
            }
            // TODO: Handle other mov variants:
            // 0x8E | 0xA0..=0xA3 | 0xC6..=0xC7
            _ => {
                inst.op_type = Some("unknown".to_string());
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
            Some(ModRmBytes::ModRegRm) => {
                // Get the next (mod rm) byte in the stream
                let byte = iter.next().unwrap();
                debug_byte(byte);
                inst.processed_bytes.push(*byte);
                // Decode the second byte of the instruction.
                // Get the upper two bits
                let mode = decode_mod_field((byte & 0b11000000) >> 6);
                inst.reg_field = Some(decode_reg_field((byte & 0b00111000) >> 3, inst.w_field));
                inst.rm_field = Some(decode_rm_field(byte & 0b00000111, &mode, inst.w_field));
                match &mode {
                    ModType::MemoryMode8 => {
                        inst.data_bytes.push(DataBytesType::DispLo);
                        inst.source_type = Some(SrcDstType::Disp8);
                    }
                    ModType::MemoryMode16 => {
                        inst.data_bytes.push(DataBytesType::DispLo);
                        inst.data_bytes.push(DataBytesType::DispHi);
                        inst.source_type = Some(SrcDstType::Disp16);
                    }
                    _ => {}
                }
                inst.mod_field = Some(mode);
                // See if reg is specified as source or dest
                match (inst.d_field, &inst.reg_field) {
                    (false, Some(x)) => {
                        inst.source_type = Some(SrcDstType::Reg);
                    }
                    (true, Some(x)) => {
                        inst.dest_type = Some(SrcDstType::Reg);
                    }
                    (_, None) => {
                        unreachable!()
                    }
                };
            }
            None => {
                // No mod rm byte for this instruction. Continue
            }
            _ => {}
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

        // TODO: How do figure this out?

        source = build_src_dest(
            &inst.source_type,
            inst.rm_field,
            inst.mod_field,
            inst.w_field,
            inst.disp_lo,
            inst.disp_hi,
        );
        dest = build_src_dest(
            &inst.dest_type,
            inst.rm_field,
            inst.mod_field,
            inst.w_field,
            inst.disp_lo,
            inst.disp_hi,
        );

        inst.text = Some(format!("{} {}, {}", inst.op_type.unwrap(), dest, source));
        println!("{}", inst.text.unwrap());
        // TODO: Record instruction
        // On to the next instruction...
    }
}

fn debug_byte(byte: &u8) {
    println!("; processing byte {byte:#X} ({byte:#b})");
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

/// Build either a source or a destination
fn build_src_dest(
    src_dst_type: Option<SrcDstType>,
    rm_field: Option<u8>,
    mode: &ModType,
    w: bool,
    byte_lo: Option<u8>,
    byte_hi: Option<u8>,
) -> Option<String> {
    let output = String::new();
    match src_dst_type {
        // If there is data, it's an immediate, and must be a source
        // An immediate destination is under disp as DIRECT_ADDRESS
        Some(SrcDstType::Data8) => output.push_str(&format!("{:#X}", inst.data_lo.unwrap())),
        Some(SrcDstType::Data16) => {
            output = format!("{:#X}{:#X}", inst.data_hi.unwrap(), inst.data_lo.unwrap());
        }
        Some(SrcDstType::Disp8) | Some(SrcDstType::Disp16) | None => {
            // Get the first part of the source text
            match (inst.d_field, &inst.rm_field) {
                (false, Some(rm)) => {
                    dest.push_str(&s.unwrap());
                }
                (true, Some(rm)) => {
                    // source.push_str(rm)
                }
                (_, _) => {}
            }
            output.push_str(&format!("[{:#X}]", inst.disp_lo.unwrap()));
            output = format!("[{:#X}{:#X}]", inst.disp_hi.unwrap(), inst.disp_lo.unwrap());
        }
    }

    let rm = match rm_field {
        None => return None,
        Some(x) => x,
    };
    // R/M (Register/Memory) Field Encoding
    // See table 4-10
}

// R/M (Register/Memory) Field Encoding
// See table 4-10
fn decode_rm_field(rm: u8, mode: &ModType, w: bool) -> String {
    match (rm, mode, w) {
        (0b000, ModType::RegisterMode, false) => Some("al".to_string()),
        (0b001, ModType::RegisterMode, false) => Some("cl".to_string()),
        (0b010, ModType::RegisterMode, false) => Some("dl".to_string()),
        (0b011, ModType::RegisterMode, false) => Some("bl".to_string()),
        (0b100, ModType::RegisterMode, false) => Some("ah".to_string()),
        (0b101, ModType::RegisterMode, false) => Some("ch".to_string()),
        (0b110, ModType::RegisterMode, false) => Some("dh".to_string()),
        (0b111, ModType::RegisterMode, false) => Some("bh".to_string()),
        (0b000, ModType::RegisterMode, true) => Some("ax".to_string()),
        (0b001, ModType::RegisterMode, true) => Some("cx".to_string()),
        (0b010, ModType::RegisterMode, true) => Some("dx".to_string()),
        (0b011, ModType::RegisterMode, true) => Some("bx".to_string()),
        (0b100, ModType::RegisterMode, true) => Some("sp".to_string()),
        (0b101, ModType::RegisterMode, true) => Some("bp".to_string()),
        (0b110, ModType::RegisterMode, true) => Some("si".to_string()),
        (0b111, ModType::RegisterMode, true) => Some("di".to_string()),
        (_, ModType::RegisterMode, _) => unreachable!("ERROR: Unknown RegisterMode condition"),
        (0b000, _, _) => Some("bx + si".to_string()),
        (0b001, _, _) => Some("bx + di".to_string()),
        (0b010, _, _) => Some("bp + si".to_string()),
        (0b011, _, _) => Some("bp + di".to_string()),
        (0b100, _, _) => Some("si".to_string()),
        (0b101, _, _) => Some("di".to_string()),
        (0b110, ModType::MemoryMode0, _) => {
            // No registers - just a 16-bit immediate address from data lo, data hi
            // This will be added in later
        }
        (0b110, ModType::MemoryMode8 | ModType::MemoryMode16, _) => {
            Some("bp".to_string());
        }
        (0b111, ModType::MemoryMode0, _) => Some("[bx]".to_string()),
        (_, ModType::MemoryMode0, _) => unreachable!("ERROR: Unknown MemoryMode0 condition"),
        // TODO: For mm8 and mm16, decode byte 3
        (_, ModType::MemoryMode8, _) => unimplemented!("TODO: Implement MemoryMode8"),
        // TODO: For mm16, decode byte 4
        (_, ModType::MemoryMode16, _) => unimplemented!("TODO: Implement MemoryMode8"),
    }
}
