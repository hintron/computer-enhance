use std::collections::VecDeque;
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
enum ByteType {
    ModRegRm,
    // Mod000Rm,
    DataLo,
    DataHi,
    DispLo,
    DispHi,
}

#[derive(Debug)]
struct InstType {
    d_field: bool,
    w_field: bool,
    mod_field: Option<ModType>,
    reg_field: Option<String>,
    rm_field: Option<String>,
    op_type: Option<String>,
    processed_bytes: Vec<u8>,
    processed_byte_types: Vec<ByteType>,
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
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
    // NOTE: I don't know how to do this other than with a while let
    while iter.peek().is_some() {
        let mut inst = InstType {
            d_field: false,
            w_field: false,
            mod_field: None,
            reg_field: None,
            rm_field: None,
            op_type: None,
            processed_bytes: vec![],
            processed_byte_types: vec![],
            data_lo: None,
            data_hi: None,
            disp_lo: None,
            disp_hi: None,
            text: None,
        };
        let byte = iter.next().unwrap();
        debug_byte(byte);
        inst.processed_bytes.push(*byte);

        let mut source = String::new();
        let mut dest = String::new();
        let mut byte_order_tmp = VecDeque::new();
        let mut next_byte_type;

        // Decode the first byte of the instruction.
        // For 8086 decoding help, see pg. 4-18 through 4-36.
        match byte {
            // mov - Register/memory to/from register
            0x88..=0x8C => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = (byte & 0x1) == 1;
                inst.d_field = (byte & 0x2) == 1;
                next_byte_type = Some(ByteType::ModRegRm);
                // byte_order_tmp.push_back(ByteType::ModRegRm);
                // We need to see what mod is before we know about disp lo/hi
            }
            // mov - Immediate to register
            0xB0..=0xBF => {
                inst.op_type = Some("mov".to_string());
                inst.w_field = (byte & 0b1000) == 1;
                let reg_field = decode_reg_field(byte & 0b111, inst.w_field);
                dest.push_str(&reg_field);
                inst.reg_field = Some(reg_field);
                next_byte_type = Some(ByteType::DataLo);
                // byte_order_tmp.push_back(ByteType::DataLo);
                if inst.w_field {
                    byte_order_tmp.push_back(ByteType::DataHi);
                }
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

        // Finally, store this byte
        inst.processed_byte_types.push(next_byte_type.unwrap());

        while next_byte_type.is_some() {
            if iter.peek().is_none() {
                break;
            };
            let byte = iter.next().unwrap();
            debug_byte(byte);
            inst.processed_bytes.push(*byte);

            // Decode the next byte of the instruction
            match next_byte_type {
                Some(ByteType::ModRegRm) => {
                    // Decode the second byte of the instruction.
                    // Get the upper two bits
                    let mode = decode_mod_field((byte & 0b11000000) >> 6);
                    inst.reg_field = Some(decode_reg_field((byte & 0b00111000) >> 3, inst.w_field));
                    inst.rm_field = Some(decode_rm_field(byte & 0b00000111, &mode, inst.w_field));
                    match &mode {
                        ModType::MemoryMode8 => {
                            byte_order_tmp.push_back(ByteType::DispLo);
                        }
                        ModType::MemoryMode16 => {
                            byte_order_tmp.push_back(ByteType::DispLo);
                            byte_order_tmp.push_back(ByteType::DispHi);
                        }
                        _ => {}
                    }
                    inst.mod_field = Some(mode);

                    // See if reg is source or destination and construct instruction text
                    match inst.d_field {
                        false => {
                            source.push_str(&inst.reg_field.unwrap());
                            dest.push_str(&inst.rm_field.unwrap());
                        }
                        true => {
                            source.push_str(&inst.rm_field.unwrap());
                            dest.push_str(&inst.reg_field.unwrap());
                        }
                    };
                    // TODO: Why is source/dest backwards?
                }
                Some(ByteType::DispLo) => inst.disp_lo = Some(*byte),
                Some(ByteType::DispHi) => inst.disp_hi = Some(*byte),
                Some(ByteType::DataLo) => inst.data_lo = Some(*byte),
                Some(ByteType::DataHi) => inst.data_hi = Some(*byte),
                None => {}
            }

            // Record this processed byte
            inst.processed_byte_types.push(next_byte_type.unwrap());
            next_byte_type = byte_order_tmp.pop_front();
        }

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

// R/M (Register/Memory) Field Encoding
// See table 4-10
fn decode_rm_field(rm: u8, mode: &ModType, w: bool) -> String {
    match (rm, mode, w) {
        (0b000, ModType::RegisterMode, false) => "al".to_string(),
        (0b001, ModType::RegisterMode, false) => "cl".to_string(),
        (0b010, ModType::RegisterMode, false) => "dl".to_string(),
        (0b011, ModType::RegisterMode, false) => "bl".to_string(),
        (0b100, ModType::RegisterMode, false) => "ah".to_string(),
        (0b101, ModType::RegisterMode, false) => "ch".to_string(),
        (0b110, ModType::RegisterMode, false) => "dh".to_string(),
        (0b111, ModType::RegisterMode, false) => "bh".to_string(),
        (0b000, ModType::RegisterMode, true) => "ax".to_string(),
        (0b001, ModType::RegisterMode, true) => "cx".to_string(),
        (0b010, ModType::RegisterMode, true) => "dx".to_string(),
        (0b011, ModType::RegisterMode, true) => "bx".to_string(),
        (0b100, ModType::RegisterMode, true) => "sp".to_string(),
        (0b101, ModType::RegisterMode, true) => "bp".to_string(),
        (0b110, ModType::RegisterMode, true) => "si".to_string(),
        (0b111, ModType::RegisterMode, true) => "di".to_string(),
        (_, ModType::RegisterMode, _) => unreachable!("ERROR: Unknown RegisterMode condition"),
        (0b000, ModType::MemoryMode0, _) => "[bx + si]".to_string(),
        (0b001, ModType::MemoryMode0, _) => "[bx + di]".to_string(),
        (0b010, ModType::MemoryMode0, _) => "[bp + si]".to_string(),
        (0b011, ModType::MemoryMode0, _) => "[bp + di]".to_string(),
        (0b100, ModType::MemoryMode0, _) => "[si]".to_string(),
        (0b101, ModType::MemoryMode0, _) => "[di]".to_string(),
        (0b110, ModType::MemoryMode0, _) => {
            unimplemented!("TODO: Implement DIRECT ADDRESS for MemoryMode0")
        }
        (0b111, ModType::MemoryMode0, _) => "[bx]".to_string(),
        (_, ModType::MemoryMode0, _) => unreachable!("ERROR: Unknown MemoryMode0 condition"),
        // TODO: For mm8 and mm16, decode byte 3
        (_, ModType::MemoryMode8, _) => unimplemented!("TODO: Implement MemoryMode8"),
        // TODO: For mm16, decode byte 4
        (_, ModType::MemoryMode16, _) => unimplemented!("TODO: Implement MemoryMode8"),
    }
}
