// Bytes in an 8086 inst are numbered from 1 up to 6
enum ByteNum {
    ONE,
    TWO,
}

enum ModType {
    MemoryMode0,
    MemoryMode8,
    MemoryMode16,
    RegisterMode,
    Unknown,
}

struct InstType {
    d_field: bool,
    w_field: bool,
    mod_field: ModType,
    reg_field: String,
    rm_field: String,
    op_type: String,
    /// The final instruction representation
    text: String,
}

fn main() {
    println!("Welcome to the Computer Enhance homework program!");
    decode();
}

fn decode() {
    println!("Decoding an instruction stream...");
    let mut inst_stream: Vec<u8> = vec![];

    let mut byte_num = ByteNum::ONE;
    let mut inst = InstType {
        d_field: false,
        w_field: false,
        mod_field: ModType::Unknown,
        reg_field: String::new(),
        rm_field: String::new(),
        op_type: String::new(),
        text: String::new(),
    };
    let mut inst_ended;

    // Create a test stream of instructions to decode
    // A mov instr
    inst_stream.push(0x89);
    inst_stream.push(0xD9);
    // Add bogus instr
    inst_stream.push(0x00);
    // Another mov instr
    inst_stream.push(0x88);
    inst_stream.push(0xD9);

    for byte in inst_stream {
        // println!("{byte:#X} ({byte:#b})");
        match &mut byte_num {
            ByteNum::ONE => {
                inst_ended = decode_1(&byte, &mut inst);
                byte_num = ByteNum::TWO;
            }
            ByteNum::TWO => {
                inst_ended = decode_2(&byte, &mut inst);
            }
        }

        if inst_ended {
            println!("{}", inst.text);
            byte_num = ByteNum::ONE;
        }
    }
}

/// Decode the first byte of the instruction. If there are no other bytes left
/// in the instruction, return true, otherwise return false.
/// For 8086 decoding help, see pg. 4-18 through 4-36.
fn decode_1(byte: &u8, inst: &mut InstType) -> bool {
    match byte {
        // mov
        0x88..=0x8C | 0x8E | 0xA0..=0xA3 | 0xB0..=0xBF | 0xC6..=0xC7 => {
            inst.op_type = "mov".to_string();
            inst.w_field = (byte & 0x1) == 1;
            inst.d_field = (byte & 0x2) == 1;
            false
        }
        _ => {
            inst.op_type = "unknown".to_string();
            inst.text = inst.op_type.clone();
            true
        }
    }
}

/// Decode the second byte of the instruction. If there are no other bytes left
/// in the instruction, return true, otherwise return false.
fn decode_2(byte: &u8, inst: &mut InstType) -> bool {
    // See table 4-8
    // Get the upper two bits
    let mod_temp = (byte & 0b11000000) >> 6;
    match mod_temp {
        0b00 => inst.mod_field = ModType::MemoryMode0,
        0b01 => inst.mod_field = ModType::MemoryMode8,
        0b10 => inst.mod_field = ModType::MemoryMode16,
        0b11 => inst.mod_field = ModType::RegisterMode,
        _ => unreachable!(),
    }

    // See table 4-9
    let reg_temp = (byte & 0b00111000) >> 3;
    match (reg_temp, inst.w_field) {
        (0b000, false) => inst.reg_field = "al".to_string(),
        (0b001, false) => inst.reg_field = "cl".to_string(),
        (0b010, false) => inst.reg_field = "dl".to_string(),
        (0b011, false) => inst.reg_field = "bl".to_string(),
        (0b100, false) => inst.reg_field = "ah".to_string(),
        (0b101, false) => inst.reg_field = "ch".to_string(),
        (0b110, false) => inst.reg_field = "dh".to_string(),
        (0b111, false) => inst.reg_field = "bh".to_string(),
        (0b000, true) => inst.reg_field = "ax".to_string(),
        (0b001, true) => inst.reg_field = "cx".to_string(),
        (0b010, true) => inst.reg_field = "dx".to_string(),
        (0b011, true) => inst.reg_field = "bx".to_string(),
        (0b100, true) => inst.reg_field = "sp".to_string(),
        (0b101, true) => inst.reg_field = "bp".to_string(),
        (0b110, true) => inst.reg_field = "si".to_string(),
        (0b111, true) => inst.reg_field = "di".to_string(),
        _ => unreachable!(),
    }

    // See table 4-10
    let rm_temp = byte & 0b00000111;
    match (rm_temp, &inst.mod_field, inst.w_field) {
        (0b000, ModType::RegisterMode, false) => inst.rm_field = "al".to_string(),
        (0b001, ModType::RegisterMode, false) => inst.rm_field = "cl".to_string(),
        (0b010, ModType::RegisterMode, false) => inst.rm_field = "dl".to_string(),
        (0b011, ModType::RegisterMode, false) => inst.rm_field = "bl".to_string(),
        (0b100, ModType::RegisterMode, false) => inst.rm_field = "ah".to_string(),
        (0b101, ModType::RegisterMode, false) => inst.rm_field = "ch".to_string(),
        (0b110, ModType::RegisterMode, false) => inst.rm_field = "dh".to_string(),
        (0b111, ModType::RegisterMode, false) => inst.rm_field = "bh".to_string(),
        (0b000, ModType::RegisterMode, true) => inst.rm_field = "ax".to_string(),
        (0b001, ModType::RegisterMode, true) => inst.rm_field = "cx".to_string(),
        (0b010, ModType::RegisterMode, true) => inst.rm_field = "dx".to_string(),
        (0b011, ModType::RegisterMode, true) => inst.rm_field = "bx".to_string(),
        (0b100, ModType::RegisterMode, true) => inst.rm_field = "sp".to_string(),
        (0b101, ModType::RegisterMode, true) => inst.rm_field = "bp".to_string(),
        (0b110, ModType::RegisterMode, true) => inst.rm_field = "si".to_string(),
        (0b111, ModType::RegisterMode, true) => inst.rm_field = "di".to_string(),
        _ => unimplemented!(),
        // TODO: Implement other memory modes!
        // TODO: For mm8 and mm16, decode byte 3
        // TODO: For mm16, decode byte 4
    };

    // See if reg is source or destination and construct instruction text
    match inst.d_field {
        false => inst.text = format!("{} {}, {}", inst.op_type, inst.rm_field, inst.reg_field),
        true => inst.text = format!("{} {}, {}", inst.op_type, inst.reg_field, inst.rm_field),
    };

    true
}
