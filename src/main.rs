// Bytes in an 8086 inst are numbered from 1 up to 6
enum ByteNum {
    ONE,
    TWO,
    THREE,
    FOUR,
    FIVE,
    SIX,
}

enum OpType {
    MOV,
    UNKNOWN
}

fn main() {
    println!("Welcome to the Computer Enhance homework program!");
    decode();
}

fn decode() {
    println!("Decoding an instruction stream...");
    let mut inst_stream: Vec<u8> = vec![];

    let mut byte_num = ByteNum::ONE;
    let mut inst = OpType::UNKNOWN;
    let mut inst_ended = false;

    // Create a test stream of instructions to decode
    inst_stream.push(0x89);
    inst_stream.push(0xD9);
    inst_stream.push(0x88);
    inst_stream.push(0xD9);

    for byte in inst_stream {
        println!("{byte:#X} ({byte:#b})");
        match &mut byte_num {
            ByteNum::ONE => {
                println!("    byte 1");
                inst = decode_1(&byte);
                byte_num = ByteNum::TWO;
            }
            ByteNum::TWO => {
                println!("    byte 2");
                inst_ended = true;
            }
            ByteNum::THREE => {
                println!("    byte 3");
            }
            ByteNum::FOUR => {
                println!("    byte 4");
            }
            ByteNum::FIVE => {
                println!("    byte 5");
            }
            ByteNum::SIX => {
                println!("    byte 6");
            }
        }

        if inst_ended {
            match inst {
                OpType::MOV => {
                    println!("mov");
                }
                OpType::UNKNOWN => {
                    println!("unknown instruction");
                }
            }
            inst_ended = false;
            byte_num = ByteNum::ONE;
        }
    }

}

fn decode_1(byte: &u8) -> OpType {
    // For 8086 decoding help, see pg. 4-18 through 4-36
    match byte {
        // mov
        0x88..=0x8C | 0x8E | 0xA0..=0xA3 | 0xB0..=0xBF | 0xC6..=0xC7 => {
            return OpType::MOV;
        }
        _ => {
            return OpType::UNKNOWN;
        }
    }
}
