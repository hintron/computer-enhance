// Bytes in an 8086 inst are numbered from 1 up to 6
enum ByteNum {
    ONE,
    TWO,
    THREE,
    FOUR,
    FIVE,
    SIX
}

fn main() {
    println!("Welcome to the Computer Enhance homework program!");
    decode_mov();
}

fn decode_mov() {
    println!("Decoding a mov instruction...");
    let mut inst_stream: Vec<u8> = vec![];

    let mut byte_num = ByteNum::ONE;

    // Create a test stream of instructions to decode
    inst_stream.push(0x89);
    inst_stream.push(0xD9);

    for byte in inst_stream {
        println!("{byte:#X} ({byte:#b})");
        match &mut byte_num {
            ByteNum::ONE => {
                println!("    byte 1");
                byte_num = ByteNum::TWO;
            }
            ByteNum::TWO => {
                println!("    byte 2");
                byte_num = ByteNum::ONE;
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
    }
}

