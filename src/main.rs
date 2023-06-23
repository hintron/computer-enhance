fn main() {
    println!("Welcome to the Computer Enhance homework program!");
    decode_mov();
}

fn decode_mov() {
    println!("Decoding a mov instruction...");
    let mut inst_stream: Vec<u8> = vec![];

    // Create a test stream of instructions to decode
    inst_stream.push(0x89);
    inst_stream.push(0xD9);

    for byte in inst_stream {
        println!("{byte:#X} ({byte:#b})")
    }
}

