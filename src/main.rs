use std::env;
use std::fs::File;
use std::io::{self, Read};

mod decode;

// Internal imports
use crate::decode::decode;

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
