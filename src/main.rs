use std::env;
use std::fs::File;
use std::io::{self, Read};

mod decode;
#[cfg(test)]
mod tests;

// Internal imports
use crate::decode::decode;

fn main() -> io::Result<()> {
    println!("; Welcome to the Computer Enhance homework program!");

    let args: Vec<String> = env::args().collect();

    let mut inst_stream: Vec<u8> = vec![];
    // Get the instruction stream from a file.
    println!("; Decoding instructions from file '{}'...", &args[1]);
    let mut file = File::open(&args[1])?;
    file.read_to_end(&mut inst_stream)?;

    println!("bits 16");
    let _insts = decode(inst_stream);

    Ok(())
}
