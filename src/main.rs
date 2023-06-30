use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{self, Read, Write};

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
    println!("; Outputting decoded assembly to file '{}'...", &args[2]);
    let mut output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&args[2])?;
    file.read_to_end(&mut inst_stream)?;

    let insts = decode(inst_stream);

    // Print out instructions to the output file
    writeln!(output, "bits 16")?;
    for inst in insts {
        writeln!(output, "{}", inst.text.unwrap())?;
    }

    Ok(())
}
