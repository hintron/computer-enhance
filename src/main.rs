//! # 8086 Decoder and Simulator
//!
//! This is an 8086 instruction decoder and simulator, implemented by following
//! the [Computer Enhance][1] performance awareness course.
//!
//! [1]: https://www.computerenhance.com/.

use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{self, Read, Write};

mod decode;
#[cfg(test)]
mod tests;

// Internal imports
use crate::decode::decode;

/// A custom struct holding parsed command line arguments
struct ArgsType {
    /// The File object to decode
    input_file: File,
    /// The File object to output decoded assembly to
    output_file: File,
}

/// Parse command line arguments.
/// Return an ArgsType struct
fn parse_args() -> io::Result<ArgsType> {
    let args: Vec<String> = env::args().collect();
    // Get the instruction stream from a file.
    println!("Decoding instructions from file '{}'...", &args[1]);
    let file = File::open(&args[1])?;
    println!("Outputting decoded assembly to file '{}'...", &args[2]);
    let output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&args[2])?;
    Ok(ArgsType {
        input_file: file,
        output_file: output,
    })
}

fn main() -> io::Result<()> {
    println!("Welcome to the Computer Enhance homework program!");

    let mut args = parse_args()?;
    let mut inst_stream: Vec<u8> = vec![];
    args.input_file.read_to_end(&mut inst_stream)?;
    let insts = decode(inst_stream);

    // Print out instructions to the output file
    writeln!(args.output_file, "bits 16")?;
    for inst in insts {
        writeln!(args.output_file, "{}", inst.text.unwrap())?;
    }

    Ok(())
}
