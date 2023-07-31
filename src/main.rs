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

/// Parse command line arguments.
/// Return a tuple with the following contents (in order):
/// * input File object to decode
/// * output File object to output decoded assembly to
fn parse_args() -> io::Result<(File, File)> {
    let args: Vec<String> = env::args().collect();
    // Get the instruction stream from a file.
    println!("Decoding instructions from file '{}'...", &args[1]);
    let file = File::open(&args[1])?;
    println!("Outputting decoded assembly to file '{}'...", &args[2]);
    let output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&args[2])?;
    Ok((file, output))
}

fn main() -> io::Result<()> {
    println!("Welcome to the Computer Enhance homework program!");

    let (mut file, mut output) = parse_args()?;
    let mut inst_stream: Vec<u8> = vec![];
    file.read_to_end(&mut inst_stream)?;
    let insts = decode(inst_stream);

    // Print out instructions to the output file
    writeln!(output, "bits 16")?;
    for inst in insts {
        writeln!(output, "{}", inst.text.unwrap())?;
    }

    Ok(())
}
