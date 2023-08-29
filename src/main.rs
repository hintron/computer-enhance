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
use std::io::{Error, ErrorKind};

mod decode;
#[cfg(test)]
mod tests;

// Internal imports
use crate::decode::decode;

/// A custom struct holding parsed command line arguments
#[derive(Default)]
struct ArgsType {
    /// The File object to decode
    input_file: Option<File>,
    /// The File object to output decoded assembly to
    output_file: Option<File>,
}

/// Take the given arg and parse it as a positional argument. Modify parsed_args
fn parse_positional(arg: &str, parsed_args: &mut ArgsType) -> io::Result<()> {
    match (&parsed_args.input_file, &parsed_args.output_file) {
        (None, _) => {
            // Get the instruction stream from a file.
            println!("Decoding instructions from file '{}'...", arg);
            parsed_args.input_file = Some(File::open(arg)?);
        }
        (_, None) => {
            println!("Outputting decoded assembly to file '{}'...", arg);
            parsed_args.output_file =
                Some(OpenOptions::new().write(true).create_new(true).open(arg)?);
        }
        _ => {
            return Err(Error::new(
                ErrorKind::Other,
                format!("ERROR: Unexpected positional arg '{arg}'..."),
            ));
        }
    }

    Ok(())
}

/// Parse command line arguments.
/// Return an ArgsType struct
fn parse_args() -> io::Result<ArgsType> {
    let args: Vec<String> = env::args().collect();
    let mut parsed_args = ArgsType {
        ..Default::default()
    };

    // Now parse args, excluding the first arg
    for arg in &args[1..] {
        if arg.starts_with("-") {
            unimplemented!();
        } else if arg.starts_with("--") {
            unimplemented!();
        } else {
            parse_positional(arg, &mut parsed_args)?;
        }
    }

    Ok(parsed_args)
}

fn main() -> io::Result<()> {
    println!("Welcome to the Computer Enhance homework program!");

    let args = parse_args()?;
    let mut inst_stream: Vec<u8> = vec![];
    args.input_file.unwrap().read_to_end(&mut inst_stream)?;
    let insts = decode(inst_stream);

    let mut output_file = args.output_file.unwrap();
    // Print out instructions to the output file
    writeln!(output_file, "bits 16")?;
    for inst in insts {
        writeln!(output_file, "{}", inst.text.unwrap())?;
    }

    Ok(())
}
