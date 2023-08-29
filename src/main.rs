//! # 8086 Decoder and Simulator
//!
//! This is an 8086 instruction decoder and simulator, implemented by following
//! the [Computer Enhance][1] performance awareness course.
//!
//! [1]: https://www.computerenhance.com/.

use anyhow::{bail, Result};
use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{Read, Write};

mod decode;
#[cfg(test)]
mod tests;

// Internal imports
use crate::decode::decode;

/// A custom struct holding parsed command line arguments
#[derive(Default)]
struct ArgsType {
    first_arg: Option<String>,
    /// The File object to decode
    input_file: Option<String>,
    /// The File object to output decoded assembly to
    output_file: Option<String>,
    /// If true, execute the stream. If false, just decode it
    execute: bool,
    help: bool,
}

const USAGE: &str = "Usage: computer-enhance <input> <output> [-h|--help] [OPTIONS]";
const HELP: &str = "
The Computer Enhance x86 Decoder and Simulator

Required Parameters:
<input> : The input binary file containing x86 binary code.
<output> : The output file to print decoded assembly to.

Options:
-e|--exec : If specified, simulate the input instruction stream in addition to
decoding it.
-h|--help : Print this help message.
";

fn print_help() {
    println!("{USAGE}");
    println!("{HELP}");
}

/// Take a given arg and parse it as an optional argument. Modify parsed_args.
/// Return true if the next argument is a value for this argument.
fn parse_optional(arg: String, parsed_args: &mut ArgsType) -> Result<bool> {
    if arg.starts_with("-h") || arg.starts_with("--help") {
        parsed_args.help = true;
        Ok(false)
    } else if arg.starts_with("-e") || arg.starts_with("--exec") {
        parsed_args.execute = true;
        Ok(false)
    } else {
        bail!("ERROR: Unexpected optional arg '{arg}'\n{USAGE}");
    }
}

/// Take the given arg and parse it as a positional argument. Modify parsed_args
fn parse_positional(arg: String, parsed_args: &mut ArgsType) -> Result<()> {
    match (&parsed_args.input_file, &parsed_args.output_file) {
        (None, _) => {
            parsed_args.input_file = Some(arg);
        }
        (_, None) => {
            parsed_args.output_file = Some(arg);
        }
        _ => {
            bail!("ERROR: Unexpected positional arg '{arg}'\n{USAGE}");
        }
    }

    Ok(())
}

/// Parse command line arguments.
/// Return an ArgsType struct
fn parse_args() -> Result<ArgsType> {
    let args: Vec<String> = env::args().collect();
    let mut parsed_args = ArgsType {
        ..Default::default()
    };

    let mut get_arg_value = false;
    // Now parse args, excluding the first arg
    for arg in args {
        if parsed_args.first_arg.is_none() {
            parsed_args.first_arg = Some(arg);
        } else if get_arg_value {
            // This argument is a value for the last argument
            unimplemented!();
        } else if arg.starts_with("-") {
            get_arg_value = parse_optional(arg, &mut parsed_args)?;
        } else {
            parse_positional(arg, &mut parsed_args)?;
        }
    }

    Ok(parsed_args)
}

fn main() -> Result<()> {
    let args = parse_args()?;

    // Now, *process* parsed args
    if args.help {
        print_help();
        return Ok(());
    }

    println!("Executable: {}", args.first_arg.unwrap());
    // Make sure required args exist
    let mut input_file = match &args.input_file {
        None => {
            bail!("ERROR: Missing required positional arg <input-file>\n{USAGE}");
        }
        Some(file) => File::open(file)?,
    };
    // Get the instruction stream from a file.
    println!(
        "Decoding instructions from file '{}'...",
        args.input_file.unwrap()
    );

    let mut output_file = match &args.output_file {
        None => {
            bail!("ERROR: Missing required positional arg <output_file>\n{USAGE}");
        }
        Some(file) => OpenOptions::new().write(true).create(true).open(file)?,
    };
    println!(
        "Outputting decoded assembly to file '{}'...",
        args.output_file.unwrap()
    );

    let mut inst_stream: Vec<u8> = vec![];
    input_file.read_to_end(&mut inst_stream)?;
    let insts = decode(inst_stream);

    // Print out instructions to the output file
    writeln!(output_file, "bits 16")?;
    for inst in insts {
        writeln!(output_file, "{}", inst.text.unwrap())?;
    }

    Ok(())
}
