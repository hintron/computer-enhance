//! # 8086 Decoder and Simulator - Executable

use anyhow::{bail, Result};
use std::env;
use std::io::Write;

// Internal imports
use computer_enhance::decode::{decode, decode_execute};
use computer_enhance::{file_to_byte_vec, get_output_file_from_path};

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
    print: bool,
    verbose: bool,
    /// If true, do NOT calculate and print out changes to the IP register
    /// during execution.
    no_ip: bool,
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
-p|--print : Print out the instructions as decoded/executed.
-v|--verbose : Increase verbosity of print to include debug information.
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
    } else if arg.starts_with("-p") || arg.starts_with("--print") {
        parsed_args.print = true;
        Ok(false)
    } else if arg.starts_with("-v") || arg.starts_with("--verbose") {
        parsed_args.verbose = true;
        Ok(false)
    } else if arg.starts_with("--no-ip") {
        parsed_args.no_ip = true;
        Ok(false)
    } else {
        bail!("Unexpected optional arg '{arg}'\n{USAGE}");
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
            bail!("Unexpected positional arg '{arg}'\n{USAGE}");
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

    // Check to make sure all required args exist
    match (
        &parsed_args.input_file,
        &parsed_args.output_file,
        &parsed_args.help,
    ) {
        (_, _, true) => { /* Don't check with -h */ }
        (None, _, _) => {
            bail!("Missing required positional arg <input>\n{USAGE}");
        }
        (_, None, _) => {
            bail!("Missing required positional arg <output>\n{USAGE}");
        }
        _ => {}
    }

    Ok(parsed_args)
}

fn main() -> Result<()> {
    // Parse args. Fail if incorrect args are given
    let args = parse_args()?;

    // Now, *process* parsed args
    if args.help {
        print_help();
        return Ok(());
    }

    println!("Executable: {}", args.first_arg.unwrap());
    // Make sure required args exist

    let inst_stream = file_to_byte_vec(&args.input_file)?;
    let mut output_file = get_output_file_from_path(&args.output_file)?;
    println!(
        "Decoding instructions from file '{}'...",
        args.input_file.unwrap()
    );
    println!(
        "Outputting decoded assembly to file '{}'...",
        args.output_file.unwrap()
    );

    if args.execute {
        let text_lines = decode_execute(inst_stream, args.print, args.verbose, args.no_ip);
        for line in text_lines {
            writeln!(output_file, "{}", line)?;
        }
    } else {
        let insts = decode(inst_stream, args.print, args.verbose);
        // Print out instructions to the output file
        writeln!(output_file, "bits 16")?;
        for inst in insts {
            writeln!(output_file, "{}", inst.text.unwrap())?;
        }
    };

    Ok(())
}
