//! # 8086 Decoder and Simulator - Executable

use anyhow::{bail, Result};
use std::env;
use std::io::Write;
use std::sync::mpsc;

// Internal imports
use emu86rs::cycles::print_cycle_header;
use emu86rs::decode::{decode, decode_execute, CpuType, DecodeSettings, ExecuteSettings};
use emu86rs::display::{graphics_loop, memory_to_file, ImageFormat, MemImage};
use emu86rs::execute::print_final_state;
use emu86rs::{file_to_byte_vec, get_output_file_from_path};

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
    verbose: bool,
    /// If true, do NOT calculate and print out changes to the IP register
    /// during execution.
    no_ip: bool,
    /// If true, overwrite the output file. If false (default), don't overwrite.
    overwrite: bool,
    /// If specified, do cycle estimates during execution for the given CPU
    /// type.
    cycle_type: Option<CpuType>,
    /// If specified, make any RET instruction stop the simulator.
    stop_on_ret: bool,
    /// The value to initially set the IP register to
    init_ip: Option<u16>,
    /// The value to initially set the SP register to
    init_sp: Option<u16>,
    /// If true, enable a graphical display to be used for the program.
    /// The graphical display will take over the main thread, and the decode and
    /// simulate logic will be moved into a separate thread.
    /// If the program didn't print anything to the graphical display, then
    /// print the first 64x65 bytes of memory at the end of execution.
    display_window: bool,
    /// If true, save final memory contents to a file
    display_file: Option<String>,
    /// If specified, exit simulation after this many cycles have elapsed
    exit_after: Option<u64>,
    /// If true, exit the decoder when an int3 is encountered.
    stop_on_int3: bool,
    /// If true, save a screenshot of each rendered frame to a file, for debug.
    screenshots: bool,
}

#[derive(PartialEq, Eq, Debug)]
enum ArgType {
    /// This arg is the cycles estimate arg
    Cycles,
    /// This arg is a flag, and does not have a value after it.
    NoValue,
    /// Get a value for the initial IP.
    InitIp,
    /// Get a value for the initial SP.
    InitSp,
    /// Get a value for exit after.
    ExitAfter,
    /// File to save memory image to
    DisplayFile,
}

const USAGE: &str = "Usage: computer-enhance <input> <output> [-h|--help] [OPTIONS]";
const HELP: &str = "
The Computer Enhance 8086 Decoder and Simulator

Required Parameters:
<input> : The input binary file containing 8086 binary code.
<output> : The output file to print decoded assembly to.

Options:

-e|--exec : If specified, simulate the input instruction stream in addition to
            decoding it.

-h|--help : Print this help message.

-v|--verbose : Increase verbosity of print to include debug information.

--no-ip : If specified, do NOT print out IP register info.

--overwrite : If specified, overwrite the output file instead of appending to it.

-c|--model-cycles {8086|8088} : If specified, estimate cycles during execution
                                for the given CPU (8086 or 8088).

--stop-on-ret : If specified, exit the simulator when instruction RET is hit.
                Useful for running functions without running code that calls
                into them.

--ip <value> : If specified, set the instruction pointer to begin with
this value.

--sp <value> : If specified, initialize the stack pointer to this value.

--display-window : If specified, graphically display final memory contents in a
                   window.

--display-file <file>: If specified, save final memory contents to the given
                       file. (To view, save the file with the .data extension,
                       load it into Gimp, set width and height to 64, and change
                       display format from RGB to RGBA.)

--exit-after <value> : Quit the program after <value> instructions. Useful for
                       limiting the execution of never-ending programs (like an
                       RTOS) for regressions.

--stop-on-int3 : If specified, exit the decoder when an int3 is encountered.
--screenshots : If specified, save screenshots of each rendered frame to a
                file.
";

fn print_help() {
    println!("{USAGE}");
    println!("{HELP}");
}

fn parse_arg_value(arg: String, arg_type: &ArgType, parsed_args: &mut ArgsType) -> Result<()> {
    // Parse a numeric value, if needed
    let num_val = match arg_type {
        ArgType::InitIp | ArgType::InitSp | ArgType::ExitAfter => {
            let val = if arg.starts_with("0x") || arg.starts_with("0X") {
                println!("Parsing {arg} as hex, skipping initial 0x");
                u64::from_str_radix(&arg[2..], 16)?
            } else if arg.ends_with("h") || arg.ends_with("H") {
                println!("Parsing {arg} as hex, skipping last h");
                u64::from_str_radix(&arg[..arg.len() - 1], 16)?
            } else {
                println!("Parsing {arg} as decimal");
                arg.parse()?
            };
            Some(val)
        }
        _ => None,
    };
    match (arg_type, num_val) {
        (ArgType::NoValue, _) => unreachable!(),
        (ArgType::Cycles, _) => {
            parsed_args.cycle_type = if arg == "8086" {
                Some(CpuType::Intel8086)
            } else if arg == "8088" {
                // default to 8086 cycle estimates
                Some(CpuType::Intel8088)
            } else {
                bail!("Unsupported value for -c|--model-cycles: {arg}")
            }
        }
        (ArgType::InitIp, Some(val)) => {
            println!("Initializing IP to {val} ({val:x})");
            parsed_args.init_ip = Some(val as u16);
        }
        (ArgType::InitSp, Some(val)) => {
            println!("Initializing SP to {val} ({val:x})");
            parsed_args.init_sp = Some(val as u16);
        }
        (ArgType::ExitAfter, Some(val)) => {
            println!("Exiting program after {val} instructions");
            parsed_args.exit_after = Some(val);
        }
        (ArgType::DisplayFile, _) => {
            parsed_args.display_file = Some(arg);
        }
        _ => bail!("Unhandled arg value for {arg_type:?} and {num_val:?}"),
    };
    Ok(())
}

/// Take a given arg and parse it as an optional argument. Modify parsed_args.
/// Return true if the next argument is a value for this argument.
fn parse_optional(arg: String, parsed_args: &mut ArgsType) -> Result<ArgType> {
    if arg.starts_with("-h") || arg.starts_with("--help") {
        parsed_args.help = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("-e") || arg.starts_with("--exec") {
        parsed_args.execute = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("-v") || arg.starts_with("--verbose") {
        parsed_args.verbose = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("--no-ip") {
        parsed_args.no_ip = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("--overwrite") {
        parsed_args.overwrite = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("-c") || arg.starts_with("--model-cycles") {
        // Get the value from the next arg iteration
        Ok(ArgType::Cycles)
    } else if arg.starts_with("--stop-on-ret") {
        parsed_args.stop_on_ret = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("--ip") {
        Ok(ArgType::InitIp)
    } else if arg.starts_with("--sp") {
        Ok(ArgType::InitSp)
    } else if arg.starts_with("--display-window") {
        parsed_args.display_window = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("--display-file") {
        Ok(ArgType::DisplayFile)
    } else if arg.starts_with("--exit-after") {
        Ok(ArgType::ExitAfter)
    } else if arg.starts_with("--stop-on-int3") {
        parsed_args.stop_on_int3 = true;
        Ok(ArgType::NoValue)
    } else if arg.starts_with("--screenshots") {
        parsed_args.screenshots = true;
        Ok(ArgType::NoValue)
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

    let mut get_arg_value = ArgType::NoValue;
    // Now parse args, excluding the first arg
    for arg in args {
        if parsed_args.first_arg.is_none() {
            parsed_args.first_arg = Some(arg);
        } else if get_arg_value != ArgType::NoValue {
            // This argument is a value for the previous argument
            parse_arg_value(arg, &get_arg_value, &mut parsed_args)?;
            // Reset to NoValue
            get_arg_value = ArgType::NoValue;
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

    let screenshots = args.screenshots;

    if args.display_window {
        let (send_to_gfx, recv_from_emu) = mpsc::channel();

        // Move emulation logic into separate thread
        let emulation_thread = std::thread::spawn(move || {
            // Don't move send_to_gfx, because we don't want it dropped when
            // this thread quits in case decode_simulate() finishes before the
            // graphics loop has a chance to pull anything out of it.
            match decode_simulate(args, Some(&send_to_gfx)) {
                Ok(_) => {}
                Err(e) => println!("ERROR: Emulation thread failed: {e}"),
            }
            println!("Exiting emulation thread...")
        });

        // Sit in graphics loop until user exits!
        graphics_loop(recv_from_emu, screenshots);
        // User has exited graphics loop - let's quit the program

        match emulation_thread.join() {
            Ok(_) => {}
            Err(e) => println!("ERROR: Emulation thread failed to join: {e:#?}"),
        }
        Ok(())
    } else {
        decode_simulate(args, None)
    }
}

/// The emulation thread that does decode and/or simulation
fn decode_simulate(args: ArgsType, send_to_gfx: Option<&mpsc::Sender<MemImage>>) -> Result<()> {
    println!("Executable: {}", args.first_arg.unwrap());
    // Make sure required args exist

    let (program_bytes, program_length) = file_to_byte_vec(&args.input_file, args.execute)?;
    let mut output_file = get_output_file_from_path(&args.output_file, args.overwrite)?;
    println!(
        "Decoding instructions from file '{}'...",
        args.input_file.unwrap()
    );
    println!(
        "Outputting decoded assembly to file '{}'...",
        args.output_file.as_ref().unwrap()
    );

    let decode_settings = DecodeSettings {
        verbose: args.verbose,
        stop_on_int3: args.stop_on_int3,
    };

    if args.execute {
        let cycle_lines = print_cycle_header(args.cycle_type);
        for line in cycle_lines {
            writeln!(output_file, "{}", line)?;
        }

        let execute_settings = ExecuteSettings {
            no_ip: args.no_ip,
            cycle_model: args.cycle_type,
            stop_on_ret: args.stop_on_ret,
            init_ip: args.init_ip,
            init_sp: args.init_sp,
            exit_after: args.exit_after,
        };
        let (text_lines, mut cpu_state) = decode_execute(
            program_bytes,
            program_length,
            &decode_settings,
            &execute_settings,
            send_to_gfx,
        );
        for line in text_lines {
            writeln!(output_file, "{}", line)?;
        }

        let final_state_lines = print_final_state(&cpu_state, args.no_ip);
        for line in &final_state_lines {
            writeln!(output_file, "{}", line)?;
        }

        match args.display_file {
            Some(file) => {
                println!("Saving memory image to file {file}...");
                memory_to_file(&mut cpu_state.memory, &file);
            }
            None => {}
        }
        match (args.display_window, &send_to_gfx) {
            (true, Some(send_to_gfx)) => {
                println!("Graphically displaying memory (64x65)...");
                let slice = &cpu_state.memory[..];
                let mut byte_vec = vec![0; slice.len()];
                byte_vec.copy_from_slice(slice);
                match send_to_gfx.send(MemImage {
                    bytes: byte_vec,
                    width: 64,
                    height: 65,
                    format: ImageFormat::DataAlpha,
                }) {
                    Ok(_) => println!("Sent final memory image to graphics loop"),
                    Err(e) => println!("ERROR: Failed to send image to graphics loop: {e}"),
                };
            }
            (true, None) => println!("ERROR: Can't display image: No channel to graphics loop"),
            (false, _) => {}
        }
    } else {
        let insts = decode(program_bytes, &decode_settings);
        // Print out instructions to the output file
        writeln!(output_file, "bits 16")?;
        for inst in insts {
            writeln!(output_file, "{}", inst.text.unwrap())?;
        }
    };

    Ok(())
}
