//! # 8086 Decoder and Simulator - Executable

// External imports
use anyhow::Result;
use std::io::Write;
use std::sync::mpsc;
use std::time::Instant;

// Internal imports
use emu86rs::cycles::print_cycle_header;
use emu86rs::decode::{decode, decode_execute};
use emu86rs::display::{graphics_loop, memory_to_file, ImageFormat, MemImage};
use emu86rs::execute::print_final_state;
use emu86rs::settings::{
    args_to_settings, parse_args, print_help, DecodeSettings, ExecuteSettings, MainSettings,
};
use emu86rs::{file_to_byte_vec, get_output_file_from_path};

fn main() -> Result<()> {
    let time_program_started = Instant::now();

    // Parse args. Fail if incorrect args are given
    let args = parse_args()?;

    // Now, *process* parsed args
    if args.help {
        print_help();
        return Ok(());
    }

    let (main_settings, decode_settings, execute_settings, gfx_settings) = args_to_settings(args);
    let time_args_parsed = Instant::now();

    let result = if main_settings.display_window {
        let (send_to_gfx, recv_from_emu) = mpsc::channel();

        // Move emulation logic into separate thread
        let emulation_thread = std::thread::spawn(move || {
            // Don't move send_to_gfx, because we don't want it dropped when
            // this thread quits in case decode_simulate() finishes before the
            // graphics loop has a chance to pull anything out of it.
            match decode_simulate(
                main_settings,
                decode_settings,
                execute_settings,
                Some(&send_to_gfx),
            ) {
                Ok(_) => {}
                Err(e) => println!("ERROR: Emulation thread failed: {e}"),
            }
            println!("Exiting emulation thread...")
        });

        // Sit in graphics loop until user exits!
        graphics_loop(recv_from_emu, gfx_settings);
        // User has exited graphics loop - let's quit the program

        match emulation_thread.join() {
            Ok(_) => {}
            Err(e) => println!("ERROR: Emulation thread failed to join: {e:#?}"),
        }
        Ok(())
    } else {
        decode_simulate(main_settings, decode_settings, execute_settings, None)
    };

    let time_program_ended = Instant::now();
    let duration_program = time_program_ended.duration_since(time_program_started);
    let duration_arg_parse = time_args_parsed.duration_since(time_program_started);

    println!(
        "Program execution time: {} ms",
        duration_program.as_millis()
    );
    println!(
        "(Arg parse execution time: {} us)",
        duration_arg_parse.as_micros()
    );

    result
}

/// The emulation thread that does decode and/or simulation
fn decode_simulate(
    main_settings: MainSettings,
    decode_settings: DecodeSettings,
    execute_settings: ExecuteSettings,
    send_to_gfx: Option<&mpsc::Sender<MemImage>>,
) -> Result<()> {
    // Make sure required args exist
    println!("Executable: {}", main_settings.first_arg.unwrap());

    let (program_bytes, program_length) =
        file_to_byte_vec(&main_settings.input_file, main_settings.execute)?;
    let mut output_file =
        get_output_file_from_path(&main_settings.output_file, main_settings.overwrite)?;
    println!(
        "Decoding instructions from file '{}'...",
        main_settings.input_file.unwrap()
    );
    println!(
        "Outputting decoded assembly to file '{}'...",
        main_settings.output_file.as_ref().unwrap()
    );

    if main_settings.execute {
        let cycle_lines = print_cycle_header(main_settings.cycle_type);
        for line in cycle_lines {
            writeln!(output_file, "{}", line)?;
        }

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

        let final_state_lines = print_final_state(&cpu_state, main_settings.no_ip);
        for line in &final_state_lines {
            writeln!(output_file, "{}", line)?;
        }

        match main_settings.display_file {
            Some(file) => {
                println!("Saving memory image to file {file}...");
                memory_to_file(&mut cpu_state.memory, &file);
            }
            None => {}
        }
        match (main_settings.display_window, &send_to_gfx) {
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
