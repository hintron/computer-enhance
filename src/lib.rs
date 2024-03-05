//! # 8086 Decoder and Simulator - Library
//!
//! This is an 8086 instruction decoder and simulator, implemented by following
//! the [Computer Enhance][1] performance awareness course.
//!
//! [1]: https://www.computerenhance.com/.
//!
// Define the modules in this library
pub mod cycles;
pub mod decode;
pub mod display;
pub mod execute;
#[cfg(test)]
mod tests;

// Imports
use anyhow::Result;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;

use execute::MEMORY_SIZE;

/// Takes in a file path string and returns a byte vector containing the
/// entire contents of the file. If we are executing, the byte vector is
/// expanded to be 1 MB. Return the byte vector as well as the length of the
/// code, so we know when we run off the end in simple code snippets.
pub fn file_to_byte_vec(input_path: &Option<String>, exec: bool) -> Result<(Vec<u8>, u64)> {
    // Make sure required args exist
    let mut input_file = match input_path {
        Some(file) => {
            // Get the instruction stream from a file.
            File::open(file)?
        }
        _ => unreachable!(),
    };

    let mut inst_stream: Vec<u8> = vec![];
    input_file.read_to_end(&mut inst_stream)?;

    let original_program_size = inst_stream.len() as u64;

    // If we are executing, then make sure this vector is 1 MB so it can act as
    // the program's memory.
    if exec {
        inst_stream.resize(MEMORY_SIZE, 0)
    };

    println!("Original program bytes length: {original_program_size}");
    println!("Current program bytes length: {}", inst_stream.len());

    Ok((inst_stream, original_program_size))
}

/// Takes in an output file path string and returns a File handle
pub fn get_output_file_from_path(output_path: &Option<String>, overwrite: bool) -> Result<File> {
    let output_file = match output_path {
        Some(file) => {
            let mut file_options = OpenOptions::new();
            file_options.write(true).create(true);
            if overwrite {
                file_options.truncate(true);
            } else {
                file_options.append(true);
            }
            file_options.open(file)?
        }
        _ => unreachable!(),
    };
    Ok(output_file)
}
