//! Haversine distance processor
//!
//! This is for part 2 of the Computer Enhance programming course at
//! https://www.computerenhance.com/.

// Standard library imports
use std::env;
use std::fs::OpenOptions;
use std::io::Write;

// External imports
use anyhow::Result;
// Internal imports

const DEFAULT_COUNT: u64 = 10_000_000;

/// Create a JSON file of Haversine pairs.
/// first arg: # of pairs to generate (10 million if not specified)
/// second arg: output file (stdout if not specified)
fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut count = 0;
    let mut output_file = None;

    // Parse args
    for arg in &args[1..] {
        if count == 0 {
            count = arg.parse()?;
        } else if output_file.is_none() {
            output_file = Some(arg);
        }
    }

    // Handle arg values
    if count == 0 {
        count = DEFAULT_COUNT;
    }
    let output_file_handle = match output_file {
        Some(file) => {
            let mut file_options = OpenOptions::new();
            file_options.write(true).create(true);
            file_options.truncate(true);
            Some(file_options.open(file)?)
        }
        None => None,
    };

    // TODO: Try ArrayVec or ArrayString, to avoid heap to see if faster
    // TODO: Try streaming in chunks to avoid needing a huge buffer
    // TODO: Push bytes instead of formatted strings!
    let mut output_buffer = String::new();
    output_buffer.push_str("{\"pairs\": [\n");
    for i in 0..count {
        let x0 = i as f64;
        let y0 = i as f64;
        let x1 = i as f64;
        let y1 = i as f64;
        let comma = if i == (count - 1) { "" } else { "," };
        output_buffer.push_str(&format!(
            "    {{\"x0\": {x0}, \"y0\": {y0}, \"x1\": {x1}, \"y1\": {y1}}}{comma}\n"
        ));
    }
    output_buffer.push_str("]}\n");

    match output_file_handle {
        Some(mut file) => file.write_all(output_buffer.as_bytes())?,
        None => print!("{}", output_buffer),
    }

    Ok(())
}
