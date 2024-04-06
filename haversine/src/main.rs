//! Haversine distance processor
//!
//! This is for part 2 of the Computer Enhance programming course at
//! https://www.computerenhance.com/.

// Standard library imports
use std::env;
// External imports
use anyhow::Result;

// Internal imports
const DEFAULT_COUNT: u64 = 10_000_000;
fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut count = 0;
    for arg in &args[1..] {
        if count == 0 {
            count = arg.parse()?;
        }
    }
    if count == 0 {
        count = DEFAULT_COUNT;
    }

    println!("{{\"pairs\": [");
    for i in 0..count {
        let x0 = i as f64;
        let y0 = i as f64;
        let x1 = i as f64;
        let y1 = i as f64;
        let comma = if i == (count - 1) { "" } else { "," };
        println!("    {{\"x0\": {x0}, \"y0\": {y0}, \"x1\": {x1}, \"y1\": {y1}}}{comma}");
    }
    println!("]}}");

    Ok(())
}
