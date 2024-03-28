//! Haversine distance processor
//!
//! This is for part 2 of the Computer Enhance programming course at
//! https://www.computerenhance.com/.

// Standard library imports
// External imports
// Internal imports

fn main() {
    println!("{{\"pairs\": [");
    let max = 10;
    // let max = 10_000;
    // max = 10_000_000;
    for i in 0..max {
        let x0 = i as f64;
        let y0 = i as f64;
        let x1 = i as f64;
        let y1 = i as f64;
        let comma = if i == (max - 1) { "" } else { "," };
        println!("    {{\"x0\": {x0}, \"y0\": {y0}, \"x1\": {x1}, \"y1\": {y1}}}{comma}");
    }
    println!("]}}");
}
