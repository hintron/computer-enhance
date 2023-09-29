//! # 8086 Decoder and Simulator - Library
//!
//! This is an 8086 instruction decoder and simulator, implemented by following
//! the [Computer Enhance][1] performance awareness course.
//!
//! [1]: https://www.computerenhance.com/.
//!
// Define the modules in this library
pub mod decode;
pub mod execute;
#[cfg(test)]
mod tests;
