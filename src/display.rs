//! This module implements any display-related code

use std::fs::File;
use std::io::Write;

// Third-party imports
use minifb::{Window, WindowOptions};

/// Write the CPU memory array to a file.
pub fn memory_to_file(memory: &Vec<u8>, output_file: &str) {
    let mut file =
        File::create(output_file).expect(&format!("Failed to create file {output_file}"));
    let all_zeros = memory.iter().all(|&x| x == 0);
    if all_zeros {
        file.write_all("Memory was empty".as_bytes())
            .expect("Failed to write to file");
    } else {
        file.write_all(memory).expect("Failed to write to file");
    }
    file.flush().expect("Failed to flush file");
}

/// Display a 64x64 image from program memory, starting at location 0
/// The output image is 4 bytes per pixel (RGBA). minifb expects 32 bits (4
/// bytes) per pixel.
pub fn display_memory(memory: &mut Vec<u8>) {
    // Image dimensions
    const WIDTH: usize = 64;
    const HEIGHT: usize = 64;

    let window_result = Window::new(
        "Memory Display - Computer Enhance Simulator",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    );
    let mut window = match window_result {
        Ok(x) => x,
        Err(e) => {
            println!("ERROR: Unable to display memory in window: {e}");
            return;
        }
    };

    // Ensure that the length memory is divisible by 4
    assert_eq!(memory.len() % 4, 0);

    // Reinterpret u8 memory vector as a u32 memory slice
    let memory_u32: &mut [u32] = unsafe {
        std::slice::from_raw_parts_mut(memory.as_mut_ptr() as *mut u32, memory.len() / 4)
    };

    let mut count: u64 = 0;
    let mut count2: u64 = 0;
    let mut update = true;
    // Main loop: draw the image until the window is closed
    while window.is_open() && !window.is_key_down(minifb::Key::Escape) {
        let (tmp, overflowed) = count.overflowing_add(1);
        count = tmp;
        if overflowed {
            println!("Poll {count2}");
            count2 += 1;
        }
        if !update {
            // Update the window with the image data
            window
                .update_with_buffer(&memory_u32[..], WIDTH, HEIGHT)
                .unwrap();
            update = false;
        }
    }
}
