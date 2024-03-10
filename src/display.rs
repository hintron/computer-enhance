//! This module implements any display-related code
//!
//! Windows Subsystem for Linux 2 (WSL2) now supports displaying Linux GUI
//! applications natively on the Windows desktop. And it seems that this works
//! with softbuffer and winit (it's buggy, but at least it works, while minifb
//! didn't seem to work).
//! https://learn.microsoft.com/en-us/windows/wsl/tutorials/gui-apps
//!

use std::fs::File;
use std::io::Write;
use std::num::NonZeroU32;
use std::rc::Rc;

// Third-party imports
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

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
/// The output image is 4 bytes per pixel (RGBA). softbuffer expects 32 bits (4
/// bytes) per pixel, and the first byte is all 0's (no alpha channel).
///
/// Run this on files/build-simulate-ip-regress/listing_0054_draw_rectangle
/// and files/build-simulate-ip-regress/listing_0055_challenge_rectangle
pub fn display_memory(memory: &mut Vec<u8>) {
    // Image dimensions
    const MEM_IMAGE_WIDTH: u32 = 64;
    const _MEM_IMAGE_HEIGHT: u32 = MEM_IMAGE_WIDTH;
    const _MEM_IMAGE_SCALE: u32 = 10;

    let event_loop = EventLoop::new().unwrap();
    // Customize properties of the window
    let window_builder =
        WindowBuilder::new().with_title("Memory Display - Computer Enhance Simulator");
    let window = Rc::new(window_builder.build(&event_loop).unwrap());
    let context = softbuffer::Context::new(window.clone()).unwrap();
    let mut surface = softbuffer::Surface::new(&context, window.clone()).unwrap();

    // Reinterpret u8 memory vector as a u32 memory slice
    let memory_u32: &mut [u32] = unsafe {
        std::slice::from_raw_parts_mut(memory.as_mut_ptr() as *mut u32, memory.len() / 4)
    };

    let result = event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Wait);

        match event {
            Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                window_id,
            } if window_id == window.id() => {
                let (width, height) = {
                    let size = window.inner_size();
                    (size.width, size.height)
                };
                surface
                    .resize(
                        NonZeroU32::new(width).unwrap(),
                        NonZeroU32::new(height).unwrap(),
                    )
                    .unwrap();

                let mut buffer = surface.buffer_mut().unwrap();
                println!("memory_u32 len: {}", memory_u32.len());
                println!("buffer len: {}", buffer.len());
                let mem_len = memory_u32.len() as u32;
                // Advance index of 8086 memory separately from surface index
                let mut mem_index = 0;
                // Copy contents of memory into buffer here!
                // MGH TODO: Try branchless programming to remove if
                for index in 0..(width * height) {
                    let column = index % width;
                    // Only get mem val if we are in correct column and there
                    // are mem bytes still to display
                    let val = if mem_index < mem_len && column < MEM_IMAGE_WIDTH {
                        let val = memory_u32[mem_index as usize];
                        mem_index += 1;
                        val
                    } else {
                        // Fill empty space with design from softbuffer readme
                        let y = index / width;
                        let x = index % width;
                        let red = x % 255;
                        let green = y % 255;
                        let blue = (x * y) % 255;
                        blue | (green << 8) | (red << 16)
                    };
                    buffer[index as usize] = val;
                }

                buffer.present().unwrap();
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => {
                elwt.exit();
            }
            event => println!("Window event: {event:?}"),
        }
    });
    match result {
        Ok(x) => println!("Finished displaying memory : {x:?}"),
        Err(e) => eprintln!("ERROR: {e:?}"),
    }
}
