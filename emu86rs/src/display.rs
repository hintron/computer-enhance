//! This module implements any display-related code
//!
//! Windows Subsystem for Linux 2 (WSL2) now supports displaying Linux GUI
//! applications natively on the Windows desktop. And it seems that this works
//! with softbuffer and winit (it's buggy, but at least it works, while minifb
//! didn't seem to work).
//! https://learn.microsoft.com/en-us/windows/wsl/tutorials/gui-apps
//!

use std::cmp;
use std::fs::File;
use std::io::Write;
use std::num::NonZeroU32;
use std::rc::Rc;

// Third-party imports
use winit::event::{ElementState, Event, KeyEvent, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::keyboard::Key;
use winit::keyboard::NamedKey;
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

/// Display an image from the given byte slice in a graphical window.
///
/// The output image is 4 bytes per pixel (RGBA). softbuffer expects 32 bits (4
/// bytes) per pixel, and the first byte is all 0's (no alpha channel).
///
/// Run this on files/build-simulate-ip-regress/listing_0054_draw_rectangle
/// and files/build-simulate-ip-regress/listing_0055_challenge_rectangle
pub fn display_memory(memory: &[u8], image_width: u32, image_height: u32) {
    let event_loop = EventLoop::new().unwrap();
    // Customize properties of the window
    let window_builder =
        WindowBuilder::new().with_title("Memory Display - Computer Enhance Simulator");
    let window = Rc::new(window_builder.build(&event_loop).unwrap());
    let context = softbuffer::Context::new(window.clone()).unwrap();
    let mut surface = softbuffer::Surface::new(&context, window.clone()).unwrap();

    // Reinterpret u8 memory vector as a u32 memory slice
    let memory_u32: &[u32] =
        unsafe { std::slice::from_raw_parts(memory.as_ptr() as *const u32, memory.len() / 4) };

    // Remember scale state
    let mut scale_req = 0;
    let mut scale_max = 1;

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
                let mem_len = memory_u32.len();
                // Advance index of 8086 memory separately from surface index
                let mut mem_index = 0;
                // Copy contents of memory into buffer here!
                // MGH TODO: Try branchless programming to remove if
                let mut row: u32 = 0;
                let mut index = 0;
                scale_max = cmp::min(width / image_width, height / image_height);
                let scale = if scale_req == 0 || scale_req > scale_max {
                    println!("Max scale: {scale_max}");
                    scale_req = scale_max;
                    scale_max
                } else {
                    println!("Req scale: {scale_req}");
                    scale_req
                };
                let width_scaled = image_width * scale;
                let height_scaled = image_height * scale;
                let mut row_repeat = scale;
                while index < (width * height) {
                    let column = index % width;
                    if column == 0 && index > 0 && row < height_scaled {
                        row += 1;
                        if mem_index < mem_len && row_repeat > 1 {
                            // Print the row over again, to scale it vertically
                            row_repeat -= 1;
                            mem_index -= image_width as usize;
                        } else {
                            // Reset row repeat
                            row_repeat = scale;
                        }
                    }
                    // Only get mem val if we are in correct column and there
                    // are mem bytes still to display
                    if mem_index < mem_len && column < width_scaled && row < height_scaled {
                        let val = memory_u32[mem_index];
                        mem_index += 1;
                        for i in 0..scale {
                            buffer[(index + i) as usize] = val;
                        }
                        index += scale;
                    } else {
                        // Fill empty space with design from softbuffer readme
                        let y = index / width;
                        let x = index % width;
                        let red = x % 255;
                        let green = y % 255;
                        let blue = (x * y) % 255;
                        let val = blue | (green << 8) | (red << 16);
                        buffer[index as usize] = val;
                        index += 1;
                    };
                }

                buffer.present().unwrap();
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => {
                elwt.exit();
            }
            Event::WindowEvent {
                event:
                    WindowEvent::KeyboardInput {
                        event:
                            KeyEvent {
                                logical_key: key,
                                state: ElementState::Pressed,
                                ..
                            },
                        ..
                    },
                window_id,
            } if window_id == window.id() => {
                let mut increase_scale = false;
                let mut decrease_scale = false;
                match key {
                    Key::Named(NamedKey::Escape) => {
                        println!("Escape key pressed! Exiting...");
                        elwt.exit();
                    }
                    Key::Named(NamedKey::ArrowUp) => increase_scale = true,
                    Key::Named(NamedKey::ArrowDown) => decrease_scale = true,
                    Key::Character(char) => {
                        if char.starts_with("+") {
                            increase_scale = true;
                        } else if char.starts_with("-") {
                            decrease_scale = true;
                        }
                    }
                    other => println!("{other:#?}"),
                }
                if increase_scale && scale_req < scale_max {
                    scale_req += 1;
                    window.request_redraw();
                    println!("Increasing requested scale to: {scale_req}");
                }
                if decrease_scale && scale_req > 1 {
                    scale_req -= 1;
                    window.request_redraw();
                    println!("Decreasing requested scale to: {scale_req}");
                }
            }
            event => println!("Window event: {event:?}"),
        }
    });
    match result {
        Ok(x) => println!("Finished displaying memory : {x:?}"),
        Err(e) => eprintln!("ERROR: {e:?}"),
    }
}
