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
use std::mem::take;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::sync::mpsc::{Receiver, TryRecvError};

use winit::dpi::{PhysicalSize, Size};
// Third-party imports
use winit::event::{ElementState, Event, KeyEvent, StartCause, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::keyboard::Key;
use winit::keyboard::NamedKey;
use winit::window::WindowBuilder;

use crate::GraphicsSettings;

/// How the image format lays out the data in bytes
pub enum ImageFormat {
    /// `RGBA` - 0: RR, 1: GG, 2: BB, 3: AA
    DataAlpha,
    /// `RGB` - 0: RR, 1: GG, 2: BB
    Data,
    /// `BGRA` - 0: BB, 1: GG, 2: RR, 3: AA
    /// This matches the SoftBuffer format, where it's a vector of u32s with
    /// the format of `0b00000000RRRRRRRRGGGGGGGGBBBBBBBB`.
    SoftBuffer,
}

/// This struct contains a u8 byte slice in addition to the width and height
/// of the image to display from the beginning of that byte slice
pub struct MemImage {
    pub bytes: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub format: ImageFormat,
}

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

/// Create a GPU-less graphics render loop that renders to a softbuffer-based
/// winit window. The graphics loop has a mpsc channel it polls for incoming
/// MemImage objects to render.
///
/// The output image is 4 bytes per pixel (RGBA). softbuffer expects 32 bits (4
/// bytes) per pixel, and the first byte is all 0's (no alpha channel).
pub fn graphics_loop(recv_from_emu: Receiver<MemImage>, gfx_settings: GraphicsSettings) {
    let event_loop = EventLoop::new().unwrap();
    // Customize properties of the window
    let window_builder =
        WindowBuilder::new()
            .with_title("Emu86rs")
            .with_inner_size(Size::Physical(PhysicalSize {
                width: 1024,
                height: 1024,
            }));
    let window = Rc::new(window_builder.build(&event_loop).unwrap());
    let context = softbuffer::Context::new(window.clone()).unwrap();
    let mut surface = softbuffer::Surface::new(&context, window.clone()).unwrap();

    // Start off assuming that we have a working reciever from the emulator code
    let mut emu_connected = true;

    // Set up splash screen
    let splash_screen = data_to_softbuffer(include_bytes!("splash-512x512.data"), true);
    let splash_width: u32 = 512;
    let splash_height: u32 = 512;

    // Remember scale state
    let mut scale_req = 0;
    let mut scale_max = 1;
    let mut mem_image: Option<MemImage> = None;

    let mut image_counter = 0;

    let result = event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);

        match event {
            Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                window_id,
            } if window_id == window.id() => {
                // Display the current frame buffer/memory image
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

                // Forward declare these to live long enough for slice
                let image_vec_u32: Vec<u32>;
                let image_vec_u8: Vec<u8>;

                let (memory_u32, image_width, image_height) = match &mut mem_image {
                    Some(x) => {
                        let image_vec = match x.format {
                            ImageFormat::SoftBuffer => {
                                // Use take to move bytes out of mem_image
                                image_vec_u8 = take(&mut x.bytes);
                                // Reinterpret u8 memory vector as a u32 memory slice
                                let memory_u32: &[u32] = unsafe {
                                    std::slice::from_raw_parts(
                                        image_vec_u8.as_ptr() as *const u32,
                                        image_vec_u8.len() / 4,
                                    )
                                };
                                memory_u32
                            }
                            ImageFormat::DataAlpha => {
                                image_vec_u8 = take(&mut x.bytes);
                                // Massage input bytes into a softbuffer copy
                                image_vec_u32 = data_to_softbuffer(&image_vec_u8[..], true);
                                &image_vec_u32[..]
                            }
                            ImageFormat::Data => {
                                image_vec_u8 = take(&mut x.bytes);
                                // Massage input bytes into a softbuffer copy
                                image_vec_u32 = data_to_softbuffer(&image_vec_u8[..], false);
                                &image_vec_u32[..]
                            }
                        };

                        (&image_vec[..], x.width, x.height)
                    }
                    None => {
                        println!("Displaying splash screen");
                        (&splash_screen[..], splash_width, splash_height)
                    }
                };

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

                if gfx_settings.screenshots {
                    // Save off a screenshot of the buffer on each render, for debugging
                    let screenshot_name = format!("graphics_loop_img{image_counter}.data");
                    println!("Saving screenshot: {screenshot_name}");
                    let screenshot_data = softbuffer_to_data(&buffer[..], true);
                    memory_to_file(&screenshot_data, &screenshot_name);
                    image_counter += 1;
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
            Event::NewEvents(StartCause::Poll) => {
                // Check to see if the emulator sent a new image to display
                if emu_connected {
                    match recv_from_emu.try_recv() {
                        Ok(item) => {
                            // We got something new to render!
                            mem_image = Some(item);
                            window.request_redraw();
                            // Automatically expand image as big as possible
                            scale_req = 0;
                        }
                        Err(TryRecvError::Empty) => {}
                        Err(TryRecvError::Disconnected) => {
                            println!("Graphics loop: Emulation thread apparently finished");
                            emu_connected = false;
                            // Don't exit - let user exit, so that there is time to
                            // see graphical output
                        }
                    }
                }
            }
            _ => {}
        }
    });
    match result {
        Ok(x) => println!("Finished graphical display loop: {x:?}"),
        Err(e) => eprintln!("ERROR: {e:?}"),
    }
}

/// Convert a .data image (with or without the alpha channel) into a
/// softbuffer-compatible image.
/// .data: RGB(A); softbuffer: BGR0
/// softbuffer interprets pixels as a u32 of form
/// 00000000RRRRRRRRGGGGGGGGBBBBBBBB, B being the LSB. So the R and B are
/// inverse of RGBA .data files, where R would be the LSB if mapped to a u32.
pub fn data_to_softbuffer(data_image: &[u8], alpha_channel: bool) -> Vec<u32> {
    let mut softbuffer_image: Vec<u32> = vec![];
    let chunk_size = if alpha_channel { 4 } else { 3 };

    for byte in data_image.chunks(chunk_size) {
        // Alpha channel gets discarded
        let pixel = ((byte[0] as u32) << 16) | ((byte[1] as u32) << 8) | (byte[2] as u32);
        softbuffer_image.push(pixel);
    }
    softbuffer_image
}

/// Convert a softbuffer image into a .data image (with optional alpha channel).
/// .data: RGB(A); softbuffer: BGR0
/// softbuffer interprets pixels as a u32 of form
/// 00000000RRRRRRRRGGGGGGGGBBBBBBBB, B being the LSB. So the R and B are
/// inverse of RGBA .data files, where R would be the LSB if mapped to a u32.
pub fn softbuffer_to_data(softbuffer_image: &[u32], alpha_channel: bool) -> Vec<u8> {
    let mut data_image: Vec<u8> = vec![];

    for pixel in softbuffer_image {
        // Set R
        data_image.push((*pixel >> 16) as u8);
        // Set G
        data_image.push((*pixel >> 8) as u8);
        // Set B
        data_image.push(*pixel as u8);
        if alpha_channel {
            // Set A. Since softbuffer images have no alpha, set to 100%
            data_image.push(0xFF);
        }
    }
    data_image
}

/// Create a softbuffer-compatible image
/// softbuffer interprets pixels as a u32 of form
/// 00000000RRRRRRRRGGGGGGGGBBBBBBBB, B being the LSB. So the R and B are
/// inverse of RGBA .data files, where R would be the LSB if mapped to a u32.
pub fn create_rgb_sb_image(red: u8, green: u8, blue: u8, width: u64, height: u64) -> Vec<u32> {
    let mut image: Vec<u32> = vec![];
    for _i in 0..width * height {
        // softbuffer interprets pixels as a u32 with 0RGB, B being the LSB
        // So the R and B are inverse of RGBA .data files, where R would be the
        // LSB if mapped to a u32.
        let pixel = ((red as u32) << 16) | ((green as u32) << 8) | (blue as u32);
        image.push(pixel);
    }
    image
}

/// Create an all-red 512x512 pixel image, immediately usable for softbuffer.
pub fn create_red_sb_image() -> Vec<u32> {
    create_rgb_sb_image(0xff, 0, 0, 512, 512)
}

/// Create an all-green 512x512 pixel image, immediately usable for softbuffer.
pub fn create_green_sb_image() -> Vec<u32> {
    create_rgb_sb_image(0, 0xff, 0, 512, 512)
}

/// Create an all-blue 512x512 pixel image, immediately usable for softbuffer.
pub fn create_blue_sb_image() -> Vec<u32> {
    create_rgb_sb_image(0, 0, 0xff, 512, 512)
}
