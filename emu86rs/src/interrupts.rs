//! Interrupts
//!
//! This module defines all things related to interrupts

/// External interrupts that the CPU receives. Some interrupts are virtual, like
/// the Snake game interrupts, and some represent actual hardware interrupts
/// (coming soon).
pub enum InterruptType {
    // Tell the emulator to exit
    Exit,
    // Snake game key down
    SnakeDown,
    // Snake game key up
    SnakeUp,
    // Snake game key left
    SnakeLeft,
    // Snake game key right
    SnakeRight,
    // Snake game menu key
    SnakeMenu,
}
