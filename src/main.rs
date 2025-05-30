use minifb::{Key, Window, WindowOptions};
use std::time::{Duration, Instant};
use std::thread::sleep;

mod constant;
mod instruction;
mod interrupt;
mod memory;
mod ppu;
mod register;
mod utils;
mod cpu;
use cpu::Cpu;

use constant::*;

const CLOCK: u64 = 4194304;
const CYCLES_PER_SCREEN: u64 = 70224;
const FPS: f32 = CLOCK as f32 / CYCLES_PER_SCREEN as f32;
const MICROS_PER_FRAME: Duration = Duration::from_micros((1000.0 * 1000.0 / FPS) as u64);

fn run(mut cpu: Cpu, mut window: Window, mut tileset_window: Window, mut background_window: Window) {
    let mut window_buffer = vec![0; SCREEN_SIZE];
    let mut tileset_window_buffer = vec![0; TILESET_SIZE];
    let mut background_window_buffer = vec![0; BG_SIZE];

    while window.is_open() && tileset_window.is_open() &&
            !window.is_key_down(Key::Escape) && !tileset_window.is_key_down(Key::Escape) {

        // 17 556 clocks (70 224 cycles) per frame. Equals 59.7 Hz refresh rate
        let start_time = Instant::now();
        cpu.run();
        let finish_time = Instant::now();
        let diff = finish_time - start_time;

        // Sleep if cpu was faster than the gameboy expects
        if MICROS_PER_FRAME > diff {
            sleep(MICROS_PER_FRAME - diff);
        }

        // Update screen buffer
        for (i, pixel) in cpu.ppu.get_screen_buffer().iter().enumerate() {
            window_buffer[i] = *pixel as u32;
        }

        // Update tileset buffer
        for (i, pixel) in cpu.ppu.get_tileset_buffer().iter().enumerate() {
            tileset_window_buffer[i] = *pixel as u32;
        }

        // Update background buffer
        for (i, pixel) in cpu.ppu.get_background_buffer().iter().enumerate() {
            background_window_buffer[i] = *pixel as u32;
        }

        window.update_with_buffer(window_buffer.as_slice(), WIDTH, HEIGHT).unwrap();
        tileset_window.update_with_buffer(tileset_window_buffer.as_slice(), TILESET_WIDTH, TILESET_HEIGHT).unwrap();
        background_window.update_with_buffer(background_window_buffer.as_slice(), BG_WIDTH, BG_HEIGHT).unwrap();
    }
}

fn main() {
    let mut window = Window::new(
        "Gameboy",
        WIDTH,
        HEIGHT,
        WindowOptions::default()
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let mut tileset_window = Window::new(
        "Tileset",
        TILESET_WIDTH,
        TILESET_HEIGHT,
        WindowOptions::default()
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let mut background_window = Window::new(
        "Background",
        BG_WIDTH,
        BG_HEIGHT,
        WindowOptions::default()
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });


    // gameboy framerate is 59.727500569606 Hz
    window.limit_update_rate(Some(std::time::Duration::from_micros(1667)));
    tileset_window.limit_update_rate(Some(std::time::Duration::from_micros(1667)));
    background_window.limit_update_rate(Some(std::time::Duration::from_micros(1667)));

    let mut cpu = Cpu::with_rom("tests/blargg/01-special.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/02-interrupts.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/03-op sp,hl.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/04-op r,imm.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/05-op rp.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/06-ld r,r.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/07-jr,jp,call,ret,rst.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/08-misc instrs.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/09-op r,r.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/10-bit ops.gb");
    // let mut cpu = Cpu::with_rom("tests/blargg/11-op a,(hl).gb");
    cpu.bus.read_cartridge_header();
    cpu.log_state();

    run(cpu, window, tileset_window, background_window);
}
