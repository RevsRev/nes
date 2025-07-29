use std::{
    fmt,
    fs::File,
    io::Read,
    sync::{Arc, atomic::AtomicBool},
};

use crate::traits::mem::Mem;
use clap::Parser;
use cpu::CPU;
use nes::NES;
use rand::Rng;
use render::{frame::Frame, show_tile};
use rom::Rom;
use sdl2::{
    EventPump,
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    sys::{KeyCode, SDL_Keycode},
};
use traits::bus::Bus;

mod bus;
mod cpu;
mod nes;
mod opp;
mod ppu;
mod render;
mod rom;
mod traits;

#[macro_use]
extern crate lazy_static;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'd', long = "debug")]
    debug: bool,
    #[arg(short = 'f', long = "file")]
    file_path: Option<String>,
}

impl fmt::Display for Args {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "debug: {}", if self.debug { "ON" } else { "OFF" })
    }
}

fn main() {
    let args = Args::parse();

    println!("Starting nes emulator with args {}", args);

    let file_path = match args.file_path {
        Some(f) => f,
        None => panic!("No program file provided to emulator"),
    };

    let mut file = match File::open(file_path) {
        Ok(f) => f,
        Err(e) => panic!("Could not open file {}", e),
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => panic!("Could not read file contents {}", e),
    };

    let rom = match Rom::new(&buffer) {
        Ok(r) => r,
        Err(s) => panic!("{}", s),
    };

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Tile Viewer", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();

    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    let mut bank_0_tile_frame = Frame::new();
    let mut bank_1_tile_frame = Frame::new();

    for i in 0..256 {
        show_tile(&mut bank_0_tile_frame, &rom.chr_rom, 0, i);
        show_tile(&mut bank_1_tile_frame, &rom.chr_rom, 1, i);
    }

    let mut combined_data =
        Vec::with_capacity(bank_0_tile_frame.data.len() + bank_1_tile_frame.data.len());
    combined_data.extend_from_slice(&bank_0_tile_frame.data[0..64 * 256 * 3].to_vec());
    combined_data.extend_from_slice(&bank_1_tile_frame.data[0..64 * 256 * 3].to_vec());

    texture.update(None, &combined_data, 256 * 3);
    canvas.copy(&texture, None, None).unwrap();
    canvas.present();

    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                _ => {}
            }
        }
    }

    // let halt = Arc::new(AtomicBool::new(false));
    // let mut nes = NES::new(rom, halt);
    // nes.set_debug(args.debug);
    //
    // let mut screen_state = [0 as u8; 32 * 3 * 32];
    // let mut rng = rand::thread_rng();
    //
    // let _ = nes.run_with_callback(move |cpu| {
    //     handle_user_input(cpu, &mut event_pump);
    //     cpu.mem_write(0xFE, rng.gen_range(1, 16));
    //
    //     if read_screen_state(cpu, &mut screen_state) {
    //         texture.update(None, &screen_state, 32 * 3).unwrap();
    //         canvas.copy(&texture, None, None).unwrap();
    //         canvas.present();
    //     }
    //
    //     ::std::thread::sleep(std::time::Duration::new(0, 70_000));
    // });
}

fn handle_user_input<T: Bus>(cpu: &mut CPU<T>, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),

            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => cpu.mem_write(0xFF, 0x77),

            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => cpu.mem_write(0xFF, 0x73),

            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => cpu.mem_write(0xFF, 0x61),

            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => cpu.mem_write(0xFF, 0x64),

            _ => {}
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
        1 => sdl2::pixels::Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => sdl2::pixels::Color::CYAN,
    }
}

fn read_screen_state<T: Bus>(cpu: &mut CPU<T>, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;

    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let (b1, b2, b3) = color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    return update;
}
