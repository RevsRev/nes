use std::{
    fmt,
    fs::File,
    io::Read,
    sync::{Arc, atomic::AtomicBool},
};

use crate::traits::mem::Mem;
use clap::Parser;
use io::render::frame::Frame;
use nes::NES;
use ppu::PPU;
use rand::Rng;
use rom::Rom;
use sdl2::{event::Event, keyboard::Keycode, pixels::PixelFormatEnum};

mod bus;
mod cpu;
mod io;
mod nes;
mod opp;
mod ppu;
mod rom;
mod traits;

#[macro_use]
extern crate lazy_static;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 't', long = "trace")]
    trace: bool,
    #[arg(short = 'd', long = "debug")]
    debug: bool,
    #[arg(short = 'f', long = "file")]
    file_path: Option<String>,
}

impl fmt::Display for Args {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "trace: {}, debug: {}", self.trace, self.debug)
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

    let mut frame = Frame::new();
    let halt = Arc::new(AtomicBool::new(false));
    let mut nes = NES::new(rom, halt, move |ppu: &PPU| {
        io::render::render(&mut frame, ppu);

        texture.update(None, &frame.data, 256 * 3).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                _ => { /* do nothing */ }
            }
        }
    });

    nes.set_tracing(args.trace);

    let mut rng = rand::thread_rng();

    let _ = nes.run_with_callback(move |cpu| {
        // handle_user_input(cpu, &mut event_pump);
        cpu.mem_write(0xFE, rng.gen_range(1, 16));

        // if args.debug {
        //     for event in event_pump.wait_iter() {
        //         if let Event::KeyDown {
        //             keycode: Some(Keycode::Return),
        //             ..
        //         } = event
        //         {
        //             return;
        //         }
        //
        //         if let Event::Quit { .. }
        //         | Event::KeyDown {
        //             keycode: Some(Keycode::Escape),
        //             ..
        //         } = event
        //         {
        //             std::process::exit(0)
        //         }
        //     }
        // }

        // if read_screen_state(cpu, &mut screen_state) {
        //     texture.update(None, &screen_state, 32 * 3).unwrap();
        //     canvas.copy(&texture, None, None).unwrap();
        //     canvas.present();
        // }

        // ::std::thread::sleep(std::time::Duration::new(0, 70_000));
    });
}
