use std::{
    collections::HashMap,
    fmt::{self, Debug},
    fs::File,
    io::Read,
    sync::{Arc, Mutex, atomic::AtomicBool},
};

use clap::Parser;
use cpal::{
    SampleFormat,
    traits::{DeviceTrait, HostTrait, StreamTrait},
};
use io::{
    joypad::{BUTTON_A, BUTTON_B, DOWN, Joypad, LEFT, RIGHT, SELECT, START, UP},
    render::frame::Frame,
};
use nes::NES;
use ppu::PPU;
use rom::Rom;
use sdl2::{event::Event, keyboard::Keycode, pixels::PixelFormatEnum};

use crate::{apu::APU, io::audio::sound_frame::SoundFrame};

mod apu;
mod bus;
mod cpu;
mod interrupt;
mod io;
mod nes;
mod opp;
mod ppu;
mod rom;
mod trace;
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
    let mut event_loop_sound_frame = Arc::new(Mutex::new(SoundFrame::new()));
    let audio_sound_frame = event_loop_sound_frame.clone();
    let halt = Arc::new(AtomicBool::new(false));
    let mut nes = NES::new(
        rom,
        halt,
        move |ppu: &PPU, apu: &APU, joypad: &mut Joypad| {
            io::render::render(&mut frame, ppu);
            io::audio::sound(&mut event_loop_sound_frame, apu);

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
                    Event::KeyDown { keycode, .. } => {
                        if let Some(key) = KEY_MAP.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_pressed_status(*key, true);
                        }
                    }
                    Event::KeyUp { keycode, .. } => {
                        if let Some(key) = KEY_MAP.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_pressed_status(*key, false);
                        }
                    }
                    _ => { /* do nothing */ }
                }
            }
        },
    );

    nes.set_tracing(args.trace);

    let host = cpal::default_host();

    let device = host
        .default_output_device()
        .expect("no output device available");

    let mut supported_configs_range = device
        .supported_output_configs()
        .expect("error while querying configs");
    let supported_config = supported_configs_range
        .next()
        .expect("no supported config?!")
        .with_max_sample_rate();

    let sample_format = supported_config.sample_format();
    let config = supported_config.into();
    let stream = match sample_format {
        SampleFormat::U8 => device.build_output_stream(
            &config,
            move |data: &mut [f32], _| {
                for sample in data {
                    *sample = audio_sound_frame.lock().unwrap().output;
                }
            },
            move |err| eprintln!("stream error: {err}"),
            None,
        ),
        sample_format => panic!("Unsupported sample format '{sample_format}'"),
    }
    .unwrap();

    stream.play().unwrap();

    let result = nes.run_with_callback(move |_| {});

    match result {
        Ok(_) => {}
        Err(s) => println!("{}", s),
    }
}

lazy_static! {
    pub static ref KEY_MAP: HashMap<Keycode, u8> = {
        let mut map = HashMap::new();
        map.insert(Keycode::Down, DOWN);
        map.insert(Keycode::Up, UP);
        map.insert(Keycode::Right, RIGHT);
        map.insert(Keycode::Left, LEFT);
        map.insert(Keycode::Space, SELECT);
        map.insert(Keycode::Return, START);
        map.insert(Keycode::A, BUTTON_A);
        map.insert(Keycode::S, BUTTON_B);
        map
    };
}
