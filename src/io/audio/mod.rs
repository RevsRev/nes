use std::sync::{Arc, Mutex};

use crate::{apu::APU, io::audio::sound_frame::SoundFrame};

pub mod sound_frame;

pub fn sound(frame: &mut Arc<Mutex<SoundFrame>>, apu: &APU) {
    frame.lock().unwrap().add_output(apu.output());
}
