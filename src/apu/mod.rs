use crate::{
    apu::{
        channel::{
            dmc::DmcChannel, noise::NoiseChannel, square::SquareChannel, triangle::TriangleChannel,
        },
        registers::{frame::FrameCounter, status::Status},
    },
    traits::{mem::Mem, tick::Tick},
};

pub mod channel;
pub mod registers;

pub struct APU {
    pub pulse_1: SquareChannel,
    pub pulse_2: SquareChannel,
    pub triangle: TriangleChannel,
    pub noise: NoiseChannel,
    pub dmc: DmcChannel,
    status: Status,
    frame: FrameCounter,

    cycles: u16,
}

impl APU {
    pub fn new() -> Self {
        APU {
            pulse_1: SquareChannel::new(),
            pulse_2: SquareChannel::new(),
            triangle: TriangleChannel::new(),
            noise: NoiseChannel::new(),
            dmc: DmcChannel::new(),
            status: Status::new(),
            frame: FrameCounter::new(),
            cycles: 0,
        }
    }

    pub fn write_to_status(&mut self, data: u8) -> u8 {
        //TODO - This should clear the DMC interrupt flag
        self.status.write(data)
    }

    pub fn write_to_frame_counter(&mut self, data: u8) -> u8 {
        self.frame.write(data)
    }
}

impl Tick for APU {
    fn tick(&mut self, cycles: u8) {
        self.cycles.wrapping_add(1);

        if self.status.pulse_1_enabled() {
            self.pulse_1.decrement_timer();
        }

        if self.status.pulse_2_enabled() {
            self.pulse_2.decrement_timer();
        }

        self.triangle.decrement_timer();
    }
}
