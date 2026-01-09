use std::{cell::RefCell, rc::Rc};

use crate::{
    apu::{
        channel::{
            dmc::DmcChannel, noise::NoiseChannel, square::SquareChannel, triangle::TriangleChannel,
        },
        mixer::Mixer,
        registers::{frame::FrameCounter, status::Status},
    },
    interrupt::{Interrupt, InterruptImpl},
    traits::tick::Tick,
};

pub mod channel;
pub mod mixer;
pub mod registers;

pub struct APU {
    pub pulse_1: SquareChannel,
    pub pulse_2: SquareChannel,
    pub triangle: TriangleChannel,
    pub noise: NoiseChannel,
    pub dmc: DmcChannel,
    pub status: Status,
    frame: FrameCounter,
    interrupt: Rc<RefCell<InterruptImpl>>,
    mixer: Mixer,

    cpu_cycles: u8,
    sequencer_cycles: u16,
}

impl APU {
    pub fn new(interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        APU {
            pulse_1: SquareChannel::new(),
            pulse_2: SquareChannel::new(),
            triangle: TriangleChannel::new(),
            noise: NoiseChannel::new(),
            dmc: DmcChannel::new(),
            status: Status::new(),
            frame: FrameCounter::new(),
            interrupt: interrupt,
            mixer: Mixer::new(),
            cpu_cycles: 0,
            sequencer_cycles: 0,
        }
    }

    pub fn write_to_status(&mut self, data: u8) -> u8 {
        self.interrupt.borrow_mut().set_irq(false);
        self.status.write(data)
    }

    pub fn write_to_frame_counter(&mut self, data: u8) -> u8 {
        self.frame.write(data)
    }

    pub fn output(&self) -> f32 {
        self.mixer.output
    }

    pub fn read_status(&self) -> Result<u8, String> {
        let status_data = self.status.read();
        let pulse_1_expired = self.pulse_1.len_counter_expired();
        let pulse_2_expired = self.pulse_2.len_counter_expired();
        let triangle_expired = self.triangle.len_counter_expired();

        let pulse_1_flag = if pulse_1_expired {
            !0b0000_0001
        } else {
            0b1111_1111
        };
        let pulse_2_flag = if pulse_2_expired {
            !0b0000_0010
        } else {
            0b1111_1111
        };
        let triangle_flag = if triangle_expired {
            !0b0000_0100
        } else {
            0b1111_1111
        };

        status_data.map(|d| d & pulse_1_flag & pulse_2_flag & triangle_flag)
    }
}

impl Tick for APU {
    fn tick(&mut self, cycles: u8) {
        for c in 0..cycles {
            if (self.cpu_cycles.wrapping_add(c)) % 2 == 0 {
                let emit_clock = self.frame.clock();

                if emit_clock {
                    self.pulse_1.frame_clock();
                    self.pulse_2.frame_clock();
                    self.triangle.frame_clock();
                }

                self.pulse_1.decrement_timer();
                self.pulse_2.decrement_timer();
            }
            self.triangle.decrement_timer();
            self.mixer.output(
                self.pulse_1.get_out(),
                self.pulse_2.get_out(),
                self.triangle.get_out(),
            );
        }

        let num_apu_cycles = ((self.cpu_cycles % 2 + cycles) / 2) as u16;
        self.cpu_cycles = self.cpu_cycles.wrapping_add(cycles);
        self.sequencer_cycles = self.sequencer_cycles.wrapping_add(num_apu_cycles);
    }
}
