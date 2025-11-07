use std::{cell::RefCell, rc::Rc};

use crate::{
    apu::{
        channel::{
            dmc::DmcChannel, noise::NoiseChannel, square::SquareChannel, triangle::TriangleChannel,
        },
        registers::{frame::FrameCounter, status::Status},
    },
    interrupt::{Interrupt, InterruptImpl},
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
    interrupt: Rc<RefCell<InterruptImpl>>,

    cpu_cycles: u16,
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
}

impl Tick for APU {
    fn tick(&mut self, cycles: u8) {
        self.cpu_cycles.wrapping_add(1);

        //APU ticks at the half the speed of the cpu, which is what drives this clock
        if self.cpu_cycles % 2 == 0 {
            return;
        }

        if self.status.pulse_1_enabled() {
            self.pulse_1.decrement_timer();
        }

        if self.status.pulse_2_enabled() {
            self.pulse_2.decrement_timer();
        }

        if self.status.triangle_enabled() {
            self.triangle.decrement_timer();
        }

        self.sequencer_cycles.wrapping_add(1);
    }
}
