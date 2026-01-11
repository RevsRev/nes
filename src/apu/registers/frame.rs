use std::{cell::RefCell, rc::Rc};

use crate::{
    interrupt::{Interrupt, InterruptImpl},
    traits::tick::Tick,
};

const MODE: u8 = 0b1000_0000;
const INTERRUPT: u8 = 0b0100_0000;

pub struct FrameCounter {
    data: u8,
    apu_cycles: u32,
    written_during_cycle: bool,
    reset_timer_countdown: i8,
    clock: Option<FrameClock>,
    interrupt: Rc<RefCell<InterruptImpl>>,
}

pub enum FrameClock {
    QUARTER,
    HALF,
}

impl FrameCounter {
    pub fn new(interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        FrameCounter {
            data: 0,
            apu_cycles: 0,
            written_during_cycle: false,
            reset_timer_countdown: -1,
            clock: Option::None,
            interrupt: interrupt,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;

        if self.written_during_cycle {
            self.reset_timer_countdown = 3;
        } else {
            self.reset_timer_countdown = 4;
        }

        self.data = data;
        old_value
    }

    pub fn get_data(&self) -> u8 {
        self.data
    }

    pub fn emit_clock(&mut self) -> Option<FrameClock> {
        self.clock.take()
    }

    pub fn step(&mut self) -> bool {
        if self.data & MODE == MODE {
            self.five_step_clock()
        } else {
            self.four_step_clock()
        }
    }

    fn four_step_clock(&mut self) -> bool {
        if self.apu_cycles == 7456 || self.apu_cycles == 14914 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.apu_cycles == 3728 || self.apu_cycles == 11185 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        let irq_set = self.apu_cycles == 14914 && self.data & 0b1000_0000 == 0b0;

        self.apu_cycles = self.apu_cycles + 1;
        if self.apu_cycles > 14914 {
            self.apu_cycles = 0;
        }
        irq_set
    }

    fn five_step_clock(&mut self) -> bool {
        if self.apu_cycles == 7456 || self.apu_cycles == 18640 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.apu_cycles == 3728 || self.apu_cycles == 11185 || self.apu_cycles == 14914 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        self.apu_cycles = self.apu_cycles + 1;
        if self.apu_cycles > 18640 {
            self.apu_cycles = 0;
        }
        false
    }
}

impl Tick for FrameCounter {
    fn tick(&mut self, cycles: u8) {
        self.written_during_cycle = if cycles == 0 { true } else { false };

        if self.reset_timer_countdown == -1 {
            return;
        }
        if self.reset_timer_countdown == 0 {
            self.clock = Option::Some(FrameClock::HALF);
            self.apu_cycles = 0;
        }

        self.reset_timer_countdown = self.reset_timer_countdown - 1;
    }
}
