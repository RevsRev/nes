use std::{cell::RefCell, rc::Rc};

use crate::{
    interrupt::{Interrupt, InterruptImpl},
    trace::FrameTrace,
    traits::tick::Tick,
};

const MODE: u8 = 0b1000_0000;
const INTERRUPT: u8 = 0b0100_0000;

pub struct FrameCounter {
    data: u8,
    apu_cycles: u32,
    written_during_cycle: bool,
    reset_timer_countdown: u64,
    clock: Option<FrameClock>,
    interrupt: Rc<RefCell<InterruptImpl>>,
    irq_flag: bool,

    apu_ticks: u64,
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
            reset_timer_countdown: 0,
            clock: Option::None,
            interrupt: interrupt,
            irq_flag: false,
            apu_ticks: 0,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;

        if data & 0b0100_0000 == 0b0100_0000 {
            self.set_irq_flag(false);
        }

        self.reset_timer_countdown = self.apu_ticks + 1;

        self.data = data;
        old_value
    }

    pub fn get_data(&self) -> u8 {
        self.data
    }

    pub fn set_irq_flag(&mut self, flag: bool) -> bool {
        let retval = self.irq_flag;
        self.irq_flag = flag;
        self.interrupt.borrow_mut().set_irq(self.irq_flag);
        retval
    }

    pub fn emit_clock(&mut self) -> Option<FrameClock> {
        self.clock.take()
    }

    pub fn step(&mut self) {
        self.apu_ticks = self.apu_ticks + 1;
        if self.apu_ticks == self.reset_timer_countdown {
            if self.data & MODE == MODE {
                self.clock = Option::Some(FrameClock::HALF);
            }
            self.apu_cycles = 0;
        }

        if self.data & MODE == MODE {
            self.five_step_clock()
        } else {
            self.four_step_clock()
        }
    }

    fn four_step_clock(&mut self) {
        self.apu_cycles = self.apu_cycles + 1;
        if self.apu_cycles > 14914 {
            self.apu_cycles = 0;
        }

        if self.apu_cycles == 7456 || self.apu_cycles == 14914 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.apu_cycles == 3728 || self.apu_cycles == 11185 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        if self.apu_cycles == 14914 && self.data & 0b0100_0000 == 0b0 {
            self.set_irq_flag(true);
        }
    }

    fn five_step_clock(&mut self) {
        if self.apu_cycles == 7456 || self.apu_cycles == 18640 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.apu_cycles == 3728 || self.apu_cycles == 11185 || self.apu_cycles == 14914 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        self.apu_cycles = self.apu_cycles + 1;
        if self.apu_cycles > 18640 {
            self.apu_cycles = 0;
        }
    }

    pub fn trace(&self) -> FrameTrace {
        FrameTrace {
            irq_flag: self.irq_flag,
            apu_cycles: self.apu_cycles,
        }
    }
}
