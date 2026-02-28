use std::{cell::RefCell, rc::Rc};

use crate::{
    interrupt::{Interrupt, InterruptImpl},
    trace::FrameTrace,
};

const MODE: u8 = 0b1000_0000;
const INTERRUPT: u8 = 0b0100_0000;

pub struct FrameCounter {
    data: u8,
    frame_cycles: u32,
    written_during_cycle: bool,
    clock: Option<FrameClock>,
    interrupt: Rc<RefCell<InterruptImpl>>,
    irq_flag: bool,
}

pub enum FrameClock {
    QUARTER,
    HALF,
}

impl FrameCounter {
    pub fn new(interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        FrameCounter {
            data: 0,
            frame_cycles: 0,
            written_during_cycle: false,
            clock: Option::None,
            interrupt: interrupt,
            irq_flag: false,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;

        if data & 0b0100_0000 == 0b0100_0000 {
            self.set_irq_flag(None);
        }

        self.data = data;
        old_value
    }

    pub fn get_data(&self) -> u8 {
        self.data
    }

    pub fn set_irq_flag(&mut self, value: Option<u64>) -> bool {
        let retval = self.irq_flag;
        self.irq_flag = value.is_some();
        if !(self.interrupt.borrow().poll_irq().is_some() && value.is_some()) {
            self.interrupt.borrow_mut().set_irq(value);
        }
        retval
    }

    pub fn pending_frame_clock(&self) -> bool {
        self.clock.is_some()
    }

    pub fn emit_clock(&mut self) -> Option<FrameClock> {
        self.clock.take()
    }

    pub fn reset(&mut self) {
        if self.data & MODE == MODE {
            self.clock = Option::Some(FrameClock::HALF);
        }
        self.frame_cycles = 0;
    }

    pub fn step(&mut self, total_cpu_cycles: u64) {
        if self.data & MODE == MODE {
            self.five_step_clock()
        } else {
            self.four_step_clock(total_cpu_cycles)
        }
    }

    fn four_step_clock(&mut self, total_cpu_cycles: u64) {
        if self.frame_cycles > 14914 {
            if self.data & 0b0100_0000 == 0b0 {
                self.set_irq_flag(Some(total_cpu_cycles));
            }
            self.frame_cycles = 0;
        }

        if self.frame_cycles == 7456 || self.frame_cycles == 14914 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.frame_cycles == 3728 || self.frame_cycles == 11185 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        if self.frame_cycles == 14914 && self.data & 0b0100_0000 == 0b0 {
            self.set_irq_flag(Some(total_cpu_cycles));
        }
        self.frame_cycles = self.frame_cycles + 1;
    }

    fn five_step_clock(&mut self) {
        if self.frame_cycles > 18640 {
            self.frame_cycles = 0;
        }

        if self.frame_cycles == 7456 || self.frame_cycles == 18640 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.frame_cycles == 3728
            || self.frame_cycles == 11185
            || self.frame_cycles == 14914
        {
            self.clock = Option::Some(FrameClock::QUARTER);
        }
        self.frame_cycles = self.frame_cycles + 1;
    }

    pub fn trace(&self) -> FrameTrace {
        FrameTrace {
            irq_flag: self.irq_flag,
            apu_cycles: self.frame_cycles,
        }
    }
}
