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
    frame_cycles: u32,
    written_during_cycle: bool,
    reset_timer_countdown: u64,
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
            reset_timer_countdown: 0,
            clock: Option::None,
            interrupt: interrupt,
            irq_flag: false,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;

        if data & 0b0100_0000 == 0b0100_0000 {
            self.set_irq_flag(false);
        }

        self.reset_timer_countdown = 2;

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
        if self.reset_timer_countdown > 0 {
            self.reset_timer_countdown = self.reset_timer_countdown - 1;
            if self.reset_timer_countdown == 0 {
                if self.data & MODE == MODE {
                    self.clock = Option::Some(FrameClock::HALF);
                }
                self.frame_cycles = 0;
            }
        }
        if self.data & MODE == MODE {
            self.five_step_clock()
        } else {
            self.four_step_clock()
        }
    }

    fn four_step_clock(&mut self) {
        if self.frame_cycles > 14914 {
            self.frame_cycles = 0;
        }

        if self.frame_cycles == 7456 || self.frame_cycles == 14914 {
            self.clock = Option::Some(FrameClock::HALF);
        } else if self.frame_cycles == 3728 || self.frame_cycles == 11185 {
            self.clock = Option::Some(FrameClock::QUARTER);
        }

        if self.frame_cycles == 14914 && self.data & 0b0100_0000 == 0b0 {
            self.set_irq_flag(true);
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
