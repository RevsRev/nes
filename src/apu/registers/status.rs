use std::{cell::RefCell, rc::Rc};

use crate::apu::channel::{
    dmc::DmcChannel, noise::NoiseChannel, square::SquareChannel, triangle::TriangleChannel,
};

use super::frame::FrameCounter;

pub const PULSE_1: u8 = 0b0000_0001;
pub const PULSE_2: u8 = 0b0000_0010;
pub const TRIANGLE: u8 = 0b0000_0100;
pub const NOISE: u8 = 0b0000_1000;
pub const ENABLE_DMC: u8 = 0b0001_0000;
pub const UNUSED_1: u8 = 0b0010_0000;
pub const UNUSED_2: u8 = 0b0100_0000;
pub const UNUSED_3: u8 = 0b1000_0000;

pub struct Status {
    pulse_1: Rc<RefCell<SquareChannel>>,
    pulse_2: Rc<RefCell<SquareChannel>>,
    triangle: Rc<RefCell<TriangleChannel>>,
    noise: Rc<RefCell<NoiseChannel>>,
    dmc: Rc<RefCell<DmcChannel>>,
    frame: Rc<RefCell<FrameCounter>>,
}

impl Status {
    pub fn new(
        pulse_1: Rc<RefCell<SquareChannel>>,
        pulse_2: Rc<RefCell<SquareChannel>>,
        triangle: Rc<RefCell<TriangleChannel>>,
        noise: Rc<RefCell<NoiseChannel>>,
        dmc: Rc<RefCell<DmcChannel>>,

        frame: Rc<RefCell<FrameCounter>>,
    ) -> Self {
        Status {
            pulse_1,
            pulse_2,
            triangle,
            noise,
            dmc,
            frame,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let mut old_val = 0b0;
        if !self.pulse_1.borrow_mut().len_counter_expired() {
            old_val |= 0b0000_0001;
        }
        if !self.pulse_2.borrow_mut().len_counter_expired() {
            old_val |= 0b0000_0010;
        }
        if !self.triangle.borrow_mut().len_counter_expired() {
            old_val |= 0b0000_0100;
        }

        //Do this while we haven't implemented noise/dmc properly
        old_val |= 0b1111_1000;

        if !Self::has_flag(data, PULSE_1) {
            self.pulse_1.borrow_mut().disable();
        } else {
            self.pulse_1.borrow_mut().enable();
        }
        if !Self::has_flag(data, PULSE_2) {
            self.pulse_2.borrow_mut().disable();
        } else {
            self.pulse_2.borrow_mut().enable();
        }

        old_val
    }

    fn has_flag(data: u8, flag: u8) -> bool {
        data & flag == flag
    }

    pub fn read(&mut self) -> Result<u8, String> {
        let mut read = 0b0;

        if self.frame.borrow_mut().set_irq_flag(false) {
            read |= 0b0100_0000;
        }
        if !self.pulse_1.borrow_mut().len_counter_expired() {
            read |= 0b0000_0001;
        }
        if !self.pulse_2.borrow_mut().len_counter_expired() {
            read |= 0b0000_0010;
        }
        if !self.triangle.borrow_mut().len_counter_expired() {
            read |= 0b0000_0100;
        }
        Result::Ok(read)
    }
}
