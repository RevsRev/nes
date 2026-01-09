use crate::apu::registers::divider::Divider;

use super::len_counter::LenCounter;

const SEQUENCE_PATTERNS: [u8; 32] = [
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15,
];

pub struct TriangleChannel {
    halt: bool,
    linear_counter: Divider,
    timer: Divider,
    len_counter: LenCounter,
    length_counter_idx: u8,
    counter_reload_value: u8,

    sequence_step: u8,
    linear_counter_reload: bool,
    out: u8,
}

impl TriangleChannel {
    pub fn new() -> Self {
        TriangleChannel {
            halt: true,
            linear_counter: Divider::new(0xFFFF),
            timer: Divider::new(0xFFFF),
            len_counter: LenCounter::new(),
            length_counter_idx: 0xFF,
            counter_reload_value: 0xFF,
            sequence_step: 0,
            linear_counter_reload: false,
            out: 0,
        }
    }

    pub fn write_to_linear_counter(&mut self, data: u8) -> u8 {
        let flag = if self.halt { 0b1000_0000 } else { 0b0 };
        let old_value = (self.counter_reload_value & 0b0111_1111) + flag;
        self.halt = data & 0b1000_0000 == 0b1000_0000;
        self.counter_reload_value = data & 0b0111_1111;
        old_value
    }

    pub fn write_to_unused(&mut self, data: u8) -> u8 {
        //TODO - What is this for?
        return data;
    }

    pub fn write_to_timerl(&mut self, data: u8) -> u8 {
        let old_value = self.timer.reload_value() as u8;

        let new_reload_value = (self.timer.reload_value() & 0b0000_0111_0000_0000) | (data as u16);

        self.timer.reset_reload_value(new_reload_value);

        old_value
    }

    pub fn write_to_len_timerh(&mut self, data: u8) -> u8 {
        let timer_h_bits = ((self.timer.reload_value() & 0b0000_0111_0000_000) >> 8) as u8;
        let old_value = self.length_counter_idx | timer_h_bits;

        self.length_counter_idx = (data & 0b1111_1000) >> 3;

        let new_reload_value = (self.timer.reload_value() & 0b0000_0000_0111_1111)
            | (((data & 0b0000_0111) as u16) << 8);
        self.timer.reset_reload_value(new_reload_value);

        self.len_counter.set(self.length_counter_idx as usize);

        self.linear_counter_reload = true;

        old_value
    }

    pub fn decrement_timer(&mut self) {
        if self.linear_counter.value() == 0 || self.len_counter.get() == 0 {
            self.out = 0;
            return;
        }

        self.sequence_step = (self.sequence_step + 1) % 32;
        self.out = SEQUENCE_PATTERNS[self.sequence_step as usize];
    }

    pub fn frame_clock(&mut self) {
        if self.len_counter.get() != 0 && !self.halt {
            self.len_counter.decrement();
        }

        if self.linear_counter_reload {
            self.linear_counter
                .reset_reload_value(self.counter_reload_value as u16); //todo
        } else if self.linear_counter.value() != 0 {
            self.linear_counter.clock();
        }

        if !self.halt {
            self.linear_counter_reload = false;
        }
    }

    pub fn get_out(&self) -> u8 {
        self.out
    }

    pub fn len_counter_expired(&self) -> bool {
        self.len_counter.get() == 0
    }
}
