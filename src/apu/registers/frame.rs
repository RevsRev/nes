const MODE: u8 = 0b1000_0000;
const INTERRUPT: u8 = 0b0100_0000;

pub struct FrameCounter {
    data: u8,
    apu_cycles: u32,
}

impl FrameCounter {
    pub fn new() -> Self {
        FrameCounter {
            data: 0,
            apu_cycles: 0,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;
        self.data = data;
        old_value
    }

    pub fn clock(&mut self) -> bool {
        if self.data & MODE == MODE {
            self.five_step_clock()
        } else {
            self.four_step_clock()
        }
    }

    fn four_step_clock(&mut self) -> bool {
        let emit_clock = self.apu_cycles == 3728
            || self.apu_cycles == 7456
            || self.apu_cycles == 11185
            || self.apu_cycles == 14914;
        if self.apu_cycles >= 14914 {
            self.apu_cycles = 0;
        }
        self.apu_cycles = self.apu_cycles + 1;
        emit_clock
    }

    fn five_step_clock(&mut self) -> bool {
        let emit_clock = self.apu_cycles == 3728
            || self.apu_cycles == 7456
            || self.apu_cycles == 11185
            || self.apu_cycles == 14914
            || self.apu_cycles == 18640;
        if self.apu_cycles >= 18640 {
            self.apu_cycles = 0;
        }
        self.apu_cycles = self.apu_cycles + 1;
        emit_clock
    }
}
