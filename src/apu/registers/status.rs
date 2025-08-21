pub const PULSE_1: u8 = 0b0000_0001;
pub const PULSE_2: u8 = 0b0000_0010;
pub const TRIANGLE: u8 = 0b0000_0100;
pub const NOISE: u8 = 0b0000_1000;
pub const ENABLE_DMC: u8 = 0b0001_0000;
pub const UNUSED_1: u8 = 0b0010_0000;
pub const UNUSED_2: u8 = 0b0100_0000;
pub const UNUSED_3: u8 = 0b1000_0000;

pub struct Status {
    data: u8,
}

impl Status {
    pub fn new() -> Self {
        Status { data: 0xFF }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_val = self.data;
        self.data = data;
        old_val
    }

    pub fn pulse_1_enabled(&self) -> bool {
        self.has_flag(PULSE_1)
    }

    pub fn pulse_2_enabled(&self) -> bool {
        self.has_flag(PULSE_2)
    }

    pub fn triangle_enabled(&self) -> bool {
        self.has_flag(TRIANGLE)
    }

    pub fn noise_enabled(&self) -> bool {
        self.has_flag(NOISE)
    }

    fn has_flag(&self, flag: u8) -> bool {
        self.data & flag == flag
    }
}
