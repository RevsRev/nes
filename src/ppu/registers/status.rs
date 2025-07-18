const UNUSED_0: u8 = 0b0000_0001;
const UNUSED_1: u8 = 0b0000_0010;
const UNUSED_2: u8 = 0b0000_0100;
const UNUSED_3: u8 = 0b0000_1000;
const UNUSED_4: u8 = 0b0001_0000;
const SPRITE_OVERFLOW: u8 = 0b0010_0000;
const SPRITE_0_HIT: u8 = 0b0100_0000;
const VBLANK: u8 = 0b1000_0000;

pub struct StatusRegister {
    value: u8,
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister { value: 0 }
    }

    pub fn set_sprite_overflow(&mut self, status: bool) {
        self.set(SPRITE_OVERFLOW, status);
    }

    pub fn set_sprite_0_hit(&mut self, status: bool) {
        self.set(SPRITE_0_HIT, status);
    }

    pub fn set_vblank(&mut self, status: bool) {
        self.set(VBLANK, status);
    }

    pub fn reset_vblank_status(&mut self) {
        self.set_vblank(false);
    }

    pub fn is_vblank(&self) -> bool {
        self.value & VBLANK != 0
    }

    pub fn snapshot(&self) -> u8 {
        self.value
    }

    fn set(&mut self, bits: u8, status: bool) {
        if status {
            self.value = self.value | bits;
        } else {
            self.value = self.value & !bits;
        }
    }
}
