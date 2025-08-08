pub const NAMETABLE1: u8 = 0b0000_0001;
pub const NAMETABLE2: u8 = 0b0000_0010;
pub const VRAM_ADD_INCREMENT: u8 = 0b0000_0100;
pub const SPRITE_PATTERN_ADDR: u8 = 0b0000_1000;
pub const BACKGROUND_PATTERN_ADDR: u8 = 0b0001_0000;
pub const SPRITE_SIZE: u8 = 0b0010_0000;
pub const MASTER_SLAVE_SELECT: u8 = 0b0100_0000;
pub const GENERATE_NMI: u8 = 0b1000_0000;

pub struct ControlRegister {
    bits: u8,
}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister { bits: 0b0000_0000 }
    }

    fn has_flag(&self, flag: u8) -> bool {
        return flag & self.bits == flag;
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if !self.has_flag(VRAM_ADD_INCREMENT) {
            // 0b0000_0001 //1
            1
        } else {
            // 0b0010_0000 //32
            32
        }
    }

    pub fn update(&mut self, data: u8) -> u8 {
        let retval = self.bits;
        self.bits = data;
        retval
    }

    pub fn generate_vblank_nmi(&self) -> bool {
        self.bits & GENERATE_NMI != 0
    }

    pub fn bknd_pattern_addr(&self) -> u16 {
        if self.has_flag(BACKGROUND_PATTERN_ADDR) {
            return 0x1000;
        }
        0x0000
    }

    pub fn sprite_pattern_addr(&self) -> u16 {
        if self.has_flag(SPRITE_PATTERN_ADDR) {
            return 0x1000;
        }
        0x0000
    }
}
