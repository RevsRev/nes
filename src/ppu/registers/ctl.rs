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

    pub fn sprite_size(&self) -> u8 {
        return if self.has_flag(SPRITE_SIZE) { 16 } else { 8 };
    }

    pub fn sprite_pattern_addr(&self) -> u16 {
        if self.has_flag(SPRITE_PATTERN_ADDR) {
            return 0x1000;
        }
        0x0000
    }

    pub(crate) fn nametable_address(&self) -> u16 {
        match self.bits & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => panic!("Impossible!"),
        }
    }
}

#[cfg(test)]
mod test {

    use super::ControlRegister;

    #[test]
    fn should_get_correct_increment() {
        let mut ctl = ControlRegister::new();
        ctl.update(0b0000_0100);
        assert_eq!(ctl.vram_addr_increment(), 32);
        ctl.update(0b1100_0101);
        assert_eq!(ctl.vram_addr_increment(), 32);
        ctl.update(0b0000_0000);
        assert_eq!(ctl.vram_addr_increment(), 1);
        ctl.update(0b1111_0001);
        assert_eq!(ctl.vram_addr_increment(), 1);
    }

    #[test]
    fn should_get_correct_vblank_nmi() {
        let mut ctl = ControlRegister::new();
        ctl.update(0b0000_0000);
        assert_eq!(ctl.generate_vblank_nmi(), false);
        ctl.update(0b0100_0101);
        assert_eq!(ctl.generate_vblank_nmi(), false);
        ctl.update(0b1000_0000);
        assert_eq!(ctl.generate_vblank_nmi(), true);
        ctl.update(0b1111_0001);
        assert_eq!(ctl.generate_vblank_nmi(), true);
    }
}
