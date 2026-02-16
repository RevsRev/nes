pub const GREYSCALE: u8 = 0b0000_0001;
pub const SHOW_BACKGROUND_LEFTMOST_8_PIXELS: u8 = 0b0000_0010;
pub const SHOW_SPRITES_LEFTMOST_8_PIXELS: u8 = 0b0000_0100;
pub const ENABLE_BACKGROUND_RENDERING: u8 = 0b0000_1000;
pub const ENABLE_SPRITE_RENDERING: u8 = 0b0001_0000;
pub const EMPHASIZE_RED: u8 = 0b0010_0000;
pub const EMPHASIZE_GREEN: u8 = 0b0100_0000;
pub const EMPHASIZE_BLUE: u8 = 0b1000_0000;

pub struct MaskRegister {
    value: u8,
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister { value: 0 }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let retval = self.value;
        self.value = data;
        retval
    }

    pub fn read(&self) -> u8 {
        self.value
    }

    pub fn show_sprites(&self) -> bool {
        has_flag(self.value, ENABLE_SPRITE_RENDERING)
    }

    pub fn is_rendering_enabled(&self) -> bool {
        has_flag(self.value, ENABLE_SPRITE_RENDERING)
            || has_flag(self.value, ENABLE_BACKGROUND_RENDERING)
    }

    pub fn is_left_side_clipping_window_enabled(&self) -> bool {
        !has_flag(self.value, GREYSCALE) || !has_flag(self.value, SHOW_BACKGROUND_LEFTMOST_8_PIXELS)
    }
}

fn has_flag(value: u8, flag: u8) -> bool {
    value & flag == flag
}
