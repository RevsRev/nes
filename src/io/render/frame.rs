use super::{HEIGHT, WIDTH};

pub struct Frame {
    pub data: Vec<u8>,
    pub sprite: Vec<u8>,
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            data: vec![0; (WIDTH) * (HEIGHT) * 3],
            sprite: vec![0; WIDTH * HEIGHT],
        }
    }

    pub fn set_background_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        self.set_pixel(x, y, rgb);
    }

    pub fn clear_sprite_pixels(&mut self) {
        self.sprite.fill(0);
    }

    pub fn set_sprite_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8), sprite: u8) {
        self.set_pixel(x, y, rgb);

        let base = y * WIDTH + x;
        if base < self.sprite.len() {
            self.sprite[base] = sprite;
        }
    }

    fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2;
        }
    }

    pub fn get_sprite_pixel_at(&self, x: usize, y: usize) -> u8 {
        let base = y * WIDTH + x;
        self.sprite[base]
    }
}
