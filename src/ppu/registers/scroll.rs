pub struct ScrollRegister {
    pub scroll_x: u8,
    pub scroll_y: u8,
    pub latch: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let retval: u8;
        if self.latch {
            retval = self.scroll_y;
            self.scroll_y = data;
        } else {
            retval = self.scroll_x;
            self.scroll_x = data;
        }
        self.latch = !self.latch;
        retval
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}
