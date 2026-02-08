pub struct ScrollRegister {
    pub scroll_x: u8,
    pub scroll_y: u8,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
        }
    }

    pub fn write(&mut self, data: u8, first_write: bool) -> u8 {
        let retval: u8;
        if first_write {
            retval = self.scroll_x;
            self.scroll_x = data;
        } else {
            retval = self.scroll_y;
            self.scroll_y = data;
        }
        retval
    }
}
