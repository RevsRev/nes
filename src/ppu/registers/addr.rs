pub struct AddrRegister {
    value: (u8, u8),
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister { value: (0, 0) }
    }

    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xFF) as u8;
    }

    pub fn update(&mut self, data: u8, hi_ptr: bool) -> u8 {
        let retval: u8;
        if hi_ptr {
            retval = self.value.0;
            self.value.0 = data;
        } else {
            retval = self.value.1;
            self.value.1 = data;
        }

        if self.get() > 0x3FFF {
            self.set(self.get() & 0b111111_11111111);
        }
        retval
    }

    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);
        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }
        if self.get() > 0x3FFF {
            self.set(self.get() & 0b111111_11111111);
        }
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
}
