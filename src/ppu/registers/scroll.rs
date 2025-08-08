pub struct ScrollRegister {
    value: (u8, u8),
    first: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            value: (0, 0),
            first: true,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let retval: u8;
        if self.first {
            retval = self.value.0;
            self.value.0 = data;
        } else {
            retval = self.value.1;
            self.value.1 = data;
        }
        self.first = !self.first;
        retval
    }

    pub fn read(&self) -> u8 {
        if self.first {
            return self.value.0;
        }
        self.value.1
    }
}
