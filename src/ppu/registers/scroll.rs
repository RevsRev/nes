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

    pub fn write(&mut self, data: u8) {
        if self.first {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }
        self.first = !self.first;
    }

    pub fn read(&self) -> u8 {
        if self.first {
            return self.value.0;
        }
        self.value.1
    }
}
