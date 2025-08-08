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
}
