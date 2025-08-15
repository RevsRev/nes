pub struct FrameCounter {
    data: u8,
}

impl FrameCounter {
    pub fn new() -> Self {
        FrameCounter { data: 0 }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_value = self.data;
        self.data = data;
        old_value
    }
}
