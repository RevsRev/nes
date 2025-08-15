pub struct TriangleChannel {
    linear_counter: u8,
    unused: u8,
    timerl: u8,
    len_timerh: u8,
}

impl TriangleChannel {
    pub fn new() -> Self {
        TriangleChannel {
            linear_counter: 0,
            unused: 0,
            timerl: 0,
            len_timerh: 0,
        }
    }

    pub fn write_to_linear_counter(&mut self, data: u8) -> u8 {
        let old_value = self.linear_counter;
        self.linear_counter = data;
        old_value
    }

    pub fn write_to_unused(&mut self, data: u8) -> u8 {
        let old_value = self.unused;
        self.unused = data;
        old_value
    }

    pub fn write_to_timerl(&mut self, data: u8) -> u8 {
        let old_value = self.timerl;
        self.timerl = data;
        old_value
    }

    pub fn write_to_len_timerh(&mut self, data: u8) -> u8 {
        let old_value = self.len_timerh;
        self.len_timerh = data;
        old_value
    }
}
