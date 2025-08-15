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

    pub fn decrement_timer(&mut self) {
        let time = (((self.len_timerh & 0b0000_0111) as u16) << 8) | (self.timerl as u16);
        let next_time = if time == 0 {
            0b0000_0111_1111_1111
        } else {
            time - 1
        };

        self.timerl = (next_time & 0xFF) as u8;
        self.len_timerh = self.len_timerh & (0b1111_1000 | ((next_time >> 8) as u8));
    }
}
