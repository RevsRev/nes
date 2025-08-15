pub struct DmcChannel {
    flags_and_rate: u8,
    direct_load: u8,
    sample_address: u8,
    sample_length: u8,
}

impl DmcChannel {
    pub fn new() -> Self {
        DmcChannel {
            flags_and_rate: 0,
            direct_load: 0,
            sample_address: 0,
            sample_length: 0,
        }
    }

    pub fn write_to_flags_and_rate(&mut self, data: u8) -> u8 {
        let old_value = self.flags_and_rate;
        self.flags_and_rate = data;
        old_value
    }

    pub fn write_to_direct_load(&mut self, data: u8) -> u8 {
        let old_value = self.direct_load;
        self.direct_load = data;
        old_value
    }

    pub fn write_to_sample_address(&mut self, data: u8) -> u8 {
        let old_value = self.sample_address;
        self.sample_address = data;
        old_value
    }

    pub fn write_to_sample_length(&mut self, data: u8) -> u8 {
        let old_value = self.sample_length;
        self.sample_length = data;
        old_value
    }
}
