pub struct NoiseChannel {
    env_loop_len_ctr_halt_cvol: u8,
    unused: u8,
    noise_mode_period: u8,
    len_counter_load: u8,
}

impl NoiseChannel {
    pub fn new() -> Self {
        NoiseChannel {
            env_loop_len_ctr_halt_cvol: 0,
            unused: 0,
            noise_mode_period: 0,
            len_counter_load: 0,
        }
    }

    pub fn write_to_env_loop_len_ctr_halt_cvol(&mut self, data: u8) -> u8 {
        let old_value = self.env_loop_len_ctr_halt_cvol;
        self.env_loop_len_ctr_halt_cvol = data;
        old_value
    }

    pub fn write_unused(&mut self, data: u8) -> u8 {
        let old_value = self.unused;
        self.unused = data;
        old_value
    }

    pub fn write_noise_mode_period(&mut self, data: u8) -> u8 {
        let old_value = self.noise_mode_period;
        self.noise_mode_period = data;
        old_value
    }

    pub fn write_len_counter_load(&mut self, data: u8) -> u8 {
        let old_value = self.env_loop_len_ctr_halt_cvol;
        self.env_loop_len_ctr_halt_cvol = data;
        old_value
    }
}
