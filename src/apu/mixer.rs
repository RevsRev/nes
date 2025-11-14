pub struct Mixer {
    apu_cycles: u8,
    pub output: f32, //TODO - Should this be a f32?
}

impl Mixer {
    pub fn new() -> Self {
        Mixer {
            apu_cycles: 0,
            output: 0.0,
        }
    }

    //TODO - Output type - f32?
    //TODO - Wire in other channel types
    pub fn output(&mut self, pulse1: u8, pulse2: u8) {
        self.apu_cycles = (self.apu_cycles + 1) % 20;

        if self.apu_cycles % 20 != 0 {
            return;
        }

        let pulse_out = if pulse1 == 0 && pulse2 == 0 {
            0.0
        } else {
            95.8 / (8128.0 / (pulse1 as f32 + pulse2 as f32) + 100.0)
        };

        let triangle_out = 0.0;
        let noise_out = 0.0;
        let dmc_out = 0.0;

        let tnd_out = if triangle_out == 0.0 && noise_out == 0.0 && dmc_out == 0.0 {
            0.0
        } else {
            159.79
                / (1.0 / (triangle_out / 8227.0 + noise_out / 12241.0 + dmc_out / 22638.0) + 100.0)
        };

        self.output = pulse_out + tnd_out;
    }
}
