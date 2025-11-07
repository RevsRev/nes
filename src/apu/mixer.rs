pub struct Mixer {}

impl Mixer {
    //TODO - Output type - f32?
    //TODO - Wire in other channel types
    pub fn output(pulse1: u8) -> f32 {
        let pulse2 = 0.0;

        let pulse_out = if pulse1 == 0 && pulse2 == 0.0 {
            0.0
        } else {
            95.8 / (8128.0 / (pulse1 as f32 + pulse2) + 100.0)
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

        return pulse_out + tnd_out;
    }
}
