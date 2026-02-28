pub struct SoundFrame {
    last_output: f32,
    sample_sum: f32,
    sample_count: f32,
}

impl SoundFrame {
    pub fn new() -> Self {
        SoundFrame {
            last_output: 0.0,
            sample_sum: 0.0,
            sample_count: 0.0,
        }
    }

    pub fn get_output(&mut self) -> f32 {
        if self.sample_count == 0.0 {
            return self.last_output;
        }
        self.last_output = 3.0 * self.sample_sum / self.sample_count;
        self.sample_sum = 0.0;
        self.sample_count = 0.0;

        self.last_output
    }

    pub fn add_output(&mut self, output: f32) {
        self.sample_sum += output;
        self.sample_count += 1.0;
    }
}
