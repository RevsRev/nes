pub struct SoundFrame {
    output: f32,
}

impl SoundFrame {
    pub fn new() -> Self {
        SoundFrame { output: 0.0 }
    }

    pub fn get_output(&mut self) -> f32 {
        self.output
    }

    pub fn add_output(&mut self, output: f32) {
        self.output = output;
    }
}
