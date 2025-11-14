pub struct SoundFrame {
    pub output: f32,
}

impl SoundFrame {
    pub fn new() -> Self {
        SoundFrame { output: 0.0 }
    }
}
