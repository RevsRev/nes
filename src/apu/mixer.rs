use ringbuf::HeapProducer;

const CPU_FREQUENCY: f32 = 1_790_000.0;
const SAMPLE_FREQUENCY: f32 = 44_100.0;
const FLUSH_SIZE: f32 = CPU_FREQUENCY / SAMPLE_FREQUENCY;

pub struct Mixer {
    sample_sum: f32,
    sample_count: f32,
    last_output: f32,
    producer: HeapProducer<f32>,
}

impl Mixer {
    pub fn new(producer: HeapProducer<f32>) -> Self {
        Mixer {
            sample_sum: 0.0,
            sample_count: 0.0,
            last_output: 0.0,
            producer,
        }
    }

    //TODO - Output type - f32?
    //TODO - Wire in other channel types
    pub fn output(&mut self, pulse1: u8, pulse2: u8, triangle_out: u8) {
        let pulse_out = if pulse1 + pulse2 == 0 {
            0.0
        } else {
            95.8 / (8128.0 / (pulse1 as f32 + pulse2 as f32) + 100.0)
        };

        let noise_out = 0;
        let dmc_out = 0;

        let tnd_out = if triangle_out + noise_out + dmc_out == 0 {
            0.0
        } else {
            159.79
                / (1.0
                    / (triangle_out as f32 / 8227.0
                        + noise_out as f32 / 12241.0
                        + dmc_out as f32 / 22638.0)
                    + 100.0)
        };

        let out = pulse_out + tnd_out;
        self.add_output(out);
    }

    pub fn flush(&mut self) {
        let sample = if self.sample_count == 0.0 {
            self.last_output
        } else {
            self.last_output = 3.0 * self.sample_sum / self.sample_count;
            self.sample_sum = 0.0;
            self.sample_count = 0.0;

            self.last_output
        };
        let _ = self.producer.push(sample);
    }

    pub fn add_output(&mut self, output: f32) {
        self.sample_sum += output;
        self.sample_count += 1.0;

        if self.sample_count > FLUSH_SIZE {
            self.flush();
        }
    }
}
