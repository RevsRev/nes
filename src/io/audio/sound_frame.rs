use ringbuf::HeapProducer;

const CPU_FREQUENCY: f32 = 1_790_000.0;
const SAMPLE_FREQUENCY: f32 = 44_100.0;
const FLUSH_SIZE: f32 = CPU_FREQUENCY / SAMPLE_FREQUENCY;

pub struct SoundFrame {
    producer: HeapProducer<f32>,

    last_output: f32,
    sample_sum: f32,
    sample_count: f32,
}

impl SoundFrame {
    pub fn new(producer: HeapProducer<f32>) -> Self {
        SoundFrame {
            producer,
            last_output: 0.0,
            sample_sum: 0.0,
            sample_count: 0.0,
        }
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
        self.producer.push(sample).unwrap();
    }

    pub fn add_output(&mut self, output: f32) {
        self.sample_sum += output;
        self.sample_count += 1.0;

        if self.sample_count > FLUSH_SIZE {
            self.flush();
        }
    }
}
