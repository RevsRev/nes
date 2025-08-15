const ENVELOPE_VOL_OR_ENV_DIVIDER_1: u8 = 0b0000_0001;
const ENVELOPE_VOL_OR_ENV_DIVIDER_2: u8 = 0b0000_0010;
const ENVELOPE_VOL_OR_ENV_DIVIDER_4: u8 = 0b0000_0100;
const ENVELOPE_VOL_OR_ENV_DIVIDER_3: u8 = 0b0000_1000;
const ENVELOPE_CONST_VOL_OR_ENV_FLAG: u8 = 0b0001_0000;
const ENVELOPE_LENGTH_COUNTER_HALT: u8 = 0b0010_0000;
const ENVELOPE_DUTY_1: u8 = 0b0100_0000;
const ENVELOPE_DUTY_2: u8 = 0b1000_0001;

const SWEEP_SHIFT_COUNT_0: u8 = 0b0000_0001;
const SWEEP_SHIFT_COUNT_1: u8 = 0b0000_0010;
const SWEEP_SHIFT_COUNT_2: u8 = 0b0000_0100;
const SWEEP_NEGATE: u8 = 0b0000_1000;
const SWEEP_PERIOD_0: u8 = 0b0001_0000;
const SWEEP_PERIOD_1: u8 = 0b0010_0000;
const SWEEP_PERIOD_2: u8 = 0b0100_0000;
const SWEEP_ENABLED: u8 = 0b1000_0000;

pub struct Sweep {
    data: u8,
}

impl Sweep {
    pub fn new() -> Self {
        Sweep { data: 0 }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_val = self.data;
        self.data = data;
        old_val
    }
}
pub struct Envelope {
    data: u8,
}

impl Envelope {
    pub fn new() -> Self {
        Envelope { data: 0 }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_val = self.data;
        self.data = data;
        old_val
    }
}

pub struct SquareChannel {
    envelope: Envelope,
    sweep: Sweep,
    timerl: u8,
    len_timeh: u8,
}

impl SquareChannel {
    pub fn new() -> Self {
        SquareChannel {
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            timerl: 0,
            len_timeh: 0,
        }
    }

    pub fn write_to_envelope(&mut self, data: u8) -> u8 {
        self.envelope.write(data)
    }

    pub fn write_to_sweep(&mut self, data: u8) -> u8 {
        self.sweep.write(data)
    }

    pub fn write_to_timerl(&mut self, data: u8) -> u8 {
        let old_value = self.timerl;
        self.timerl = data;
        old_value
    }

    pub fn write_to_len_timerh(&mut self, data: u8) -> u8 {
        let old_value = self.len_timeh;
        self.len_timeh = data;
        old_value
    }
}
