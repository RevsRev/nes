const ENVELOPE_VOL_OR_ENV_DIVIDER_1: u8 = 0b0000_0001;
const ENVELOPE_VOL_OR_ENV_DIVIDER_2: u8 = 0b0000_0010;
const ENVELOPE_VOL_OR_ENV_DIVIDER_4: u8 = 0b0000_0100;
const ENVELOPE_VOL_OR_ENV_DIVIDER_3: u8 = 0b0000_1000;
const ENVELOPE_CONST_VOL_OR_ENV_FLAG: u8 = 0b0001_0000;
const ENVELOPE_LENGTH_COUNTER_HALT: u8 = 0b0010_0000;
const ENVELOPE_DUTY_1: u8 = 0b0100_0000;
const ENVELOPE_DUTY_2: u8 = 0b1000_0001;

const VOLUME_SELECTOR: u8 = ENVELOPE_VOL_OR_ENV_DIVIDER_1
    | ENVELOPE_VOL_OR_ENV_DIVIDER_2
    | ENVELOPE_VOL_OR_ENV_DIVIDER_3
    | ENVELOPE_VOL_OR_ENV_DIVIDER_4;

const SWEEP_SHIFT_COUNT_0: u8 = 0b0000_0001;
const SWEEP_SHIFT_COUNT_1: u8 = 0b0000_0010;
const SWEEP_SHIFT_COUNT_2: u8 = 0b0000_0100;
const SWEEP_NEGATE: u8 = 0b0000_1000;
const SWEEP_PERIOD_0: u8 = 0b0001_0000;
const SWEEP_PERIOD_1: u8 = 0b0010_0000;
const SWEEP_PERIOD_2: u8 = 0b0100_0000;
const SWEEP_ENABLED: u8 = 0b1000_0000;

const DUTY_PATTERNS: [[u8; 8]; 4] = [
    [0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 1],
    [0, 0, 0, 0, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 0, 0],
];

pub struct Sweep {
    data: u8,
}

impl Sweep {
    pub fn new() -> Self {
        Sweep { data: 0xFF }
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
        Envelope { data: 0xFF }
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
    len_timerh: u8,
    sequence_step: u8,

    last_timerl: u8,
    last_timerh: u8,
    out: u8,
}

impl SquareChannel {
    pub fn new() -> Self {
        SquareChannel {
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            timerl: 0,
            len_timerh: 0,
            sequence_step: 0,
            last_timerl: 0xFF,
            last_timerh: 0xFF,
            out: 0,
        }
    }

    pub fn write_to_envelope(&mut self, data: u8) -> u8 {
        self.envelope.write(data)
    }

    pub fn write_to_sweep(&mut self, data: u8) -> u8 {
        self.sweep.write(data)
    }

    pub fn write_to_timerl(&mut self, data: u8) -> u8 {
        let old_value = self.last_timerl;
        self.timerl = data;
        self.last_timerl = data;
        old_value
    }

    pub fn write_to_len_timerh(&mut self, data: u8) -> u8 {
        let old_value = self.last_timerh;
        self.len_timerh = data;
        self.last_timerh = data;
        old_value
    }

    pub fn get_out(&mut self) -> u8 {
        self.out
    }

    pub fn decrement_timer(&mut self) {
        let time = (((self.len_timerh & 0b0000_0111) as u16) << 8) | (self.timerl as u16);
        let next_time = if time == 0 {
            self.sequence_step = (self.sequence_step + 1) % 8;
            0b0000_0111_1111_1111
        } else {
            time - 1
        };

        self.timerl = (next_time & 0xFF) as u8;
        self.len_timerh = self.len_timerh & (0b1111_1000 | ((next_time >> 8) as u8));

        let duty = self.envelope.data & 0b1100_0000 >> 6;
        self.out = (self.envelope.data & VOLUME_SELECTOR)
            * DUTY_PATTERNS[duty as usize][self.sequence_step as usize];
    }
}
