use crate::apu::{channel::sweep::Sweep, mixer, registers::divider::Divider};

const ENVELOPE_VOL_OR_ENV_DIVIDER_1: u8 = 0b0000_0001;
const ENVELOPE_VOL_OR_ENV_DIVIDER_2: u8 = 0b0000_0010;
const ENVELOPE_VOL_OR_ENV_DIVIDER_4: u8 = 0b0000_0100;
const ENVELOPE_VOL_OR_ENV_DIVIDER_3: u8 = 0b0000_1000;
const ENVELOPE_CONST_VOL_OR_ENV_FLAG: u8 = 0b0001_0000;
const ENVELOPE_LENGTH_COUNTER_HALT: u8 = 0b0010_0000;
const ENVELOPE_DUTY_1: u8 = 0b0100_0000;
const ENVELOPE_DUTY_2: u8 = 0b1000_0001;

const ENVELOPE_DUTY_SELECTOR: u8 = ENVELOPE_DUTY_1 | ENVELOPE_DUTY_2;

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

pub const LENGTH_TABLE: [u8; 32] = [
    10,  //0 0000
    254, //0 0001
    20,  //0 0010
    2,   //0 0011
    40,  //0 0100
    4,   //0 0101
    80,  //0 0110
    6,   //0 0111
    160, //0 1000
    8,   //0 1001
    60,  //0 1010
    10,  //0 1011
    14,  //0 1100
    12,  //0 1101
    26,  //0 1110
    14,  //0 1111
    12,  //1 0000
    16,  //1 0001
    24,  //1 0010
    18,  //1 0011
    48,  //1 0100
    20,  //1 0101
    96,  //1 0110
    22,  //1 0111
    192, //1 1000
    24,  //1 1001
    72,  //1 1010
    26,  //1 1011
    16,  //1 1100
    28,  //1 1101
    32,  //1 1110
    30,  //1 1111
];

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
    sequence_step: u8,
    decay_counter: u8,
    divider: u8,

    length_counter_halt: bool,
    timer: Divider,
    length_counter_idx: u8,
    length_counter: u8,

    start_flag: bool,

    last_timerl: u8,
    last_timerh: u8,
    out: u8,
}

impl SquareChannel {
    pub fn new() -> Self {
        SquareChannel {
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            sequence_step: 0,
            decay_counter: 0,
            start_flag: false,
            divider: 0,
            timer: Divider::new(0),
            length_counter_halt: false,
            length_counter_idx: 0,
            length_counter: 0,
            last_timerl: 0xFF,
            last_timerh: 0xFF,
            out: 0,
        }
    }

    pub fn write_to_envelope(&mut self, data: u8) -> u8 {
        self.length_counter_halt =
            data & ENVELOPE_LENGTH_COUNTER_HALT == ENVELOPE_LENGTH_COUNTER_HALT;
        self.envelope.write(data)
    }

    pub fn write_to_sweep(&mut self, data: u8) -> u8 {
        self.sweep.write(data)
    }

    pub fn write_to_timerl(&mut self, data: u8) -> u8 {
        let old_value = self.timer.reload_value() as u8;

        let new_reload_value = (self.timer.reload_value() & 0b0000_0111_0000_0000) | (data as u16);

        self.timer.reset_reload_value(new_reload_value);

        old_value
    }

    pub fn write_to_len_timerh(&mut self, data: u8) -> u8 {
        let timer_h_bits = ((self.timer.reload_value() & 0b0000_0111_0000_000) >> 8) as u8;
        let old_value = self.length_counter_idx | timer_h_bits;

        self.length_counter_idx = (data & 0b1111_1000) >> 3;

        let new_reload_value = (self.timer.reload_value() & 0b0000_0000_0111_1111)
            | (((data & 0b0000_0111) as u16) << 8);
        self.timer.reset_reload_value(new_reload_value);

        self.length_counter = LENGTH_TABLE[self.length_counter_idx as usize];

        old_value
    }

    pub fn get_out(&mut self) -> u8 {
        match self.sweep.is_muted() || self.timer.reload_value() < 8 {
            true => 0,
            false => self.out,
        }
    }

    pub fn decrement_timer(&mut self) {
        if self.timer.clock() {
            self.sequence_step = (self.sequence_step + 1) % 8;
        }

        if self.length_counter == 0 {
            self.out = 0;
            return;
        }

        let duty = self.envelope.data & ENVELOPE_DUTY_SELECTOR >> 6;
        let volume = match self.envelope.data & ENVELOPE_CONST_VOL_OR_ENV_FLAG
            == ENVELOPE_CONST_VOL_OR_ENV_FLAG
        {
            true => self.envelope.data & VOLUME_SELECTOR,
            false => self.decay_counter,
        };
        self.out = volume * DUTY_PATTERNS[duty as usize][self.sequence_step as usize];
    }

    fn get_time(&self) -> u16 {
        self.timer.reload_value()
    }

    fn set_time(&mut self, time: u16) {
        self.timer.reset_reload_value(time);
    }

    pub fn frame_clock(&mut self) {
        if self.length_counter != 0 && !self.length_counter_halt {
            self.length_counter = self.length_counter - 1;
        }

        if self.start_flag {
            self.divider = self.envelope.data & VOLUME_SELECTOR;
            self.decay_counter = 15;
            self.start_flag = false;
        }

        let time = self.get_time();
        let next_time = self.sweep.on_frame(time);
        self.set_time(next_time);

        if self.divider == 0 {
            self.divider = self.envelope.data & VOLUME_SELECTOR;
            if self.decay_counter != 0 {
                self.decay_counter = self.decay_counter - 1;
            } else if self.envelope.data & ENVELOPE_LENGTH_COUNTER_HALT
                == ENVELOPE_LENGTH_COUNTER_HALT
            {
                self.decay_counter = 15;
            }
        } else {
            self.divider = self.divider - 1;
        }
    }
}
