use crate::apu::registers::divider::Divider;

pub struct Sweep {
    data: u8,
    enabled: bool,
    divider: Divider,
    negate_flag: bool,
    shift_count: u8,
    reload_flag: bool,

    muted: bool,
}

impl Sweep {
    pub fn new() -> Self {
        Sweep {
            data: 0,
            enabled: true,
            divider: Divider::new(0),
            negate_flag: false,
            shift_count: 0,
            reload_flag: false,

            muted: false,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_val = self.data;
        self.data = data;

        self.enabled = data & 0b1000_0000 == 0b1000_0000;
        // self.divider.reset_reload_value(data & (0b0111_0000) >> 4);
        self.negate_flag = data & 0b0000_1000 == 0b0000_1000;
        self.shift_count = data & 0b0000_0111;

        self.reload_flag = true;

        old_val
    }

    pub fn on_frame(&mut self, current_period: u16) -> u16 {
        let clocked = self.divider.clock();

        let sign: i16 = match self.negate_flag {
            true => -1,
            false => 1,
        };

        let change_amount: i16 = sign * ((current_period >> self.shift_count) as i16);
        let target_period: u16 = (current_period as i16)
            .wrapping_add(change_amount)
            .clamp(0, 0b0000_0111_1111_1111)
            .try_into()
            .unwrap();

        self.muted = target_period > 0x7FF || current_period < 8;

        if clocked && self.enabled && self.shift_count != 0 {
            if !self.muted {
                return target_period;
            }
        }

        if self.reload_flag {
            self.divider
                .reset_reload_value((self.data & (0b0111_0000) >> 4) as u16);
            self.reload_flag = false;
        }

        return current_period;
    }

    pub fn is_muted(&self) -> bool {
        self.muted
    }
}
