use crate::apu::registers::divider::Divider;

pub enum SweepChangeMethod {
    OnesCompliment,
    TwosCompliment,
}

pub struct Sweep {
    data: u8,
    enabled: bool,
    divider: Divider,
    negate_flag: bool,
    shift_count: u8,
    reload_flag: bool,

    muted: bool,
    target_period: u16,
    change_method: SweepChangeMethod,
}

impl Sweep {
    pub fn new(change_method: SweepChangeMethod) -> Self {
        Sweep {
            data: 0xFF,
            enabled: true,
            divider: Divider::new(0),
            negate_flag: false,
            shift_count: 0,
            reload_flag: false,

            muted: false,
            target_period: 0,
            change_method,
        }
    }

    pub fn write(&mut self, data: u8) -> u8 {
        let old_val = self.data;
        self.data = data;

        self.enabled = data & 0b1000_0000 == 0b1000_0000;
        self.divider
            .reset_reload_value(((data & 0b0111_0000) >> 4) as u16);
        self.negate_flag = data & 0b0000_1000 == 0b0000_1000;
        self.shift_count = data & 0b0000_0111;

        self.reload_flag = true;

        old_val
    }

    pub fn tick(&mut self, current_period: u16) {
        let change_amount = (current_period >> self.shift_count) as i16;

        let signed_change_amount = match (self.negate_flag, &self.change_method) {
            (false, _) => change_amount,
            (true, SweepChangeMethod::OnesCompliment) => (-1 * change_amount).wrapping_sub(1),
            (true, SweepChangeMethod::TwosCompliment) => -1 * change_amount,
        };

        let raw_target = (current_period as i16).wrapping_add(signed_change_amount);

        self.muted = raw_target > 0x7FF || current_period < 8;
        self.target_period = raw_target.clamp(0, 0x7FF) as u16;
    }

    pub fn on_frame(&mut self, current_period: u16) -> u16 {
        let clock = self.divider.clock();

        let retval = if clock && self.enabled && self.shift_count != 0 && !self.muted {
            self.target_period
        } else {
            current_period
        };

        if self.reload_flag {
            self.divider.reset_counter();
            self.reload_flag = false;
        }
        return retval;
    }

    pub fn is_muted(&self) -> bool {
        self.muted
    }
}
