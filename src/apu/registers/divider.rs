pub struct Divider {
    counter: u16,
    reload_value: u16,
}

impl Divider {
    pub fn new(reload_value: u16) -> Self {
        Divider {
            counter: 0,
            reload_value: reload_value,
        }
    }

    pub fn clock(&mut self) -> bool {
        if self.counter == 0 {
            self.counter = self.reload_value;
            return true;
        }
        self.counter = self.counter - 1;
        return false;
    }

    pub fn reset_counter(&mut self) {
        self.counter = 0;
    }

    pub fn reset_reload_value(&mut self, reload_value: u16) {
        self.reload_value = reload_value;
    }
}

#[cfg(test)]
mod test {
    use crate::apu::registers::divider::Divider;

    #[test]
    pub fn should_countdown_and_generate_clock() {
        let mut divider = Divider::new(3);

        assert!(divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(divider.clock());
    }

    #[test]
    pub fn should_reset_counter() {
        let mut divider = Divider::new(3);

        assert!(divider.clock());
        assert!(!divider.clock());
        divider.reset_counter();

        assert!(divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(divider.clock());
    }

    #[test]
    pub fn should_reset_reload_value() {
        let mut divider = Divider::new(3);

        assert!(divider.clock());

        divider.reset_reload_value(1);

        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(!divider.clock());
        assert!(divider.clock());
        assert!(!divider.clock());
        assert!(divider.clock());
        assert!(!divider.clock());
    }
}
