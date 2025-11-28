pub struct Divider {
    counter: u8,
    reload_value: u8,
}

impl Divider {
    pub fn new(reload_value: u8) -> Self {
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
}
