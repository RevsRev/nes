//Flags
pub const ZERO_FLAG: u8 = 0b0000_0010;
pub const NEGATIVE_FLAG: u8 = 0b1000_0000;

//Instruction codes
pub const LDA: u8 = 0xA9;
pub const TAX: u8 = 0xAA;
pub const INX: u8 = 0xE8;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub status: u8,
    pub program_counter: u16,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.program_counter = 0;

        loop {
            let opscode = program[self.program_counter as usize];
            self.program_counter += 1;

            match opscode {
                LDA => {
                    let param = program[self.program_counter as usize];
                    self.program_counter += 1;
                    self.lda(param)
                }
                TAX => self.tax(),
                INX => {
                    self.inx();
                }
                0x00 => {
                    return;
                }
                _ => todo!(),
            }
        }
    }

    fn set_status_flag(&mut self, flag: u8, set: bool) {
        self.status = match set {
            true => self.status | flag,
            false => self.status & (!flag),
        }
    }

    fn get_flag(register: u8, flag: u8) -> bool {
        return register & flag == flag;
    }

    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x90, 0x00]);
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xaa_tax_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.register_a = 5;
        cpu.interpret(vec![0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.register_a = 0;
        cpu.interpret(vec![0xaa, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_negative_flag() {
        let mut cpu = CPU::new();
        cpu.register_a = 250;
        cpu.interpret(vec![0xaa, 0x00]);
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xe8_inc() {
        let mut cpu = CPU::new();
        cpu.register_x = 67;
        cpu.interpret(vec![0xe8, 0x00]);
        assert_eq!(cpu.register_x, 68);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xe8_inc_zero_flag() {
        let mut cpu = CPU::new();
        cpu.register_x = 255;
        cpu.interpret(vec![0xe8, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xe8_inc_negative_flag() {
        let mut cpu = CPU::new();
        cpu.register_x = 250;
        cpu.interpret(vec![0xe8, 0x00]);
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }
}
