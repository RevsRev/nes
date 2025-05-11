use crate::opp;
use std::collections::HashMap;
use strum_macros::Display;

//Flags
pub const CARRY_FLAG: u8 = 0b0000_0001;
pub const ZERO_FLAG: u8 = 0b0000_0010;
pub const INTERRUPT_DISABLE_FLAG: u8 = 0b0000_0100;
pub const DECIMAL_MODE_FLAG: u8 = 0b0000_1000;
pub const BREAK_FLAG: u8 = 0b0001_0000;
pub const BREAK2_FLAG: u8 = 0b0010_0000;
pub const OVERFLOW_FLAG: u8 = 0b0100_0000;
pub const NEGATIVE_FLAG: u8 = 0b1000_0000;

#[derive(Debug, PartialEq, Display)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Accumulator,
    Implied,
    Immediate,
    Indirect,
    Relative,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static opp::OpCode> = *opp::OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;
            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not recognized", code));

            match code {
                0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.mode);
                }

                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.mode);
                }

                0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
                    self.asl(&opcode.mode);
                }

                0x90 => self.bcc(&opcode.mode),

                0xB0 => self.bcs(&opcode.mode),

                0xF0 => self.beq(&opcode.mode),

                0x24 | 0x2C => self.bit(&opcode.mode),

                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }

                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                0xAA => self.tax(),
                0xE8 => self.inx(),
                0x00 => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    fn mem_read(&mut self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        let lo = self.mem_read(addr) as u16;
        let hi = self.mem_read(addr + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let lo = data as u8;
        let hi = (data >> 8) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    fn copy_bit_to_status(&mut self, data: u8, source_flag: u8, status_flag: u8) {
        let mut set = self.status;
        set = set & !status_flag; //clear the flag bit
        if data & source_flag == source_flag {
            set = set | status_flag;
        }
        self.status = set;
    }

    fn remove_status_flag_if_true(&mut self, flag: u8, set: bool) {
        if set {
            self.status = self.status & !flag;
        }
    }

    fn set_status_flag_if_true(&mut self, flag: u8, set: bool) {
        if set {
            self.status = self.status | flag;
        }
    }

    fn get_flag(register: u8, flag: u8) -> bool {
        return register & flag == flag;
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate | AddressingMode::Implied | AddressingMode::Relative => {
                self.program_counter
            }
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let ptr: u8 = base.wrapping_add(self.register_y);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::NoneAddressing
            | AddressingMode::Accumulator
            | AddressingMode::Indirect => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        let carry = match self.register_a.checked_add(value) {
            Some(_sum) => false,
            None => true,
        };

        let sign = Self::get_flag(self.register_a, NEGATIVE_FLAG);

        self.register_a = self.register_a.wrapping_add(value);

        let new_sign = Self::get_flag(self.register_a, NEGATIVE_FLAG);

        self.set_status_flag_if_true(OVERFLOW_FLAG, new_sign != sign);
        self.set_status_flag_if_true(CARRY_FLAG, carry);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a & value;
        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn asl(&mut self, mode: &AddressingMode) {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a << 1;

            self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
            self.set_status_flag_if_true(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return;
        }

        let addr = self.get_operand_address(mode);
        let old = self.mem_read(addr);

        let value = old << 1;

        self.mem_write(addr, value);

        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn bcc(&mut self, mode: &AddressingMode) {
        if Self::get_flag(self.status, CARRY_FLAG) {
            return;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
    }

    fn bcs(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, CARRY_FLAG) {
            return;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
    }

    fn beq(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, ZERO_FLAG) {
            return;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_status_flag_if_true(ZERO_FLAG, value & self.register_a == 0);
        self.copy_bit_to_status(value, OVERFLOW_FLAG, OVERFLOW_FLAG);
        self.copy_bit_to_status(value, NEGATIVE_FLAG, NEGATIVE_FLAG);
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
    use super::*;

    #[test]
    fn test_adc_0x69_carry_no_overflow_or_negative() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x82, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        cpu.run();
        assert_eq!(0x01, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_adc_0x69_overflow_and_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        cpu.run();
        assert_eq!(0x82, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == 0);
        assert!(OVERFLOW_FLAG & cpu.status == OVERFLOW_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_adc_0x69_negative_flag_no_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x8F;
        cpu.run();
        assert_eq!(0x92, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == 0);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_adc_0x69_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0xFF;
        cpu.run();
        assert_eq!(0x00, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == OVERFLOW_FLAG);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_and_0x29() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0xF1, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB3;
        cpu.run();
        assert_eq!(0xB1, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_and_0x3d_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x3d, 0xF1, 0xa2, 0x00]);
        cpu.mem_write(0xa2f1, 0x03);
        cpu.reset();
        cpu.register_a = 0xB0;
        cpu.run();
        assert_eq!(0x00, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
    }

    #[test]
    fn test_asl_0x0a_accumulator_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0a, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB2;
        cpu.run();
        assert_eq!(0x64, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
    }

    #[test]
    fn test_asl_0x0e_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        cpu.run();
        assert_eq!(0x86, cpu.mem_read_u16(0xA2F1));
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(CARRY_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x90, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x90, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bit_0x24_zero_page() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x24, 0xA1]);
        cpu.reset();
        cpu.mem_write(0xA1, 0b1001_0110);
        cpu.register_a = 0b0110_1001;
        cpu.run();
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_bit_0x2c_absolute() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x2c, 0xA1, 0x0F, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0x0FA1, 0b0101_0110);
        cpu.register_a = 0b1110_1001;
        cpu.run();
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & OVERFLOW_FLAG == OVERFLOW_FLAG);
    }

    #[test]
    fn test_0x85_sta_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x85, 0xa1, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert!(cpu.mem_read(0x00a1) == 240);
    }

    #[test]
    fn test_0x95_sta_zero_page_X() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x95, 0x9f, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.register_x = 2;
        cpu.run();
        assert!(cpu.mem_read(0x00a1) == 240);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x8d, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.run();
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x9d_sta_absolute_X() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x9d, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_x = 0x11;
        cpu.run();
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x99_sta_absolute_Y() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x99, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_y = 0x11;
        cpu.run();
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x81_sta_indirect_X() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.load(vec![0x81, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_x = 0x04;
        cpu.register_a = 0xf0;
        cpu.run();
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x91_sta_indirect_Y() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.load(vec![0x91, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_y = 0x04;
        cpu.register_a = 0xf0;
        cpu.run();
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xa9, 0x05, 0x00]);
        cpu.reset();
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xa9, 0x00, 0x00]);
        cpu.reset();
        cpu.run();
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xa9, 0x90, 0x00]);
        cpu.reset();
        cpu.run();
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xa5_lda_zero_page() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00a1, 240);
        cpu.load(vec![0xa5, 0xa1, 0x00]);
        cpu.reset();
        cpu.run();
        assert!(cpu.register_a == 240);
    }

    #[test]
    fn test_0xb5_lda_zero_page_X() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00a1, 240);
        cpu.load(vec![0xb5, 0x9f, 0x00]);
        cpu.reset();
        cpu.register_x = 2;
        cpu.run();
        assert!(cpu.register_a == 240);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let mut cpu = CPU::new();
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load(vec![0xad, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        cpu.run();
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xbd_lda_absolute_X() {
        let mut cpu = CPU::new();
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load(vec![0xbd, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_x = 0x11;
        cpu.run();
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xb9_lda_absolute_Y() {
        let mut cpu = CPU::new();
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load(vec![0xb9, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_y = 0x11;
        cpu.run();
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xa1_lda_indirect_X() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load(vec![0xa1, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_x = 0x04;
        cpu.run();
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xb1_lda_indirect_Y() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load(vec![0xb1, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_y = 0x04;
        cpu.run();
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xaa_tax_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        cpu.run();
        assert_eq!(cpu.register_x, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 0;
        cpu.run();
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 250;
        cpu.run();
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xe8_inx() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 67;
        cpu.run();
        assert_eq!(cpu.register_x, 68);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xe8_inx_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 255;
        cpu.run();
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xe8_inx_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 250;
        cpu.run();
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        cpu.run();
        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        cpu.run();
        assert_eq!(cpu.register_x, 1)
    }
}
