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

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

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
    pub stack_pointer: u8,
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
            stack_pointer: STACK_RESET,
            memory: [0; 0xFFFF],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;
        self.stack_pointer = STACK_RESET;
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

            let mut increment_program_counter = true;
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

                0x90 => increment_program_counter &= !self.bcc(&opcode.mode),

                0xB0 => increment_program_counter &= !self.bcs(&opcode.mode),

                0xF0 => increment_program_counter &= !self.beq(&opcode.mode),

                0x24 | 0x2C => self.bit(&opcode.mode),

                0x30 => increment_program_counter &= !self.bmi(&opcode.mode),

                0xD0 => increment_program_counter &= !self.bne(&opcode.mode),

                0x10 => increment_program_counter &= !self.bpl(&opcode.mode),

                0x00 => break,

                0x50 => increment_program_counter &= !self.bvc(&opcode.mode),

                0x70 => increment_program_counter &= !self.bvs(&opcode.mode),

                0x18 => self.clc(&opcode.mode),

                0xD8 => self.cld(&opcode.mode),

                0x58 => self.cli(&opcode.mode),

                0xB8 => self.clv(&opcode.mode),

                0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.cmp(&opcode.mode),

                0xE0 | 0xE4 | 0xEC => self.cpx(&opcode.mode),

                0xC0 | 0xC4 | 0xCC => self.cpy(&opcode.mode),

                0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.mode),

                0xCA => self.dex(&opcode.mode),

                0x88 => self.dey(&opcode.mode),

                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),

                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.mode),

                0xE8 => self.inx(),

                0xC8 => self.iny(&opcode.mode),

                0x4C | 0x6C => increment_program_counter &= !self.jmp(&opcode.mode),

                0x20 => increment_program_counter &= !self.jsr(&opcode.mode),

                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }

                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),

                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xAC => self.ldy(&opcode.mode),

                0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(&opcode.mode),

                0xEA => self.nop(&opcode.mode),

                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),

                0x48 => self.pha(&opcode.mode),

                0x08 => self.php(&opcode.mode),

                0x68 => self.pla(&opcode.mode),

                0x28 => self.plp(&opcode.mode),

                0x2A | 0x26 | 0x36 | 0x2E | 0x3E => self.rol(&opcode.mode),

                0x6A | 0x66 | 0x76 | 0x6E | 0x7E => self.ror(&opcode.mode),

                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                0xAA => self.tax(),
                0x00 => return,
                _ => todo!(),
            }

            if increment_program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        return self.mem_read((STACK as u16) + (self.stack_pointer as u16));
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;
        return hi << 8 | lo;
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK as u16) + (self.stack_pointer as u16), data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
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

    fn mem_write_vec(&mut self, addr: u16, program: &Vec<u8>) {
        self.memory[addr as usize..(addr as usize + program.len())].copy_from_slice(&program[..]);
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
        let mut accumulator = self.register_a;
        let mut carry = match accumulator.checked_add(value) {
            Some(_sum) => false,
            None => true,
        };

        accumulator = accumulator.wrapping_add(value);

        if Self::get_flag(self.status, CARRY_FLAG) {
            carry = carry
                | match accumulator.checked_add(1) {
                    Some(_sum) => false,
                    None => true,
                };
            accumulator = accumulator.wrapping_add(1);
        }

        let sign = Self::get_flag(self.register_a, NEGATIVE_FLAG);

        self.register_a = accumulator;

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

    fn bcc(&mut self, mode: &AddressingMode) -> bool {
        if Self::get_flag(self.status, CARRY_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bcs(&mut self, mode: &AddressingMode) -> bool {
        if !Self::get_flag(self.status, CARRY_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn beq(&mut self, mode: &AddressingMode) -> bool {
        if !Self::get_flag(self.status, ZERO_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_status_flag_if_true(ZERO_FLAG, value & self.register_a == 0);
        self.copy_bit_to_status(value, OVERFLOW_FLAG, OVERFLOW_FLAG);
        self.copy_bit_to_status(value, NEGATIVE_FLAG, NEGATIVE_FLAG);
    }

    fn bmi(&mut self, mode: &AddressingMode) -> bool {
        if !Self::get_flag(self.status, NEGATIVE_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bne(&mut self, mode: &AddressingMode) -> bool {
        if Self::get_flag(self.status, ZERO_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bpl(&mut self, mode: &AddressingMode) -> bool {
        if Self::get_flag(self.status, NEGATIVE_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bvc(&mut self, mode: &AddressingMode) -> bool {
        if Self::get_flag(self.status, OVERFLOW_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn bvs(&mut self, mode: &AddressingMode) -> bool {
        if !Self::get_flag(self.status, OVERFLOW_FLAG) {
            return false;
        }

        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);
        self.program_counter = self.program_counter + (value as u16);
        return true;
    }

    fn clc(&mut self, mode: &AddressingMode) {
        self.remove_status_flag_if_true(CARRY_FLAG, true);
    }

    fn cld(&mut self, mode: &AddressingMode) {
        self.remove_status_flag_if_true(DECIMAL_MODE_FLAG, true);
    }

    fn cli(&mut self, mode: &AddressingMode) {
        self.remove_status_flag_if_true(INTERRUPT_DISABLE_FLAG, true);
    }

    fn clv(&mut self, mode: &AddressingMode) {
        self.remove_status_flag_if_true(OVERFLOW_FLAG, true);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        let sub = self.register_a.wrapping_sub(value);

        self.set_status_flag_if_true(CARRY_FLAG, self.register_a >= value);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == value);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        let sub = self.register_x.wrapping_sub(value);

        self.set_status_flag_if_true(CARRY_FLAG, self.register_x >= value);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == value);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        let sub = self.register_y.wrapping_sub(value);

        self.set_status_flag_if_true(CARRY_FLAG, self.register_y >= value);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_y == value);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        let sub = value.wrapping_sub(1);

        self.mem_write(address, sub);

        self.set_status_flag_if_true(ZERO_FLAG, sub == 0);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn dex(&mut self, mode: &AddressingMode) {
        self.register_x = self.register_x.wrapping_sub(1);

        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn dey(&mut self, mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_sub(1);

        self.set_status_flag_if_true(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        self.register_a = self.register_a ^ value;

        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address).wrapping_add(1);
        self.mem_write(address, value);

        self.set_status_flag_if_true(ZERO_FLAG, value == 0);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn iny(&mut self, mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_add(1);
        self.set_status_flag_if_true(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn jmp(&mut self, mode: &AddressingMode) -> bool {
        let pc_jump;
        if mode == &AddressingMode::Indirect {
            pc_jump = self.mem_read_u16(self.program_counter);
        } else {
            let address = self.get_operand_address(mode);
            pc_jump = self.mem_read_u16(address);
        }

        self.program_counter = pc_jump;
        return true;
    }

    fn jsr(&mut self, mode: &AddressingMode) -> bool {
        let address = self.get_operand_address(mode);
        let pc_jump = self.mem_read_u16(address);

        self.stack_push_u16(self.program_counter + 2 - 1);

        self.program_counter = pc_jump;
        return true;
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

    fn ldx(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        self.register_x = value;

        self.set_status_flag_if_true(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        self.register_y = value;

        self.set_status_flag_if_true(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a >> 1;

            self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
            self.set_status_flag_if_true(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return;
        }

        let addr = self.get_operand_address(mode);
        let old = self.mem_read(addr);

        let value = old >> 1;

        self.mem_write(addr, value);

        self.set_status_flag_if_true(ZERO_FLAG, value == 0);
        self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn nop(&mut self, mode: &AddressingMode) {}

    fn ora(&mut self, mode: &AddressingMode) {
        let address = self.get_operand_address(mode);
        let value = self.mem_read(address);

        self.register_a = self.register_a | value;

        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn pha(&mut self, mode: &AddressingMode) {
        self.stack_push(self.register_a);
    }

    fn php(&mut self, mode: &AddressingMode) {
        self.stack_push(self.status);
    }

    fn pla(&mut self, mode: &AddressingMode) {
        self.register_a = self.stack_pop();
        self.set_status_flag_if_true(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag_if_true(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn plp(&mut self, mode: &AddressingMode) {
        self.status = self.stack_pop();
    }

    fn rol(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::Accumulator {
            let old_value = self.register_a;
            let carry = Self::get_flag(self.status, CARRY_FLAG);

            let value = match carry {
                true => (old_value << 1) + 1,
                false => old_value << 1,
            };

            self.register_a = value;

            self.copy_bit_to_status(old_value, NEGATIVE_FLAG, CARRY_FLAG);
            self.set_status_flag_if_true(ZERO_FLAG, value == 0);
            self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));

            return;
        }

        let address = self.get_operand_address(mode);
        let old_value = self.mem_read(address);
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value << 1) + 1,
            false => old_value << 1,
        };

        self.mem_write(address, value);

        self.copy_bit_to_status(old_value, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag_if_true(ZERO_FLAG, value == 0);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn ror(&mut self, mode: &AddressingMode) {
        if mode == &AddressingMode::Accumulator {
            let old_value = self.register_a;
            let carry = Self::get_flag(self.status, CARRY_FLAG);

            let value = match carry {
                true => (old_value >> 1) + NEGATIVE_FLAG,
                false => old_value >> 1,
            };

            self.register_a = value;

            self.copy_bit_to_status(old_value, CARRY_FLAG, CARRY_FLAG);
            self.set_status_flag_if_true(ZERO_FLAG, value == 0);
            self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));

            return;
        }

        let address = self.get_operand_address(mode);
        let old_value = self.mem_read(address);
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value >> 1) + NEGATIVE_FLAG,
            false => old_value >> 1,
        };

        self.mem_write(address, value);

        self.copy_bit_to_status(old_value, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag_if_true(ZERO_FLAG, value == 0);
        self.set_status_flag_if_true(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
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
    fn test_adc_0x69_with_carry_pre_set() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0xFE;
        cpu.status = cpu.status | CARRY_FLAG;
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
    fn test_bmi_0x30_negative_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bmi_0x30_negative_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bne_0xd0_zero_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bne_0xd0_zero_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bpl_0x10_negative_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bpl_0x10_negative_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_not_set() {
        let mut cpu = CPU::new();
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load(vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.run();
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_clc_0x18() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x18, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == 0);
    }

    #[test]
    fn test_cld_0xd8() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xD8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | DECIMAL_MODE_FLAG;
        cpu.run();
        assert!(cpu.status & DECIMAL_MODE_FLAG == 0);
    }

    #[test]
    fn test_cli_0x58() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x58, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | INTERRUPT_DISABLE_FLAG;
        cpu.run();
        assert!(cpu.status & INTERRUPT_DISABLE_FLAG == 0);
    }

    #[test]
    fn test_clv_0xb8() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xb8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.run();
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cmp_0xcd_carry_and_zero() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCD, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_a = 10;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cmp_0xcd_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCD, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_a = 0;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_cpx_0xec_carry_and_zero() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xEC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_x = 10;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cpx_0xec_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xEC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_x = 0;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }
    #[test]
    fn test_cpy_0xcc_carry_and_zero() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_y = 10;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cpy_0xcc_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_y = 0;
        cpu.run();
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_dec_0xce() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCE, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 0);
        cpu.reset();
        cpu.run();
        assert_eq!(cpu.mem_read(0xAAAA), 0xFF);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_dex_0xca() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xCA, 0x00]);
        cpu.reset();
        cpu.register_x = 1;
        cpu.run();
        assert_eq!(cpu.register_x, 0);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
    }

    #[test]
    fn test_dey_0x88() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x88, 0x00]);
        cpu.reset();
        cpu.register_y = 0xF9;
        cpu.run();
        assert_eq!(cpu.register_y, 0xF8);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_eor_0x4d_neg_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4D, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 0b1001_1100);
        cpu.register_a = 0b0001_0111;
        cpu.run();

        assert_eq!(0b1000_1011, cpu.register_a);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_eor_0x4d_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4D, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 0b1001_0111);
        cpu.register_a = 0b1001_0111;
        cpu.run();

        assert_eq!(0, cpu.register_a);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
    }

    #[test]
    fn test_inc_0xee() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xEE, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 250);
        cpu.run();

        let value = cpu.mem_read(0xBAA4);

        assert!(value & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_iny_0xc8() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xC8, 0x00]);
        cpu.reset();
        cpu.register_y = 100;
        cpu.run();

        assert_eq!(101, cpu.register_y);
        assert!(cpu.register_y & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_jmp_0x4c_absolute() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4C, 0xAB, 0xBC]);
        cpu.mem_write(0xBCAB, 0x90);
        cpu.mem_write(0xBCAC, 0x21); // Memory address to jump to (0x2190)
        cpu.mem_write(0x2190, 0xE8); // INX
        cpu.mem_write(0x2191, 0xE8); // INX
        cpu.mem_write(0x2192, 0x00); // BRK
        cpu.reset();
        cpu.run();

        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_jmp_0x6c_indirect() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x6C, 0xAB, 0xBC]); // JMP 0xBCAB
        cpu.mem_write(0xBCAB, 0xE8); // INX
        cpu.mem_write(0xBCAC, 0xE8); // INX
        cpu.mem_write(0xBCAD, 0x00); // BRK
        cpu.reset();
        cpu.run();

        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_jmp_0x6c_indirect_loop() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x6C, 0xAB, 0xBC]);
        //increment x, branch if negative else loop back
        cpu.mem_write_vec(0xBCAB, &vec![0xE8, 0x30, 0x04, 0x6C, 0xAB, 0xBC, 0x00]);
        cpu.reset();
        cpu.run();
        assert_eq!(cpu.register_x, 128);
        cpu.run();
    }

    #[test]
    fn test_jsr_0x20() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x20, 0xAB, 0xBC]);
        cpu.mem_write(0xBCAB, 0x90);
        cpu.mem_write(0xBCAC, 0x21); // Memory address to jump to (0x2190)
        cpu.mem_write(0x2190, 0xE8); // INX
        cpu.mem_write(0x2191, 0xE8); // INX
        cpu.mem_write(0x2192, 0x00); // BRK
        cpu.reset();
        cpu.run();

        assert_eq!(STACK_RESET.wrapping_sub(2) as u8, cpu.stack_pointer);
        assert_eq!(0x8002, cpu.mem_read_u16((STACK_RESET as u16) + STACK - 1));
        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_0xa2_ldx_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xA2, 21, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(21, cpu.register_x)
    }

    #[test]
    fn test_0xa0_ldy_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xA0, 21, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(21, cpu.register_y)
    }

    #[test]
    fn test_lsr_0x4a_accumulator_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4A, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB2;
        cpu.run();
        assert_eq!(0x59, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_lsr_0x4e_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        cpu.run();
        assert_eq!(0x21, cpu.mem_read_u16(0xA2F1));
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
    }

    #[test]
    fn test_ora_0x0d_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0D, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.register_a = 0b1010_0010;
        cpu.mem_write_u16(0xA2F1, 0b0011_1010);
        cpu.run();
        assert_eq!(0b1011_1010, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_pha_0x48_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x48, 0x00]);
        cpu.reset();
        cpu.register_a = 0xAA;
        cpu.run();
        assert_eq!(0xAA, cpu.mem_read((STACK_RESET as u16) + STACK));
    }

    #[test]
    fn test_php_0x08_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x08, 0x00]);
        cpu.reset();
        cpu.status = 0xAC;
        cpu.run();
        assert_eq!(0xAC, cpu.mem_read((STACK_RESET as u16) + STACK));
    }

    #[test]
    fn test_pla_0x68_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x68, 0x00]);
        cpu.reset();
        cpu.stack_push(0x87);
        cpu.run();
        assert_eq!(0x87, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_plp_0x28_absolute_addr() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x28, 0x00]);
        cpu.reset();
        cpu.stack_push(0x87);
        cpu.run();
        assert_eq!(0x87, cpu.status);
    }

    #[test]
    fn test_nop_0xea() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xEA, 0x00]);
        cpu.reset();
        cpu.run();
        assert_eq!(0, cpu.register_a);
    }

    #[test]
    fn test_rol_0x2a_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x2A, 0x00]);
        cpu.reset();
        cpu.status = CARRY_FLAG;
        cpu.register_a = 0b1001_0010;
        cpu.run();
        assert_eq!(0b0010_0101, cpu.register_a);
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_rol_0x2e_absolute() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x2E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b0001_0010);
        cpu.run();
        assert_eq!(0b0010_0100, cpu.mem_read_u16(0xAAAA));
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_ror_0x6a_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x6A, 0x00]);
        cpu.reset();
        cpu.status = CARRY_FLAG;
        cpu.register_a = 0b1001_0010;
        cpu.run();
        assert_eq!(0b1100_1001, cpu.register_a);
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_ror_0x6e_absolute() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x6E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b1001_0011);
        cpu.run();
        assert_eq!(0b0100_1001, cpu.mem_read_u16(0xAAAA));
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
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
