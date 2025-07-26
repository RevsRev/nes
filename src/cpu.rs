use crate::opp::{OpCode, OpCodeBehaviour};
use crate::traits::bus::Bus;
use crate::traits::tick::Tick;
use crate::{opp, traits::mem::Mem};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{collections::HashMap, fmt};
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

const PC_START_ADDRESS: u16 = 0xFFFC;
const INTERRUPT_ADDRESS: u16 = 0xFFFE;
const HALT_VALUE: u16 = 0x00FF;

#[derive(Debug, PartialEq, Display, Clone, Copy)]
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
}

struct CpuTrace {
    pub pc: u16,
    pub op_code: OpCode,
    pub param_1: Option<u8>,
    pub param_2: Option<u8>,
    pub absolute_address: Option<u16>,
    pub mem_value: Option<u8>,
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub stack: u8,
}

impl fmt::Display for CpuTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut raw_text = String::new();

        let mut addr = String::new();

        if let Some(ref p2) = self.param_2 {
            raw_text.push_str(&format!(" {:02X}", p2));
            addr.push_str(&format!("{:02X}", p2));
        }

        if let Some(ref p1) = self.param_1 {
            raw_text = format!(" {:02X}{}", p1, raw_text);
            // raw_text.push_str(&format!(" {:02X}", p1));
            addr.push_str(&format!("{:02X}", p1));
        }

        raw_text = format!("{:02X}{}", self.op_code.code, raw_text);

        let mut assembly_text = String::new();
        assembly_text.push_str(&format!("{:>4} ", self.op_code.mnemonic));

        match (self.op_code.mode, self.op_code.behaviour) {
            (AddressingMode::Immediate, _) => {
                assembly_text.push_str(&format!("#${}", addr));
            }
            (AddressingMode::Accumulator, _) => {
                assembly_text.push_str(&format!("A"));
            }
            (AddressingMode::Indirect_Y, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => a,
                    None => 0,
                };
                let resolved_addr = address.wrapping_sub(self.register_y as u16);

                assembly_text.push_str(&format!(
                    "(${}),Y = {:04X} @ {:04X} = {}",
                    addr, resolved_addr, address, mem_value
                ));
            }
            (AddressingMode::Absolute_Y, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => a,
                    None => 0,
                };
                assembly_text.push_str(&format!("${},Y @ {:04X} = {}", addr, address, mem_value));
            }
            (AddressingMode::Indirect_X, _) => {
                let zero_page_addr = match self.param_1 {
                    Some(p) => p,
                    None => 0,
                };
                let resolved_addr = zero_page_addr.wrapping_add(self.register_x);
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => format!("{:04X}", a),
                    None => format!("NULL"),
                };

                assembly_text.push_str(&format!(
                    "(${},X) @ {:02X} = {} = {}",
                    addr, resolved_addr, address, mem_value
                ));
            }
            (AddressingMode::Absolute_X, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => a,
                    None => 0,
                };
                assembly_text.push_str(&format!("${},X @ {:04X} = {}", addr, address, mem_value));
            }
            (AddressingMode::Indirect, _) => {
                let address = match self.absolute_address {
                    Some(a) => format!("{:04X}", a),
                    None => format!("NULL"),
                };
                assembly_text.push_str(&format!("(${}) = {}", addr, address));
            }
            (AddressingMode::ZeroPage, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                assembly_text.push_str(&format!("${} = {}", addr, mem_value));
            }
            (AddressingMode::ZeroPage_X, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => a,
                    None => 0,
                };
                assembly_text.push_str(&format!("${},X @ {:02X} = {}", addr, address, mem_value));
            }
            (AddressingMode::ZeroPage_Y, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                let address = match self.absolute_address {
                    Some(a) => a,
                    None => 0,
                };
                assembly_text.push_str(&format!("${},Y @ {:02X} = {}", addr, address, mem_value));
            }
            (AddressingMode::Relative, _) => {
                let address = match self.absolute_address {
                    Some(a) => format!("{:04X}", a),
                    None => format!("NULL"),
                };
                assembly_text.push_str(&format!("${}", address));
            }
            (_, OpCodeBehaviour::MemoryWrite) | (_, OpCodeBehaviour::MemoryRead) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                assembly_text.push_str(&format!("${} = {}", addr, mem_value));
            }
            (_, OpCodeBehaviour::ControlFlow) => {
                assembly_text.push_str(&format!("${}", addr));
            }
            (AddressingMode::Absolute, _) => {
                let mem_value = match self.mem_value {
                    Some(v) => format!("{:02X}", v),
                    None => format!("NULL"),
                };
                assembly_text.push_str(&format!("${} = {}", addr, mem_value));
            }
            _ => {}
        };

        write!(
            f,
            "{:04X}  {:<9}{:<33}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.pc,
            raw_text,
            assembly_text,
            self.register_a,
            self.register_x,
            self.register_y,
            self.status,
            self.stack
        )
    }
}

pub struct CPU<T: Bus> {
    pub debug: bool,

    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Rc<RefCell<T>>,

    trace: Option<CpuTrace>,
    halt: Arc<AtomicBool>,
    graceful_shutdown: bool,

    next_program_counter: u16,
    total_cycles: u64,
    op_cycles: u8,
}

impl<T: Bus> Mem for CPU<T> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.borrow_mut().mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.borrow_mut().mem_write(addr, data);
    }

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        self.bus.borrow_mut().mem_read_u16(addr)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.bus.borrow_mut().mem_write_u16(addr, data);
    }
}

impl<T: Bus> fmt::Display for CPU<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:#04x}\t{:#04x}\t A:{:#04x} X:{:#04x} Y:{:#04x} P{:#04x} SP:{:#04x}",
            self.program_counter,
            self.program_counter,
            self.register_a,
            self.register_x,
            self.register_y,
            self.status,
            self.stack_pointer
        )
    }
}

impl<T: Bus> Tick for CPU<T> {
    fn tick(&mut self, cycles: u8) {
        self.total_cycles += cycles as u64;
        self.bus.borrow_mut().tick(cycles);
    }
}

impl<T: Bus> CPU<T> {
    pub fn new(bus: Rc<RefCell<T>>, halt: Arc<AtomicBool>) -> Self {
        CPU {
            debug: false,
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus: bus,
            trace: Option::None,
            halt: halt,
            graceful_shutdown: true,
            next_program_counter: 0,
            total_cycles: 0,
            op_cycles: 0,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = INTERRUPT_DISABLE_FLAG | BREAK2_FLAG;
        self.stack_pointer = STACK_RESET;
        self.program_counter = self.mem_read_u16(PC_START_ADDRESS);
        self.total_cycles = 0;
    }

    pub fn graceful_shutdown(&self) -> bool {
        self.graceful_shutdown
    }

    pub fn load_with_start_address(&mut self, start_address: u16, program: Vec<u8>) {
        self.mem_write_vec(start_address, &program);
        self.mem_write_u16(0xFFFC, start_address);
    }

    pub fn get_trace_str(&self) -> Option<String> {
        match self.trace.as_ref() {
            None => Option::None,
            Some(tr) => Option::Some(tr.to_string()),
        }
    }

    fn store_trace(&mut self, op: &OpCode) {
        let mut param_1 = Option::None;
        let mut param_2 = Option::None;

        let absolute_address = match (&op.mode) {
            AddressingMode::Accumulator => Option::None,
            a => Option::Some(
                self.evaluate_operand_at_address(a, self.program_counter + 1)
                    .0,
            ),
        };

        let mem_value = match absolute_address {
            Some(addr) => Option::Some(self.mem_read(addr)),
            None => Option::None,
        };

        match &op.len {
            2 => param_1 = Option::Some(self.mem_read(self.program_counter + 1)),
            3 => {
                param_1 = Option::Some(self.mem_read(self.program_counter + 1));
                param_2 = Option::Some(self.mem_read(self.program_counter + 2));
            }
            _ => {}
        }

        self.trace = Option::Some(CpuTrace {
            pc: self.program_counter,
            op_code: (*op).to_owned(),
            param_1: param_1,
            param_2: param_2,
            absolute_address: absolute_address,
            mem_value: mem_value,
            register_a: self.register_a,
            register_x: self.register_x,
            register_y: self.register_y,
            status: self.status,
            stack: self.stack_pointer,
        });
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: FnMut(&mut CPU<T>),
    {
        if self.debug {
            println!("Starting NES emulator run loop");
        }

        let ref opcodes: HashMap<u8, &'static opp::OpCode> = *opp::OPCODES_MAP;

        loop {
            let op = self.mem_read(self.program_counter);

            if self.debug {
                println!("{}", self);
            }

            let opcode_result = opcodes.get(&op);

            let opcode = match opcode_result {
                Some(o) => o,
                None => return Err(format!("Opcode {:x} is not recognised", op)),
            };

            self.store_trace(opcode);

            callback(self);
            self.op_cycles = opcode.cycles;
            self.program_counter += 1;
            self.next_program_counter = self.program_counter + (opcode.len - 1) as u16;

            match op {
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

                0x30 => self.bmi(&opcode.mode),

                0xD0 => self.bne(&opcode.mode),

                0x10 => self.bpl(&opcode.mode),

                0x50 => self.bvc(&opcode.mode),

                0x70 => self.bvs(&opcode.mode),

                0x18 => self.clc(&opcode.mode),

                0xD8 => self.cld(&opcode.mode),

                0x58 => self.cli(&opcode.mode),

                0xB8 => self.clv(&opcode.mode),

                0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.cmp(&opcode.mode),

                0xE0 | 0xE4 | 0xEC => self.cpx(&opcode.mode),

                0xC0 | 0xC4 | 0xCC => self.cpy(&opcode.mode),

                0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.mode),

                0xC7 | 0xD7 | 0xCF | 0xDF | 0xDB | 0xC3 | 0xD3 => self.dcp(&opcode.mode),

                0xCA => self.dex(&opcode.mode),

                0x88 => self.dey(&opcode.mode),

                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),

                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.mode),

                0xE8 => self.inx(),

                0xC8 => self.iny(&opcode.mode),

                0xE7 | 0xF7 | 0xEF | 0xFF | 0xFB | 0xE3 | 0xF3 => self.isb(&opcode.mode),

                0x4C | 0x6C => self.jmp(&opcode.mode),

                0x20 => self.jsr(&opcode.mode),

                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }

                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),

                0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => self.lax(&opcode.mode),

                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(&opcode.mode),

                0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(&opcode.mode),

                0xEA | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => self.nop(&opcode.mode),

                0x04 | 0x14 | 0x34 | 0x44 | 0x54 | 0x64 | 0x74 | 0x80 | 0x82 | 0x89 | 0xC2
                | 0xD4 | 0xE2 | 0xF4 => self.dop(&opcode.mode),

                0x0C | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.top(&opcode.mode),

                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),

                0x48 => self.pha(&opcode.mode),

                0x08 => self.php(&opcode.mode),

                0x68 => self.pla(&opcode.mode),

                0x28 => self.plp(&opcode.mode),

                0x2A | 0x26 | 0x36 | 0x2E | 0x3E => self.rol(&opcode.mode),

                0x6A | 0x66 | 0x76 | 0x6E | 0x7E => self.ror(&opcode.mode),

                0x27 | 0x37 | 0x2F | 0x3F | 0x3B | 0x23 | 0x33 => self.rla(&opcode.mode),

                0x67 | 0x77 | 0x6F | 0x7F | 0x7B | 0x63 | 0x73 => self.rra(&opcode.mode),

                0x40 => self.rti(&opcode.mode),

                0x60 => self.rts(&opcode.mode),

                0x87 | 0x97 | 0x83 | 0x8F => self.sax(&opcode.mode),

                0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 | 0xEB => {
                    self.sbc(&opcode.mode)
                }

                0x38 => self.sec(&opcode.mode),

                0xF8 => self.sed(&opcode.mode),

                0x78 => self.sei(&opcode.mode),

                0x07 | 0x17 | 0x0F | 0x1F | 0x1B | 0x03 | 0x13 => self.slo(&opcode.mode),

                0x47 | 0x57 | 0x4F | 0x5F | 0x5B | 0x43 | 0x53 => self.sre(&opcode.mode),

                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                0x86 | 0x96 | 0x8E => self.stx(&opcode.mode),

                0x84 | 0x94 | 0x8C => self.sty(&opcode.mode),

                0xAA => self.tax(),

                0xA8 => self.tay(),

                0xBA => self.tsx(),

                0x8A => self.txa(),

                0x9A => self.txs(),

                0x98 => self.tya(),

                0x00 => {
                    if self.brk(&opcode.mode) {
                        return Result::Ok(());
                    }
                }
                _ => todo!(),
            }

            self.program_counter = self.next_program_counter;
            self.tick(self.op_cycles);

            if self.halt.load(Ordering::Relaxed) {
                self.graceful_shutdown = false;
                break;
            }
        }
        Result::Ok(())
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

    fn mem_write_vec(&mut self, addr: u16, program: &Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(addr + i, program[i as usize]);
        }
    }

    fn copy_bit_to_status(&mut self, data: u8, source_flag: u8, status_flag: u8) {
        let mut set = self.status;
        set = set & !status_flag; //clear the flag bit
        if data & source_flag == source_flag {
            set = set | status_flag;
        }
        self.status = set;
    }

    fn set_status_flag(&mut self, flag: u8, set: bool) {
        if set {
            self.status = self.status | flag;
        } else {
            self.status = self.status & (!flag);
        }
    }

    fn get_flag(register: u8, flag: u8) -> bool {
        return register & flag == flag;
    }

    fn evaluate_operand(&mut self, mode: &AddressingMode) -> (u16, bool) {
        self.evaluate_operand_at_address(mode, self.program_counter)
    }

    fn evaluate_operand_at_address(&mut self, mode: &AddressingMode, begin: u16) -> (u16, bool) {
        match mode {
            AddressingMode::Immediate | AddressingMode::Implied => (begin, false),
            AddressingMode::Relative => {
                let value = self.mem_read(begin) as i8;
                let addr = begin.wrapping_add(1).wrapping_add(value as u16);
                (addr, Self::page_boundary_crossed(begin, addr))
            }
            AddressingMode::ZeroPage => (self.mem_read(begin) as u16, false),
            AddressingMode::Absolute => (self.mem_read_u16(begin), false),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(begin);
                let addr = pos.wrapping_add(self.register_x) as u16;
                (addr, false)
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(begin);
                let addr = pos.wrapping_add(self.register_y) as u16;
                (addr, false)
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(begin);
                let addr = base.wrapping_add(self.register_x as u16);
                (addr, Self::page_boundary_crossed(base, addr))
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(begin);
                let addr = base.wrapping_add(self.register_y as u16);
                (addr, Self::page_boundary_crossed(base, addr))
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(begin);
                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                ((hi as u16) << 8 | (lo as u16), false)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(begin);
                let lo = self.mem_read(base as u16);
                let hi = self.mem_read(base.wrapping_add(1) as u16);
                (
                    ((hi as u16) << 8 | (lo as u16)).wrapping_add(self.register_y as u16),
                    false,
                )
            }
            AddressingMode::Indirect => {
                let target_addr = self.bus.borrow_mut().mem_read_u16(begin);

                //Nasty bug for indirect reads on a page boundary!
                //If we are at a page boundry e.g. 0x02FF, then we read the lo from 0x02FF and the hi
                //from 0x0200, i.e. the page address (02) is the same and the index on the page
                //wraps around
                let page = (target_addr >> 8) as u8;
                let index = (target_addr & 0xFF) as u8;
                let first = ((page as u16) << 8) + (index as u16);
                let second = ((page as u16) << 8) + (index.wrapping_add(1) as u16);

                let lo = self.bus.borrow_mut().mem_read(first) as u16;
                let hi = self.bus.borrow_mut().mem_read(second) as u16;

                // let target_addr = self.mem_read_u16(begin);
                ((hi << 8) | lo, false)
            }
            AddressingMode::Accumulator => panic!("Unsupported op code 'Accumulator'"),
        }
    }

    fn page_boundary_crossed(old_address: u16, new_address: u16) -> bool {
        let old_high = old_address >> 8;
        let new_high = new_address >> 8;
        old_high != new_high
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        let mut result = self.register_a;
        let mut carry = match result.checked_add(value) {
            Some(_sum) => false,
            None => true,
        };

        result = result.wrapping_add(value);

        if Self::get_flag(self.status, CARRY_FLAG) {
            carry = carry
                | match result.checked_add(1) {
                    Some(_sum) => false,
                    None => true,
                };
            result = result.wrapping_add(1);
        }

        let set_overflow = (value ^ result) & (self.register_a ^ result) & NEGATIVE_FLAG != 0;

        self.register_a = result;

        self.set_status_flag(OVERFLOW_FLAG, set_overflow);
        self.set_status_flag(CARRY_FLAG, carry);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn and(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a & value;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn asl(&mut self, mode: &AddressingMode) {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a << 1;

            self.set_status_flag(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
            self.set_status_flag(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return;
        }

        let addr = self.evaluate_operand(mode).0;
        let old = self.mem_read(addr);

        let value = old << 1;

        self.mem_write(addr, value);

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.evaluate_operand(mode).0;
        self.mem_write(addr, self.register_a);
    }

    fn bcc(&mut self, mode: &AddressingMode) {
        if Self::get_flag(self.status, CARRY_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bcs(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, CARRY_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn beq(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, ZERO_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.evaluate_operand(mode).0;
        let value = self.mem_read(addr);

        self.set_status_flag(ZERO_FLAG, value & self.register_a == 0);
        self.copy_bit_to_status(value, OVERFLOW_FLAG, OVERFLOW_FLAG);
        self.copy_bit_to_status(value, NEGATIVE_FLAG, NEGATIVE_FLAG);
    }

    fn bmi(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, NEGATIVE_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bne(&mut self, mode: &AddressingMode) {
        if Self::get_flag(self.status, ZERO_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bpl(&mut self, mode: &AddressingMode) {
        if Self::get_flag(self.status, NEGATIVE_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bvc(&mut self, mode: &AddressingMode) {
        if Self::get_flag(self.status, OVERFLOW_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn bvs(&mut self, mode: &AddressingMode) {
        if !Self::get_flag(self.status, OVERFLOW_FLAG) {
            return;
        }

        let eval = self.evaluate_operand(mode);
        self.op_cycles += 1;
        self.next_program_counter = eval.0;

        if eval.1 {
            self.op_cycles += 2;
        }
    }

    fn clc(&mut self, _mode: &AddressingMode) {
        self.set_status_flag(CARRY_FLAG, false);
    }

    fn cld(&mut self, _mode: &AddressingMode) {
        self.set_status_flag(DECIMAL_MODE_FLAG, false);
    }

    fn cli(&mut self, _mode: &AddressingMode) {
        self.set_status_flag(INTERRUPT_DISABLE_FLAG, false);
    }

    fn clv(&mut self, _mode: &AddressingMode) {
        self.set_status_flag(OVERFLOW_FLAG, false);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        let sub = self.register_a.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_a >= value);
        self.set_status_flag(ZERO_FLAG, self.register_a == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address);

        let sub = self.register_x.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_x >= value);
        self.set_status_flag(ZERO_FLAG, self.register_x == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address);

        let sub = self.register_y.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_y >= value);
        self.set_status_flag(ZERO_FLAG, self.register_y == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address);

        let sub = value.wrapping_sub(1);

        self.mem_write(address, sub);

        self.set_status_flag(ZERO_FLAG, sub == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
    }

    fn dcp(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address);

        let sub = value.wrapping_sub(1);

        self.mem_write(address, sub);

        let diff = self.register_a.wrapping_sub(sub);

        self.set_status_flag(CARRY_FLAG, self.register_a >= sub);
        self.set_status_flag(ZERO_FLAG, self.register_a == sub);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(diff, NEGATIVE_FLAG));
    }

    fn dex(&mut self, _mode: &AddressingMode) {
        self.register_x = self.register_x.wrapping_sub(1);

        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn dey(&mut self, _mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_sub(1);

        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a ^ value;

        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address).wrapping_add(1);
        self.mem_write(address, value);

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn iny(&mut self, _mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_add(1);
        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn isb(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.mem_read(address).wrapping_add(1);
        self.mem_write(address, value);

        let c = match Self::get_flag(self.status, CARRY_FLAG) {
            true => 1,
            false => 0,
        };

        let a = self.register_a;

        let clear_carry = match a.checked_sub(value) {
            Some(_sub) => match _sub.checked_sub(1 - c) {
                Some(_sub2) => false,
                None => true,
            },
            None => true,
        };

        let result = a.wrapping_add(!value).wrapping_add(c); //take advantage of twos compliment
        let set_overflow = (value ^ a) & (a ^ result) & NEGATIVE_FLAG != 0;

        self.register_a = result;
        self.set_status_flag(CARRY_FLAG, !clear_carry);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(OVERFLOW_FLAG, set_overflow);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let pc_jump = self.evaluate_operand(mode).0;
        self.next_program_counter = pc_jump;
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let pc_jump = self.evaluate_operand(mode).0;

        self.stack_push_u16(self.program_counter + 2 - 1);

        self.next_program_counter = pc_jump;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = value;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_x = value;

        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn lax(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = value;
        self.register_x = value;

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_y = value;

        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a >> 1;

            self.set_status_flag(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
            self.set_status_flag(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return;
        }

        let addr = self.evaluate_operand(mode).0;
        let old = self.mem_read(addr);

        let value = old >> 1;

        self.mem_write(addr, value);

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn nop(&mut self, _mode: &AddressingMode) {}

    fn dop(&mut self, _mode: &AddressingMode) {}

    fn top(&mut self, mode: &AddressingMode) {
        let cross_page = self.evaluate_operand(mode).1;
        if cross_page {
            self.op_cycles += 1
        }
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let eval = self.evaluate_operand(mode);
        let addr = eval.0;
        let value = self.mem_read(addr);

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a | value;

        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn pha(&mut self, _mode: &AddressingMode) {
        self.stack_push(self.register_a);
    }

    fn php(&mut self, _mode: &AddressingMode) {
        self.stack_push(self.status | BREAK_FLAG | BREAK2_FLAG);
    }

    fn pla(&mut self, _mode: &AddressingMode) {
        self.register_a = self.stack_pop();
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn plp(&mut self, _mode: &AddressingMode) {
        self.status = self.stack_pop() & !(BREAK_FLAG) | BREAK2_FLAG;
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
            self.set_status_flag(ZERO_FLAG, value == 0);
            self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));

            return;
        }

        let address = self.evaluate_operand(mode).0;
        let old_value = self.mem_read(address);
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value << 1) + 1,
            false => old_value << 1,
        };

        self.mem_write(address, value);

        self.copy_bit_to_status(old_value, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
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
            self.set_status_flag(ZERO_FLAG, value == 0);
            self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));

            return;
        }

        let address = self.evaluate_operand(mode).0;
        let old_value = self.mem_read(address);
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value >> 1) + NEGATIVE_FLAG,
            false => old_value >> 1,
        };

        self.mem_write(address, value);

        self.copy_bit_to_status(old_value, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn rla(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let orig_value = self.mem_read(address);
        let value = (orig_value << 1) | (self.status & CARRY_FLAG);

        self.mem_write(address, value);
        self.register_a = self.register_a & value;

        self.copy_bit_to_status(orig_value, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn rra(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let orig_value = self.mem_read(address);

        let should_carry = Self::get_flag(orig_value, CARRY_FLAG);

        let value = orig_value >> 1 | ((self.status & CARRY_FLAG) << 7);
        self.mem_write(address, value);

        let mut result = self.register_a;
        let mut carry = match result.checked_add(value) {
            Some(_sum) => false,
            None => true,
        };

        result = result.wrapping_add(value);

        if should_carry {
            carry = carry
                | match result.checked_add(1) {
                    Some(_sum) => false,
                    None => true,
                };
            result = result.wrapping_add(1);
        }

        let set_overflow = (value ^ result) & (self.register_a ^ result) & NEGATIVE_FLAG != 0;

        self.register_a = result;

        self.set_status_flag(OVERFLOW_FLAG, set_overflow);
        self.set_status_flag(CARRY_FLAG, carry);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn rti(&mut self, _mode: &AddressingMode) {
        self.status = self.stack_pop() | BREAK2_FLAG;
        self.next_program_counter = self.stack_pop_u16();
    }

    fn rts(&mut self, _mode: &AddressingMode) {
        self.next_program_counter = self.stack_pop_u16() + 1;
    }

    fn brk(&mut self, _mode: &AddressingMode) -> bool {
        self.stack_push_u16(self.program_counter);
        self.stack_push(self.status);

        self.next_program_counter = self.mem_read_u16(INTERRUPT_ADDRESS);
        if self.next_program_counter == HALT_VALUE {
            return true;
        }

        //we do this after the return check, because it's easier to test and doesn't make a
        //difference when we exit :)
        self.set_status_flag(BREAK_FLAG, true);
        return false;
    }

    fn sax(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let value = self.register_x & self.register_a;

        self.mem_write(address, value);

        //Even though these flags are documented, they don't get updated (the docs are wrong)
        // self.set_status_flag(ZERO_FLAG, value == 0);
        // self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let c = match Self::get_flag(self.status, CARRY_FLAG) {
            true => 1,
            false => 0,
        };

        let a = self.register_a;
        let eval = self.evaluate_operand(mode);
        let address = eval.0;
        let value = self.mem_read(address);

        if eval.1 {
            self.op_cycles += 1;
        }

        let clear_carry = match a.checked_sub(value) {
            Some(_sub) => match _sub.checked_sub(1 - c) {
                Some(_sub2) => false,
                None => true,
            },
            None => true,
        };

        let result = a.wrapping_add(!value).wrapping_add(c); //take advantage of twos compliment
        let set_overflow = (value ^ a) & (a ^ result) & NEGATIVE_FLAG != 0;

        self.register_a = result;
        self.set_status_flag(CARRY_FLAG, !clear_carry);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(OVERFLOW_FLAG, set_overflow);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn sec(&mut self, _mode: &AddressingMode) {
        self.status = self.status | CARRY_FLAG;
    }

    fn sed(&mut self, _mode: &AddressingMode) {
        self.status = self.status | DECIMAL_MODE_FLAG;
    }

    fn sei(&mut self, _mode: &AddressingMode) {
        self.status = self.status | INTERRUPT_DISABLE_FLAG;
    }

    fn slo(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let original = self.mem_read(address);
        let value = original << 1;
        self.mem_write(address, value);

        let result = self.register_a | value;

        self.register_a = result;

        self.copy_bit_to_status(original, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, result == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG));
    }

    fn sre(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        let original = self.mem_read(address);
        let value = original >> 1;

        self.mem_write(address, value);
        let result = self.register_a ^ value;

        self.register_a = result;

        self.copy_bit_to_status(original, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, result == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG));
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        self.mem_write(address, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let address = self.evaluate_operand(mode).0;
        self.mem_write(address, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
    use crate::traits::tick::Tick;

    use super::*;

    struct BusStub {
        pub memory: [u8; 0x00010000],
        pub cycles: u64,
    }

    impl BusStub {
        pub fn new() -> Self {
            let mut mem = [0; 0x0001_0000];
            mem[INTERRUPT_ADDRESS as usize] = (HALT_VALUE & 0xFF) as u8;
            mem[(INTERRUPT_ADDRESS + 1) as usize] = (HALT_VALUE >> 8) as u8;

            BusStub {
                memory: mem,
                cycles: 0,
            }
        }
    }

    impl Bus for BusStub {}

    impl Tick for BusStub {
        fn tick(&mut self, cycles: u8) {
            self.cycles += cycles as u64;
        }
    }

    impl Mem for BusStub {
        fn mem_read(&mut self, addr: u16) -> u8 {
            self.memory[addr as usize]
        }

        fn mem_write(&mut self, addr: u16, data: u8) {
            self.memory[addr as usize] = data;
        }
    }

    #[test]
    fn test_adc_0x69_carry_no_overflow_or_negative() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x69, 0x82, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x01, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_adc_0x69_overflow_and_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x69, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x82, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == 0);
        assert!(OVERFLOW_FLAG & cpu.status == OVERFLOW_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_adc_0x69_negative_flag_no_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x69, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x8F;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x92, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == 0);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_adc_0x69_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x69, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0xFF;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x00, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_adc_0x69_with_carry_pre_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x69, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0xFE;
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x00, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }
    #[test]
    fn test_and_0x29() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x29, 0xF1, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB3;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xB1, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_and_0x3d_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x3d, 0xF1, 0xa2, 0x00]);
        cpu.mem_write(0xa2f1, 0x03);
        cpu.reset();
        cpu.register_a = 0xB0;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x00, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
    }

    #[test]
    fn test_asl_0x0a_accumulator_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x0a, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB2;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x64, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
    }

    #[test]
    fn test_asl_0x0e_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x0E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x86, cpu.mem_read_u16(0xA2F1));
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x80F0, vec![0x90, 0x0E]);
        cpu.mem_write_vec(0x8100, &vec![0x85, 0xA1]);
        cpu.reset();
        cpu.debug = true;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
        assert_eq!(8, cpu.bus.borrow().cycles);
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x90, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
        assert_eq!(2, cpu.bus.borrow().cycles);
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bit_0x24_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x24, 0xA1]);
        cpu.reset();
        cpu.mem_write(0xA1, 0b1001_0110);
        cpu.register_a = 0b0110_1001;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_bit_0x2c_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x2c, 0xA1, 0x0F, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0x0FA1, 0b0101_0110);
        cpu.register_a = 0b1110_1001;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & OVERFLOW_FLAG == OVERFLOW_FLAG);
    }

    #[test]
    fn test_bmi_0x30_negative_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bmi_0x30_negative_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bne_0xd0_zero_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bne_0xd0_zero_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bpl_0x10_negative_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bpl_0x10_negative_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1));
    }

    #[test]
    fn test_clc_0x18() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x18, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == 0);
    }

    #[test]
    fn test_cld_0xd8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xD8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | DECIMAL_MODE_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & DECIMAL_MODE_FLAG == 0);
    }

    #[test]
    fn test_cli_0x58() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x58, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | INTERRUPT_DISABLE_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & INTERRUPT_DISABLE_FLAG == 0);
    }

    #[test]
    fn test_clv_0xb8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xb8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cmp_0xcd_carry_and_zero() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCD, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_a = 10;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cmp_0xcd_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCD, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_a = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_cpx_0xec_carry_and_zero() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xEC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_x = 10;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cpx_0xec_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xEC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_x = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }
    #[test]
    fn test_cpy_0xcc_carry_and_zero() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_y = 10;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cpy_0xcc_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCC, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 10);
        cpu.reset();
        cpu.register_y = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_dec_0xce() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCE, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 0);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xAAAA), 0xFF);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_dex_0xca() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xCA, 0x00]);
        cpu.reset();
        cpu.register_x = 1;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 0);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
    }

    #[test]
    fn test_dey_0x88() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x88, 0x00]);
        cpu.reset();
        cpu.register_y = 0xF9;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_y, 0xF8);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_eor_0x4d_neg_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x4D, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 0b1001_1100);
        cpu.register_a = 0b0001_0111;
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(0b1000_1011, cpu.register_a);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_eor_0x4d_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x4D, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 0b1001_0111);
        cpu.register_a = 0b1001_0111;
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(0, cpu.register_a);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == ZERO_FLAG);
    }

    #[test]
    fn test_inc_0xee() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xEE, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 250);
        let _ = cpu.run_with_callback(|_| {});

        let value = cpu.mem_read(0xBAA4);

        assert!(value & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_iny_0xc8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xC8, 0x00]);
        cpu.reset();
        cpu.register_y = 100;
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(101, cpu.register_y);
        assert!(cpu.register_y & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_jmp_0x4c_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x4C, 0xAB, 0xBC]);
        cpu.mem_write(0xBCAB, 0xE8); // INX
        cpu.mem_write(0xBCAC, 0xE8); // INX
        cpu.mem_write(0xBCAD, 0x00); // BRK
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_jmp_0x6c_indirect() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x6C, 0xAB, 0xBC]); // JMP 0xBCAB
        cpu.mem_write(0xBCAB, 0x01); // Lo of real jump location
        cpu.mem_write(0xBCAC, 0x02); // Hi of real jump location
        cpu.mem_write(0x0201, 0xE8); // INX
        cpu.mem_write(0x0202, 0xE8); // INX
        cpu.mem_write(0x0203, 0x00); // BRK
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_jmp_0x6c_indirect_loop() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x6C, 0xAB, 0xBC]);

        cpu.mem_write(0xBCAB, 0x01); // Lo of real jump location
        cpu.mem_write(0xBCAC, 0x02); // Hi of real jump location

        //increment x, branch if negative else loop back
        cpu.mem_write_vec(0x0201, &vec![0xE8, 0x30, 0x04, 0x6C, 0xAB, 0xBC, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 128);
        let _ = cpu.run_with_callback(|_| {});
    }

    #[test]
    fn test_jsr_0x20() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x20, 0xAB, 0xBC]);
        cpu.mem_write(0xBCAB, 0xE8); // INX
        cpu.mem_write(0xBCAC, 0xE8); // INX
        cpu.mem_write(0xBCAD, 0x00); // BRK
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(STACK_RESET.wrapping_sub(5) as u8, cpu.stack_pointer); // 2 (jsr) + 3 (brk)
        assert_eq!(0x8002, cpu.mem_read_u16((STACK_RESET as u16) + STACK - 1));
        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_0xa2_ldx_immediate() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xA2, 21, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(21, cpu.register_x)
    }

    #[test]
    fn test_0xa0_ldy_immediate() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xA0, 21, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(21, cpu.register_y)
    }

    #[test]
    fn test_lsr_0x4a_accumulator_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x4A, 0x00]);
        cpu.reset();
        cpu.register_a = 0xB2;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x59, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_lsr_0x4e_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x4E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x21, cpu.mem_read_u16(0xA2F1));
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
    }

    #[test]
    fn test_ora_0x0d_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x0D, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.register_a = 0b1010_0010;
        cpu.mem_write_u16(0xA2F1, 0b0011_1010);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b1011_1010, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_pha_0x48_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x48, 0x00]);
        cpu.reset();
        cpu.register_a = 0xAA;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xAA, cpu.mem_read((STACK_RESET as u16) + STACK));
    }

    #[test]
    fn test_php_0x08_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x08, 0x00]);
        cpu.reset();
        cpu.status = 0xAC;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(
            0xAC | BREAK_FLAG | BREAK2_FLAG,
            cpu.mem_read((STACK_RESET as u16) + STACK)
        );
    }

    #[test]
    fn test_pla_0x68_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x68, 0x00]);
        cpu.reset();
        cpu.stack_push(0x87);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x87, cpu.register_a);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_php_pla() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x08, 0x68, 0x00]);
        cpu.reset();
        cpu.status = 0x6F;
        cpu.stack_pointer = 0xFB;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x7F, cpu.register_a);
        //assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        //assert!(ZERO_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_plp_0x28_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x28, 0x00]);
        cpu.reset();
        cpu.stack_push(0x87);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x87 & !BREAK_FLAG | BREAK2_FLAG, cpu.status);
    }

    #[test]
    fn test_nop_0xea() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xEA, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.register_a);
    }

    #[test]
    fn test_rol_0x2a_accumulator() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x2A, 0x00]);
        cpu.reset();
        cpu.status = CARRY_FLAG;
        cpu.register_a = 0b1001_0010;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b0010_0101, cpu.register_a);
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_rol_0x2e_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x2E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b0001_0010);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b0010_0100, cpu.mem_read_u16(0xAAAA));
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_ror_0x6a_accumulator() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x6A, 0x00]);
        cpu.reset();
        cpu.status = CARRY_FLAG;
        cpu.register_a = 0b1001_0010;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b1100_1001, cpu.register_a);
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_ror_0x6e_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x6E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b1001_0011);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b0100_1001, cpu.mem_read_u16(0xAAAA));
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_brk_0x00() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.debug = true;
        cpu.load_with_start_address(0x8000, vec![0x00]); //Break immediately, which will take us to 0xAABB
        cpu.reset();
        cpu.mem_write_u16(INTERRUPT_ADDRESS, 0xAABB); //So we don't quit immediately
        cpu.mem_write_u16(0x0000, HALT_VALUE); //Store the HALT value at 0x0000
        cpu.mem_write_vec(
            0xAABB,
            &vec![
                0x08, //push status onto the stack, so we can check that the break flag is set
                0xAD, 0x00, 0x00, //load lo byte of HALT into ACC
                0x8D, 0xFE, 0xFF, //store ACC in 0xFFFE
                0xAD, 0x00, 0x01, //load hi byte of HALT into ACC
                0x8D, 0xFF, 0xFF, //store ACC in 0xFFFF
                0xE8, //INX, another thing to assert on to check program executed as expected.
                0x00, //BRK
            ],
        );
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(1, cpu.register_x);
        assert_eq!(0x00, cpu.register_a); //hi bytes
        assert_eq!(
            BREAK_FLAG,
            BREAK_FLAG & cpu.mem_read((STACK_RESET as u16) + STACK - 3)
        );
    }

    //(This also doubles up as another test for BRK)
    #[test]
    fn test_rti_0x40() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x00, 0x00]); //First 0x00 takes us to 0xAABB, second exits program
        cpu.reset();
        cpu.mem_write_u16(INTERRUPT_ADDRESS, 0xAABB); //So we don't quit immediately
        cpu.mem_write_u16(0x0000, HALT_VALUE); //Store the HALT value at 0x0000
        cpu.mem_write_vec(
            0xAABB,
            &vec![
                0xAD, 0x00, 0x00, //load lo byte of HALT into ACC
                0x8D, 0xFE, 0xFF, //store ACC in 0xFFFE
                0xAD, 0x00, 0x01, //load hi byte of HALT into ACC
                0x8D, 0xFF, 0xFF, //store ACC in 0xFFFF
                0xE8, //INX, another thing to assert on to check program executed as expected.
                0x40,
            ],
        ); //BRK
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(1, cpu.register_x);
        assert_eq!(0x00, cpu.register_a); //hi bytes
    }

    #[test]
    fn test_rts_0x60() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x20, 0xAB, 0xBC, 0xE8]);
        cpu.mem_write(0xBCAB, 0xE8); // INX
        cpu.mem_write(0xBCAC, 0xE8); // INX
        cpu.mem_write(0xBCAD, 0x60); // RTS, what we are testing
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!((STACK_RESET as u8).wrapping_sub(3), cpu.stack_pointer); //3 because the BRK
        //instruction adds sp (2) and status (1) to the stack before we quit
        assert_eq!(3, cpu.register_x);
    }

    #[test]
    fn test_sbc_0xe9_carry_no_overflow_or_negative() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x70, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        cpu.set_status_flag(CARRY_FLAG, true);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x0F, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_sbc_0xe9_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0x80;
        cpu.set_status_flag(CARRY_FLAG, true);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x7F, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == OVERFLOW_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_sbc_0xe9_negative_flag_no_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x8F;
        cpu.set_status_flag(CARRY_FLAG, true);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x8C, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_sbc_0xe9_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x81, 0x00]);
        cpu.reset();
        cpu.register_a = 0x81;
        cpu.set_status_flag(CARRY_FLAG, true);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x00, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_sbc_0xe9_with_carry_cleared() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0x00;
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xFF, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == 0);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_sec_0x38() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x38, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(CARRY_FLAG, cpu.status & CARRY_FLAG);
    }

    #[test]
    fn test_sed_0xF8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xF8, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(DECIMAL_MODE_FLAG, DECIMAL_MODE_FLAG & cpu.status);
    }

    #[test]
    fn test_sei_0x78() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x78, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(INTERRUPT_DISABLE_FLAG, cpu.status & INTERRUPT_DISABLE_FLAG);
    }

    #[test]
    fn test_0x85_sta_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x85, 0xa1, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0x00a1) == 240);
    }

    #[test]
    fn test_0x95_sta_zero_page_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x95, 0x9f, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.register_x = 2;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0x00a1) == 240);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x8d, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x9d_sta_absolute_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x9d, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_x = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x99_sta_absolute_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x99, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_y = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x81_sta_indirect_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.load_with_start_address(0x8000, vec![0x81, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_x = 0x04;
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1) == 0xf0);
    }

    #[test]
    fn test_0x91_sta_indirect_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00e0, 0xa1);
        cpu.mem_write(0x00e1, 0xdd);
        cpu.load_with_start_address(0x8000, vec![0x91, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_y = 0x04;
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xf0, cpu.mem_read(0xDDA5));
    }

    #[test]
    fn test_stx_0x8e() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x8E, 0xAA, 0xBB, 0x00]);
        cpu.reset();
        cpu.register_x = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xBBAA), 0x05);
    }

    #[test]
    fn test_sty_0x8c() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x8C, 0xAA, 0xBB, 0x00]);
        cpu.reset();
        cpu.register_y = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xBBAA), 0x05);
    }

    #[test]
    fn test_0xaa_tax_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xA8_tay_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xA8, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_y, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xA8_tay_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xA8, 0x00]);
        cpu.reset();
        cpu.register_a = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xA8_tay_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xA8, 0x00]);
        cpu.reset();
        cpu.register_a = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xba_tsx() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xBA, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_x == STACK_RESET);
    }

    #[test]
    fn test_0x8a_txa() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x8A, 0x00]);
        cpu.reset();
        cpu.register_x = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_a, 0x05);
    }

    #[test]
    fn test_0x9a_txs() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(
            0x8000,
            vec![
                0x9A, // X TO STACK PTR
                0xA2, 0x01, // LD 0x01 INTO X
                0x8E, 0xAA, 0xAA, // STR X IN 0xAAAA
                0xBA, // LD STACK PTR TO X
                0x00, // BRK
            ],
        );
        cpu.reset();
        cpu.register_x = 0xFF;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x01, cpu.mem_read(0xAAAA));
        assert_eq!(0xFF, cpu.register_x)
    }

    #[test]
    fn test_0x98_txy() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0x98, 0x00]);
        cpu.reset();
        cpu.register_y = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_a, 0x05);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xa9, 0x05, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xa9, 0x00, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xa9, 0x90, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xa5_lda_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00a1, 240);
        cpu.load_with_start_address(0x8000, vec![0xa5, 0xa1, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 240);
    }

    #[test]
    fn test_0xb5_lda_zero_page_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00a1, 240);
        cpu.load_with_start_address(0x8000, vec![0xb5, 0x9f, 0x00]);
        cpu.reset();
        cpu.register_x = 2;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 240);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xad, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xbd_lda_absolute_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xbd, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_x = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xb9_lda_absolute_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xb9, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_y = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xa1_lda_indirect_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xa1, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_x = 0x04;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xb1_lda_indirect_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.mem_write(0x00e0, 0xa1);
        cpu.mem_write(0x00e1, 0xdd);
        cpu.mem_write(0xdda5, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xb1, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_y = 0x04;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xe8_inx() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 67;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 68);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xe8_inx_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 255;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xe8_inx_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_5_ops_working_together() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(Rc::new(RefCell::new(bus)), Arc::new(AtomicBool::new(false)));
        cpu.load_with_start_address(0x8000, vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 1)
    }
}
