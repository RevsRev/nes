use crate::interrupt::{Interrupt, InterruptImpl};
use crate::opp::{AddressingMode, OpCode};
use crate::trace::{CpuTrace, CpuTraceFormatOptions, CpuTraceFormatter};
use crate::traits::bus::Bus;
use crate::traits::mos_6502_registers::Registers;
use crate::traits::mos_65902::MOS6502;
use crate::traits::tick::Tick;
use crate::traits::tracing::Tracing;
use crate::{opp, traits::mem::Mem};
use indoc::indoc;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

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

const NMI_INTERRUPT_ADDRESS: u16 = 0xFFFA;
const BRK_INTERRUPT_ADDRESS: u16 = 0xFFFE;
const PC_START_ADDRESS: u16 = 0xFFFC;
const HALT_VALUE: u16 = 0x00FF;

pub struct CPU<T: Bus> {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: u8,
    program_counter: u16,
    stack_pointer: u8,
    bus: Rc<RefCell<T>>,

    interrupt: Rc<RefCell<InterruptImpl>>,

    //tracing info
    trace: Option<CpuTrace>,
    trace_cycles: u64,
    trace_pc: u16,
    trace_reg_a: u8,
    trace_reg_x: u8,
    trace_reg_y: u8,
    trace_sp: u8,
    trace_status: u8,
    operand_address: Option<u16>,
    reads: Vec<(u16, u8)>,
    writes: Vec<(u16, u8)>,

    halt: Arc<AtomicBool>,

    next_program_counter: u16,
    total_cycles: u64,
    op_cycles: u8,
}

impl<T: Bus> MOS6502 for CPU<T> {
    fn get_cycles(&self) -> u64 {
        self.total_cycles
    }

    fn set_cycles(&mut self, value: u64) {
        self.total_cycles = value;
    }
}

impl<T: Bus> Registers for CPU<T> {
    fn get_register_a(&self) -> u8 {
        self.register_a
    }

    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
    }

    fn get_register_x(&self) -> u8 {
        self.register_x
    }

    fn set_register_x(&mut self, value: u8) {
        self.register_x = value;
    }

    fn get_register_y(&self) -> u8 {
        self.register_y
    }

    fn set_register_y(&mut self, value: u8) {
        self.register_y = value;
    }

    fn get_status(&self) -> u8 {
        self.status
    }

    fn set_status(&mut self, value: u8) {
        self.status = value;
    }

    fn get_program_counter(&self) -> u16 {
        self.program_counter
    }

    fn set_program_counter(&mut self, value: u16) {
        self.program_counter = value;
    }

    fn get_stack_pointer(&self) -> u8 {
        self.stack_pointer
    }

    fn set_stack_pointer(&mut self, value: u8) {
        self.stack_pointer = value;
    }
}

impl<T: Bus> Tracing for CPU<T> {
    fn take_trace(&mut self) -> Option<CpuTrace> {
        self.trace.take()
    }
}

impl<T: Bus> Mem for CPU<T> {
    fn mem_read(&mut self, addr: u16) -> Result<u8, std::string::String> {
        let read = self.bus.borrow_mut().mem_read(addr)?;
        self.reads.push((addr, read));
        Result::Ok(read)
    }

    fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, std::string::String> {
        let retval = self.bus.borrow_mut().mem_write(addr, data)?;
        self.writes.push((addr, retval));
        Result::Ok(retval)
    }

    fn mem_read_u16(&mut self, addr: u16) -> Result<u16, std::string::String> {
        let lo = self.mem_read(addr)? as u16;
        let hi = self.mem_read(addr + 1)? as u16;
        Result::Ok((hi << 8) | lo)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) -> Result<u16, std::string::String> {
        let mut hi = (data >> 8) as u8;
        let mut lo = (data & 0xFF) as u8;
        lo = self.mem_write(addr, lo)?;
        hi = self.mem_write(addr + 1, hi)?;
        Result::Ok(((hi as u16) << 8) | (lo as u16))
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
        if (cycles == 0) {
            return;
        }

        self.total_cycles += cycles as u64;
        self.bus.borrow_mut().tick(cycles);
    }
}

impl<T: Bus> CPU<T> {
    pub fn new(
        bus: Rc<RefCell<T>>,
        interrupt: Rc<RefCell<InterruptImpl>>,
        halt: Arc<AtomicBool>,
    ) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus: bus,
            interrupt: interrupt,
            trace: Option::None,
            trace_cycles: 0,
            trace_pc: 0,
            trace_sp: 0,
            trace_status: 0,
            trace_reg_a: 0,
            trace_reg_x: 0,
            trace_reg_y: 0,
            reads: Vec::new(),
            writes: Vec::new(),
            operand_address: Option::None,
            halt: halt,
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
        self.program_counter = self.mem_read_u16(PC_START_ADDRESS).unwrap();
        self.total_cycles = 0;
    }

    pub fn load_with_start_address(&mut self, start_address: u16, program: Vec<u8>) {
        self.mem_write_vec(start_address, &program);
        self.mem_write_u16(0xFFFC, start_address);
    }

    fn store_trace(&mut self, op: &OpCode) {
        self.trace = Option::Some(CpuTrace {
            cpu_cycles: self.trace_cycles,
            pc: self.trace_pc,
            op_code: (*op).to_owned(),
            absolute_address: self.operand_address,
            register_a: self.trace_reg_a,
            register_x: self.trace_reg_x,
            register_y: self.trace_reg_y,
            status: self.trace_status,
            stack: self.trace_sp,
            reads: self.reads.clone(),
            writes: self.writes.clone(),
        });

        self.operand_address = Option::None;
        self.reads.clear();
        self.writes.clear();
        self.trace_sp = 0;
        self.trace_pc = 0;
        self.trace_status = 0;
        self.trace_reg_a = 0;
        self.trace_reg_x = 0;
        self.trace_reg_y = 0;
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: FnMut(&mut CPU<T>),
    {
        loop {
            match self.step_with_callback(&mut callback) {
                Ok(b) => match b {
                    true => {}
                    false => break,
                },
                Err(s) => {
                    return Err(s);
                }
            }
        }
        Result::Ok(())
    }

    pub fn step_with_callback<F>(&mut self, callback: &mut F) -> Result<bool, String>
    where
        F: FnMut(&mut CPU<T>),
    {
        let should_nmi = self.interrupt.borrow_mut().take_nmi();

        let op = self.mem_read(self.program_counter);

        let opcode = op.and_then(|o| {
            let option = opp::OPCODES_MAP.get(&o);
            match option {
                Some(o) => Result::Ok(o),
                None => Result::Err(format!("Opcode {:x} is not recognised", o)),
            }
        })?;

        self.reads.clear();
        self.writes.clear();
        self.trace_cycles = self.total_cycles;
        self.trace_pc = self.program_counter;
        self.trace_sp = self.stack_pointer;
        self.trace_status = self.status;
        self.trace_reg_a = self.register_a;
        self.trace_reg_x = self.register_x;
        self.trace_reg_y = self.register_y;

        self.op_cycles = opcode.cycles;
        self.program_counter += 1;
        self.next_program_counter = self.program_counter + (opcode.len - 1) as u16;

        self.tick(self.op_cycles);

        let execution_result: Result<(), String> = match opcode.code {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => self.adc(&opcode.mode),

            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),

            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => self.asl(&opcode.mode),

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

            0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => self.lda(&opcode.mode),

            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),

            0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => self.lax(&opcode.mode),

            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(&opcode.mode),

            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(&opcode.mode),

            0xEA | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => self.nop(&opcode.mode),

            0x04 | 0x14 | 0x34 | 0x44 | 0x54 | 0x64 | 0x74 | 0x80 | 0x82 | 0x89 | 0xC2 | 0xD4
            | 0xE2 | 0xF4 => self.dop(&opcode.mode),

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

            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 | 0xEB => self.sbc(&opcode.mode),

            0x38 => self.sec(&opcode.mode),

            0xF8 => self.sed(&opcode.mode),

            0x78 => self.sei(&opcode.mode),

            0x07 | 0x17 | 0x0F | 0x1F | 0x1B | 0x03 | 0x13 => self.slo(&opcode.mode),

            0x47 | 0x57 | 0x4F | 0x5F | 0x5B | 0x43 | 0x53 => self.sre(&opcode.mode),

            0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => self.sta(&opcode.mode),

            0x86 | 0x96 | 0x8E => self.stx(&opcode.mode),

            0x84 | 0x94 | 0x8C => self.sty(&opcode.mode),

            0xAA => self.tax(),

            0xA8 => self.tay(),

            0xBA => self.tsx(),

            0x8A => self.txa(),

            0x9A => self.txs(),

            0x98 => self.tya(),

            0x00 => {
                let break_result = self.brk(&opcode.mode);
                break_result.map(|v| {
                    if v {
                        self.halt.store(true, Ordering::Relaxed);
                    }
                    ()
                })
            }
            _ => todo!(),
        };

        match execution_result {
            Ok(_) => {}
            Err(s) => {
                return Err(self.format_fatal_error(opcode, s));
            }
        }

        self.store_trace(&opcode);
        callback(self);

        if self.halt.load(Ordering::Relaxed) {
            return Ok(false);
        }

        if !Self::get_flag(self.status, INTERRUPT_DISABLE_FLAG)
            && self.interrupt.borrow_mut().poll_irq()
        {
            self.stack_push_u16(self.next_program_counter);
            self.stack_push((self.status & !BREAK_FLAG) | BREAK2_FLAG);

            let pc = self.mem_read_u16(0xFFFE);

            match pc {
                Ok(val) => {
                    self.set_status_flag(INTERRUPT_DISABLE_FLAG, true);
                    self.program_counter = val;
                    self.tick(7);
                }
                Err(s) => {
                    return Err(self.format_fatal_error(opcode, s));
                }
            }
        } else {
            self.program_counter = self.next_program_counter;
            self.tick(self.op_cycles - opcode.cycles);
        }

        if should_nmi {
            self.interrupt_nmi();
        }

        return Ok(true);
    }

    fn format_fatal_error(&mut self, opcode: &&OpCode, s: String) -> String {
        let registers_and_pointers = format!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.register_a, self.register_x, self.register_y, self.status, self.stack_pointer
        );
        let last_cpu_trace = match &self.trace {
            Some(t) => {
                let fmt_options = CpuTraceFormatOptions {
                    write_break_2_flag: true,
                    write_cpu_cycles: true,
                };
                let trace_formatter = CpuTraceFormatter {
                    options: fmt_options,
                };
                format!("{}", trace_formatter.format(t))
            }
            None => format!("NULL"),
        };
        return format!(
            indoc! {"
                            A fatal error occurred during CPU execution

                            Error was:
                            {}

                            Last successful trace:
                            {}

                            Current execution:
                            {:04X}  {:02X} {:>60}
                            Reads {:?}:
                            Writes {:?}:
                        "},
            s,
            last_cpu_trace,
            self.program_counter,
            opcode.code,
            registers_and_pointers,
            self.reads,
            self.writes
        );
    }

    fn stack_pop(&mut self) -> Result<u8, String> {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read((STACK as u16) + (self.stack_pointer as u16))
    }

    fn stack_pop_u16(&mut self) -> Result<u16, String> {
        let lo = self.stack_pop()? as u16;
        let hi = self.stack_pop()? as u16;
        Result::Ok(hi << 8 | lo)
    }

    fn stack_push(&mut self, data: u8) -> Result<(), String> {
        self.mem_write((STACK as u16) + (self.stack_pointer as u16), data)?;
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        Result::Ok(())
    }

    fn stack_push_u16(&mut self, data: u16) -> Result<(), String> {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.stack_push(hi)?;
        self.stack_push(lo)?;
        Result::Ok(())
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

    fn evaluate_operand(&mut self, mode: &AddressingMode) -> Result<(u16, bool), String> {
        self.evaluate_operand_at_address(mode, self.program_counter)
    }

    fn evaluate_operand_at_address(
        &mut self,
        mode: &AddressingMode,
        begin: u16,
    ) -> Result<(u16, bool), String> {
        let result = match mode {
            AddressingMode::Immediate | AddressingMode::Implied => (begin, false),
            AddressingMode::Relative => {
                let value = self.mem_read(begin)? as i8;
                let addr = begin.wrapping_add(1).wrapping_add(value as u16);
                (addr, Self::page_boundary_crossed(begin, addr))
            }
            AddressingMode::ZeroPage => (self.mem_read(begin)? as u16, false),
            AddressingMode::Absolute => (self.mem_read_u16(begin)?, false),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(begin)?;
                let addr = pos.wrapping_add(self.register_x) as u16;
                (addr, false)
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(begin)?;
                let addr = pos.wrapping_add(self.register_y) as u16;
                (addr, false)
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(begin)?;
                let addr = base.wrapping_add(self.register_x as u16);
                (addr, Self::page_boundary_crossed(base, addr))
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(begin)?;
                let addr = base.wrapping_add(self.register_y as u16);
                (addr, Self::page_boundary_crossed(base, addr))
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(begin)?;
                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16)?;
                let hi = self.mem_read(ptr.wrapping_add(1) as u16)?;
                ((hi as u16) << 8 | (lo as u16), false)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(begin)?;
                let lo = self.mem_read(base as u16)?;
                let hi = self.mem_read(base.wrapping_add(1) as u16)?;
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                (deref, Self::page_boundary_crossed(deref, deref_base))
            }
            AddressingMode::Indirect => {
                let target_addr = self.mem_read_u16(begin)?;

                //Nasty bug for indirect reads on a page boundary!
                //If we are at a page boundry e.g. 0x02FF, then we read the lo from 0x02FF and the hi
                //from 0x0200, i.e. the page address (02) is the same and the index on the page
                //wraps around
                let page = (target_addr >> 8) as u8;
                let index = (target_addr & 0xFF) as u8;
                let first = ((page as u16) << 8) + (index as u16);
                let second = ((page as u16) << 8) + (index.wrapping_add(1) as u16);

                let lo = self.mem_read(first)? as u16;
                let hi = self.mem_read(second)? as u16;

                // let target_addr = self.mem_read_u16(begin);
                ((hi << 8) | lo, false)
            }
            AddressingMode::Accumulator => panic!("Unsupported op code 'Accumulator'"),
        };
        self.operand_address = Option::Some(result.0);
        Result::Ok(result)
    }

    fn page_boundary_crossed(old_address: u16, new_address: u16) -> bool {
        let old_high = old_address >> 8;
        let new_high = new_address >> 8;
        old_high != new_high
    }

    fn adc(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

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
        Result::Ok(())
    }

    fn and(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a & value;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn asl(&mut self, mode: &AddressingMode) -> Result<(), String> {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a << 1;

            self.set_status_flag(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
            self.set_status_flag(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return Result::Ok(());
        }

        let addr = self.evaluate_operand(mode)?.0;
        let old = self.mem_read(addr)?;

        let value = old << 1;

        self.mem_write(addr, value)?;

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.copy_bit_to_status(old, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn sta(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let addr = self.evaluate_operand(mode)?.0;
        self.mem_write(addr, self.register_a)?;
        Result::Ok(())
    }

    fn bcc(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if Self::get_flag(self.status, CARRY_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bcs(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if !Self::get_flag(self.status, CARRY_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn beq(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if !Self::get_flag(self.status, ZERO_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bit(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let addr = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(addr)?;

        self.set_status_flag(ZERO_FLAG, value & self.register_a == 0);
        self.copy_bit_to_status(value, OVERFLOW_FLAG, OVERFLOW_FLAG);
        self.copy_bit_to_status(value, NEGATIVE_FLAG, NEGATIVE_FLAG);
        Result::Ok(())
    }

    fn bmi(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if !Self::get_flag(self.status, NEGATIVE_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bne(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if Self::get_flag(self.status, ZERO_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bpl(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if Self::get_flag(self.status, NEGATIVE_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bvc(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if Self::get_flag(self.status, OVERFLOW_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn bvs(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;

        if !Self::get_flag(self.status, OVERFLOW_FLAG) {
            return Result::Ok(());
        }

        if eval.1 {
            self.op_cycles += 1;
        }

        self.op_cycles += 1;
        self.next_program_counter = eval.0;
        Result::Ok(())
    }

    fn clc(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.set_status_flag(CARRY_FLAG, false);
        Result::Ok(())
    }

    fn cld(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.set_status_flag(DECIMAL_MODE_FLAG, false);
        Result::Ok(())
    }

    fn cli(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.set_status_flag(INTERRUPT_DISABLE_FLAG, false);
        Result::Ok(())
    }

    fn clv(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.set_status_flag(OVERFLOW_FLAG, false);
        Result::Ok(())
    }

    fn cmp(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        let sub = self.register_a.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_a >= value);
        self.set_status_flag(ZERO_FLAG, self.register_a == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn cpx(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?;

        let sub = self.register_x.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_x >= value);
        self.set_status_flag(ZERO_FLAG, self.register_x == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn cpy(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?;

        let sub = self.register_y.wrapping_sub(value);

        self.set_status_flag(CARRY_FLAG, self.register_y >= value);
        self.set_status_flag(ZERO_FLAG, self.register_y == value);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn dec(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?;

        let sub = value.wrapping_sub(1);

        self.mem_write(address, sub)?;

        self.set_status_flag(ZERO_FLAG, sub == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn dcp(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?;

        let sub = value.wrapping_sub(1);

        self.mem_write(address, sub)?;

        let diff = self.register_a.wrapping_sub(sub);

        self.set_status_flag(CARRY_FLAG, self.register_a >= sub);
        self.set_status_flag(ZERO_FLAG, self.register_a == sub);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(diff, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn dex(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.register_x = self.register_x.wrapping_sub(1);

        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn dey(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.register_y = self.register_y.wrapping_sub(1);

        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn eor(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a ^ value;

        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn inc(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?.wrapping_add(1);
        self.mem_write(address, value)?;

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn inx(&mut self) -> Result<(), String> {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn iny(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.register_y = self.register_y.wrapping_add(1);
        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn isb(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.mem_read(address)?.wrapping_add(1);
        self.mem_write(address, value)?;

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
        Result::Ok(())
    }

    fn jmp(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let pc_jump = self.evaluate_operand(mode)?.0;
        self.next_program_counter = pc_jump;
        Result::Ok(())
    }

    fn jsr(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let pc_jump = self.evaluate_operand(mode)?.0;

        self.stack_push_u16(self.program_counter + 2 - 1)?;

        self.next_program_counter = pc_jump;
        Result::Ok(())
    }

    fn lda(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = value;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn ldx(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_x = value;

        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn lax(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = value;
        self.register_x = value;

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn ldy(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_y = value;

        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn lsr(&mut self, mode: &AddressingMode) -> Result<(), String> {
        if *mode == AddressingMode::Accumulator {
            let old = self.register_a;
            self.register_a = self.register_a >> 1;

            self.set_status_flag(ZERO_FLAG, self.register_a == 0);
            self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
            self.set_status_flag(
                NEGATIVE_FLAG,
                Self::get_flag(self.register_a, NEGATIVE_FLAG),
            );
            return Result::Ok(());
        }

        let addr = self.evaluate_operand(mode)?.0;
        let old = self.mem_read(addr)?;

        let value = old >> 1;

        self.mem_write(addr, value)?;

        self.set_status_flag(ZERO_FLAG, value == 0);
        self.copy_bit_to_status(old, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn nop(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let addr = self.evaluate_operand(mode)?.0;
        self.mem_read(addr)?;
        Result::Ok(())
    }

    fn dop(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let addr = self.evaluate_operand(mode)?.0;
        self.mem_read(addr)?;
        Result::Ok(())
    }

    fn top(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let addr = self.evaluate_operand(mode)?;
        self.mem_read(addr.0)?;
        if addr.1 {
            self.op_cycles += 1
        }
        Result::Ok(())
    }

    fn ora(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let eval = self.evaluate_operand(mode)?;
        let addr = eval.0;
        let value = self.mem_read(addr)?;

        if eval.1 {
            self.op_cycles += 1;
        }

        self.register_a = self.register_a | value;

        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn pha(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.stack_push(self.register_a)?;
        Result::Ok(())
    }

    fn php(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.stack_push(self.status | BREAK_FLAG | BREAK2_FLAG)?;
        Result::Ok(())
    }

    fn pla(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.register_a = self.stack_pop()?;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn plp(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.status = self.stack_pop()? & !(BREAK_FLAG) | BREAK2_FLAG;
        Result::Ok(())
    }

    fn rol(&mut self, mode: &AddressingMode) -> Result<(), String> {
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

            return Result::Ok(());
        }

        let address = self.evaluate_operand(mode)?.0;
        let old_value = self.mem_read(address)?;
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value << 1) + 1,
            false => old_value << 1,
        };

        self.mem_write(address, value)?;

        self.copy_bit_to_status(old_value, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn ror(&mut self, mode: &AddressingMode) -> Result<(), String> {
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

            return Result::Ok(());
        }

        let address = self.evaluate_operand(mode)?.0;
        let old_value = self.mem_read(address)?;
        let carry = Self::get_flag(self.status, CARRY_FLAG);

        let value = match carry {
            true => (old_value >> 1) + NEGATIVE_FLAG,
            false => old_value >> 1,
        };

        self.mem_write(address, value)?;

        self.copy_bit_to_status(old_value, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, value == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn rla(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let orig_value = self.mem_read(address)?;
        let value = (orig_value << 1) | (self.status & CARRY_FLAG);

        self.mem_write(address, value)?;
        self.register_a = self.register_a & value;

        self.copy_bit_to_status(orig_value, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn rra(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let orig_value = self.mem_read(address)?;

        let should_carry = Self::get_flag(orig_value, CARRY_FLAG);

        let value = orig_value >> 1 | ((self.status & CARRY_FLAG) << 7);
        self.mem_write(address, value)?;

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
        Result::Ok(())
    }

    fn rti(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.status = self.stack_pop()? | BREAK2_FLAG;
        self.next_program_counter = self.stack_pop_u16()?;
        Result::Ok(())
    }

    fn rts(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.next_program_counter = self.stack_pop_u16()? + 1;
        Result::Ok(())
    }

    fn brk(&mut self, _mode: &AddressingMode) -> Result<bool, String> {
        self.stack_push_u16(self.program_counter.wrapping_add(1))?;
        self.stack_push(self.status | (BREAK_FLAG & BREAK2_FLAG))?;

        self.next_program_counter = self.mem_read_u16(BRK_INTERRUPT_ADDRESS)?;
        if self.next_program_counter == HALT_VALUE {
            return Result::Ok(true);
        }

        //we do this after the return check, because it's easier to test and doesn't make a
        //difference when we exit :)
        self.set_status_flag(INTERRUPT_DISABLE_FLAG, true);
        Result::Ok(false)
    }

    fn sax(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let value = self.register_x & self.register_a;

        self.mem_write(address, value)?;

        //Even though these flags are documented, they don't get updated (the docs are wrong)
        // self.set_status_flag(ZERO_FLAG, value == 0);
        // self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn sbc(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let c = match Self::get_flag(self.status, CARRY_FLAG) {
            true => 1,
            false => 0,
        };

        let a = self.register_a;
        let eval = self.evaluate_operand(mode)?;
        let address = eval.0;
        let value = self.mem_read(address)?;

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
        Result::Ok(())
    }

    fn sec(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.status = self.status | CARRY_FLAG;
        Result::Ok(())
    }

    fn sed(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.status = self.status | DECIMAL_MODE_FLAG;
        Result::Ok(())
    }

    fn sei(&mut self, _mode: &AddressingMode) -> Result<(), String> {
        self.status = self.status | INTERRUPT_DISABLE_FLAG;
        Result::Ok(())
    }

    fn slo(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let original = self.mem_read(address)?;
        let value = original << 1;
        self.mem_write(address, value)?;

        let result = self.register_a | value;

        self.register_a = result;

        self.copy_bit_to_status(original, NEGATIVE_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, result == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn sre(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        let original = self.mem_read(address)?;
        let value = original >> 1;

        self.mem_write(address, value)?;
        let result = self.register_a ^ value;

        self.register_a = result;

        self.copy_bit_to_status(original, CARRY_FLAG, CARRY_FLAG);
        self.set_status_flag(ZERO_FLAG, result == 0);
        self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG));
        Result::Ok(())
    }

    fn stx(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        self.mem_write(address, self.register_x)?;
        Result::Ok(())
    }

    fn sty(&mut self, mode: &AddressingMode) -> Result<(), String> {
        let address = self.evaluate_operand(mode)?.0;
        self.mem_write(address, self.register_y)?;
        Result::Ok(())
    }

    fn tax(&mut self) -> Result<(), String> {
        self.register_x = self.register_a;
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn tay(&mut self) -> Result<(), String> {
        self.register_y = self.register_a;
        self.set_status_flag(ZERO_FLAG, self.register_y == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_y, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn tsx(&mut self) -> Result<(), String> {
        self.register_x = self.stack_pointer;
        self.set_status_flag(ZERO_FLAG, self.register_x == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_x, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn txa(&mut self) -> Result<(), String> {
        self.register_a = self.register_x;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn txs(&mut self) -> Result<(), String> {
        self.stack_pointer = self.register_x;
        Result::Ok(())
    }

    fn tya(&mut self) -> Result<(), String> {
        self.register_a = self.register_y;
        self.set_status_flag(ZERO_FLAG, self.register_a == 0);
        self.set_status_flag(
            NEGATIVE_FLAG,
            Self::get_flag(self.register_a, NEGATIVE_FLAG),
        );
        Result::Ok(())
    }

    fn interrupt_nmi(&mut self) -> Result<(), String> {
        self.stack_push_u16(self.program_counter)?;
        let status = (self.status.clone() | BREAK2_FLAG) & !BREAK_FLAG;

        self.stack_push(status)?;
        self.status = self.status | INTERRUPT_DISABLE_FLAG;

        self.total_cycles = self.total_cycles + 7;
        self.bus.borrow_mut().tick(7);
        self.program_counter = self.mem_read_u16(NMI_INTERRUPT_ADDRESS)?;
        Result::Ok(())
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
            mem[BRK_INTERRUPT_ADDRESS as usize] = (HALT_VALUE & 0xFF) as u8;
            mem[(BRK_INTERRUPT_ADDRESS + 1) as usize] = (HALT_VALUE >> 8) as u8;

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
        fn mem_read(&mut self, addr: u16) -> Result<u8, String> {
            Result::Ok(self.memory[addr as usize])
        }

        fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, String> {
            self.memory[addr as usize] = data;
            Result::Ok(0)
        }
    }

    #[test]
    fn test_adc_0x69_carry_no_overflow_or_negative() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x0E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x86, cpu.mem_read_u16(0xA2F1).unwrap());
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x80F0, vec![0x90, 0x0E]);
        cpu.mem_write_vec(0x8100, &vec![0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
        assert_eq!(14, cpu.bus.borrow().cycles);
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x90, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
        assert_eq!(9, cpu.bus.borrow().cycles);
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xB0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_beq_0xf0_absolute_addr_zero_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xF0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bit_0x24_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bmi_0x30_negative_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x30, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bne_0xd0_zero_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bne_0xd0_zero_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0xd0, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | ZERO_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bpl_0x10_negative_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bpl_0x10_negative_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | NEGATIVE_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bvc_0x50_overflow_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x50, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(240, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_bvs_0x70_overflow_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        cpu.load_with_start_address(0x8000, vec![0x70, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.mem_read(0x00A1).unwrap());
    }

    #[test]
    fn test_clc_0x18() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x18, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & CARRY_FLAG == 0);
    }

    #[test]
    fn test_cld_0xd8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xD8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | DECIMAL_MODE_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & DECIMAL_MODE_FLAG == 0);
    }

    #[test]
    fn test_cli_0x58() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x58, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | INTERRUPT_DISABLE_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & INTERRUPT_DISABLE_FLAG == 0);
    }

    #[test]
    fn test_clv_0xb8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xb8, 0x00]);
        cpu.reset();
        cpu.status = cpu.status | OVERFLOW_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & OVERFLOW_FLAG == 0);
    }

    #[test]
    fn test_cmp_0xcd_carry_and_zero() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xCE, 0xAA, 0xAA, 0x00]);
        cpu.mem_write(0xAAAA, 0);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xAAAA).unwrap(), 0xFF);
        assert!(cpu.status & ZERO_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == NEGATIVE_FLAG);
    }

    #[test]
    fn test_dex_0xca() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xEE, 0xA4, 0xBA, 0x00]);
        cpu.reset();
        cpu.mem_write(0xBAA4, 250);
        let _ = cpu.run_with_callback(|_| {});

        let value = cpu.mem_read(0xBAA4).unwrap();

        assert!(value & NEGATIVE_FLAG == NEGATIVE_FLAG);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_iny_0xc8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x20, 0xAB, 0xBC]);
        cpu.mem_write(0xBCAB, 0xE8); // INX
        cpu.mem_write(0xBCAC, 0xE8); // INX
        cpu.mem_write(0xBCAD, 0x00); // BRK
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(STACK_RESET.wrapping_sub(5) as u8, cpu.stack_pointer); // 2 (jsr) + 3 (brk)
        assert_eq!(
            0x8002,
            cpu.mem_read_u16((STACK_RESET as u16) + STACK - 1).unwrap()
        );
        assert_eq!(2, cpu.register_x);
    }

    #[test]
    fn test_0xa2_ldx_immediate() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xA2, 21, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(21, cpu.register_x)
    }

    #[test]
    fn test_0xa0_ldy_immediate() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xA0, 21, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});

        assert_eq!(21, cpu.register_y)
    }

    #[test]
    fn test_lsr_0x4a_accumulator_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x4E, 0xF1, 0xA2, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xA2F1, 0x43);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x21, cpu.mem_read_u16(0xA2F1).unwrap());
        assert!(NEGATIVE_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
    }

    #[test]
    fn test_ora_0x0d_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x48, 0x00]);
        cpu.reset();
        cpu.register_a = 0xAA;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xAA, cpu.mem_read((STACK_RESET as u16) + STACK).unwrap());
    }

    #[test]
    fn test_php_0x08_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x08, 0x00]);
        cpu.reset();
        cpu.status = 0xAC;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(
            0xAC | BREAK_FLAG | BREAK2_FLAG,
            cpu.mem_read((STACK_RESET as u16) + STACK).unwrap()
        );
    }

    #[test]
    fn test_pla_0x68_absolute_addr() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x28, 0x00]);
        cpu.reset();
        cpu.stack_push(0x87);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x87 & !BREAK_FLAG | BREAK2_FLAG, cpu.status);
    }

    #[test]
    fn test_nop_0xea() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xEA, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0, cpu.register_a);
    }

    #[test]
    fn test_rol_0x2a_accumulator() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x2E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b0001_0010);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b0010_0100, cpu.mem_read_u16(0xAAAA).unwrap());
        assert!(cpu.status & CARRY_FLAG == 0);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_ror_0x6a_accumulator() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x6E, 0xAA, 0xAA, 0x00]);
        cpu.reset();
        cpu.mem_write_u16(0xAAAA, 0b1001_0011);
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0b0100_1001, cpu.mem_read_u16(0xAAAA).unwrap());
        assert!(cpu.status & CARRY_FLAG == CARRY_FLAG);
        assert!(cpu.status & NEGATIVE_FLAG == 0);
        assert!(cpu.status & ZERO_FLAG == 0);
    }

    #[test]
    fn test_brk_0x00() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x00]); //Break immediately, which will take us to 0xAABB
        cpu.reset();
        cpu.mem_write_u16(BRK_INTERRUPT_ADDRESS, 0xAABB); //So we don't quit immediately
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
            BREAK_FLAG & cpu.mem_read((STACK_RESET as u16) + STACK - 3).unwrap()
        );
    }

    //(This also doubles up as another test for BRK)
    #[test]
    fn test_rti_0x40() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x00, 0x00]); //First 0x00 takes us to 0xAABB, second exits program
        cpu.reset();
        cpu.mem_write_u16(BRK_INTERRUPT_ADDRESS, 0xAABB); //So we don't quit immediately
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x38, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(CARRY_FLAG, cpu.status & CARRY_FLAG);
    }

    #[test]
    fn test_sed_0xF8() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xF8, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(DECIMAL_MODE_FLAG, DECIMAL_MODE_FLAG & cpu.status);
    }

    #[test]
    fn test_sei_0x78() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x78, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(INTERRUPT_DISABLE_FLAG, cpu.status & INTERRUPT_DISABLE_FLAG);
    }

    #[test]
    fn test_0x85_sta_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x85, 0xa1, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0x00a1).unwrap() == 240);
    }

    #[test]
    fn test_0x95_sta_zero_page_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x95, 0x9f, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        cpu.register_x = 2;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0x00a1).unwrap() == 240);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x8d, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1).unwrap() == 0xf0);
    }

    #[test]
    fn test_0x9d_sta_absolute_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x9d, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_x = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1).unwrap() == 0xf0);
    }

    #[test]
    fn test_0x99_sta_absolute_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x99, 0x90, 0xdd, 0x00]);
        cpu.reset();
        cpu.register_a = 0xf0;
        cpu.register_y = 0x11;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1).unwrap() == 0xf0);
    }

    #[test]
    fn test_0x81_sta_indirect_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.mem_write(0x00e4, 0xa1);
        cpu.mem_write(0x00e5, 0xdd);
        cpu.load_with_start_address(0x8000, vec![0x81, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_x = 0x04;
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.mem_read(0xdda1).unwrap() == 0xf0);
    }

    #[test]
    fn test_0x91_sta_indirect_Y() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.mem_write(0x00e0, 0xa1);
        cpu.mem_write(0x00e1, 0xdd);
        cpu.load_with_start_address(0x8000, vec![0x91, 0xe0, 0x00]);
        cpu.reset();
        cpu.register_y = 0x04;
        cpu.register_a = 0xf0;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0xf0, cpu.mem_read(0xDDA5).unwrap());
    }

    #[test]
    fn test_stx_0x8e() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x8E, 0xAA, 0xBB, 0x00]);
        cpu.reset();
        cpu.register_x = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xBBAA).unwrap(), 0x05);
    }

    #[test]
    fn test_sty_0x8c() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x8C, 0xAA, 0xBB, 0x00]);
        cpu.reset();
        cpu.register_y = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.mem_read(0xBBAA).unwrap(), 0x05);
    }

    #[test]
    fn test_0xaa_tax_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xA8_tay_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xA8, 0x00]);
        cpu.reset();
        cpu.register_a = 0;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xA8_tay_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xA8, 0x00]);
        cpu.reset();
        cpu.register_a = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xba_tsx() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xBA, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_x == STACK_RESET);
    }

    #[test]
    fn test_0x8a_txa() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x8A, 0x00]);
        cpu.reset();
        cpu.register_x = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_a, 0x05);
    }

    #[test]
    fn test_0x9a_txs() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        assert_eq!(0x01, cpu.mem_read(0xAAAA).unwrap());
        assert_eq!(0xFF, cpu.register_x)
    }

    #[test]
    fn test_0x98_txy() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x98, 0x00]);
        cpu.reset();
        cpu.register_y = 0x05;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_a, 0x05);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xa9, 0x00, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xa9, 0x90, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_0xa5_lda_zero_page() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.mem_write(0x00a1, 240);
        cpu.load_with_start_address(0x8000, vec![0xa5, 0xa1, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 240);
    }

    #[test]
    fn test_0xb5_lda_zero_page_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.mem_write(0xdda1, 0xf0);
        cpu.load_with_start_address(0x8000, vec![0xad, 0xa1, 0xdd, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.register_a == 0xf0);
    }

    #[test]
    fn test_0xbd_lda_absolute_X() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
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
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 255;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xe8_inx_negative_flag() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 250;
        let _ = cpu.run_with_callback(|_| {});
        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_5_ops_working_together() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let bus = BusStub::new();
        let mut cpu = CPU::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(cpu.register_x, 1)
    }
}
