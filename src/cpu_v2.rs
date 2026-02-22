use crate::interrupt::{Interrupt, InterruptImpl};
use crate::opp::{AddressingMode, OPCODES_MAP, OpCode, OpCodeBehaviour};
use crate::trace::{CpuTrace, CpuTraceFormatOptions, CpuTraceFormatter};
use crate::traits::bus::Bus;
use crate::traits::cpu::Cpu;
use crate::traits::mos_6502_registers::Registers;
use crate::traits::mos_65902::{
    BREAK_FLAG, BREAK2_FLAG, BRK_INTERRUPT_ADDRESS, CARRY_FLAG, DECIMAL_MODE_FLAG, HALT_VALUE,
    INTERRUPT_DISABLE_FLAG, MOS6502, NEGATIVE_FLAG, NMI_INTERRUPT_ADDRESS, OVERFLOW_FLAG,
    PC_START_ADDRESS, STACK, STACK_RESET, ZERO_FLAG,
};
use crate::traits::tick::Tick;
use crate::traits::tracing::Tracing;
use crate::{opp, traits::mem::Mem};
use indoc::indoc;
use std::cell::RefCell;
use std::fmt;
use std::ops::Add;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct CpuV2<T: Bus> {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: u8,
    program_counter: u16,
    stack_pointer: u8,
    bus: Rc<RefCell<T>>,

    interrupt: Rc<RefCell<InterruptImpl>>,

    //tracing info
    tracing: bool,
    trace: Option<CpuTrace>,
    trace_cycles: u64,
    trace_pc: u16,
    trace_reg_a: u8,
    trace_reg_x: u8,
    trace_reg_y: u8,
    trace_sp: u8,
    trace_status: u8,
    reads: Vec<(u16, u8)>,
    writes: Vec<(u16, u8)>,

    halt: Arc<AtomicBool>,

    total_cycles: u64,

    //Internal state
    instruction_cycle: u16,
    current_op_cycle: u8,
    current_op_length: u8,
    nmi_at_fetch: bool, //TODO - I wonder if we can get rid of this?
    op: OpCode,
    resolved_addr: u16,
    resolved_mem_read: u8,
    dummy_read: bool,
    should_branch: bool,
    check_for_interrupts: bool,
}

impl<T: Bus> Cpu<T> for CpuV2<T> {
    fn should_halt(&self) -> bool {
        self.halt.load(Ordering::Relaxed)
    }
}

impl<T: Bus> MOS6502<T> for CpuV2<T> {
    fn get_cycles(&self) -> u64 {
        self.total_cycles
    }

    fn set_cycles(&mut self, value: u64) {
        self.total_cycles = value;
    }

    fn step_with_callback<F>(&mut self, mut callback: &mut F) -> Result<bool, String>
    where
        F: for<'a> FnMut(&'a Self),
    {
        loop {
            if self.tick_2()? {
                return Ok(false);
            }
            if self.instruction_cycle == 0 {
                callback(self);
                break;
            }
        }
        return Ok(true);
    }

    fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: for<'a> FnMut(&'a Self),
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

    fn load_with_start_address(&mut self, start_address: u16, program: Vec<u8>) {
        self.mem_write_vec(start_address, &program);
        self.mem_write_u16(0xFFFC, start_address);
    }

    fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = INTERRUPT_DISABLE_FLAG | BREAK2_FLAG;
        self.stack_pointer = STACK_RESET;
        self.program_counter = self
            .bus
            .borrow_mut()
            .mem_read_u16(PC_START_ADDRESS)
            .unwrap();
        self.total_cycles = 0;
    }
}

impl<T: Bus> Registers for CpuV2<T> {
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

impl<T: Bus> Tracing for CpuV2<T> {
    fn take_trace(&mut self) -> Option<CpuTrace> {
        self.trace.take()
    }

    fn peek_trace(&self) -> Option<&CpuTrace> {
        self.trace.as_ref()
    }

    fn format_options(
        &self,
        write_break_2_flag: bool,
        write_cycles: bool,
    ) -> CpuTraceFormatOptions {
        CpuTraceFormatOptions {
            write_break_2_flag,
            write_cpu_cycles: write_cycles,
            reads_offset: 1,
        }
    }

    fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
    }
}

impl<T: Bus> Mem for CpuV2<T> {
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

impl<T: Bus> fmt::Display for CpuV2<T> {
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

impl<T: Bus> Tick for CpuV2<T> {
    fn tick(&mut self) -> Result<(), String> {
        self.tick_2().map(|_| ())
    }
}

impl<T: Bus> CpuV2<T> {
    pub fn new(
        bus: Rc<RefCell<T>>,
        interrupt: Rc<RefCell<InterruptImpl>>,
        halt: Arc<AtomicBool>,
    ) -> Self {
        CpuV2 {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus: bus,
            interrupt: interrupt,
            tracing: false,
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
            halt: halt,
            total_cycles: 0,

            instruction_cycle: 0,
            current_op_cycle: 0,
            current_op_length: 0xFF,
            nmi_at_fetch: false,
            op: **OPCODES_MAP.get(&0x00).unwrap(),
            resolved_addr: 0x0000,
            resolved_mem_read: 0x0000,
            dummy_read: false,
            check_for_interrupts: false,
            should_branch: false,
        }
    }

    fn store_trace(&mut self) {
        if self.tracing {
            self.trace = Option::Some(CpuTrace {
                cpu_cycles: self.trace_cycles,
                pc: self.trace_pc,
                op_code: self.op,
                absolute_address: Some(self.resolved_addr),
                register_a: self.trace_reg_a,
                register_x: self.trace_reg_x,
                register_y: self.trace_reg_y,
                status: self.trace_status,
                stack: self.trace_sp,
                reads: self.reads.clone(),
                writes: self.writes.clone(),
            });
        }

        self.reads.clear();
        self.writes.clear();
        self.trace_sp = 0;
        self.trace_pc = 0;
        self.trace_status = 0;
        self.trace_reg_a = 0;
        self.trace_reg_x = 0;
        self.trace_reg_y = 0;
    }

    fn tick_2(&mut self) -> Result<bool, String> {
        //TODO - Remember to map err for this result at a higher level

        if !self.check_for_interrupts {
            match self.fetch_decode_execute()? {
                true => {
                    self.instruction_cycle = self.instruction_cycle + 1;
                    self.total_cycles = self.total_cycles + 1;
                    return Ok(false);
                }
                false => {
                    self.instruction_cycle = 0;
                    self.check_for_interrupts = true;
                }
            }
        }

        let nmi_interrupt = self.interrupt.borrow().poll_nmi();
        let nmi_in_progress = match nmi_interrupt {
            Some(_) => self.interrupt_nmi(),
            None => Ok(false),
        }?;

        if nmi_in_progress {
            self.instruction_cycle = self.instruction_cycle + 1;
            self.total_cycles = self.total_cycles + 1;
            return Ok(false);
        }

        let oam_stall = self.interrupt.borrow().poll_oam_data();
        let oam_stall_in_progress = match oam_stall {
            Some(page_hi) => {
                let result = self.interrupt_oam_data(page_hi);
                result
            }
            None => Ok(false),
        }?;

        if oam_stall_in_progress {
            self.total_cycles = self.total_cycles + 1;
            return Ok(false);
        }

        if !Self::get_flag(self.status, INTERRUPT_DISABLE_FLAG) {
            let irq_interrupt = self.interrupt.borrow().poll_irq();
            let irq_in_progress = if irq_interrupt {
                self.interrupt_irq()
            } else {
                Ok(false)
            }?;
            if irq_in_progress {
                self.total_cycles = self.total_cycles + 1;
                self.instruction_cycle = self.instruction_cycle + 1;
                return Ok(false);
            }
        }

        self.store_trace();

        self.check_for_interrupts = false;
        self.instruction_cycle = 0;

        if self.halt.load(Ordering::Relaxed) {
            return Ok(true);
        }
        self.tick_2() //We haven't processed anything, so we should start the next cycle
    }

    fn interrupt_irq(&mut self) -> Result<bool, String> {
        match self.instruction_cycle {
            0 => self
                .stack_push((self.program_counter >> 8) as u8)
                .map(|_| true),
            1 => self.stack_push(self.program_counter as u8).map(|_| true),
            2 => self
                .stack_push((self.status & !BREAK_FLAG) | BREAK2_FLAG)
                .map(|_| true),
            3 => {
                self.program_counter = self.mem_read(BRK_INTERRUPT_ADDRESS)? as u16;
                Ok(true)
            }
            4 => {
                self.program_counter = (self.program_counter & 0x00FF)
                    | ((self.mem_read(BRK_INTERRUPT_ADDRESS + 1)? as u16) << 8);
                Ok(true)
            }
            5 => {
                self.status = self.status | INTERRUPT_DISABLE_FLAG;
                Ok(true)
            }
            6 => {
                self.interrupt.borrow_mut().take_irq();
                Ok(true)
            }
            c => Err(format!("Unexpected irq interrupt cycle {}", c)),
        }
    }

    //TODO - Move instruction cycle out of this level
    fn interrupt_oam_data(&mut self, page_hi: u8) -> Result<bool, String> {
        if self.instruction_cycle == 0 && self.total_cycles % 2 == 1 {
            return Ok(true);
        }

        if self.instruction_cycle == 0 {
            self.instruction_cycle = self.instruction_cycle + 1;
            self.total_cycles = self.total_cycles + 1;
            return Ok(true);
        }

        let oam_cycle = self.instruction_cycle - 1;
        if oam_cycle % 2 == 0 {
            self.resolved_mem_read = self.mem_read(((page_hi as u16) << 8) | (oam_cycle / 2))?;
        } else {
            self.mem_write(0x4014, self.resolved_mem_read)?;
        }
        self.instruction_cycle = self.instruction_cycle + 1;

        if oam_cycle < 511 {
            return Ok(true);
        }

        self.interrupt.borrow_mut().take_oam_data();
        self.instruction_cycle = 0;
        Ok(false)
    }

    fn fetch_decode_execute(&mut self) -> Result<bool, String> {
        if self.instruction_cycle == 0 {
            self.fetch()?;

            if self.op.mode == AddressingMode::Implied {
                self.resolved_addr = self.program_counter;
            } else if self.op.mode == AddressingMode::Immediate {
                self.resolved_addr = self.program_counter;
                self.program_counter = self.program_counter + 1;
            } else if self.op.mode == AddressingMode::Relative {
                let result = self.mem_read(self.program_counter).map(|mem| mem as i8);
                match result {
                    Ok(value) => {
                        self.program_counter = self.program_counter + 1;
                        self.resolved_addr = self.program_counter.wrapping_add(value as u16);
                    }
                    Err(s) => return Err(s),
                }
            }
            return Ok(true);
        }
        if self.instruction_cycle < 1 + self.op.mode.base_cycles() as u16 {
            self.evaluate_operand()?;
            return Ok(true);
        }
        if self.dummy_read {
            self.dummy_read = false;
            self.mem_read(self.resolved_addr)?;
            return Ok(true);
        }

        if self.current_op_length == 0xFF {
            self.current_op_length = self.op.cycles - (1 + self.op.mode.base_cycles());
            //Special case. If we have an extra cycle (because of a page cross), we know that
            //memory write instructions ALWAYS do this. So we should do 1 less operation.
            if (self.instruction_cycle != 1 + self.op.mode.base_cycles() as u16)
                && self.op.behaviour == OpCodeBehaviour::MemoryWrite
            {
                self.current_op_length = self.current_op_length - 1;
            }
        }

        //Special case for JMP. Should execute immediately without consuming a cycle
        if self.current_op_cycle == 0 && (self.op.code == 0x4C || self.op.code == 0x6C) {
            self.execute_op()?;
            self.current_op_cycle = self.current_op_cycle + 1;
        }

        if self.current_op_cycle < self.current_op_length {
            self.execute_op()?;
            self.current_op_cycle = self.current_op_cycle + 1;
            return Ok(true);
        }
        if self.should_branch {
            self.program_counter = self.resolved_addr;
            self.should_branch = false;
            return Ok(true);
        }
        self.current_op_length = 0xFF;
        self.current_op_cycle = 0;
        return Ok(false);
    }

    fn execute_op(&mut self) -> Result<(), String> {
        match self.op.code {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => self.adc(),

            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(),

            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => self.asl(),

            0x90 => self.bcc(),

            0xB0 => self.bcs(),

            0xF0 => self.beq(),

            0x24 | 0x2C => self.bit(),

            0x30 => self.bmi(),

            0xD0 => self.bne(),

            0x10 => self.bpl(),

            0x50 => self.bvc(),

            0x70 => self.bvs(),

            0x18 => self.clc(),

            0xD8 => self.cld(),

            0x58 => self.cli(),

            0xB8 => self.clv(),

            0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.cmp(),

            0xE0 | 0xE4 | 0xEC => self.cpx(),

            0xC0 | 0xC4 | 0xCC => self.cpy(),

            0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(),

            0xC7 | 0xD7 | 0xCF | 0xDF | 0xDB | 0xC3 | 0xD3 => self.dcp(),

            0xCA => self.dex(),

            0x88 => self.dey(),

            0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(),

            0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(),

            0xE8 => self.inx(),

            0xC8 => self.iny(),

            0xE7 | 0xF7 | 0xEF | 0xFF | 0xFB | 0xE3 | 0xF3 => self.isb(),

            0x4C | 0x6C => self.jmp(),

            0x20 => self.jsr(),

            0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => self.lda(),

            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(),

            0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => self.lax(),

            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(),

            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => self.lsr(),

            0xEA | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => self.nop(),

            0x04 | 0x14 | 0x34 | 0x44 | 0x54 | 0x64 | 0x74 | 0x80 | 0x82 | 0x89 | 0xC2 | 0xD4
            | 0xE2 | 0xF4 => self.dop(),

            0x0C | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.top(),

            0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(),

            0x48 => self.pha(),

            0x08 => self.php(),

            0x68 => self.pla(),

            0x28 => self.plp(),

            0x2A | 0x26 | 0x36 | 0x2E | 0x3E => self.rol(),

            0x6A | 0x66 | 0x76 | 0x6E | 0x7E => self.ror(),

            0x27 | 0x37 | 0x2F | 0x3F | 0x3B | 0x23 | 0x33 => self.rla(),

            0x67 | 0x77 | 0x6F | 0x7F | 0x7B | 0x63 | 0x73 => self.rra(),

            0x40 => self.rti(),

            0x60 => self.rts(),

            0x87 | 0x97 | 0x83 | 0x8F => self.sax(),

            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 | 0xEB => self.sbc(),

            0x38 => self.sec(),

            0xF8 => self.sed(),

            0x78 => self.sei(),

            0x07 | 0x17 | 0x0F | 0x1F | 0x1B | 0x03 | 0x13 => self.slo(),

            0x47 | 0x57 | 0x4F | 0x5F | 0x5B | 0x43 | 0x53 => self.sre(),

            0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => self.sta(),

            0x86 | 0x96 | 0x8E => self.stx(),

            0x84 | 0x94 | 0x8C => self.sty(),

            0xAA => self.tax(),

            0xA8 => self.tay(),

            0xBA => self.tsx(),

            0x8A => self.txa(),

            0x9A => self.txs(),

            0x98 => self.tya(),

            0x00 => self.brk(),
            _ => todo!(),
        }
    }

    fn evaluate_operand(&mut self) -> Result<(), String> {
        let evaluate_cycle = self.instruction_cycle - 1;
        Ok(match (self.op.mode, evaluate_cycle) {
            (AddressingMode::Accumulator, _)
            | (AddressingMode::Implied, _)
            | (AddressingMode::Immediate, _)
            | (AddressingMode::Relative, _) => {
                return Err(format!(
                    "AddressingMode {} should have been handled already",
                    self.op.mode
                ));
            }

            (AddressingMode::ZeroPage, 0) => {
                self.resolved_addr = self.mem_read(self.program_counter)? as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute, 0) => {
                self.resolved_addr = self.mem_read(self.program_counter)? as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute, 1) => {
                self.resolved_addr =
                    self.resolved_addr | ((self.mem_read(self.program_counter)? as u16) << 8);
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::ZeroPage_X, 0) => {
                self.resolved_mem_read = self.mem_read(self.program_counter)?;
            }
            (AddressingMode::ZeroPage_X, 1) => {
                self.resolved_addr = self.resolved_mem_read.wrapping_add(self.register_x) as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::ZeroPage_Y, 0) => {
                self.resolved_mem_read = self.mem_read(self.program_counter)?;
            }
            (AddressingMode::ZeroPage_Y, 1) => {
                self.resolved_addr = self.resolved_mem_read.wrapping_add(self.register_y) as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute_X, 0) => {
                self.resolved_addr = self.mem_read(self.program_counter)? as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute_X, 1) => {
                let base =
                    self.resolved_addr | ((self.mem_read(self.program_counter)? as u16) << 8);
                self.resolved_addr = base.wrapping_add(self.register_x as u16);
                if Self::page_boundary_crossed(base, self.resolved_addr)
                    || self.op.behaviour == OpCodeBehaviour::MemoryWrite
                {
                    self.dummy_read = true;
                }
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute_Y, 0) => {
                self.resolved_addr = self.mem_read(self.program_counter)? as u16;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Absolute_Y, 1) => {
                let base =
                    self.resolved_addr | ((self.mem_read(self.program_counter)? as u16) << 8);
                self.resolved_addr = base.wrapping_add(self.register_y as u16);
                if Self::page_boundary_crossed(base, self.resolved_addr)
                    || self.op.behaviour == OpCodeBehaviour::MemoryWrite
                {
                    self.dummy_read = true;
                }
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Indirect_X, 0) => {
                self.resolved_mem_read = self
                    .mem_read(self.program_counter)?
                    .wrapping_add(self.register_x)
            }
            (AddressingMode::Indirect_X, 1) => {
                self.mem_read(self.resolved_mem_read as u16)?; //dummy read
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Indirect_X, 2) => {
                let lo = self.mem_read(self.resolved_mem_read as u16)? as u16;
                self.resolved_addr = lo;
            }
            (AddressingMode::Indirect_X, 3) => {
                let hi = self.mem_read(self.resolved_mem_read.wrapping_add(1) as u16)? as u16;
                self.resolved_addr = (hi << 8) | self.resolved_addr;
            }
            (AddressingMode::Indirect_Y, 0) => {
                self.resolved_mem_read = self.mem_read(self.program_counter)?;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Indirect_Y, 1) => {
                let lo = self.mem_read(self.resolved_mem_read as u16)? as u16;
                self.resolved_addr = lo as u16;
            }
            (AddressingMode::Indirect_Y, 2) => {
                let hi = self.mem_read(self.resolved_mem_read.wrapping_add(1) as u16)? as u16;
                let deref_base = hi << 8 | (self.resolved_addr & 0x00FF);
                self.resolved_addr = deref_base.wrapping_add(self.register_y as u16);
                if Self::page_boundary_crossed(self.resolved_addr, deref_base)
                    || self.op.behaviour == OpCodeBehaviour::MemoryWrite
                {
                    self.dummy_read = true;
                }
            }
            (AddressingMode::Indirect, 0) => {
                self.resolved_mem_read = self.mem_read(self.program_counter)?;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Indirect, 1) => {
                self.resolved_addr = (self.mem_read(self.program_counter)? as u16) << 8;
                self.program_counter = self.program_counter + 1;
            }
            (AddressingMode::Indirect, 2) => {
                //Nasty bug for indirect reads on a page boundary!
                //If we are at a page boundry e.g. 0x02FF, then we read the lo from 0x02FF and the hi
                //from 0x0200, i.e. the page address (02) is the same and the index on the page
                //wraps around
                let page = (self.resolved_addr >> 8) as u8;
                let index = self.resolved_mem_read;
                let first = ((page as u16) << 8) + (index as u16);

                self.resolved_addr = self.resolved_addr | self.mem_read(first)? as u16;
            }
            (AddressingMode::Indirect, 3) => {
                let page = (self.resolved_addr >> 8) as u8;
                let index = self.resolved_mem_read;
                let second = ((page as u16) << 8) + (index.wrapping_add(1) as u16);
                self.resolved_addr =
                    (self.resolved_addr & 0x00FF) | ((self.mem_read(second)? as u16) << 8);
            }
            (mode, cycle) => {
                return Err(format!("Unimplemented mode and cycle {}, {}", mode, cycle));
            }
        })
    }

    fn fetch(&mut self) -> Result<(), String> {
        self.reads.clear();
        self.writes.clear();
        self.cache_trace_state();

        let op = self.mem_read(self.program_counter)?;
        self.program_counter = self.program_counter + 1;

        match opp::OPCODES_MAP.get(&op) {
            Some(o) => {
                self.op = **o;
                return Ok(());
            }
            None => return Err(format!("Opcode {:x} is not recognised", op)),
        }
    }

    fn cache_trace_state(&mut self) {
        self.trace_cycles = self.total_cycles;
        self.trace_pc = self.program_counter;
        self.trace_sp = self.stack_pointer;
        self.trace_status = self.status;
        self.trace_reg_a = self.register_a;
        self.trace_reg_x = self.register_x;
        self.trace_reg_y = self.register_y;
    }

    fn format_fatal_error(&mut self, s: String) -> String {
        let registers_and_pointers = format!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.register_a, self.register_x, self.register_y, self.status, self.stack_pointer
        );
        let last_cpu_trace = match &self.trace {
            Some(t) => {
                let fmt_options = CpuTraceFormatOptions {
                    write_break_2_flag: true,
                    write_cpu_cycles: true,
                    reads_offset: 1,
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
            self.op.code,
            registers_and_pointers,
            self.reads,
            self.writes
        );
    }

    fn stack_pop(&mut self) -> Result<u8, String> {
        self.bus.borrow_mut().mem_read(self.program_counter)?; //dummy read that doesn't tick
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

    fn get_flag(register: u8, flag: u8) -> bool {
        return register & flag == flag;
    }

    fn page_boundary_crossed(old_address: u16, new_address: u16) -> bool {
        let old_high = old_address >> 8;
        let new_high = new_address >> 8;
        old_high != new_high
    }

    fn set_status_flags(&mut self, flags: &[(u8, bool)]) {
        let mut set = 0;
        let mut clear = 0;

        for &(flag, value) in flags {
            match value {
                true => set |= flag,
                false => clear |= flag,
            }
        }

        self.status = (self.status | set) & !clear;
    }

    fn unexpected_op_cycle<S>(&self, c: u8) -> Result<S, String> {
        Err(format!("Unexpected {} cycle {}", self.op.mnemonic, c))
    }

    fn adc(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
                let mut result = self.register_a;
                let mut carry = match result.checked_add(self.resolved_mem_read) {
                    Some(_sum) => false,
                    None => true,
                };

                result = result.wrapping_add(self.resolved_mem_read);

                if Self::get_flag(self.status, CARRY_FLAG) {
                    carry = carry
                        | match result.checked_add(1) {
                            Some(_sum) => false,
                            None => true,
                        };
                    result = result.wrapping_add(1);
                }

                let set_overflow =
                    (self.resolved_mem_read ^ result) & (self.register_a ^ result) & NEGATIVE_FLAG
                        != 0;

                self.register_a = result;

                self.set_status_flags(&[
                    (OVERFLOW_FLAG, set_overflow),
                    (CARRY_FLAG, carry),
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ])
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn and(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
                self.register_a = self.register_a & self.resolved_mem_read;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ])
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn asl(&mut self) -> Result<(), String> {
        if self.op.mode == AddressingMode::Accumulator {
            return Ok(match self.current_op_cycle {
                0 => {
                    let old = self.register_a;
                    self.register_a = self.register_a << 1;
                    self.set_status_flags(&[
                        (ZERO_FLAG, self.register_a == 0),
                        (CARRY_FLAG, old & NEGATIVE_FLAG == NEGATIVE_FLAG),
                        (
                            NEGATIVE_FLAG,
                            Self::get_flag(self.register_a, NEGATIVE_FLAG),
                        ),
                    ]);
                }
                c => return self.unexpected_op_cycle(c),
            });
        }

        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read << 1);
            }
            2 => {
                let old = self.resolved_mem_read;
                let value = self.resolved_mem_read << 1;
                self.set_status_flags(&[
                    (ZERO_FLAG, value == 0),
                    (CARRY_FLAG, old & NEGATIVE_FLAG == NEGATIVE_FLAG),
                    (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sta(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_write(self.resolved_addr, self.register_a);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn branch_if_true(&mut self, condition: bool) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                if condition {
                    if Self::page_boundary_crossed(self.program_counter, self.resolved_addr) {
                        self.dummy_read = true;
                    }
                    self.should_branch = true;
                }
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn bcc(&mut self) -> Result<(), String> {
        self.branch_if_true(!Self::get_flag(self.status, CARRY_FLAG))
    }

    fn bcs(&mut self) -> Result<(), String> {
        self.branch_if_true(Self::get_flag(self.status, CARRY_FLAG))
    }

    fn beq(&mut self) -> Result<(), String> {
        self.branch_if_true(Self::get_flag(self.status, ZERO_FLAG))
    }

    fn bit(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
                self.set_status_flags(&[
                    (ZERO_FLAG, self.resolved_mem_read & self.register_a == 0),
                    (
                        OVERFLOW_FLAG,
                        self.resolved_mem_read & OVERFLOW_FLAG == OVERFLOW_FLAG,
                    ),
                    (
                        NEGATIVE_FLAG,
                        self.resolved_mem_read & NEGATIVE_FLAG == NEGATIVE_FLAG,
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn bmi(&mut self) -> Result<(), String> {
        self.branch_if_true(Self::get_flag(self.status, NEGATIVE_FLAG))
    }

    fn bne(&mut self) -> Result<(), String> {
        self.branch_if_true(!Self::get_flag(self.status, ZERO_FLAG))
    }

    fn bpl(&mut self) -> Result<(), String> {
        self.branch_if_true(!Self::get_flag(self.status, NEGATIVE_FLAG))
    }

    fn bvc(&mut self) -> Result<(), String> {
        self.branch_if_true(!Self::get_flag(self.status, OVERFLOW_FLAG))
    }

    fn bvs(&mut self) -> Result<(), String> {
        self.branch_if_true(Self::get_flag(self.status, OVERFLOW_FLAG))
    }

    fn clc(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(CARRY_FLAG, false)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn cld(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(DECIMAL_MODE_FLAG, false)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn cli(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(INTERRUPT_DISABLE_FLAG, false)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn clv(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(OVERFLOW_FLAG, false)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn cmp(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;

                let sub = self.register_a.wrapping_sub(value);

                self.set_status_flags(&[
                    (CARRY_FLAG, self.register_a >= value),
                    (ZERO_FLAG, self.register_a == value),
                    (NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn cpx(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;

                let sub = self.register_x.wrapping_sub(value);

                self.set_status_flags(&[
                    (CARRY_FLAG, self.register_x >= value),
                    (ZERO_FLAG, self.register_x == value),
                    (NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn cpy(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;

                let sub = self.register_y.wrapping_sub(value);

                self.set_status_flags(&[
                    (CARRY_FLAG, self.register_y >= value),
                    (ZERO_FLAG, self.register_y == value),
                    (NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn dec(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read)?;
            }
            2 => {
                let sub = self.resolved_mem_read.wrapping_sub(1);

                self.mem_write(self.resolved_addr, sub)?;

                self.set_status_flags(&[
                    (ZERO_FLAG, sub == 0),
                    (NEGATIVE_FLAG, Self::get_flag(sub, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn dcp(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?.wrapping_sub(1);
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read)?;
            }
            2 => {
                let diff = self.register_a.wrapping_sub(self.resolved_mem_read);

                self.set_status_flags(&[
                    (CARRY_FLAG, self.register_a >= self.resolved_mem_read),
                    (ZERO_FLAG, self.register_a == self.resolved_mem_read),
                    (NEGATIVE_FLAG, Self::get_flag(diff, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn dex(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_x = self.register_x.wrapping_sub(1);

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_x == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_x, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn dey(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_y = self.register_y.wrapping_sub(1);

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_y == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_y, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn eor(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;
                self.register_a = self.register_a ^ value;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn inc(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?.wrapping_add(1);
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read)?;
            }
            2 => {
                self.set_status_flags(&[
                    (ZERO_FLAG, self.resolved_mem_read == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.resolved_mem_read, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn inx(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_x = self.register_x.wrapping_add(1);

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_x == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_x, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn iny(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_y = self.register_y.wrapping_add(1);

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_y == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_y, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn isb(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?.wrapping_add(1);
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read)?;
            }
            2 => {
                let c = match Self::get_flag(self.status, CARRY_FLAG) {
                    true => 1,
                    false => 0,
                };

                let a = self.register_a;

                let clear_carry = match a.checked_sub(self.resolved_mem_read) {
                    Some(_sub) => match _sub.checked_sub(1 - c) {
                        Some(_sub2) => false,
                        None => true,
                    },
                    None => true,
                };

                let result = a.wrapping_add(!self.resolved_mem_read).wrapping_add(c); //take advantage of twos compliment
                let set_overflow = (self.resolved_mem_read ^ a) & (a ^ result) & NEGATIVE_FLAG != 0;

                self.register_a = result;

                self.set_status_flags(&[
                    (CARRY_FLAG, !clear_carry),
                    (ZERO_FLAG, self.register_a == 0),
                    (OVERFLOW_FLAG, set_overflow),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn jmp(&mut self) -> Result<(), String> {
        //TODO! This takes one cycle too many :(
        self.program_counter = self.resolved_addr;
        Result::Ok(())
    }

    fn jsr(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.stack_push(((self.program_counter - 1) >> 8) as u8)?;
            }
            1 => {
                self.stack_push((self.program_counter - 1) as u8)?;
            }
            2 => {
                self.program_counter = self.resolved_addr;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn lda(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
                self.register_a = self.resolved_mem_read;
                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn ldx(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
                self.register_x = self.resolved_mem_read;
                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_x == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_x, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn lax(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;
                self.register_a = value;
                self.register_x = value;
                self.set_status_flags(&[
                    (ZERO_FLAG, value == 0),
                    (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn ldy(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;
                self.register_y = value;
                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_y == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_y, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn lsr(&mut self) -> Result<(), String> {
        if self.op.mode == AddressingMode::Accumulator {
            return Ok(match self.current_op_cycle {
                0 => {
                    let old = self.register_a;
                    self.register_a = self.register_a >> 1;

                    self.set_status_flags(&[
                        (ZERO_FLAG, self.register_a == 0),
                        (CARRY_FLAG, old & CARRY_FLAG == CARRY_FLAG),
                        (
                            NEGATIVE_FLAG,
                            Self::get_flag(self.register_a, NEGATIVE_FLAG),
                        ),
                    ]);
                }
                c => return self.unexpected_op_cycle(c),
            });
        }

        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read >> 1)?;
            }
            2 => {
                let value = self.resolved_mem_read >> 1;
                self.set_status_flags(&[
                    (ZERO_FLAG, value == 0),
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & CARRY_FLAG == CARRY_FLAG,
                    ),
                    (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn nop(&mut self) -> Result<(), String> {
        //TODO - Handling of absolute_x and absolute_y? They should take a cycle :(
        // if opcode.mode == AddressingMode::Absolute_X || opcode.mode == AddressingMode::Absolute_Y {
        //     return Result::Ok(());
        // }
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read(self.resolved_addr);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn dop(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read(self.resolved_addr);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn top(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read(self.resolved_addr);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn ora(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.mem_read(self.resolved_addr)?;
                self.register_a = self.register_a | value;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn pha(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.stack_push(self.register_a)?;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn php(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.stack_push(self.status | BREAK_FLAG | BREAK2_FLAG)?;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn pla(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.resolved_mem_read = self.stack_pop()?;
            }
            2 => {
                self.register_a = self.resolved_mem_read;
                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn plp(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.resolved_mem_read = self.stack_pop()?;
            }
            2 => {
                self.status = (self.resolved_mem_read & !BREAK_FLAG) | BREAK2_FLAG;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn rol(&mut self) -> Result<(), String> {
        if self.op.mode == AddressingMode::Accumulator {
            return Ok(match self.current_op_cycle {
                0 => {
                    let old_value = self.register_a;
                    let carry = Self::get_flag(self.status, CARRY_FLAG);

                    let value = match carry {
                        true => (old_value << 1) + 1,
                        false => old_value << 1,
                    };

                    self.register_a = value;
                    self.set_status_flags(&[
                        (CARRY_FLAG, old_value & NEGATIVE_FLAG == NEGATIVE_FLAG),
                        (ZERO_FLAG, value == 0),
                        (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                    ]);
                }
                c => return self.unexpected_op_cycle(c),
            });
        }

        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                let carry = Self::get_flag(self.status, CARRY_FLAG);

                let value = match carry {
                    true => (self.resolved_mem_read << 1) + 1,
                    false => self.resolved_mem_read << 1,
                };
                self.mem_write(self.resolved_addr, value)?;
                self.set_status_flags(&[
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & NEGATIVE_FLAG == NEGATIVE_FLAG,
                    ),
                    (ZERO_FLAG, value == 0),
                    (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                ]);
            }
            2 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn ror(&mut self) -> Result<(), String> {
        if self.op.mode == AddressingMode::Accumulator {
            return Ok(match self.current_op_cycle {
                0 => {
                    let old_value = self.register_a;
                    let carry = Self::get_flag(self.status, CARRY_FLAG);

                    let value = match carry {
                        true => (old_value >> 1) + NEGATIVE_FLAG,
                        false => old_value >> 1,
                    };

                    self.register_a = value;

                    self.set_status_flags(&[
                        (CARRY_FLAG, old_value & CARRY_FLAG == CARRY_FLAG),
                        (ZERO_FLAG, value == 0),
                        (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                    ]);
                }
                c => return self.unexpected_op_cycle(c),
            });
        }

        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                let carry = Self::get_flag(self.status, CARRY_FLAG);

                let value = match carry {
                    true => (self.resolved_mem_read >> 1) + NEGATIVE_FLAG,
                    false => self.resolved_mem_read >> 1,
                };

                self.mem_write(self.resolved_addr, value)?;

                self.set_status_flags(&[
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & CARRY_FLAG == CARRY_FLAG,
                    ),
                    (ZERO_FLAG, value == 0),
                    (NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG)),
                ]);
            }
            2 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn rla(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                let value = (self.resolved_mem_read << 1) | (self.status & CARRY_FLAG);

                self.mem_write(self.resolved_addr, value)?;
                self.register_a = self.register_a & value;

                self.set_status_flags(&[
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & NEGATIVE_FLAG == NEGATIVE_FLAG,
                    ),
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            2 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn rra(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                let should_carry = Self::get_flag(self.resolved_mem_read, CARRY_FLAG);

                let value = self.resolved_mem_read >> 1 | ((self.status & CARRY_FLAG) << 7);
                self.mem_write(self.resolved_addr, value)?;

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

                let set_overflow =
                    (value ^ result) & (self.register_a ^ result) & NEGATIVE_FLAG != 0;

                self.register_a = result;

                self.set_status_flags(&[
                    (OVERFLOW_FLAG, set_overflow),
                    (CARRY_FLAG, carry),
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            2 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn rti(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.status = self.stack_pop()? | BREAK2_FLAG;
            }
            2 => {
                self.program_counter = self.stack_pop()? as u16;
            }
            3 => {
                self.program_counter =
                    (self.program_counter & 0x00FF) | ((self.stack_pop()? as u16) << 8);
            }
            4 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn rts(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_read((STACK as u16) + (self.stack_pointer as u16))?; //dummy read top of stack
            }
            1 => {
                self.program_counter = self.stack_pop()? as u16;
            }
            2 => {
                self.program_counter =
                    ((self.program_counter & 0x00FF) | ((self.stack_pop()? as u16) << 8)) + 1;
            }
            3 => {}
            4 => {}
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn brk(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.stack_push((self.program_counter >> 8) as u8)?;
            }
            1 => {
                self.stack_push(self.program_counter as u8)?;
            }
            2 => {
                self.stack_push(self.status | (BREAK_FLAG & BREAK2_FLAG))?;
            }
            3 => {
                self.program_counter = self.mem_read(BRK_INTERRUPT_ADDRESS)? as u16;
            }
            4 => {
                self.program_counter = (self.program_counter & 0x00FF)
                    | ((self.mem_read(BRK_INTERRUPT_ADDRESS + 1)? as u16) << 8);
            }
            5 => {
                if self.program_counter == HALT_VALUE {
                    self.halt.store(true, Ordering::Relaxed);
                } else {
                    //we do this after the return check, because it's easier to test and doesn't make a
                    //difference when we exit :)

                    self.set_status_flags(&[(INTERRUPT_DISABLE_FLAG, true)]);
                }
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sax(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let value = self.register_x & self.register_a;
                self.mem_write(self.resolved_addr, value)?;
            }
            c => return self.unexpected_op_cycle(c),
        })

        //Even though these flags are documented, they don't get updated (the docs are wrong)
        // self.set_status_flag(ZERO_FLAG, value == 0);
        // self.set_status_flag(NEGATIVE_FLAG, Self::get_flag(value, NEGATIVE_FLAG));
    }

    fn sbc(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                let c = match Self::get_flag(self.status, CARRY_FLAG) {
                    true => 1,
                    false => 0,
                };

                let a = self.register_a;

                let value = self.mem_read(self.resolved_addr)?;

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

                self.set_status_flags(&[
                    (OVERFLOW_FLAG, set_overflow),
                    (CARRY_FLAG, !clear_carry),
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sec(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(CARRY_FLAG, true)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sed(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(DECIMAL_MODE_FLAG, true)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sei(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.set_status_flags(&[(INTERRUPT_DISABLE_FLAG, true)]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn slo(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read << 1)?;
            }
            2 => {
                let result = self.register_a | (self.resolved_mem_read << 1);

                self.register_a = result;

                self.set_status_flags(&[
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & NEGATIVE_FLAG == NEGATIVE_FLAG,
                    ),
                    (ZERO_FLAG, result == 0),
                    (NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sre(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.resolved_mem_read = self.mem_read(self.resolved_addr)?;
            }
            1 => {
                self.mem_write(self.resolved_addr, self.resolved_mem_read >> 1);
            }
            2 => {
                let result = self.register_a ^ (self.resolved_mem_read >> 1);

                self.register_a = result;

                self.set_status_flags(&[
                    (
                        CARRY_FLAG,
                        self.resolved_mem_read & CARRY_FLAG == CARRY_FLAG,
                    ),
                    (ZERO_FLAG, result == 0),
                    (NEGATIVE_FLAG, Self::get_flag(result, NEGATIVE_FLAG)),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn stx(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_write(self.resolved_addr, self.register_x)?;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn sty(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.mem_write(self.resolved_addr, self.register_y)?;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn tax(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_x = self.register_a;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_x == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_x, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn tay(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_y = self.register_a;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_y == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_y, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn tsx(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_x = self.stack_pointer;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_x == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_x, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn txa(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_a = self.register_x;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn txs(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.stack_pointer = self.register_x;
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn tya(&mut self) -> Result<(), String> {
        Ok(match self.current_op_cycle {
            0 => {
                self.register_a = self.register_y;

                self.set_status_flags(&[
                    (ZERO_FLAG, self.register_a == 0),
                    (
                        NEGATIVE_FLAG,
                        Self::get_flag(self.register_a, NEGATIVE_FLAG),
                    ),
                ]);
            }
            c => return self.unexpected_op_cycle(c),
        })
    }

    fn interrupt_nmi(&mut self) -> Result<bool, String> {
        match self.instruction_cycle {
            0 => self.mem_read(self.program_counter).map(|_| true),
            1 => self
                .stack_push((self.program_counter >> 8) as u8)
                .map(|_| true),
            2 => self.stack_push(self.program_counter as u8).map(|_| true),
            3 => {
                let result = self
                    .stack_push((self.status | BREAK2_FLAG) & !BREAK_FLAG)
                    .map(|_| true);
                self.status = self.status | INTERRUPT_DISABLE_FLAG;
                result
            }
            4 => {
                self.program_counter = self.mem_read(NMI_INTERRUPT_ADDRESS)? as u16;
                Ok(true)
            }
            5 => {
                self.program_counter = (0x00FF & self.program_counter)
                    | ((self.mem_read(NMI_INTERRUPT_ADDRESS + 1)? as u16) << 8);
                Ok(true)
            }
            6 => Ok(true),
            7 => {
                self.interrupt.borrow_mut().take_nmi();
                Ok(false)
            }
            c => Err(format!("Unexpected cycle in NMI interrupt routine: {}", c)),
        }
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
    use crate::{
        bus::BusImpl,
        opp::OPCODES_MAP,
        traits::{
            mos_65902::{BRK_INTERRUPT_ADDRESS, HALT_VALUE, STACK},
            tick::Tick,
        },
    };

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

        fn load_with_start_address(&mut self, start_address: u16, program: Vec<u8>) {
            self.mem_write_vec(start_address, &program);
            self.mem_write_u16(0xFFFC, start_address);
        }

        fn mem_write_vec(&mut self, addr: u16, program: &Vec<u8>) {
            for i in 0..(program.len() as u16) {
                self.mem_write(addr + i, program[i as usize]);
            }
        }
    }

    impl Bus for BusStub {}

    impl Mem for BusStub {
        fn mem_read(&mut self, addr: u16) -> Result<u8, String> {
            Result::Ok(self.memory[addr as usize])
        }

        fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, String> {
            self.memory[addr as usize] = data;
            Result::Ok(0)
        }
    }

    fn tracing_callback(cpu: &CpuV2<BusStub>) {
        let formatter = CpuTraceFormatter {
            options: CpuTraceFormatOptions {
                write_break_2_flag: false,
                write_cpu_cycles: true,
                reads_offset: 1,
            },
        };
        match &cpu.peek_trace() {
            None => println!("NULL Trace"),
            Some(s) => {
                println!("{}", formatter.format(&s))
            }
        };
    }

    #[test]
    fn test_opcode_cycles() {
        let mut errors = String::new();
        let mut count = 0;
        for opcode in OPCODES_MAP.values() {
            let result = test_opcode(*opcode);
            match result {
                Ok(_) => {}
                Err(s) => {
                    count = count + 1;
                    errors.push_str(&format!("\t{}\n", s))
                }
            }
        }

        assert!(
            errors.len() == 0,
            "Some opcodes did not execute in the correct number of cycles. Num failures = {}:\n\n{}",
            count,
            errors
        );
    }

    fn test_opcode(opcode: &OpCode) -> Result<(), String> {
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );

        bus.borrow_mut()
            .load_with_start_address(0x8000, vec![opcode.code]);
        cpu.reset();

        match opcode.code {
            0x90 => {
                cpu.set_status_flags(&[(CARRY_FLAG, true)]);
            }
            0xB0 => {
                cpu.set_status_flags(&[(CARRY_FLAG, false)]);
            }
            0xF0 => {
                cpu.set_status_flags(&[(ZERO_FLAG, false)]);
            }
            0x30 => {
                cpu.set_status_flags(&[(NEGATIVE_FLAG, false)]);
            }
            0xD0 => {
                cpu.set_status_flags(&[(ZERO_FLAG, true)]);
            }
            0x10 => {
                cpu.set_status_flags(&[(NEGATIVE_FLAG, true)]);
            }
            0x50 => {
                cpu.set_status_flags(&[(OVERFLOW_FLAG, true)]);
            }
            0x70 => {
                cpu.set_status_flags(&[(OVERFLOW_FLAG, false)]);
            }
            _ => {}
        };

        let start_cycles = cpu.get_cycles();

        let brk = match OPCODES_MAP.get(&0x00) {
            Some(op) => op,
            None => panic!("Where is brk?"),
        };

        let expected_final_cycles = opcode.cycles as u64;

        println!("Executing {:02X} ({})", opcode.code, opcode.mnemonic);
        let run_result = cpu.run_with_callback(|cpu| tracing_callback(cpu));
        println!(
            "Finished executing {:02X} ({}). Final cycles: {}\n\n",
            opcode.code,
            opcode.mnemonic,
            cpu.get_cycles()
        );

        let actual_final_cycles = if opcode.code == brk.code {
            cpu.get_cycles() - start_cycles
        } else {
            cpu.get_cycles() - start_cycles - brk.cycles as u64
        };

        run_result.and_then(|_| {
            if actual_final_cycles == expected_final_cycles {
                return Ok(());
            } else {
                return Err(format!(
                    "Expected opcode {:02X} ({}, {}) to take {} cycles but was {}",
                    opcode.code,
                    opcode.mnemonic,
                    opcode.mode,
                    expected_final_cycles,
                    actual_final_cycles
                ));
            }
        })
    }

    #[test]
    fn test_adc_0x69_carry_no_overflow_or_negative() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        //Jump forward by 4 (to a STA instruction, check that we do in fact store)
        bus.borrow_mut()
            .load_with_start_address(0x80F0, vec![0x90, 0x0E]); //page cross, and branch, so 2 + 1 + 1 cycles
        bus.borrow_mut().mem_write_vec(0x8100, &vec![0x85, 0xA1]); //3 cycles 
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|cpu| tracing_callback(cpu)).unwrap(); //brk is 7 cycles
        assert_eq!(240, bus.borrow_mut().mem_read(0x00A1).unwrap());
        assert_eq!(14, cpu.get_cycles());
    }

    #[test]
    fn test_bcc_0x90_absolute_addr_carry_flag_set() {
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        bus.borrow_mut()
            .load_with_start_address(0x8000, vec![0x90, 0x04, 0x00, 0x00, 0x00, 0x00, 0x85, 0xA1]); //2
        //cycles, then brk (7)
        cpu.reset();
        cpu.status = cpu.status | CARRY_FLAG;
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|cpu| tracing_callback(cpu));
        assert_eq!(0, bus.borrow_mut().mem_read(0x00A1).unwrap());
        assert_eq!(9, cpu.get_cycles());
    }

    #[test]
    fn test_bcs_0xb0_absolute_addr_carry_flag_not_set() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        bus.borrow_mut()
            .load_with_start_address(0x8000, vec![0x20, 0xAB, 0xBC]);
        bus.borrow_mut().mem_write(0xBCAB, 0xE8); // INX
        bus.borrow_mut().mem_write(0xBCAC, 0xE8); // INX
        bus.borrow_mut().mem_write(0xBCAD, 0x00); // BRK
        cpu.reset();
        let _ = cpu.run_with_callback(|cpu| {
            tracing_callback(cpu);
        });

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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        bus.borrow_mut()
            .load_with_start_address(0x8000, vec![0x20, 0xAB, 0xBC, 0xE8]);
        bus.borrow_mut().mem_write(0xBCAB, 0xE8); // INX
        bus.borrow_mut().mem_write(0xBCAC, 0xE8); // INX
        bus.borrow_mut().mem_write(0xBCAD, 0x60); // RTS, what we are testing
        cpu.reset();
        let _ = cpu.run_with_callback(|cpu| {
            tracing_callback(cpu);
        });

        assert_eq!((STACK_RESET as u8).wrapping_sub(3), cpu.stack_pointer); //3 because the BRK
        //instruction adds sp (2) and status (1) to the stack before we quit
        assert_eq!(3, cpu.register_x);
    }

    #[test]
    fn test_sbc_0xe9_carry_no_overflow_or_negative() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x70, 0x00]);
        cpu.reset();
        cpu.register_a = 0x7F;
        cpu.status = cpu.status | CARRY_FLAG;
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
        let mut cpu = CpuV2::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 0x80;
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {});
        assert_eq!(0x7F, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == OVERFLOW_FLAG);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_sbc_0xe9_negative_flag_no_overflow() {
        let bus = Rc::new(RefCell::new(BusStub::new()));
        let mut cpu = CpuV2::new(
            bus.clone(),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        bus.borrow_mut()
            .load_with_start_address(0x8000, vec![0xE9, 0x03, 0x00]);
        cpu.reset();
        cpu.register_a = 0x8F;
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|cpu| tracing_callback(cpu));
        assert_eq!(0x8C, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == 0);
        assert!(NEGATIVE_FLAG & cpu.status == NEGATIVE_FLAG);
    }

    #[test]
    fn test_sbc_0xe9_zero_flag() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0xE9, 0x81, 0x00]);
        cpu.reset();
        cpu.register_a = 0x81;
        cpu.status = cpu.status | CARRY_FLAG;
        let _ = cpu.run_with_callback(|_| {}).unwrap();
        assert_eq!(0x00, cpu.register_a);
        assert!(CARRY_FLAG & cpu.status == CARRY_FLAG);
        assert!(OVERFLOW_FLAG & cpu.status == 0);
        assert!(ZERO_FLAG & cpu.status == ZERO_FLAG);
        assert!(NEGATIVE_FLAG & cpu.status == 0);
    }

    #[test]
    fn test_sbc_0xe9_with_carry_cleared() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x85, 0xa1, 0x00]);
        cpu.reset();
        cpu.register_a = 240;
        let _ = cpu.run_with_callback(|cpu| tracing_callback(cpu));
        assert!(cpu.mem_read(0x00a1).unwrap() == 240);
    }

    #[test]
    fn test_0x95_sta_zero_page_X() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let _ = cpu.run_with_callback(|_| {}).unwrap();
        assert!(cpu.mem_read(0xdda1).unwrap() == 0xf0);
    }

    #[test]
    fn test_0x91_sta_indirect_Y() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let _ = cpu.run_with_callback(|_| {}).unwrap();
        assert_eq!(0xf0, cpu.mem_read(0xDDA5).unwrap());
    }

    #[test]
    fn test_stx_0x8e() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
            Rc::new(RefCell::new(bus)),
            Rc::new(RefCell::new(InterruptImpl::new())),
            Arc::new(AtomicBool::new(false)),
        );
        cpu.load_with_start_address(0x8000, vec![0x8C, 0xAA, 0xBB, 0x00]);
        cpu.reset();
        cpu.register_y = 0x05;
        let _ = cpu.run_with_callback(|_| {}).unwrap();
        assert_eq!(cpu.mem_read(0xBBAA).unwrap(), 0x05);
    }

    #[test]
    fn test_0xaa_tax_immediate_load_data() {
        let bus = BusStub::new();
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
        let mut cpu = CpuV2::new(
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
