use crate::{
    opp::{AddressingMode, OpCode, OpCodeBehaviour},
    traits::mos_65902::BREAK2_FLAG,
};
use std::fmt;

pub struct NesTrace {
    pub cpu_trace: CpuTrace,
    pub ppu_trace: PpuTrace,
    pub apu_trace: ApuTrace,
}

pub struct CpuTrace {
    pub cpu_cycles: u64,
    pub pc: u16,
    pub op_code: OpCode,
    pub absolute_address: Option<u16>,
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub stack: u8,
    pub reads: Vec<(u16, u8)>,
    pub writes: Vec<(u16, u8)>,
}

pub struct PpuTrace {
    pub scanline: u16,
    pub dot: u16,
}

pub struct ApuTrace {
    pub pulse_1: PulseTrace,
    pub pulse_2: PulseTrace,
    pub frame_trace: FrameTrace,
}
pub struct PulseTrace {
    pub len_counter: u8,
    pub len_counter_enabled: bool,
}
pub struct FrameTrace {
    pub irq_flag: bool,
    pub apu_cycles: u32,
}

#[derive(Clone, Copy)]
pub struct CpuTraceFormatOptions {
    pub write_break_2_flag: bool,
    pub write_cpu_cycles: bool,
    pub reads_offset: u8,
}

pub struct CpuTraceFormatter {
    pub options: CpuTraceFormatOptions,
}
pub struct PpuTraceFormatter {}
pub struct ApuTraceFormatter {}

pub struct NesTraceFormatter {
    pub cpu_formatter: CpuTraceFormatter,
    pub ppu_formatter: Option<PpuTraceFormatter>,
    pub apu_formatter: Option<ApuTraceFormatter>,
}

impl NesTraceFormatter {
    pub fn format(&self, nes_trace: &NesTrace) -> String {
        use std::fmt::Write;
        let mut out = String::new();

        let _ = write!(out, "{}", self.cpu_formatter.format(&nes_trace.cpu_trace));

        let _ = match self.ppu_formatter.as_ref() {
            None => Ok(()),
            Some(ppu) => write!(out, " {}", ppu.format(&nes_trace.ppu_trace)),
        };

        let _ = match self.apu_formatter.as_ref() {
            None => Ok(()),
            Some(apu) => write!(out, " {}", apu.format(&nes_trace.apu_trace)),
        };

        out
    }
}

impl ApuTraceFormatter {
    pub fn format(&self, apu_trace: &ApuTrace) -> String {
        use std::fmt::Write;
        let mut out = String::new();

        write!(
            out,
            "P1(L: {}, LE: {}) P2(L: {}, LE: {}) F(IRQ: {} C:{})",
            apu_trace.pulse_1.len_counter,
            apu_trace.pulse_1.len_counter_enabled,
            apu_trace.pulse_2.len_counter,
            apu_trace.pulse_2.len_counter_enabled,
            apu_trace.frame_trace.irq_flag,
            apu_trace.frame_trace.apu_cycles,
        );

        out
    }
}

impl PpuTraceFormatter {
    pub fn format(&self, ppu_trace: &PpuTrace) -> String {
        use std::fmt::Write;
        let mut out = String::new();
        write!(out, "V:{:<3} H:{:<3}", ppu_trace.scanline, ppu_trace.dot);
        out
    }
}

impl CpuTraceFormatter {
    pub fn format(&self, cpu_trace: &CpuTrace) -> String {
        use std::fmt::Write;
        let mut out = String::new();

        if self.options.write_cpu_cycles {
            write!(out, "c{:<10}", cpu_trace.cpu_cycles);
        }

        let p = if self.options.write_break_2_flag {
            cpu_trace.status
        } else {
            cpu_trace.status & !BREAK2_FLAG
        };
        let registers_and_pointers = format!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            cpu_trace.register_a, cpu_trace.register_x, cpu_trace.register_y, p, cpu_trace.stack
        );

        match cpu_trace.op_code.mode {
            AddressingMode::Absolute => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let second_read = cpu_trace.reads[1 + self.options.reads_offset as usize].1;

                let overwritten_value = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", cpu_trace.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(
                        " = {:02X}",
                        cpu_trace.reads[2 + self.options.reads_offset as usize].1
                    ),
                    _ => format!(""),
                };

                write!(
                    out,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X}{:<10}{:>38}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    second_read,
                    cpu_trace.op_code.mnemonic,
                    second_read,
                    first_read,
                    overwritten_value,
                    registers_and_pointers
                );
            }
            AddressingMode::Absolute_X => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let second_read = cpu_trace.reads[1 + self.options.reads_offset as usize].1;

                let overwritten_value = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", cpu_trace.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(
                        " = {:02X}",
                        cpu_trace.reads[2 + self.options.reads_offset as usize].1
                    ),
                    _ => format!(""),
                };

                write!(
                    out,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X},X @ {:04X}{}{:>34}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    second_read,
                    cpu_trace.op_code.mnemonic,
                    second_read,
                    first_read,
                    cpu_trace.absolute_address.unwrap_or_default(),
                    overwritten_value,
                    registers_and_pointers
                );
            }
            AddressingMode::Absolute_Y => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let second_read = cpu_trace.reads[1 + self.options.reads_offset as usize].1;

                let overwritten_value = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", cpu_trace.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(
                        " = {:02X}",
                        cpu_trace.reads[2 + self.options.reads_offset as usize].1
                    ),
                    _ => format!(""),
                };

                write!(
                    out,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X},Y @ {:04X}{}{:>34}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    second_read,
                    cpu_trace.op_code.mnemonic,
                    second_read,
                    first_read,
                    cpu_trace.absolute_address.unwrap_or_default(),
                    overwritten_value,
                    registers_and_pointers
                );
            }
            AddressingMode::Immediate => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} #${:02X}{:>49}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    registers_and_pointers
                );
            }
            AddressingMode::ZeroPage => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let param = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => {
                        cpu_trace.reads[1 + self.options.reads_offset as usize].1
                    }
                    OpCodeBehaviour::MemoryWrite => cpu_trace.writes[0].1,
                    _ => 0,
                };
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X} = {:02X}{:>45}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    param,
                    registers_and_pointers
                );
            }
            AddressingMode::ZeroPage_X => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let param = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => {
                        cpu_trace.reads[1 + self.options.reads_offset as usize].1
                    }
                    OpCodeBehaviour::MemoryWrite => cpu_trace.writes[0].1,
                    _ => 0,
                };
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X},X @ {:02X} = {:02X}{:>38}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(cpu_trace.register_x),
                    param,
                    registers_and_pointers
                );
            }
            AddressingMode::ZeroPage_Y => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let param = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => {
                        cpu_trace.reads[1 + self.options.reads_offset as usize].1
                    }
                    OpCodeBehaviour::MemoryWrite => cpu_trace.writes[0].1,
                    _ => 0,
                };
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X},Y @ {:02X} = {:02X}{:>38}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(cpu_trace.register_y),
                    param,
                    registers_and_pointers
                );
            }
            AddressingMode::Implied => {
                write!(
                    out,
                    "{:04X}  {:02X}       {:>4}{:>54}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    cpu_trace.op_code.mnemonic,
                    registers_and_pointers
                );
            }
            AddressingMode::Relative => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}     {} ${:02X}{:>48}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    cpu_trace.absolute_address.unwrap_or_default(),
                    registers_and_pointers
                );
            }
            AddressingMode::Accumulator => {
                write!(
                    out,
                    "{:04X}  {:02X}        {} A{:>52}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    cpu_trace.op_code.mnemonic,
                    registers_and_pointers
                );
            }
            AddressingMode::Indirect_X => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let dummy_cycle = if self.options.reads_offset == 1 { 1 } else { 0 };
                let read_value = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => cpu_trace.writes[0].1,
                    OpCodeBehaviour::MemoryRead => {
                        cpu_trace.reads[3 + dummy_cycle + self.options.reads_offset as usize].1
                    }
                    _ => 0,
                };
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} (${:02X},X) @ {:02X} = {:04X} = {:02X}    {}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(cpu_trace.register_x),
                    cpu_trace.absolute_address.unwrap_or_default(),
                    read_value,
                    registers_and_pointers
                );
            }
            AddressingMode::Indirect_Y => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let read_value = match cpu_trace.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => cpu_trace.writes[0].1,
                    OpCodeBehaviour::MemoryRead => {
                        cpu_trace.reads[3 + self.options.reads_offset as usize].1
                    }
                    _ => 0,
                };
                write!(
                    out,
                    "{:04X}  {:02X} {:02X}    {:>4} (${:02X}),Y = {:04X} @ {:04X} = {:02X}  {}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    cpu_trace.op_code.mnemonic,
                    first_read,
                    cpu_trace
                        .absolute_address
                        .unwrap_or_default()
                        .wrapping_sub(cpu_trace.register_y as u16),
                    cpu_trace.absolute_address.unwrap_or_default(),
                    read_value,
                    registers_and_pointers
                );
            }
            AddressingMode::Indirect => {
                let first_read = cpu_trace.reads[0 + self.options.reads_offset as usize].1;
                let second_read = cpu_trace.reads[1 + self.options.reads_offset as usize].1;
                write!(
                    out,
                    "{:04X}  {:02X} {:02X} {:02X}  {} (${:02X}{:02X}) = {:04X}  {:>37}",
                    cpu_trace.pc,
                    cpu_trace.op_code.code,
                    first_read,
                    second_read,
                    cpu_trace.op_code.mnemonic,
                    second_read,
                    first_read,
                    cpu_trace.absolute_address.unwrap_or_default(),
                    registers_and_pointers
                );
            }
        };
        out
    }
}
