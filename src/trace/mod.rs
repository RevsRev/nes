use crate::cpu::BREAK2_FLAG;
use crate::opp::{AddressingMode, OpCode, OpCodeBehaviour};
use std::fmt;

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
    pub format_options: TraceFormatOptions,
}

#[derive(Clone, Copy)]
pub struct TraceFormatOptions {
    pub write_break_2_flag: bool,
    pub write_cpu_cycles: bool,
}
impl fmt::Display for CpuTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.format_options.write_cpu_cycles {
            write!(f, "c{:<10}", self.cpu_cycles)?;
        }

        let p = if self.format_options.write_break_2_flag {
            self.status
        } else {
            self.status & !BREAK2_FLAG
        };
        let registers_and_pointers = format!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.register_a, self.register_x, self.register_y, p, self.stack
        );

        match self.op_code.mode {
            AddressingMode::Absolute => {
                let first_read = self.reads[0].1;
                let second_read = self.reads[1].1;

                let overwritten_value = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", self.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(" = {:02X}", self.reads[2].1),
                    _ => format!(""),
                };

                write!(
                    f,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X}{:<10}{:>38}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    second_read,
                    self.op_code.mnemonic,
                    second_read,
                    first_read,
                    overwritten_value,
                    registers_and_pointers
                )
            }
            AddressingMode::Absolute_X => {
                let first_read = self.reads[0].1;
                let second_read = self.reads[1].1;

                let overwritten_value = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", self.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(" = {:02X}", self.reads[2].1),
                    _ => format!(""),
                };

                write!(
                    f,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X},X @ {:04X}{}{:>34}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    second_read,
                    self.op_code.mnemonic,
                    second_read,
                    first_read,
                    self.absolute_address.unwrap_or_default(),
                    overwritten_value,
                    registers_and_pointers
                )
            }
            AddressingMode::Absolute_Y => {
                let first_read = self.reads[0].1;
                let second_read = self.reads[1].1;

                let overwritten_value = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => format!(" = {:02X}", self.writes[0].1),
                    OpCodeBehaviour::MemoryRead => format!(" = {:02X}", self.reads[2].1),
                    _ => format!(""),
                };

                write!(
                    f,
                    "{:04X}  {:02X} {:02X} {:02X} {:>4} ${:02X}{:02X},Y @ {:04X}{}{:>34}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    second_read,
                    self.op_code.mnemonic,
                    second_read,
                    first_read,
                    self.absolute_address.unwrap_or_default(),
                    overwritten_value,
                    registers_and_pointers
                )
            }
            AddressingMode::Immediate => {
                let first_read = self.reads[0].1;
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} #${:02X}{:>49}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    registers_and_pointers
                )
            }
            AddressingMode::ZeroPage => {
                let first_read = self.reads[0].1;
                let param = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => self.reads[1].1,
                    OpCodeBehaviour::MemoryWrite => self.writes[0].1,
                    _ => 0,
                };
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X} = {:02X}{:>45}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    param,
                    registers_and_pointers
                )
            }
            AddressingMode::ZeroPage_X => {
                let first_read = self.reads[0].1;
                let param = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => self.reads[1].1,
                    OpCodeBehaviour::MemoryWrite => self.writes[0].1,
                    _ => 0,
                };
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X},X @ {:02X} = {:02X}{:>38}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(self.register_x),
                    param,
                    registers_and_pointers
                )
            }
            AddressingMode::ZeroPage_Y => {
                let first_read = self.reads[0].1;
                let param = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryRead => self.reads[1].1,
                    OpCodeBehaviour::MemoryWrite => self.writes[0].1,
                    _ => 0,
                };
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} ${:02X},Y @ {:02X} = {:02X}{:>38}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(self.register_y),
                    param,
                    registers_and_pointers
                )
            }
            AddressingMode::Implied => {
                write!(
                    f,
                    "{:04X}  {:02X}       {:>4}{:>54}",
                    self.pc, self.op_code.code, self.op_code.mnemonic, registers_and_pointers
                )
            }
            AddressingMode::Relative => {
                let first_read = self.reads[0].1;
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}     {} ${:02X}{:>48}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    self.absolute_address.unwrap_or_default(),
                    registers_and_pointers
                )
            }
            AddressingMode::Accumulator => {
                write!(
                    f,
                    "{:04X}  {:02X}        {} A{:>52}",
                    self.pc, self.op_code.code, self.op_code.mnemonic, registers_and_pointers
                )
            }
            AddressingMode::Indirect_X => {
                let first_read = self.reads[0].1;
                let read_value = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => self.writes[0].1,
                    OpCodeBehaviour::MemoryRead => self.reads[3].1,
                    _ => 0,
                };
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} (${:02X},X) @ {:02X} = {:04X} = {:02X}    {}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    first_read.wrapping_add(self.register_x),
                    self.absolute_address.unwrap_or_default(),
                    read_value,
                    registers_and_pointers
                )
            }
            AddressingMode::Indirect_Y => {
                let first_read = self.reads[0].1;
                let read_value = match self.op_code.behaviour {
                    OpCodeBehaviour::MemoryWrite => self.writes[0].1,
                    OpCodeBehaviour::MemoryRead => self.reads[3].1,
                    _ => 0,
                };
                write!(
                    f,
                    "{:04X}  {:02X} {:02X}    {:>4} (${:02X}),Y = {:04X} @ {:04X} = {:02X}  {}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    self.op_code.mnemonic,
                    first_read,
                    self.absolute_address
                        .unwrap_or_default()
                        .wrapping_sub(self.register_y as u16),
                    self.absolute_address.unwrap_or_default(),
                    read_value,
                    registers_and_pointers
                )
            }
            AddressingMode::Indirect => {
                let first_read = self.reads[0].1;
                let second_read = self.reads[1].1;
                write!(
                    f,
                    "{:04X}  {:02X} {:02X} {:02X}  {} (${:02X}{:02X}) = {:04X}  {:>37}",
                    self.pc,
                    self.op_code.code,
                    first_read,
                    second_read,
                    self.op_code.mnemonic,
                    second_read,
                    first_read,
                    self.absolute_address.unwrap_or_default(),
                    registers_and_pointers
                )
            }
        }
    }
}
