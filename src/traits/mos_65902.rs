use super::mos_6502_registers::Registers;

pub trait MOS6502: Registers {
    fn get_cycles(self) -> u64;
    fn set_cycles(&mut self, value: u64);
}
