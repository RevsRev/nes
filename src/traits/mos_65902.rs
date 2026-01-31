use super::{bus::Bus, mos_6502_registers::Registers};

//Flags
pub const CARRY_FLAG: u8 = 0b0000_0001;
pub const ZERO_FLAG: u8 = 0b0000_0010;
pub const INTERRUPT_DISABLE_FLAG: u8 = 0b0000_0100;
pub const DECIMAL_MODE_FLAG: u8 = 0b0000_1000;
pub const BREAK_FLAG: u8 = 0b0001_0000;
pub const BREAK2_FLAG: u8 = 0b0010_0000;
pub const OVERFLOW_FLAG: u8 = 0b0100_0000;
pub const NEGATIVE_FLAG: u8 = 0b1000_0000;

pub const STACK: u16 = 0x0100;
pub const STACK_RESET: u8 = 0xfd;

pub const NMI_INTERRUPT_ADDRESS: u16 = 0xFFFA;
pub const BRK_INTERRUPT_ADDRESS: u16 = 0xFFFE;
pub const PC_START_ADDRESS: u16 = 0xFFFC;
pub const HALT_VALUE: u16 = 0x00FF;

pub trait MOS6502<T: Bus>: Registers {
    fn get_cycles(&self) -> u64;
    fn set_cycles(&mut self, value: u64);

    fn step_with_callback<F>(&mut self, callback: &mut F) -> Result<bool, String>
    where
        F: for<'a> FnMut(&'a Self);

    fn run_with_callback<F>(&mut self, callback: F) -> Result<(), String>
    where
        F: for<'a> FnMut(&'a Self);

    fn load_with_start_address(&mut self, start_address: u16, program: Vec<u8>);

    fn reset(&mut self);
}
