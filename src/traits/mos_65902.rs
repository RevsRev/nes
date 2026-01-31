use super::{bus::Bus, mos_6502_registers::Registers};

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
