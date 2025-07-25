use super::mem::Mem;

pub trait Bus: Mem {
    fn tick(&mut self, cycles: u8);
}
