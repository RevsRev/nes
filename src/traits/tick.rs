pub trait Tick {
    fn tick(&mut self, cycles: u8);
}
