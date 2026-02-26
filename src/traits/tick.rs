pub trait Tick {
    fn tick(&mut self, total_cpu_cycles: u64) -> Result<(), String>;
}
