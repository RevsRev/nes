pub trait Tick {
    fn tick(&mut self) -> Result<(), String>;
}
