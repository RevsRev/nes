use super::{mem::Mem, tick::Tick};

pub trait Bus: Mem + Tick {
    //horrible hacky method to make sure our logs line up nicely with known emulators
    fn signal_cpu_start(&mut self);
}
