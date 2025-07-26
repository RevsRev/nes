use super::{interrupt::Interrupting, mem::Mem, tick::Tick};

pub trait Bus: Mem + Tick + Interrupting {}
