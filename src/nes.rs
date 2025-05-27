use crate::bus::Bus;
use crate::cpu::CPU;

pub struct NES<'a> {
    pub cpu: CPU<'a, Bus>,
    pub bus: Bus,
}
