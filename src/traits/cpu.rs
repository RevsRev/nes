use std::fmt::Display;

use super::{bus::Bus, mos_65902::MOS6502, tracing::Tracing};

pub trait Cpu<T: Bus>: MOS6502<T> + Tracing + Display {}
