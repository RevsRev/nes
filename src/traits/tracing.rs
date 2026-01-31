use crate::trace::CpuTrace;

pub trait Tracing {
    fn take_trace(&mut self) -> Option<CpuTrace>;
    fn peek_trace(&self) -> Option<&CpuTrace>;
}
