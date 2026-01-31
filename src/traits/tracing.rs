use crate::trace::CpuTrace;

pub trait Tracing {
    fn take_trace(&mut self) -> Option<CpuTrace>;
}
