use crate::trace::{CpuTrace, CpuTraceFormatOptions};

pub trait Tracing {
    fn take_trace(&mut self) -> Option<CpuTrace>;
    fn micro_trace(&mut self) -> Option<CpuTrace>;
    fn peek_trace(&self) -> Option<&CpuTrace>;
    fn set_tracing(&mut self, tracing: bool);

    fn format_options(&self, write_break_2_flag: bool, write_cycles: bool)
    -> CpuTraceFormatOptions;
}
