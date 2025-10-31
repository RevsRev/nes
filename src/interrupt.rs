pub enum InterruptType {
    Nmi,
}

pub trait Interrupt {
    fn poll_nmi(&mut self) -> bool;
    fn take_nmi(&mut self) -> bool;
    fn set_nmi(&mut self, state: bool);
}

pub struct InterruptImpl {
    nmi_interrupt: Option<u8>,
}

impl Interrupt for InterruptImpl {
    fn poll_nmi(&mut self) -> bool {
        self.nmi_interrupt.is_some()
    }
    fn take_nmi(&mut self) -> bool {
        let state = self.nmi_interrupt.take();
        state.is_some()
    }
    fn set_nmi(&mut self, state: bool) {
        if state {
            self.nmi_interrupt = Some(1);
        } else {
            self.nmi_interrupt = None;
        }
    }
}

impl InterruptImpl {
    pub fn new() -> InterruptImpl {
        InterruptImpl {
            nmi_interrupt: None,
        }
    }
}
