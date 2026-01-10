pub trait Interrupt {
    fn poll_nmi(&mut self) -> bool;
    fn take_nmi(&mut self) -> bool;
    fn set_nmi(&mut self, state: bool);

    fn poll_dmc(&mut self) -> bool;
    fn take_dmc(&mut self) -> bool;
    fn set_dmc(&mut self, state: bool);

    fn poll_irq(&mut self) -> bool;
    fn take_irq(&mut self) -> bool;
    fn set_irq(&mut self, state: bool);
}

pub struct InterruptImpl {
    nmi_interrupt: Option<u8>,
    dmc_interrupt: Option<u8>,
    irq_interrupt: Option<u8>,
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

    fn poll_dmc(&mut self) -> bool {
        self.dmc_interrupt.is_some()
    }

    fn take_dmc(&mut self) -> bool {
        let state = self.dmc_interrupt.take();
        state.is_some()
    }

    fn set_dmc(&mut self, state: bool) {
        if state {
            self.dmc_interrupt = Some(1);
        } else {
            self.dmc_interrupt = None;
        }
    }

    fn poll_irq(&mut self) -> bool {
        self.irq_interrupt.is_some()
    }

    fn take_irq(&mut self) -> bool {
        let state = self.irq_interrupt.take();
        state.is_some()
    }

    fn set_irq(&mut self, state: bool) {
        if state {
            self.irq_interrupt = Some(1);
        } else {
            self.irq_interrupt = None;
        }
    }
}

impl InterruptImpl {
    pub fn new() -> InterruptImpl {
        InterruptImpl {
            nmi_interrupt: None,
            dmc_interrupt: None,
            irq_interrupt: None,
        }
    }
}
