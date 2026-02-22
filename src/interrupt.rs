pub trait Interrupt {
    fn poll_nmi(&self) -> Option<u16>;
    fn take_nmi(&mut self) -> Option<u16>;
    fn set_nmi(&mut self, state: Option<u16>);

    fn poll_dmc(&mut self) -> bool;
    fn take_dmc(&mut self) -> bool;
    fn set_dmc(&mut self, state: bool);

    fn poll_irq(&self) -> bool;
    fn take_irq(&mut self) -> bool;
    fn set_irq(&mut self, state: bool);

    //bastardising slightly for oamdata handling :(
    fn poll_oam_data(&self) -> Option<u8>;
    fn take_oam_data(&mut self) -> Option<u8>;
    fn set_oam_data(&mut self, hi_byte: u8);
}

pub struct InterruptImpl {
    nmi_interrupt: Option<u16>,
    dmc_interrupt: Option<u8>,
    irq_interrupt: Option<u8>,
    oam_data: Option<u8>,
}

impl Interrupt for InterruptImpl {
    fn poll_nmi(&self) -> Option<u16> {
        self.nmi_interrupt
    }
    fn take_nmi(&mut self) -> Option<u16> {
        self.nmi_interrupt.take()
    }
    fn set_nmi(&mut self, state: Option<u16>) {
        match state {
            Some(data) => self.nmi_interrupt = Some(data),
            None => self.nmi_interrupt = None,
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

    fn poll_irq(&self) -> bool {
        self.irq_interrupt.as_ref().is_some()
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

    fn poll_oam_data(&self) -> Option<u8> {
        self.oam_data
    }

    fn set_oam_data(&mut self, hi_byte: u8) {
        self.oam_data = Some(hi_byte);
    }

    fn take_oam_data(&mut self) -> Option<u8> {
        self.oam_data.take()
    }
}

impl InterruptImpl {
    pub fn new() -> InterruptImpl {
        InterruptImpl {
            nmi_interrupt: None,
            dmc_interrupt: None,
            irq_interrupt: None,
            oam_data: None,
        }
    }
}
