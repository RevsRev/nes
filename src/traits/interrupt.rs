pub enum InterruptType {
    Nmi,
}

pub trait Interrupting {
    fn poll(&self, interrupt_type: &InterruptType) -> Option<u8>;
    fn take(&mut self, interrupt_type: &InterruptType) -> Option<u8>;
}
