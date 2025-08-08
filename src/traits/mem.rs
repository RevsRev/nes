pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8) -> u8;

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        let lo = self.mem_read(addr) as u16;
        let hi = self.mem_read(addr + 1) as u16;
        return (hi << 8) | lo;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) -> u16 {
        let mut hi = (data >> 8) as u8;
        let mut lo = (data & 0xFF) as u8;
        lo = self.mem_write(addr, lo);
        hi = self.mem_write(addr + 1, hi);
        ((hi as u16) << 8) | (lo as u16)
    }
}
