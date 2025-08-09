pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> Result<u8, String>;
    fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, String>;

    fn mem_read_u16(&mut self, addr: u16) -> Result<u16, String> {
        let lo = self.mem_read(addr)? as u16;
        let hi = self.mem_read(addr + 1)? as u16;
        Result::Ok((hi << 8) | lo)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) -> Result<u16, String> {
        let mut hi = (data >> 8) as u8;
        let mut lo = (data & 0xFF) as u8;
        lo = self.mem_write(addr, lo)?;
        hi = self.mem_write(addr + 1, hi)?;
        Result::Ok(((hi as u16) << 8) | (lo as u16))
    }
}
