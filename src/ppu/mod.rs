use crate::{
    ppu::registers::{addr::AddrRegister, ctl::ControlRegister},
    rom::Mirroring,
};

pub mod registers;

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],
    pub mirroring: Mirroring,

    internal_data_buf: u8,

    addr: AddrRegister,
    pub ctl: ControlRegister,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom: chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],
            mirroring: mirroring,

            internal_data_buf: 0x0000,

            addr: AddrRegister::new(),
            ctl: ControlRegister::new(),
        }
    }

    fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0x0000..=0x1FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3EFF => panic!(
                "addr space 0x3000..0x3EFF is not expected to be used. Requested: {:#04X}",
                addr
            ),
            0x3F00..=0x3FFF => self.palette_table[(addr - 0x3F00) as usize],
            _ => panic!("Unexpected access to mirrored space {:#04X}", addr),
        }
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        // Horrizontal
        // A a
        // B b
        //
        // Vertical
        // A B
        // a b

        let mirrored_vram = addr & 0b0001_1111_1111_1111;
        let vram_index = mirrored_vram - 0x2000;
        let page = vram_index / 0x0400;
        match (&self.mirroring, page) {
            (Mirroring::Horizontal, 0) | (Mirroring::Horizontal, 1) => vram_index % 0x0400,
            (Mirroring::Horizontal, 2) | (Mirroring::Horizontal, 3) => {
                0x0400 + (vram_index % 0x0400)
            }
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctl.vram_addr_increment());
    }

    fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    fn write_to_ctl(&mut self, value: u8) {
        self.ctl.update(value);
    }
}
