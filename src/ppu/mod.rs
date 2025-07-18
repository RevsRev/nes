use registers::status::StatusRegister;

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
    status: StatusRegister,
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
            status: StatusRegister::new(),
        }
    }

    pub fn read_data(&mut self) -> u8 {
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
            0x3F00..=0x3FFF => self.palette_table[self.mirror_pallette_addr(addr) as usize],
            _ => panic!("Unexpected access to mirrored space {:#04X}", addr),
        }
    }

    pub fn write_data(&mut self, value: u8) {
        let addr = self.addr.get();

        match addr {
            0x0000..=0x1FFF => {
                panic!("Attempt to write to chr rom space {:#04X}", addr);
            }
            0x2000..=0x2FFF => {
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            }
            0x3000..=0x3EFF => panic!(
                "addr space 0x3000..0x3EFF is not expected to be used. Requested: {:#04X}",
                addr
            ),
            0x3F00..=0x3FFF => {
                self.palette_table[self.mirror_pallette_addr(addr) as usize] = value;
            }
            _ => panic!("Unexpected access to mirrored space {:#04X}", addr),
        }

        self.increment_vram_addr();
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        // Horrizontal
        // A a
        // B b
        //
        // Vertical
        // A B
        // a b

        let mirrored_vram = addr & 0b0010_1111_1111_1111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
        let vram_index = mirrored_vram - 0x2000; // to vram vector
        let name_table = vram_index / 0x400;
        match (&self.mirroring, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }

    fn mirror_pallette_addr(&self, addr: u16) -> u16 {
        //0x3F00 - 0x3FFF
        //Mirror every 32 bytes
        let first_mirror = addr & 0b1111_1111_0001_1111;

        //Now, 0x3F04, 0x3F08 0x3F0C are all mirrors of 0x3F00 (universal background colour)
        match first_mirror {
            0x3F04 | 0x3f08 | 0x3F0C => 0x3F00,
            v => v,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctl.vram_addr_increment());
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    pub fn write_to_ctl(&mut self, value: u8) {
        self.ctl.update(value);
    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.snapshot();
        self.status.reset_vblank_status();
        self.addr.reset_latch();
        //self.scroll.reset_latch();
        data
    }

    fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctl.generate_vblank_nmi();
        self.ctl.update(value);
    }
}
#[cfg(test)]
pub mod test {
    use crate::{ppu::PPU, rom::Mirroring};

    fn ppu_empty_rom(mirroring: Mirroring) -> PPU {
        PPU::new(vec![0; 2048], mirroring)
    }

    #[test]
    fn test_ppu_vram_writes() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);
        ppu.write_data(0x66);

        assert_eq!(ppu.vram[0x0305], 0x66);
    }

    #[test]
    fn test_ppu_vram_reads() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctl(0);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load_into_buffer
        assert_eq!(ppu.addr.get(), 0x2306);
        assert_eq!(ppu.read_data(), 0x66);
    }

    #[test]
    fn test_ppu_vram_reads_cross_page() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctl(0);
        ppu.vram[0x01ff] = 0x66;
        ppu.vram[0x0200] = 0x77;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0xff);

        ppu.read_data(); //load_into_buffer
        assert_eq!(ppu.read_data(), 0x66);
        assert_eq!(ppu.read_data(), 0x77);
    }

    #[test]
    fn test_ppu_vram_reads_step_32() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctl(0b100);
        ppu.vram[0x01ff] = 0x66;
        ppu.vram[0x01ff + 32] = 0x77;
        ppu.vram[0x01ff + 64] = 0x88;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0xff);

        ppu.read_data(); //load_into_buffer
        assert_eq!(ppu.read_data(), 0x66);
        assert_eq!(ppu.read_data(), 0x77);
        assert_eq!(ppu.read_data(), 0x88);
    }

    // Horizontal: https://wiki.nesdev.com/w/index.php/Mirroring
    //   [0x2000 A ] [0x2400 a ]
    //   [0x2800 B ] [0x2C00 b ]
    #[test]
    fn test_vram_horizontal_mirror() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ppu_addr(0x24);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_data(0x66); //write to a

        ppu.write_to_ppu_addr(0x28);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_data(0x77); //write to B

        ppu.write_to_ppu_addr(0x20);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x66); //read from A

        ppu.write_to_ppu_addr(0x2C);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x77); //read from b
    }

    // Vertical: https://wiki.nesdev.com/w/index.php/Mirroring
    //   [0x2000 A ] [0x2400 B ]
    //   [0x2800 a ] [0x2C00 b ]
    #[test]
    fn test_vram_vertical_mirror() {
        let mut ppu = ppu_empty_rom(Mirroring::Vertical);

        ppu.write_to_ppu_addr(0x20);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_data(0x66); //write to A

        ppu.write_to_ppu_addr(0x2C);
        ppu.write_to_ppu_addr(0x05);

        ppu.write_data(0x77); //write to b

        ppu.write_to_ppu_addr(0x28);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x66); //read from a

        ppu.write_to_ppu_addr(0x24);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into buffer
        assert_eq!(ppu.read_data(), 0x77); //read from B
    }

    #[test]
    fn test_read_status_resets_latch() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load_into_buffer
        assert_ne!(ppu.read_data(), 0x66);

        ppu.read_status();

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load_into_buffer
        assert_eq!(ppu.read_data(), 0x66);
    }

    #[test]
    fn test_ppu_vram_mirroring() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctrl(0);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x63); //0x6305 -> 0x2305
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data(); //load into_buffer
        assert_eq!(ppu.read_data(), 0x66);
        // assert_eq!(ppu.addr.read(), 0x0306)
    }

    #[test]
    fn test_read_status_resets_vblank() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.status.set_vblank(true);

        let status = ppu.read_status();

        assert_eq!(status >> 7, 1);
        assert_eq!(ppu.status.snapshot() >> 7, 0);
    }

    // #[test]
    // fn test_oam_read_write() {
    //     let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
    //     ppu.write_to_oam_addr(0x10);
    //     ppu.write_to_oam_data(0x66);
    //     ppu.write_to_oam_data(0x77);
    //
    //     ppu.write_to_oam_addr(0x10);
    //     assert_eq!(ppu.read_oam_data(), 0x66);
    //
    //     ppu.write_to_oam_addr(0x11);
    //     assert_eq!(ppu.read_oam_data(), 0x77);
    // }
    //
    // #[test]
    // fn test_oam_dma() {
    //     let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
    //
    //     let mut data = [0x66; 256];
    //     data[0] = 0x77;
    //     data[255] = 0x88;
    //
    //     ppu.write_to_oam_addr(0x10);
    //     ppu.write_oam_dma(&data);
    //
    //     ppu.write_to_oam_addr(0xf); //wrap around
    //     assert_eq!(ppu.read_oam_data(), 0x88);
    //
    //     ppu.write_to_oam_addr(0x10);
    //     assert_eq!(ppu.read_oam_data(), 0x77);
    //
    //     ppu.write_to_oam_addr(0x11);
    //     assert_eq!(ppu.read_oam_data(), 0x66);
    // }
}
