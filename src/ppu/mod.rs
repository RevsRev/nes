use std::{cell::RefCell, rc::Rc};

use registers::{mask::MaskRegister, scroll::ScrollRegister, status::StatusRegister};

use crate::{
    interrupt::{Interrupt, InterruptImpl},
    ppu::registers::{addr::AddrRegister, ctl::ControlRegister},
    rom::{Mirroring, Rom},
    trace::PpuTrace,
    traits::tick::Tick,
};

pub mod registers;

pub struct PPU {
    pub rom: Rc<RefCell<Rom>>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],

    internal_data_buf: u8,

    addr: AddrRegister,
    pub scroll: ScrollRegister,
    mask: MaskRegister,
    pub ctl: ControlRegister,
    status: StatusRegister,
    oam_addr: u8,
    interrupt: Rc<RefCell<InterruptImpl>>,

    pub frame_cycles: usize,
    total_cycles: u64,
    pub scanline: u16,
    pub new_frame: bool,
}

impl Tick for PPU {
    fn tick(&mut self, cycles: u8) {
        self.total_cycles += cycles as u64;
        // self.frame_cycles += cycles as usize;
        self.new_frame = false;

        for _ in 0..cycles {
            if self.is_sprite_0_hit(self.frame_cycles) {
                self.status.set_sprite_0_hit(true);
            }

            if self.scanline == 241 && self.frame_cycles == 1 {
                self.status.set_vblank(true);
                self.status.set_sprite_0_hit(false);
                if self.ctl.generate_vblank_nmi() {
                    self.interrupt.borrow_mut().set_nmi(true);
                }
            }

            if self.scanline == 261 && self.frame_cycles == 1 {
                self.status.set_vblank(false);
                self.interrupt.borrow_mut().set_nmi(false);
                self.status.set_sprite_0_hit(false);
            }

            self.frame_cycles += 1;
            if self.frame_cycles == 341 {
                self.frame_cycles = 0;
                self.scanline += 1;
            }

            if self.scanline == 262 {
                self.scanline = 0;
                self.new_frame = true;
            }
        }
    }
}

impl PPU {
    pub fn new(rom: Rc<RefCell<Rom>>, interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        PPU {
            rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],

            internal_data_buf: 0x0000,

            mask: MaskRegister::new(),
            scroll: ScrollRegister::new(),
            addr: AddrRegister::new(),
            ctl: ControlRegister::new(),
            status: StatusRegister::new(),
            oam_addr: 0,
            interrupt: interrupt,

            total_cycles: 0,
            frame_cycles: 0,
            scanline: 0,
            new_frame: false,
        }
    }

    pub fn trace(&self) -> PpuTrace {
        PpuTrace {
            scanline: self.scanline,
            dot: self.frame_cycles as u16,
        }
    }

    pub fn read_data(&mut self) -> Result<u8, String> {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0x0000..=0x1FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.rom.borrow().chr_rom[addr as usize];
                Result::Ok(result)
            }
            0x2000..=0x2FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                Result::Ok(result)
            }

            0x3F00..=0x3FFF => {
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr & 0x2FFF) as usize];
                Result::Ok(self.palette_table[(self.mirror_pallette_addr(addr) - 0x3F00) as usize])
            }
            _ => Result::Err(format!("Unexpected access to mirrored space {:#04X}", addr)),
        }
    }

    pub fn write_data(&mut self, value: u8) -> Result<u8, String> {
        let retval: Result<u8, String>;
        let addr = self.addr.get();
        match addr {
            0x0000..=0x1FFF => {
                retval = Result::Ok(self.rom.borrow_mut().write_to_chr_rom(addr as usize, value));
            }
            0x2000..=0x2FFF => {
                retval = Result::Ok(self.vram[self.mirror_vram_addr(addr) as usize]);
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            }

            0x3F00..=0x3FFF => {
                retval = Result::Ok(
                    self.palette_table[(self.mirror_pallette_addr(addr) - 0x3F00) as usize],
                );
                self.palette_table[(self.mirror_pallette_addr(addr) - 0x3F00) as usize] = value;
            }
            _ => retval = Result::Err(format!("Unexpected access to mirrored space {:#04X}", addr)),
        }

        self.increment_vram_addr();
        retval
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
        match (&self.rom.borrow().screen_mirroring, name_table) {
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
            0x3F10 => 0x3F00,
            0x3F14 => 0x3F04,
            0x3F18 => 0x3F08,
            0x3F1C => 0x3F0C,
            v => v,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctl.vram_addr_increment());
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) -> u8 {
        self.addr.update(value)
    }

    pub fn write_to_ctl(&mut self, value: u8) -> u8 {
        let before_nmi_status = self.ctl.generate_vblank_nmi();
        let retval = self.ctl.update(value);
        if !before_nmi_status && self.ctl.generate_vblank_nmi() && self.status.is_vblank() {
            self.interrupt.borrow_mut().set_nmi(true);
        }
        retval
    }

    pub fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.snapshot();
        self.status.set_vblank(false);
        self.addr.reset_latch();
        self.scroll.reset_latch();
        data
    }

    pub fn write_to_oam_addr(&mut self, value: u8) -> u8 {
        let retval = self.oam_addr;
        self.oam_addr = value;
        retval
    }

    pub fn write_to_oam_data(&mut self, value: u8) -> u8 {
        let retval = self.oam_data[self.oam_addr as usize];
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
        retval
    }

    pub fn write_to_mask(&mut self, value: u8) -> u8 {
        self.mask.write(value)
    }

    pub fn write_to_scroll(&mut self, value: u8) -> u8 {
        self.scroll.write(value)
    }

    pub fn write_to_oam_dma(&mut self, data: u8) -> u8 {
        if self.interrupt.borrow().poll_oam_data().is_none() {
            self.interrupt.borrow_mut().set_oam_data(data);
            return 0;
        }

        let old = self.oam_data[self.oam_addr as usize];
        self.oam_data[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
        old
    }

    pub fn is_sprite_0_hit(&self, cycle: usize) -> bool {
        let y = self.oam_data[0] as usize;
        let x = self.oam_data[3] as usize;
        (y == self.scanline as usize) && x <= cycle && self.mask.show_sprites()
    }
}
#[cfg(test)]
pub mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        interrupt::InterruptImpl,
        ppu::PPU,
        rom::{Mirroring, Rom},
    };

    fn ppu_empty_rom(mirroring: Mirroring) -> PPU {
        let mirror_byte = match mirroring {
            Mirroring::Vertical => 0b1,
            Mirroring::Horizontal => 0b0,
            Mirroring::FourScreen => 0b1000,
        };

        //TODO - set mirroring on the rom we create?
        let rom = Rom::new(&vec![
            0x4E,
            0x45,
            0x53,
            0x1A,
            0x00,
            0x00,
            mirror_byte,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
        ])
        .unwrap();
        PPU::new(
            Rc::new(RefCell::new(rom)),
            Rc::new(RefCell::new(InterruptImpl::new())),
        )
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

        ppu.read_data().unwrap(); //load_into_buffer
        assert_eq!(ppu.addr.get(), 0x2306);
        assert_eq!(ppu.read_data().unwrap(), 0x66);
    }

    #[test]
    fn test_ppu_vram_reads_cross_page() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctl(0);
        ppu.vram[0x01ff] = 0x66;
        ppu.vram[0x0200] = 0x77;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0xff);

        ppu.read_data().unwrap(); //load_into_buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66);
        assert_eq!(ppu.read_data().unwrap(), 0x77);
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

        ppu.read_data().unwrap(); //load_into_buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66);
        assert_eq!(ppu.read_data().unwrap(), 0x77);
        assert_eq!(ppu.read_data().unwrap(), 0x88);
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

        ppu.read_data().unwrap(); //load into buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66); //read from A

        ppu.write_to_ppu_addr(0x2C);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data().unwrap(); //load into buffer
        assert_eq!(ppu.read_data().unwrap(), 0x77); //read from b
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

        ppu.read_data().unwrap(); //load into buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66); //read from a

        ppu.write_to_ppu_addr(0x24);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data().unwrap(); //load into buffer
        assert_eq!(ppu.read_data().unwrap(), 0x77); //read from B
    }

    #[test]
    fn test_read_status_resets_latch() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x21);
        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data().unwrap(); //load_into_buffer
        assert_ne!(ppu.read_data().unwrap(), 0x66);

        ppu.read_status();

        ppu.write_to_ppu_addr(0x23);
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data().unwrap(); //load_into_buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66);
    }

    #[test]
    fn test_ppu_vram_mirroring() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_ctl(0);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_ppu_addr(0x63); //0x6305 -> 0x2305
        ppu.write_to_ppu_addr(0x05);

        ppu.read_data().unwrap(); //load into_buffer
        assert_eq!(ppu.read_data().unwrap(), 0x66);
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

    #[test]
    fn test_oam_read_write() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);
        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_data(0x66);
        ppu.write_to_oam_data(0x77);

        ppu.write_to_oam_addr(0x10);
        assert_eq!(ppu.read_oam_data(), 0x66);

        ppu.write_to_oam_addr(0x11);
        assert_eq!(ppu.read_oam_data(), 0x77);
    }

    #[test]
    fn test_oam_dma() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);

        let mut data = [0x66; 256];
        data[0] = 0x77;
        data[255] = 0x88;

        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_dma(0x00);

        for i in 0..256 {
            ppu.write_to_oam_dma(data[i]);
        }

        ppu.write_to_oam_addr(0xf); //wrap around
        assert_eq!(ppu.read_oam_data(), 0x88);

        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_addr(0x77);
        ppu.write_to_oam_addr(0x11);
        ppu.write_to_oam_addr(0x66);
    }
}
