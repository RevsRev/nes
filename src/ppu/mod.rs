use std::{cell::RefCell, rc::Rc};

use registers::{mask::MaskRegister, status::StatusRegister};
use sdl2::sys::SDL_PixelType;

use crate::{
    interrupt::{Interrupt, InterruptImpl},
    io::render::{background_pixel_at, frame::Frame, palette::SYSTEM_PALLETE},
    ppu::registers::ctl::ControlRegister,
    rom::{Mirroring, Rom},
    trace::PpuTrace,
    traits::tick::Tick,
};

pub mod registers;

pub const WIDTH: usize = 256;
pub const HEIGHT: usize = 240;

struct SpriteShift {
    start_x: u8,
    lo: u8,
    hi: u8,
    palette_idx: u8,
}

pub struct PPU {
    pub rom: Rc<RefCell<Rom>>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],

    //Internal registers, named to match ppu docs
    v: u16,
    t: u16,
    x: u8,
    w: u8,

    pub frame: Frame,

    internal_data_buf: u8,

    mask: MaskRegister,
    pub ctl: ControlRegister,
    status: StatusRegister,
    oam_addr: u8,
    interrupt: Rc<RefCell<InterruptImpl>>,
    tracing: bool,
    trace: Option<PpuTrace>,

    pub frame_dots: usize,
    total_ppu_cycles: u64,
    total_frames: u64,
    pub scanline: u16,
    pub new_frame: bool,
    scanline_sprites: Vec<SpriteShift>,

    nt_addr: u16,
    tile_idx: u16,
    attr_table_addr: u16,
    attr_byte: u8,
    pattern_lo_addr: u16,
    pattern_lo: u8,
    pattern_hi_addr: u16,
    pattern_hi: u8,

    bg_shift_lo: u16,
    bg_shift_hi: u16,
    attr_shift_lo: u16,
    attr_shift_hi: u16,
}

impl Tick for PPU {
    fn tick(&mut self, total_cpu_cycles: u64) -> Result<(), String> {
        self.total_ppu_cycles += 1;
        // self.frame_cycles += cycles as usize;
        self.new_frame = false;

        if self.frame_dots == 0 {
            self.compute_sprites();
        }

        let is_rendering_enabled =
            (self.scanline < 240 || self.scanline == 261) && self.mask.is_rendering_enabled();

        if self.frame_dots == 256 && is_rendering_enabled {
            self.increment_y();
        }

        if is_rendering_enabled && (self.frame_dots < 256 || self.frame_dots >= 320) {
            self.fetch_tile();
            if self.frame_dots >= 328 && self.frame_dots < 336 {
                //shift the registers
                self.bg_shift_lo = self.bg_shift_lo << 1;
                self.bg_shift_hi = self.bg_shift_hi << 1;
                self.attr_shift_lo = self.attr_shift_lo << 1;
                self.attr_shift_hi = self.attr_shift_hi << 1;
            }
        }

        if self.scanline < 240 && self.frame_dots < 256 {
            self.render_background();
            if self.is_sprite_0_hit() {
                self.status.set_sprite_0_hit(true);
            }
            self.render_sprites();
        }

        if self.frame_dots == 257 && is_rendering_enabled {
            self.v = (self.v & 0b0111_1011_1110_0000) | (self.t & 0b0000_0100_0001_1111);
        }

        if self.scanline == 261
            && self.frame_dots >= 280
            && self.frame_dots <= 304
            && is_rendering_enabled
        {
            self.v = (self.v & 0b0000_0100_0001_1111) | (self.t & 0b0111_1011_1110_0000);
        }

        if self.scanline == 241 && self.frame_dots == 1 {
            self.status.set_vblank(true);
            self.status.set_sprite_0_hit(false);
            if self.ctl.generate_vblank_nmi() {
                self.interrupt.borrow_mut().set_nmi(Some(0));
            }
        }

        if self.scanline == 261 && self.frame_dots == 1 {
            self.status.set_vblank(false);
            self.interrupt.borrow_mut().set_nmi(None);
            self.status.set_sprite_0_hit(false);
        }

        self.frame_dots += 1;

        if self.frame_dots == 340
            && self.scanline == 261
            && self.total_frames % 2 == 1
            && self.mask.is_rendering_enabled()
        {
            self.frame_dots += 1;
        }

        if self.frame_dots == 341 {
            self.frame_dots = 0;
            self.scanline += 1;
        }

        if self.scanline == 262 {
            self.scanline = 0;
            self.new_frame = true;
            self.total_frames = self.total_frames + 1;
        }
        Ok(())
    }
}

impl PPU {
    pub fn new(rom: Rc<RefCell<Rom>>, interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        PPU {
            rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],

            v: 0,
            t: 0,
            x: 0,
            w: 0,

            frame: Frame::new(),

            internal_data_buf: 0x0000,

            mask: MaskRegister::new(),
            ctl: ControlRegister::new(),
            status: StatusRegister::new(),
            oam_addr: 0,
            interrupt: interrupt,
            tracing: false,
            trace: None,

            total_ppu_cycles: 0,
            total_frames: 0,
            frame_dots: 0,
            scanline: 0,
            new_frame: false,
            scanline_sprites: Vec::new(),

            nt_addr: 0,
            tile_idx: 0,
            attr_table_addr: 0,
            attr_byte: 0,
            pattern_lo_addr: 0,
            pattern_lo: 0,
            pattern_hi_addr: 0,
            pattern_hi: 0,

            bg_shift_lo: 0,
            bg_shift_hi: 0,
            attr_shift_lo: 0,
            attr_shift_hi: 0,
        }
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
    }

    pub fn trace(&self) -> PpuTrace {
        PpuTrace {
            frame: self.total_frames,
            scanline: self.scanline,
            dot: self.frame_dots as u16,
            status: self.status.snapshot(),
            mask: self.mask.read(),
            t: self.t,
            v: self.v,
            sprite_zero_x: self.oam_data[3],
            sprite_zero_y: self.oam_data[0],
        }
    }

    pub fn take_trace(&mut self) -> Option<PpuTrace> {
        let retval = self.trace.take();
        self.trace = Some(self.trace());
        retval
    }

    pub fn trace_now(&self) -> PpuTrace {
        self.trace()
    }

    pub fn read_data(&mut self) -> Result<u8, String> {
        let addr = self.v;
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
        let addr = self.v;
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
        self.v = self.v + self.ctl.vram_addr_increment() as u16;
        if self.v > 0x3FFF {
            self.v = self.v & 0b111111_11111111;
        }
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) -> u8 {
        let retval: u8;
        if self.w == 0 {
            retval = value & 0b0011_1111;
            self.t = (self.t & 0b0000_0000_1111_1111) | ((retval as u16) << 8);
        } else {
            retval = value;
            self.t = (self.t & 0b0111_1111_0000_0000) | (retval as u16);
            self.v = self.t; //TODO - Docs say this happens 1 to 1.5 dots later, but I'm not sure
            //it matters that much (?)
        }
        self.w = (self.w + 1) % 2;
        retval
    }

    pub fn write_to_ctl(&mut self, value: u8) -> u8 {
        self.t = (self.t & 0b0111_0011_1111_1111) | (((value as u16) & 0b11) << 10);

        let before_nmi_status = self.ctl.generate_vblank_nmi();
        let retval = self.ctl.update(value);
        if !before_nmi_status && self.ctl.generate_vblank_nmi() && self.status.is_vblank() {
            self.interrupt.borrow_mut().set_nmi(Some(0));
        }
        retval
    }

    pub fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.snapshot();
        self.w = 0;
        self.status.set_vblank(false);
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
        if self.w == 0 {
            self.x = value & 0b111;
            self.t = (self.t & 0b0111_1111_1110_0000) | (value as u16 >> 3);
        } else {
            let set = ((value as u16 & 0b111) << 12) | ((value as u16 & 0b1111_1000) << 2);
            self.t = (self.t & 0b1000_1100_0001_1111) | set;
        }
        self.w = (self.w + 1) % 2;
        value
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

    pub fn is_sprite_0_hit(&self) -> bool {
        if !self.mask.is_rendering_enabled() {
            return false;
        }

        let x = self.frame_dots;
        let y = self.scanline as usize;

        let sprite_x = self.oam_data[3] as usize;
        let sprite_y = self.oam_data[0] as usize;

        if x <= 7usize && self.mask.is_left_side_clipping_window_enabled() {
            return false;
        }

        if x == 255usize {
            return false;
        }

        let sprite_height = self.ctl.sprite_size() as usize;

        let y_hit = sprite_y <= y && y < (sprite_y + sprite_height);

        if !y_hit {
            return false;
        }

        let x_hit = (sprite_x <= x) && (x < sprite_x + 8);

        if !x_hit {
            return false;
        }

        // self.frame.get_bg_pixel_at(x, y) != 0
        return self.frame.get_bg_pixel_at(x, y) != 0 && self.frame.get_sprite_pixel_at(x, y) != 0;
    }

    pub fn nametable_address(&self) -> u16 {
        0x2000 | (self.v & 0b0000_1100_0000_0000)
    }

    fn increment_y(&mut self) {
        if (self.v & 0x7000) != 0x7000 {
            self.v = self.v + 0x1000;
        } else {
            self.v = self.v & !0x7000;
            let mut y = (self.v & 0x03E0) >> 5;
            if y == 0x1D {
                y = 0;
                self.v = self.v ^ 0x0800;
            } else if y == 0x1F {
                y = 0;
            } else {
                y = y + 1;
            }
            self.v = (self.v & !0x03E0) | (y << 5);
        }
    }

    fn increment_coarse_x(&mut self) {
        if (self.v & 0x001F) == 0x1F {
            self.v = self.v & !0x001F;
            self.v = self.v ^ 0x0400;
        } else {
            self.v = self.v + 1;
        }
    }

    fn nametable_select(&self) -> u8 {
        (self.v >> 10) as u8 & 0b11
    }

    pub fn scroll_x(&self) -> u16 {
        (self.v & 0x1F) << 3 | (self.x as u16 & 0b111)
    }

    pub fn scroll_y(&self) -> u16 {
        ((self.v >> 2) & 0b1111_1000) | ((self.v >> 12) & 0b111)
    }

    fn fine_scroll_x(&self) -> u8 {
        self.x & 0b111
    }

    fn fine_scroll_y(&self) -> u8 {
        (self.v >> 12) as u8 & 0b111
    }

    fn coarse_scroll_x(&self) -> u8 {
        self.v as u8 & 0x1F
    }

    fn coarse_scroll_y(&self) -> u8 {
        (self.v >> 5) as u8 & 0x1F
    }

    fn fetch_tile(&mut self) {
        match self.frame_dots % 8 {
            0 => {
                let nt_select = ((self.nametable_address() >> 10) & 0b11) as u8;
                self.nt_addr = self.get_nametable(nt_select) as u16;
            }
            1 => {
                let tile_x = self.coarse_scroll_x() as u16;
                let tile_y = self.coarse_scroll_y() as u16;
                self.tile_idx = self.vram[(self.nt_addr + 32 * tile_y + tile_x) as usize] as u16;
            }
            2 => {
                let tile_x = self.coarse_scroll_x() as u16;
                let tile_y = self.coarse_scroll_y() as u16;

                let nt_select = ((self.nametable_address() >> 10) & 0b11) as u8;
                let nt_addr = self.get_nametable(nt_select) as u16;

                self.attr_table_addr = nt_addr + 0x3C0 + 8 * (tile_y / 4) + tile_x / 4;
            }
            3 => {
                self.attr_byte = self.vram[self.attr_table_addr as usize];
            }
            4 => {
                let bank = self.ctl.bknd_pattern_addr();
                self.pattern_lo_addr = bank + self.tile_idx * 16 + self.fine_scroll_y() as u16;
            }
            5 => {
                self.pattern_lo = self.rom.borrow().chr_rom[self.pattern_lo_addr as usize];
            }
            6 => {
                let bank = self.ctl.bknd_pattern_addr();
                self.pattern_hi_addr = bank + self.tile_idx * 16 + self.fine_scroll_y() as u16 + 8;
            }
            7 => {
                self.pattern_hi = self.rom.borrow().chr_rom[self.pattern_hi_addr as usize];

                self.bg_shift_lo = (self.bg_shift_lo & 0xFF00) | self.pattern_lo as u16;
                self.bg_shift_hi = (self.bg_shift_hi & 0xFF00) | self.pattern_hi as u16;

                let shift =
                    ((self.coarse_scroll_y() % 4) / 2) * 4 + ((self.coarse_scroll_x() % 4) / 2) * 2;
                let palette_bits = (self.attr_byte >> shift) & 0b11;

                let palette_lo = if palette_bits & 0b01 != 0 { 0xFF } else { 0x00 };
                let palette_hi = if palette_bits & 0b10 != 0 { 0xFF } else { 0x00 };

                self.attr_shift_lo = (self.attr_shift_lo & 0xFF00) | palette_lo;
                self.attr_shift_hi = (self.attr_shift_hi & 0xFF00) | palette_hi;

                self.increment_coarse_x();
            }
            _ => {}
        };
    }

    fn render_background(&mut self) {
        let bit_index = 15 - self.fine_scroll_x();
        let bg_lo = (self.bg_shift_lo >> bit_index) & 1;
        let bg_hi = (self.bg_shift_hi >> bit_index) & 1;
        let bg_value = ((1 & bg_hi) << 1 | bg_lo) as u8;

        let attr_lo = (self.attr_shift_lo >> bit_index) & 1;
        let attr_hi = (self.attr_shift_hi >> bit_index) & 1;

        let palette_bits = ((attr_hi << 1) | attr_lo) as u8;

        let rgb = if bg_value == 0 {
            SYSTEM_PALLETE[self.palette_table[0] as usize]
        } else {
            let palette_index = (palette_bits << 2) | bg_value;
            let system_palette_index = self.palette_table[palette_index as usize] as usize;

            SYSTEM_PALLETE[system_palette_index]
        };

        //shift the registers
        self.bg_shift_lo = self.bg_shift_lo << 1;
        self.bg_shift_hi = self.bg_shift_hi << 1;
        self.attr_shift_lo = self.attr_shift_lo << 1;
        self.attr_shift_hi = self.attr_shift_hi << 1;

        self.frame
            .set_background_pixel(self.frame_dots, self.scanline as usize, rgb, bg_value);
    }

    fn get_nametable(&self, nt_select: u8) -> usize {
        match self.rom.borrow().screen_mirroring {
            Mirroring::Vertical => {
                return match nt_select {
                    0b00 | 0b11 => 0x000,
                    0b10 | 0b01 => 0x400,
                    _ => unreachable!("Impossible!"),
                };
            }
            Mirroring::Horizontal => {
                return match nt_select {
                    0b00 | 0b01 => 0x000,
                    0b10 | 0b11 => 0x400,
                    _ => unreachable!("Impossible!"),
                };
            }

            Mirroring::FourScreen => todo!("FourScreen mirroring is not implemented"),
        }
    }

    fn compute_sprites(&mut self) {
        self.scanline_sprites.clear();

        for i in (0..self.oam_data.len()).step_by(4).rev() {
            let tile_y = self.oam_data[i] as u16;
            let sprite_height = self.ctl.sprite_size() as u16;

            if !(tile_y <= self.scanline && self.scanline < tile_y + sprite_height) {
                continue;
            }

            let flip_v = (self.oam_data[i + 2] >> 7) & 1 == 1;
            let y = if flip_v {
                7 - (self.scanline - tile_y) as usize
            } else {
                (self.scanline - tile_y) as usize
            };

            let bank = self.ctl.sprite_pattern_addr();
            let tile_idx = self.oam_data[i + 1] as u16;
            let tile = &self.rom.borrow().chr_rom
                [(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];

            let flip_h = (self.oam_data[i + 2] >> 6) & 1 == 1;

            let (lo, hi) = if flip_h {
                (tile[y].reverse_bits(), tile[y + 8].reverse_bits())
            } else {
                (tile[y], tile[y + 8])
            };

            self.scanline_sprites.push(SpriteShift {
                start_x: self.oam_data[i + 3],
                lo: lo,
                hi: hi,
                palette_idx: self.oam_data[i + 2] & 0b11,
            });

            if self.scanline_sprites.len() == 8 {
                break;
            }
        }
    }

    fn render_sprites(&mut self) {
        for sprite_shift in &self.scanline_sprites {
            let start_x = sprite_shift.start_x as usize;
            if !(start_x <= self.frame_dots && self.frame_dots < start_x + 8) {
                continue;
            }

            let bit_shift = 7 - (self.frame_dots - start_x);
            let lo = sprite_shift.lo >> bit_shift;
            let hi = sprite_shift.hi >> bit_shift;
            let value = ((1 & hi) << 1) | (lo & 1);

            let sprite_palette = self.sprite_pallette(sprite_shift.palette_idx);

            let rgb = match value {
                0 => continue,
                1 => SYSTEM_PALLETE[sprite_palette[1] as usize],
                2 => SYSTEM_PALLETE[sprite_palette[2] as usize],
                3 => SYSTEM_PALLETE[sprite_palette[3] as usize],
                _ => panic!("Impossible!"),
            };

            self.frame
                .set_sprite_pixel(self.frame_dots, self.scanline as usize, rgb, value);
        }
    }

    fn sprite_pallette(&self, pallette_idx: u8) -> [u8; 4] {
        let start = 0x11 + (pallette_idx * 4) as usize;
        [
            0,
            self.palette_table[start],
            self.palette_table[start + 1],
            self.palette_table[start + 2],
        ]
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

        println!("v, t = {:04X}, {:04X}", ppu.v, ppu.t);
        ppu.write_to_ppu_addr(0x23);

        println!("v, t = {:04X}, {:04X}", ppu.v, ppu.t);
        ppu.write_to_ppu_addr(0x05);

        println!("v, t = {:04X}, {:04X}", ppu.v, ppu.t);

        ppu.read_data().unwrap(); //load_into_buffer
        assert_eq!(ppu.v, 0x2306);
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

    #[test]
    fn test_write_to_scroll() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);

        //1st Write: A B C D E F G H
        //           0 1 1 1 0 1 1 0
        // t = ...._...._...ABCDE
        // x = FGH

        let write = 0b0111_0110;
        ppu.write_to_scroll(write);
        assert_eq!(0b0000_0000_0000_1110, ppu.t);
        assert_eq!(0b110, ppu.x);
        assert_eq!(1, ppu.w);

        //2nd Write: I J K L M N O P
        //           1 0 0 1 1 1 0 0
        // t = .NOP_..IJ_KLM._....

        let write_2 = 0b1001_1100;
        ppu.write_to_scroll(write_2);
        assert_eq!(0b0100_0010_0110_1110, ppu.t);
        assert_eq!(0, ppu.w);

        ppu.v = ppu.t; //just to check our scroll_x & scroll_y look correct here...
        assert_eq!(0b0111_0110, ppu.scroll_x());
        assert_eq!(0b1001_1100, ppu.scroll_y());
    }

    #[test]
    fn test_write_to_ctl() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);

        ppu.write_to_ctl(0b000_0011);

        assert_eq!(0b0000_1100_0000_0000, ppu.t);

        ppu.v = ppu.t;
        assert_eq!(ppu.nametable_select(), 0b11);
    }

    #[test]
    fn test_write_to_ppu_addr() {
        let mut ppu = ppu_empty_rom(Mirroring::Horizontal);

        ppu.t = 0b0100_0000_0000_0000; //also check bit 14 is cleared

        ppu.write_to_ppu_addr(0b1010_1011);
        assert_eq!(0b0010_1011_0000_0000, ppu.t);
        assert_eq!(1, ppu.w);

        ppu.write_to_ppu_addr(0b1100_1010);
        assert_eq!(0b0010_1011_1100_1010, ppu.t);
        assert_eq!(ppu.t, ppu.v);
        assert_eq!(0, ppu.w);
    }
}
