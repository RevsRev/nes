use crate::{ppu::PPU, rom::Mirroring};

pub mod frame;
pub mod palette;

pub const WIDTH: usize = 256;
pub const HEIGHT: usize = 240;

pub fn background_pixel_at(ppu: &PPU, x: usize, y: usize) -> u8 {
    let scrolled_x = (x + ppu.scroll_x() as usize) % 512;
    let scrolled_y = (y + ppu.scroll_y() as usize) % 480;

    let tile_x = (scrolled_x % 256) / 8;
    let tile_y = (scrolled_y % 240) / 8;

    //TODO - SCROLLING?
    let nametables = get_nametables(ppu);
    let nametable = if scrolled_x > 255 || scrolled_y > 239 {
        nametables.1
    } else {
        nametables.0
    };

    let tile_idx = nametable[32 * tile_y + tile_x] as u16;

    let fine_y = scrolled_y % 8;
    let bank = ppu.ctl.bknd_pattern_addr();

    let lo_byte = &ppu.rom.borrow().chr_rom[(bank + tile_idx * 16) as usize + fine_y];
    let hi_byte = &ppu.rom.borrow().chr_rom[(bank + tile_idx * 16) as usize + fine_y + 8];

    let fine_x = 7 - (scrolled_x % 8);
    let bit = 1 << fine_x;

    let pixel_lo = (lo_byte & bit) >> fine_x;
    let pixel_hi = (hi_byte & bit) >> fine_x;

    let pixel = pixel_hi << 1 | pixel_lo;

    pixel
}

fn get_nametables(ppu: &PPU) -> (&[u8], &[u8]) {
    let (main_nametable, second_nametable) =
        match (&ppu.rom.borrow().screen_mirroring, ppu.nametable_address()) {
            (Mirroring::Vertical, 0x2000)
            | (Mirroring::Vertical, 0x2800)
            | (Mirroring::Horizontal, 0x2000)
            | (Mirroring::Horizontal, 0x2400) => (&ppu.vram[0..0x400], &ppu.vram[0x400..0x800]),
            (Mirroring::Vertical, 0x2400)
            | (Mirroring::Vertical, 0x2C00)
            | (Mirroring::Horizontal, 0x2800)
            | (Mirroring::Horizontal, 0x2C00) => (&ppu.vram[0x400..0x800], &ppu.vram[0..0x400]),
            (_, _) => panic!(
                "Unsupported mirroring type {:?}",
                ppu.rom.borrow().screen_mirroring
            ),
        };
    (main_nametable, second_nametable)
}
