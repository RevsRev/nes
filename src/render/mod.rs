use frame::Frame;

use crate::ppu::PPU;

pub mod frame;
pub mod palette;

pub fn show_tile(frame: &mut Frame, chr_rom: &Vec<u8>, bank: usize, tile_n: usize) {
    assert!(bank <= 1);

    let bank = (bank * 0x1000) as usize;

    let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

    let col = tile_n % 32;
    let row = tile_n / 32;

    for y in 0..=7 {
        let mut upper = tile[y];
        let mut lower = tile[y + 8];

        for x in (0..=7).rev() {
            //There are 4 "colours": 00, 01, 10 & 11
            let value = (0b1 & upper) << 1 | (0b1 & lower);
            upper = upper >> 1;
            lower = lower >> 1;
            let rgb = match value {
                0 => palette::SYSTEM_PALLETE[0x01],
                1 => palette::SYSTEM_PALLETE[0x23],
                2 => palette::SYSTEM_PALLETE[0x27],
                3 => palette::SYSTEM_PALLETE[0x30],
                _ => panic!("Impossible!"),
            };
            frame.set_pixel(col * 8 + x, row * 8 + y, rgb);
            // frame.set_pixel(x, y, rgb);
        }
    }
}
