use frame::Frame;
use palette::SYSTEM_PALLETE;

use crate::ppu::PPU;

pub mod frame;
pub mod palette;

pub fn render(frame: &mut Frame, ppu: &PPU) {
    let bank = ppu.ctl.bknd_pattern_addr();

    //960 tiles make up the screen
    for i in 0..0x03C0 {
        let tile = ppu.vram[i] as u16;
        let col = i % 32;
        let row = i / 32;

        let tile = &ppu.chr_rom[(bank + tile * 16) as usize..=(bank + tile * 16 + 15) as usize];
        let pallette = background_pallette(ppu, col, row);

        for y in 0..8 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];

            for x in (0..8).rev() {
                let value = (1 & lower) << 1 | (1 & upper);
                upper = upper >> 1;
                lower = lower >> 1;
                let rgb = match value {
                    0 => SYSTEM_PALLETE[ppu.palette_table[0] as usize],
                    1 => SYSTEM_PALLETE[pallette[1] as usize],
                    2 => SYSTEM_PALLETE[pallette[2] as usize],
                    3 => SYSTEM_PALLETE[pallette[3] as usize],
                    _ => panic!("Impossible!"),
                };
                frame.set_pixel(col * 8 + x, row * 8 + y, rgb);
            }
        }
    }
}

fn background_pallette(ppu: &PPU, tile_column: usize, tile_row: usize) -> [u8; 4] {
    let attr_table_index = 8 * (tile_row / 4) + tile_column / 4;
    let attr_byte = ppu.vram[0x03C0 + attr_table_index]; //hard coded first nametable

    let pallette_index = match ((tile_column % 4) / 2, (tile_row % 4) / 2) {
        (0, 0) => attr_byte & 0b11,
        (1, 0) => (attr_byte >> 2) & 0b11,
        (0, 1) => (attr_byte >> 4) & 0b11,
        (1, 1) => (attr_byte >> 6) & 0b11,
        (_, _) => panic!("Impossible!"),
    };

    let pallette_start: usize = 1 + (pallette_index as usize) * 4;
    [
        ppu.palette_table[0],
        ppu.palette_table[pallette_start],
        ppu.palette_table[pallette_start + 1],
        ppu.palette_table[pallette_start + 2],
    ]
}
