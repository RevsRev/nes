use frame::Frame;
use palette::SYSTEM_PALLETE;

use crate::ppu::PPU;

pub mod frame;
pub mod palette;

pub fn render(frame: &mut Frame, ppu: &PPU) {
    render_background(frame, ppu);
    render_sprites(frame, ppu);
}

fn render_background(frame: &mut Frame, ppu: &PPU) {
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

fn render_sprites(frame: &mut Frame, ppu: &PPU) {
    for i in (0..ppu.oam_data.len()).step_by(4).rev() {
        let tile_idx = ppu.oam_data[i + 1] as u16;
        let tile_x = ppu.oam_data[i + 3] as usize;
        let tile_y = ppu.oam_data[i] as usize;

        //load "settings" from the third byte
        let flip_vertical = (ppu.oam_data[i + 2] >> 7) & 1 == 1;
        let flip_horizontal = (ppu.oam_data[i + 2] >> 6) & 1 == 1;
        let pallette_idx = ppu.oam_data[i + 2] & 0b11;

        let sprite_pallette = sprite_pallette(ppu, pallette_idx);

        let bank = ppu.ctl.sprite_pattern_addr();

        let tile =
            &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];

        for y in 0..8 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];

            for x in (0..8).rev() {
                let value = (1 & lower) << 1 | (1 & upper);
                upper = upper >> 1;
                lower = lower >> 1;
                let rgb = match value {
                    0 => continue,
                    1 => SYSTEM_PALLETE[sprite_pallette[1] as usize],
                    2 => SYSTEM_PALLETE[sprite_pallette[2] as usize],
                    3 => SYSTEM_PALLETE[sprite_pallette[3] as usize],
                    _ => panic!("Impossible!"),
                };
                match (flip_horizontal, flip_vertical) {
                    (false, false) => frame.set_pixel(tile_x + x, tile_y + y, rgb),
                    (true, false) => frame.set_pixel(tile_x + 7 - x, tile_y + y, rgb),
                    (false, true) => frame.set_pixel(tile_x + x, tile_y + 7 - y, rgb),
                    (true, true) => frame.set_pixel(tile_x + 7 - x, tile_y + 7 - y, rgb),
                }
            }
        }
    }
}

fn sprite_pallette(ppu: &PPU, pallette_idx: u8) -> [u8; 4] {
    let start = 0x11 + (pallette_idx * 4) as usize;
    [
        0,
        ppu.palette_table[start],
        ppu.palette_table[start + 1],
        ppu.palette_table[start + 2],
    ]
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
