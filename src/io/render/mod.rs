use frame::Frame;
use palette::SYSTEM_PALLETE;

use crate::{ppu::PPU, rom::Mirroring};

pub mod frame;
pub mod palette;

struct Rect {
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,
}

impl Rect {
    pub fn new(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Rect { x1, y1, x2, y2 }
    }
}

pub fn render(frame: &mut Frame, ppu: &PPU) {
    render_background(frame, ppu);
    render_sprites(frame, ppu);
}

fn render_background(frame: &mut Frame, ppu: &PPU) {
    let scroll_x = (ppu.scroll.scroll_x) as usize;
    let scroll_y = (ppu.scroll.scroll_y) as usize;

    let (main_nametable, second_nametable) = match (&ppu.mirroring, ppu.ctl.nametable_address()) {
        (Mirroring::Vertical, 0x2000)
        | (Mirroring::Vertical, 0x2800)
        | (Mirroring::Horizontal, 0x2000)
        | (Mirroring::Horizontal, 0x2400) => (&ppu.vram[0..0x400], &ppu.vram[0x400..0x800]),
        (Mirroring::Vertical, 0x2400)
        | (Mirroring::Vertical, 0x2C00)
        | (Mirroring::Horizontal, 0x2800)
        | (Mirroring::Horizontal, 0x2C00) => (&ppu.vram[0x400..0x800], &ppu.vram[0..0x400]),
        (_, _) => panic!("Unsupported mirroring type {:?}", ppu.mirroring),
    };

    render_name_table(
        frame,
        ppu,
        main_nametable,
        Rect::new(scroll_x, scroll_y, 256, 240),
        -(scroll_x as isize),
        -(scroll_y as isize),
    );

    if scroll_x > 0 {
        render_name_table(
            frame,
            ppu,
            second_nametable,
            Rect::new(0, 0, scroll_x, 240),
            (256 - scroll_x) as isize,
            0,
        );
    } else if scroll_y > 0 {
        render_name_table(
            frame,
            ppu,
            second_nametable,
            Rect::new(0, 0, 256, scroll_y),
            0,
            (240 - scroll_y) as isize,
        );
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

fn render_name_table(
    frame: &mut Frame,
    ppu: &PPU,
    name_table: &[u8],
    view_port: Rect,
    shift_x: isize,
    shift_y: isize,
) {
    let bank = ppu.ctl.bknd_pattern_addr();

    let attribute_table = &name_table[0x3C0..0x400];

    for i in 0..0x3C0 {
        let col = i % 32;
        let row = i / 32;
        let tile_idx = name_table[i] as u16;
        let tile =
            &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];
        let palette = background_pallette(ppu, attribute_table, col, row);

        for y in 0..8 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];

            for x in (0..8).rev() {
                let value = (1 & lower) << 1 | (1 & upper);
                upper = upper >> 1;
                lower = lower >> 1;

                let rgb = match value {
                    0 => SYSTEM_PALLETE[ppu.palette_table[0] as usize],
                    1 => SYSTEM_PALLETE[palette[1] as usize],
                    2 => SYSTEM_PALLETE[palette[2] as usize],
                    3 => SYSTEM_PALLETE[palette[3] as usize],
                    _ => panic!("Impossible!"),
                };

                let pixel_x = col * 8 + x;
                let pixel_y = row * 8 + y;

                if (pixel_x >= view_port.x1
                    && pixel_x < view_port.x2
                    && pixel_y >= view_port.y1
                    && pixel_y < view_port.y2)
                {
                    frame.set_pixel(
                        (shift_x + pixel_x as isize) as usize,
                        (shift_y + pixel_y as isize) as usize,
                        rgb,
                    );
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

fn background_pallette(
    ppu: &PPU,
    attribute_table: &[u8],
    tile_column: usize,
    tile_row: usize,
) -> [u8; 4] {
    let attr_table_index = 8 * (tile_row / 4) + tile_column / 4;
    let attr_byte = attribute_table[attr_table_index];

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
