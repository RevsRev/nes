use crate::rom::Mirroring;

pub mod registers;

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],
    pub mirroring: Mirroring,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom: chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],
            mirroring: mirroring,
        }
    }
}
