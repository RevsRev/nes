use core::fmt;

use crate::rom::Rom;
use crate::traits::mem::Mem;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const ROM_START: u16 = 0x8000;
const ROM_END: u16 = 0xFFFF;

pub struct Bus {
    pub debug: bool,

    cpu_vram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            debug: false,
            cpu_vram: [0; 2048],
            rom: rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.rom.prg_rom[addr as usize]
    }
}

impl fmt::Display for Bus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\ncpu_vram: {:?}, \nrom: {}", self.cpu_vram, self.rom)
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        let value = match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                //                todo!("PPU is not supported yet")
                0
            }
            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem read-access at {:#04x}", addr);
                0
            }
        };

        if self.debug {
            println!("Mem read at {:#04x}: {:#04x}", addr, value);
        }
        value
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        if self.debug {
            println!("Mem write at {:#04x}: {:#04x}", addr, data);
        }

        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                todo!("PPU is not supported yet");
            }
            ROM_START..=ROM_END => {
                panic!("Attempt to write to Cartridge in ROM space");
            }
            _ => {
                println!("Ignoring mem write-access at {:#04x}", addr);
            }
        }
    }
}
