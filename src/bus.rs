use core::fmt;

use crate::ppu::PPU;
use crate::rom::Rom;
use crate::traits::bus::Bus;
use crate::traits::mem::Mem;
use crate::traits::tick::Tick;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const ROM_START: u16 = 0x8000;
const ROM_END: u16 = 0xFFFF;

pub struct BusImpl {
    pub debug: bool,

    cpu_vram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: PPU,
}

impl BusImpl {
    pub fn new(rom: Rom) -> Self {
        let ppu = PPU::new(rom.chr_rom, rom.screen_mirroring);

        BusImpl {
            debug: false,
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.prg_rom[addr as usize]
    }

    pub fn tick(&mut self, cycles: u8) {}
}

impl fmt::Display for BusImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\ncpu_vram: {:?}, \nrprg_romom: {:?}",
            self.cpu_vram, self.prg_rom
        )
    }
}

impl Bus for BusImpl {}

impl Mem for BusImpl {
    fn mem_read(&mut self, addr: u16) -> u8 {
        let value = match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:#04X}", addr);
            }

            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                self.mem_read(mirror_down_addr)
            }
            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem read-access at {:#04X}", addr);
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
            0x2000 => self.ppu.write_to_ctl(data),
            0x2001 => self.ppu.write_to_mask(data),
            0x2002 => panic!("attempt to write to PPU status register"),
            0x2003 => self.ppu.write_to_oam_addr(data),
            0x2004 => self.ppu.write_to_oam_data(data),
            0x2005 => self.ppu.write_to_scroll(data),
            0x2006 => self.ppu.write_to_ppu_addr(data),
            0x2007 => self.ppu.write_data(data),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                self.mem_write(mirror_down_addr, data);
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

impl Tick for BusImpl {
    fn tick(&mut self, cycles: u8) {}
}
