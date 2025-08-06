use core::fmt;

use crate::io::joypad::Joypad;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::traits::bus::Bus;
use crate::traits::interrupt::{InterruptType, Interrupting};
use crate::traits::mem::Mem;
use crate::traits::tick::Tick;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const ROM_START: u16 = 0x8000;
const ROM_END: u16 = 0xFFFF;

pub struct BusImpl<'call> {
    cpu_vram: [u8; 2048],
    prg_rom: Vec<u8>,
    pub ppu: PPU,
    pub joypad: Joypad,
    gameloop_callback: Box<dyn FnMut(&PPU, &mut Joypad) + 'call>,
}

impl<'a> BusImpl<'a> {
    pub fn new<'call, F>(rom: Rom, gameloop_callback: F) -> BusImpl<'call>
    where
        F: FnMut(&PPU, &mut Joypad) + 'call,
    {
        let ppu = PPU::new(rom.chr_rom, rom.screen_mirroring);
        let joypad = Joypad::new();

        BusImpl {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu: ppu,
            joypad,
            gameloop_callback: Box::from(gameloop_callback),
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.prg_rom[addr as usize]
    }
}

impl<'a> fmt::Display for BusImpl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\ncpu_vram: {:?}, \nrprg_romom: {:?}",
            self.cpu_vram, self.prg_rom
        )
    }
}

impl<'a> Interrupting for BusImpl<'a> {
    fn poll(&self, interrupt_type: &InterruptType) -> Option<u8> {
        return self.ppu.poll(interrupt_type);
    }
    fn take(&mut self, interrupt_type: &InterruptType) -> Option<u8> {
        return self.ppu.take(interrupt_type);
    }
}

impl<'a> Bus for BusImpl<'a> {}

impl<'a> Mem for BusImpl<'a> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        let value = match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                // panic!("Attempt to read from write-only PPU address {:#04X}", addr);
                0
            }

            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                self.mem_read(mirror_down_addr)
            }

            0x4000..=0x4015 => {
                //ignore APU
                0
            }

            0x4016 => self.joypad.read(),

            0x4017 => {
                // ignore joypad 2
                0
            }

            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem read-access at {:#04X}", addr);
                0
            }
        };

        value
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
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

            0x4000..=0x4013 | 0x4015 => {
                //ignore APU
            }

            0x4016 => {
                self.joypad.write(data);
            }

            0x4017 => {
                // ignore joypad 2
            }

            // https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
            0x4014 => {
                let mut buffer: [u8; 256] = [0; 256];
                let hi: u16 = (data as u16) << 8;
                for i in 0..256u16 {
                    buffer[i as usize] = self.mem_read(hi + i);
                }

                self.ppu.write_to_oam_dma(&buffer);

                // todo: handle this eventually
                // let add_cycles: u16 = if self.cycles % 2 == 1 { 514 } else { 513 };
                // self.tick(add_cycles); //todo this will cause weird effects as PPU will have 513/514 * 3 ticks
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

impl<'call> Tick for BusImpl<'call> {
    fn tick(&mut self, cycles: u8) {
        // let nmi_before = self.ppu.poll(&InterruptType::Nmi).is_some();
        self.ppu.tick(3 * cycles);
        // let nmi_after = self.ppu.poll(&InterruptType::Nmi).is_some();

        if self.ppu.new_frame {
            (self.gameloop_callback)(&self.ppu, &mut self.joypad)
        }

        // if !nmi_before && nmi_after {
        //     (self.gameloop_callback)(&self.ppu)
        // }
    }
}
