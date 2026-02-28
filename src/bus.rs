use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::apu::APU;
use crate::interrupt::InterruptImpl;
use crate::io::joypad::Joypad;
use crate::ppu::{PPU, PPU_REGISTERS_MIRRORS_END};
use crate::rom::Rom;
use crate::traits::bus::Bus;
use crate::traits::mem::Mem;
use crate::traits::tick::Tick;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PRG_RAM_START: u16 = 0x6000;
const PRG_RAM_END: u16 = 0x7FFF;
const ROM_START: u16 = 0x8000;
const ROM_END: u16 = 0xFFFF;

pub struct BusImpl {
    cpu_vram: [u8; 2048],
    rom: Rc<RefCell<Rom>>,
    open_bus: u8,
    pub ppu: Rc<RefCell<PPU>>,
    pub apu: Rc<RefCell<APU>>,
    pub joypad: Joypad,
}

impl BusImpl {
    pub fn new(rom: Rc<RefCell<Rom>>, ppu: Rc<RefCell<PPU>>, apu: Rc<RefCell<APU>>) -> BusImpl {
        let joypad = Joypad::new();

        BusImpl {
            cpu_vram: [0; 2048],
            rom: rom,
            open_bus: 0,
            ppu: ppu,
            apu: apu,

            joypad,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.borrow().prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.rom.borrow().prg_rom[addr as usize]
    }

    fn read_prg_ram(&self, mut addr: u16) -> u8 {
        addr -= 0x6000;
        self.rom.borrow().prg_ram[addr as usize]
    }

    fn write_prg_ram(&self, mut addr: u16, data: u8) -> u8 {
        addr -= 0x6000;
        let overwritten = self.rom.borrow().prg_ram[addr as usize];
        self.rom.borrow_mut().prg_ram[addr as usize] = data;
        overwritten
    }
}

impl fmt::Display for BusImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\ncpu_vram: {:?}, \nrprg_romom: {:?}",
            self.cpu_vram,
            self.rom.borrow().prg_rom
        )
    }
}
impl Bus for BusImpl {}

impl Mem for BusImpl {
    fn mem_read(&mut self, addr: u16) -> Result<u8, String> {
        let value = match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                Result::Ok(self.cpu_vram[mirror_down_addr as usize])
            }

            0x2000
            | 0x2001
            | 0x2002
            | 0x2003
            | 0x2004
            | 0x2005
            | 0x2006
            | 0x2007
            | 0x2008..=PPU_REGISTERS_MIRRORS_END => self.ppu.borrow_mut().mem_read(addr),

            0x4000..=0x4013 => Result::Ok(0),
            // 0x4000..=0x4013 => Result::Err(format!(
            //     "Attempt to read from APU write only address {:#04X}",
            //     addr
            // )),
            0x4014 => Result::Err(format!("Unexpected mem read from 0x4014")),

            0x4015 => self.apu.borrow_mut().read_status(),

            0x4016 => {
                let read = self.joypad.read();
                let ret_val = (read & 0x01) | (self.open_bus & 0xFE);
                Result::Ok(ret_val)
            }

            0x4017 => {
                // ignore joypad 2
                Result::Ok(0)
            }

            PRG_RAM_START..=PRG_RAM_END => Result::Ok(self.read_prg_ram(addr)),

            ROM_START..=ROM_END => Result::Ok(self.read_prg_rom(addr)),
            _ => {
                println!("Ignoring mem read-access at {:#04X}", addr);
                Result::Ok(0)
            }
        };

        match value {
            Err(_) => {}
            Ok(data) => self.open_bus = data,
        }

        value
    }

    fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, std::string::String> {
        self.open_bus = data;
        return match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                let retval = self.cpu_vram[mirror_down_addr as usize];
                self.cpu_vram[mirror_down_addr as usize] = data;
                Result::Ok(retval)
            }

            0x2000 | 0x2001 | 0x2002 | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x4014 => {
                self.ppu.borrow_mut().mem_write(addr, data)
            }

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                Result::Ok(self.mem_write(mirror_down_addr, data)?)
            }

            0x4000 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_1
                    .borrow_mut()
                    .write_to_envelope(data),
            ),
            0x4001 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_1
                    .borrow_mut()
                    .write_to_sweep(data),
            ),
            0x4002 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_1
                    .borrow_mut()
                    .write_to_timerl(data),
            ),
            0x4003 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_1
                    .borrow_mut()
                    .write_to_len_timerh(data),
            ),
            0x4004 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_2
                    .borrow_mut()
                    .write_to_envelope(data),
            ),
            0x4005 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_2
                    .borrow_mut()
                    .write_to_sweep(data),
            ),
            0x4006 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_2
                    .borrow_mut()
                    .write_to_timerl(data),
            ),
            0x4007 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .pulse_2
                    .borrow_mut()
                    .write_to_len_timerh(data),
            ),
            0x4008 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .triangle
                    .borrow_mut()
                    .write_to_linear_counter(data),
            ),
            0x4009 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .triangle
                    .borrow_mut()
                    .write_to_unused(data),
            ),
            0x400A => Result::Ok(
                self.apu
                    .borrow_mut()
                    .triangle
                    .borrow_mut()
                    .write_to_timerl(data),
            ),
            0x400B => Result::Ok(
                self.apu
                    .borrow_mut()
                    .triangle
                    .borrow_mut()
                    .write_to_len_timerh(data),
            ),
            0x400C => Result::Ok(
                self.apu
                    .borrow_mut()
                    .noise
                    .borrow_mut()
                    .write_to_env_loop_len_ctr_halt_cvol(data),
            ),
            0x400D => Result::Ok(self.apu.borrow_mut().noise.borrow_mut().write_unused(data)),
            0x400E => Result::Ok(
                self.apu
                    .borrow_mut()
                    .noise
                    .borrow_mut()
                    .write_noise_mode_period(data),
            ),
            0x400F => Result::Ok(
                self.apu
                    .borrow_mut()
                    .noise
                    .borrow_mut()
                    .write_len_counter_load(data),
            ),
            0x4010 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .dmc
                    .borrow_mut()
                    .write_to_flags_and_rate(data),
            ),
            0x4011 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .dmc
                    .borrow_mut()
                    .write_to_direct_load(data),
            ),
            0x4012 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .dmc
                    .borrow_mut()
                    .write_to_sample_address(data),
            ),
            0x4013 => Result::Ok(
                self.apu
                    .borrow_mut()
                    .dmc
                    .borrow_mut()
                    .write_to_sample_length(data),
            ),

            0x4015 => {
                let retval = Result::Ok(self.apu.borrow_mut().write_to_status(data));
                retval
            }
            0x4017 => Result::Ok(self.apu.borrow_mut().write_to_frame_counter(data)),

            0x4016 => Result::Ok(self.joypad.write(data)),

            PRG_RAM_START..=PRG_RAM_END => Result::Ok(self.write_prg_ram(addr, data)),

            //ignore writes to rom
            // ROM_START..=ROM_END => Result::Err(format!(
            //     "Attempt to write to Cartridge in ROM space at addr: {:#04X}",
            //     addr
            // )),
            ROM_START..=ROM_END => Ok(0),

            _ => {
                println!("Ignoring mem write-access at {:#04x}", addr);
                Result::Ok(0)
            }
        };
    }
}
