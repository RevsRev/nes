use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::apu::APU;
use crate::interrupt::InterruptImpl;
use crate::io::joypad::Joypad;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::traits::bus::Bus;
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
    pub apu: APU,
    interrupt: Rc<RefCell<InterruptImpl>>,
    pub joypad: Joypad,
    gameloop_callback: Box<dyn FnMut(&PPU, &mut Joypad) + 'call>,
}

impl<'a> BusImpl<'a> {
    pub fn new<'call, F>(
        rom: Rom,
        interrupt: Rc<RefCell<InterruptImpl>>,
        gameloop_callback: F,
    ) -> BusImpl<'call>
    where
        F: FnMut(&PPU, &mut Joypad) + 'call,
    {
        let interrupt_ppu = interrupt.clone();
        let interrupt_apu = interrupt.clone();
        let apu = APU::new(interrupt_apu);
        let ppu = PPU::new(rom.chr_rom, rom.screen_mirroring, interrupt_ppu);
        let joypad = Joypad::new();

        BusImpl {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu: ppu,
            apu: apu,
            interrupt: interrupt,

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
impl<'a> Bus for BusImpl<'a> {}

impl<'a> Mem for BusImpl<'a> {
    fn mem_read(&mut self, addr: u16) -> Result<u8, String> {
        let value = match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                Result::Ok(self.cpu_vram[mirror_down_addr as usize])
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                // panic!("Attempt to read from write-only PPU address {:#04X}", addr);
                Result::Ok(0)
            }

            0x2002 => Result::Ok(self.ppu.read_status()),
            0x2004 => Result::Ok(self.ppu.read_oam_data()),
            0x2007 => self.ppu.read_data(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                self.mem_read(mirror_down_addr)
            }

            0x4000..=0x4013 => Result::Err(format!(
                "Attempt to read from APU write only address {:#04X}",
                addr
            )),

            0x4014..=0x4015 => {
                //ignore APU
                Result::Ok(0)
            }

            0x4016 => Result::Ok(self.joypad.read()),

            0x4017 => {
                // ignore joypad 2
                Result::Ok(0)
            }

            ROM_START..=ROM_END => Result::Ok(self.read_prg_rom(addr)),
            _ => {
                println!("Ignoring mem read-access at {:#04X}", addr);
                Result::Ok(0)
            }
        };

        value
    }

    fn mem_write(&mut self, addr: u16, data: u8) -> Result<u8, std::string::String> {
        return match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000111_11111111;
                let retval = self.cpu_vram[mirror_down_addr as usize];
                self.cpu_vram[mirror_down_addr as usize] = data;
                Result::Ok(retval)
            }
            0x2000 => Result::Ok(self.ppu.write_to_ctl(data)),
            0x2001 => Result::Ok(self.ppu.write_to_mask(data)),
            0x2002 => Result::Err(format!(
                "Attempt to write to PPU status register at addr: {:#04X}",
                addr
            )),
            0x2003 => Result::Ok(self.ppu.write_to_oam_addr(data)),
            0x2004 => Result::Ok(self.ppu.write_to_oam_data(data)),
            0x2005 => Result::Ok(self.ppu.write_to_scroll(data)),
            0x2006 => Result::Ok(self.ppu.write_to_ppu_addr(data)),
            0x2007 => self.ppu.write_data(data),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0010_0000_0000_0111;
                Result::Ok(self.mem_write(mirror_down_addr, data)?)
            }

            0x4000 => Result::Ok(self.apu.pulse_1.write_to_envelope(data)),
            0x4001 => Result::Ok(self.apu.pulse_1.write_to_sweep(data)),
            0x4002 => Result::Ok(self.apu.pulse_1.write_to_timerl(data)),
            0x4003 => Result::Ok(self.apu.pulse_1.write_to_len_timerh(data)),
            0x4004 => Result::Ok(self.apu.pulse_2.write_to_envelope(data)),
            0x4005 => Result::Ok(self.apu.pulse_2.write_to_sweep(data)),
            0x4006 => Result::Ok(self.apu.pulse_2.write_to_timerl(data)),
            0x4007 => Result::Ok(self.apu.pulse_2.write_to_len_timerh(data)),
            0x4008 => Result::Ok(self.apu.triangle.write_to_linear_counter(data)),
            0x4009 => Result::Ok(self.apu.triangle.write_to_unused(data)),
            0x400A => Result::Ok(self.apu.triangle.write_to_timerl(data)),
            0x400B => Result::Ok(self.apu.triangle.write_to_len_timerh(data)),
            0x400C => Result::Ok(self.apu.noise.write_to_env_loop_len_ctr_halt_cvol(data)),
            0x400D => Result::Ok(self.apu.noise.write_unused(data)),
            0x400E => Result::Ok(self.apu.noise.write_noise_mode_period(data)),
            0x400F => Result::Ok(self.apu.noise.write_len_counter_load(data)),
            0x4010 => Result::Ok(self.apu.dmc.write_to_flags_and_rate(data)),
            0x4011 => Result::Ok(self.apu.dmc.write_to_direct_load(data)),
            0x4012 => Result::Ok(self.apu.dmc.write_to_sample_address(data)),
            0x4013 => Result::Ok(self.apu.dmc.write_to_sample_length(data)),

            0x4015 => Result::Ok(self.apu.write_to_status(data)),
            0x4017 => Result::Ok(self.apu.write_to_frame_counter(data)),

            0x4016 => Result::Ok(self.joypad.write(data)),

            // https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
            0x4014 => {
                let mut buffer: [u8; 256] = [0; 256];
                let hi: u16 = (data as u16) << 8;
                for i in 0..256u16 {
                    buffer[i as usize] = self.mem_read(hi + i)?;
                }

                Result::Ok(self.ppu.write_to_oam_dma(&buffer))

                // todo: handle this eventually
                // let add_cycles: u16 = if self.cycles % 2 == 1 { 514 } else { 513 };
                // self.tick(add_cycles); //todo this will cause weird effects as PPU will have 513/514 * 3 ticks
            }

            ROM_START..=ROM_END => Result::Err(format!(
                "Attempt to write to Cartridge in ROM space at addr: {:#04X}",
                addr
            )),
            _ => {
                println!("Ignoring mem write-access at {:#04x}", addr);
                Result::Ok(0)
            }
        };
    }
}

impl<'call> Tick for BusImpl<'call> {
    fn tick(&mut self, cycles: u8) {
        // let nmi_before = self.ppu.poll(&InterruptType::Nmi).is_some();
        self.ppu.tick(3 * cycles);
        // let nmi_after = self.ppu.poll(&InterruptType::Nmi).is_some();
        //
        self.apu.tick(cycles);

        if self.ppu.new_frame {
            (self.gameloop_callback)(&self.ppu, &mut self.joypad)
        }

        // if !nmi_before && nmi_after {
        //     (self.gameloop_callback)(&self.ppu)
        // }
    }
}
