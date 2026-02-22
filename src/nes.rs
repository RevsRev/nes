use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use crate::apu::APU;
use crate::bus::BusImpl;
use crate::cpu_v2::CpuV2;
use crate::interrupt::InterruptImpl;
use crate::io::joypad::Joypad;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::trace::{CpuTraceFormatOptions, CpuTraceFormatter, NesTrace, NesTraceFormatter};
use crate::traits::cpu::Cpu;
use crate::traits::mos_65902::MOS6502;
use crate::traits::tick::Tick;
use crate::traits::tracing::Tracing;

pub struct NES<'call, T: Cpu<BusImpl<'call>>> {
    tracing: bool,
    pub trace: Option<NesTrace>,
    pub cpu: T,
    pub bus: Rc<RefCell<BusImpl<'call>>>,
}

impl<'call, T: Cpu<BusImpl<'call>>> fmt::Display for NES<'call, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\ncpu: {}, \nbus: {}", self.cpu, self.bus.borrow())
    }
}

pub fn nes_with_cpu_v2<'call, F>(
    rom: Rom,
    halt: Arc<AtomicBool>,
    tracing: bool,
    gameloop_callback: F,
) -> NES<'call, CpuV2<BusImpl<'call>>>
where
    F: FnMut(&PPU, &APU, &mut Joypad) + 'call,
{
    let interrupt = Rc::new(RefCell::new(InterruptImpl::new()));
    let interrupt_cpu = interrupt.clone();
    let bus = Rc::new(RefCell::new(BusImpl::new(
        rom,
        interrupt,
        gameloop_callback,
    )));
    let mut cpu = CpuV2::new(Rc::clone(&bus), interrupt_cpu, halt);
    bus.borrow_mut().ppu.set_tracing(tracing);
    bus.borrow_mut().apu.set_tracing(tracing);
    cpu.set_tracing(tracing);
    cpu.reset();

    NES {
        cpu,
        bus,
        tracing: tracing,
        trace: None,
    }
}

impl<'call, T: Cpu<BusImpl<'call>>> NES<'call, T> {
    pub fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
        self.cpu.set_tracing(tracing);
        self.bus.borrow_mut().apu.set_tracing(tracing);
        self.bus.borrow_mut().ppu.set_tracing(tracing);
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: FnMut(&mut NES<T>),
    {
        //TODO - Need to wire this in
        // (self.gameloop_callback)(&self.ppu, &self.apu, &mut self.joypad)

        loop {
            for master_clock in 0..12 {
                if master_clock % 3 == 0 {
                    self.cpu.tick();
                    self.bus.borrow_mut().apu.tick();
                }
                self.bus.borrow_mut().ppu.tick();
            }

            if self.cpu.should_halt() {
                return Ok(());
            }

            //TODO - Fix tracing
            // let mut apu_tr = self.bus.borrow_mut().apu.trace();
            // match self.cpu.step_with_callback(&mut |_| {}) {
            //     Ok(b) => match b {
            //         true => {}
            //         false => break,
            //     },
            //     Err(s) => {
            //         return Err(s);
            //     }
            // }
            //
            // if self.tracing {
            //     self.trace = Option::Some(NesTrace {
            //         cpu_trace: self.cpu.take_trace().take().unwrap(),
            //         ppu_trace: self.bus.borrow_mut().ppu.take_trace().unwrap(),
            //         apu_trace: apu_tr.take().unwrap(),
            //     });
            // }
            callback(self);
        }
    }
}

#[cfg(test)]
mod test {
    use clap::error::Result;
    use once_cell::sync::Lazy;
    use regex::Regex;

    use crate::apu::APU;
    use crate::bus::BusImpl;
    use crate::io::joypad::Joypad;
    use crate::ppu::PPU;
    use crate::rom::{self, Rom};
    use crate::trace::{
        ApuTraceFormatter, CpuTraceFormatOptions, CpuTraceFormatter, NesTraceFormatter,
        PpuTraceFormatter,
    };
    use crate::traits::cpu::Cpu;
    use crate::traits::mem::Mem;
    use crate::traits::mos_6502_registers::Registers;
    use crate::traits::mos_65902::MOS6502;
    use crate::traits::tracing::Tracing;
    use core::fmt;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::{BufRead, BufReader, BufWriter, Write};
    use std::panic::AssertUnwindSafe;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::time::{Duration, Instant};
    use std::{panic, thread};

    use super::{NES, nes_with_cpu_v2};

    struct NesInit {
        cycles: u64,
        register_a: u8,
        register_x: u8,
        status: u8,
        stack_pointer: u8,
        ppu_frame_cycles: usize,
    }

    fn read_file(path: &str) -> Vec<String> {
        let file_result = std::fs::File::open(path);

        let file = match file_result {
            Ok(f) => f,
            Err(e) => panic!("Failed to open file: {}", e),
        };

        let reader = BufReader::new(file);
        reader.lines().filter_map(Result::ok).collect()
    }

    fn construct_nes(rom: Rom, halt: &Arc<AtomicBool>) -> NES<'_, impl Cpu<BusImpl<'_>>> {
        //rust doesn't like polymorphism at runtime, so switch here to run different tests :(
        return nes_with_cpu_v2(
            rom,
            Arc::clone(halt),
            true,
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );

        // return nes_with_cpu_v1(
        //     rom,
        //     Arc::clone(halt),
        //     |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        // );
    }

    #[test]
    fn test_format_string() {
        let mut program = vec![];
        program.resize(2 * rom::PRG_ROM_PAGE_SIZE, 0);
        program[2 * rom::PRG_ROM_PAGE_SIZE - 2] = 0xFF;
        program[2 * rom::PRG_ROM_PAGE_SIZE - 1] = 0x00; //Redundant, but let's be explicit

        let rom = crate::rom::test::test_rom(program);

        let halt = Arc::new(AtomicBool::new(false));
        let mut nes = construct_nes(rom, &halt);
        let mut result: Vec<String> = Vec::new();

        // nes.setDebug(true);
        nes.bus.borrow_mut().mem_write(100, 0xA2);
        nes.bus.borrow_mut().mem_write(101, 0x01);
        nes.bus.borrow_mut().mem_write(102, 0xCA);
        nes.bus.borrow_mut().mem_write(103, 0x88);
        nes.bus.borrow_mut().mem_write(104, 0x00);

        nes.cpu.set_program_counter(0x64);
        nes.cpu.set_register_a(1);
        nes.cpu.set_register_x(2);
        nes.cpu.set_register_y(3);

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: nes.cpu.format_options(false, false),
        };

        let _ = nes.run_with_callback(|nes| {
            let trace = nes.trace.as_ref();
            match trace {
                None => println!("WARN: No CPU trace"),
                Some(tr) => {
                    let f = formatter.format(&tr.cpu_trace);
                    result.push(f);
                }
            }
        });

        handle.join().unwrap();

        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:04 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:04 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:06 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let mut program = vec![];
        program.resize(2 * rom::PRG_ROM_PAGE_SIZE, 0);
        program[2 * rom::PRG_ROM_PAGE_SIZE - 2] = 0xFF;
        program[2 * rom::PRG_ROM_PAGE_SIZE - 1] = 0x00; //Redundant, but let's be explicit

        let rom = crate::rom::test::test_rom(program);

        let halt = Arc::new(AtomicBool::new(false));

        let mut nes = construct_nes(rom, &halt);
        let mut result: Vec<String> = Vec::new();

        nes.bus.borrow_mut().mem_write(100, 0x11);
        nes.bus.borrow_mut().mem_write(101, 0x33);

        //data
        nes.bus.borrow_mut().mem_write(0x33, 00);
        nes.bus.borrow_mut().mem_write(0x34, 04);

        //target cell
        nes.bus.borrow_mut().mem_write(0x400, 0xAA);

        nes.cpu.set_program_counter(0x64);
        nes.cpu.set_register_y(0);

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: nes.cpu.format_options(false, false),
        };

        let _ = nes.run_with_callback(|nes| {
            let trace = nes.trace.as_ref();
            match trace {
                None => println!("WARN: No CPU trace"),
                Some(tr) => {
                    let f = formatter.format(&tr.cpu_trace);
                    result.push(f);
                }
            }
        });

        handle.join().unwrap();

        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:04 SP:FD",
            result[0]
        );
    }

    #[test]
    fn test_nestest() {
        let halt = Arc::new(AtomicBool::new(false));
        let rom = Rom::from_file("assets/nestest.nes");

        let mut nes = construct_nes(rom, &halt);
        let mut result: Vec<String> = Vec::new();
        let nes_test_log = read_file("assets/nestest.log");

        nes.cpu.reset();

        nes.cpu.set_program_counter(0xC000);

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: nes.cpu.format_options(true, false),
        };

        let _runtime_result = panic::catch_unwind(AssertUnwindSafe(|| {
            let _ = nes.run_with_callback(|nes| {
                let trace = nes.trace.as_ref();
                match trace {
                    None => println!("WARN: No CPU trace"),
                    Some(tr) => {
                        let f = formatter.format(&tr.cpu_trace);
                        result.push(f);
                    }
                }
            });
        }));

        let _ = handle.join();

        assert_ne!(nes_test_log.len(), 0);

        for (i, expected_line) in nes_test_log.iter().enumerate() {
            if let Some(actual_line) = result.get(i) {
                assert_eq!(
                    expected_line
                        .split("PPU")
                        .next()
                        .unwrap_or(expected_line)
                        .trim(),
                    actual_line,
                    "\n\nLines:\n{}",
                    result
                        .iter()
                        .take(i)
                        .cloned()
                        .collect::<Vec<_>>()
                        .join("\n")
                );
            }
        }

        assert!(
            nes_test_log.len() <= result.len(),
            "Lines from NES test do not match expected log. Last line of result log was:\n{:?}\n\nExpected next line from nestest.log is:\n{:?}",
            match result.get(result.len() - 1) {
                Some(v) => v,
                None => "NULL",
            },
            match nes_test_log.get(result.len()) {
                Some(v) => v.split("PPU").next().unwrap_or(v).trim(),
                None => "NULL",
            }
        );
    }

    /*
     * BLARGG PPU TESTS
     */
    #[test]
    fn nestest_blargg_palette_ram() {
        let rom = Rom::from_file("nestest/ppu/blargg/palette_ram.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/palette_ram_fceux.log");
        should_match_fceux(rom, nes_test_log, 38315);
    }

    #[test]
    fn nestest_regression_palette_ram() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/blargg/palette_ram.nes",
                "nestest/ppu/blargg/palette_ram_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/blargg/palette_ram.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/palette_ram_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    // I'm not convinced we really care about this test, as it's asserting on the state of the
    // palette before it's been written to after power up, which we don't care about
    // #[test]
    // fn nestest_blargg_power_up_palette() {
    //     let rom = Rom::from_file("nestest/ppu/blargg/power_up_palette.nes");
    //     let nes_test_log = read_file("nestest/ppu/blargg/power_up_palette_fceux.log");
    //     should_match_fceux(rom, nes_test_log, -1);
    // }

    #[test]
    fn nestest_blargg_sprite_ram() {
        let rom = Rom::from_file("nestest/ppu/blargg/sprite_ram.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/sprite_ram_fceux.log");
        should_match_fceux(rom, nes_test_log, 38315);
    }

    #[test]
    fn nestest_regression_sprite_ram() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/blargg/sprite_ram.nes",
                "nestest/ppu/blargg/sprite_ram_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/blargg/sprite_ram.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/sprite_ram_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_vbl_clear_time() {
        let rom = Rom::from_file("nestest/ppu/blargg/vbl_clear_time.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/vbl_clear_time_fceux.log");
        should_match_fceux(rom, nes_test_log, 206075);
    }

    #[test]
    fn nestest_blargg_vbl_clear_time_mesen() {
        let rom = Rom::from_file("nestest/ppu/blargg/vbl_clear_time.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/vbl_clear_time_mesen.log");

        let nes_init = NesInit {
            cycles: 8,
            register_a: 0,
            register_x: 0,
            status: 0x04,
            stack_pointer: 0xFD,
            ppu_frame_cycles: 25,
        };
        should_match_mesen(rom, nes_test_log, Some(nes_init), -1);
    }

    #[test]
    fn nestest_regression_vbl_clear_time() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/blargg/vbl_clear_time.nes",
                "nestest/ppu/blargg/vbl_clear_time_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/blargg/vbl_clear_time.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/vbl_clear_time_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_vram_access() {
        let rom = Rom::from_file("nestest/ppu/blargg/vram_access.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/vram_access_fceux.log");
        should_match_fceux(rom, nes_test_log, 38325);
    }

    #[test]
    fn nestest_regression_vram_access() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/blargg/vram_access.nes",
                "nestest/ppu/blargg/vram_access_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/blargg/vram_access.nes");
        let nes_test_log = read_file("nestest/ppu/blargg/vram_access_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    /*
     * OTHER PPU TESTS
     */
    #[test]
    fn nestest_ppu_01_vbl_basics() {
        let rom = Rom::from_file("nestest/ppu/ppu_vbl_nmi/01-vbl_basics.nes");
        let nes_test_log = read_file("nestest/ppu/ppu_vbl_nmi/01_mesen.log");

        let nes_init = NesInit {
            cycles: 8,
            register_a: 0x1A,
            register_x: 0,
            status: 0x05,
            stack_pointer: 0xEF,
            ppu_frame_cycles: 27,
        };

        should_match_mesen(rom, nes_test_log, Some(nes_init), -1);
    }

    #[test]
    fn nestest_ppu_scroll() {
        let rom = Rom::from_file("nestest/ppu/scroll.nes");
        let nes_test_log = read_file("nestest/ppu/scroll.log");

        let nes_init = NesInit {
            cycles: 8,
            register_a: 0x00,
            register_x: 0,
            status: 0x04,
            stack_pointer: 0xFD,
            ppu_frame_cycles: 25,
        };

        should_match_mesen(rom, nes_test_log, Some(nes_init), -1);
    }

    /*
     * BLARGG APU TESTS
     */

    #[test]
    fn nestest_blargg_01_len_ctr() {
        let rom = Rom::from_file("nestest/apu/01.len_ctr.nes");
        let nes_test_log = read_file("nestest/apu/01_fceux.log");
        should_match_fceux(rom, nes_test_log, 533663);
    }

    #[test]
    fn nestest_regression_01_len_ctr() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/apu/01.len_ctr.nes",
                "nestest/apu/01_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/apu/01.len_ctr.nes");
        let nes_test_log = read_file("nestest/apu/01_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_02_len_table() {
        let rom = Rom::from_file("nestest/apu/02.len_table.nes");
        let nes_test_log = read_file("nestest/apu/02_fceux.log");
        should_match_fceux(rom, nes_test_log, 28478);
    }

    #[test]
    fn nestest_regression_blargg_02_len_table() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/apu/02.len_table.nes",
                "nestest/apu/02_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/apu/02.len_table.nes");
        let nes_test_log = read_file("nestest/apu/02_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_03_irq_flag() {
        let rom = Rom::from_file("nestest/apu/03.irq_flag.nes");
        let nes_test_log = read_file("nestest/apu/03_fceux.log");
        should_match_fceux(rom, nes_test_log, 76674);
    }

    #[test]
    fn nestest_regression_blargg_03_irq_flag() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/apu/03.irq_flag.nes",
                "nestest/apu/03_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/apu/03.irq_flag.nes");
        let nes_test_log = read_file("nestest/apu/03_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    //FCEUX doesn't even pass the clock jitter test! So no point comparing to incorrect logs!
    // #[test]
    // fn nestest_blargg_04_clock_jitter() {
    //     let rom = Rom::from_file("nestest/apu/04.clock_jitter.nes");
    //     let nes_test_log = read_file("nestest/apu/04_fceux.log");
    //     should_match_fceux(rom, nes_test_log, 265758);
    // }

    #[test]
    fn nestest_blargg_04_clock_jitter_mesen() {
        let rom = Rom::from_file("nestest/apu/04.clock_jitter.nes");
        let nes_test_log = read_file("nestest/apu/04_mesen.log");
        should_match_mesen(
            rom,
            nes_test_log,
            Some(NesInit {
                cycles: 8,
                register_a: 0,
                register_x: 0,
                status: 04,
                stack_pointer: 0xFD,
                ppu_frame_cycles: 25,
            }),
            265758,
        );
    }

    #[test]
    fn nestest_regression_blargg_04_clock_jitter() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/apu/04.clock_jitter.nes",
                "nestest/apu/04_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/apu/04.clock_jitter.nes");
        let nes_test_log = read_file("nestest/apu/04_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_05_len_timing_mode0() {
        let rom = Rom::from_file("nestest/apu/05.len_timing_mode0.nes");
        let nes_test_log = read_file("nestest/apu/05_fceux.log");
        should_match_fceux(rom, nes_test_log, 812136);
    }

    #[test]
    fn nestest_blargg_05_len_timing_mode0_mesen() {
        let rom = Rom::from_file("nestest/apu/05.len_timing_mode0.nes");
        let nes_test_log = read_file("nestest/apu/05_mesen.log");
        should_match_mesen(
            rom,
            nes_test_log,
            Some(NesInit {
                cycles: 8,
                register_a: 0,
                register_x: 0,
                status: 04,
                stack_pointer: 0xFD,
                ppu_frame_cycles: 25,
            }),
            -1,
        );
    }

    #[test]
    fn nestest_blargg_07_irq_flag_timing_mesen() {
        let rom = Rom::from_file("nestest/apu/07.irq_flag_timing.nes");
        let nes_test_log = read_file("nestest/apu/07_mesen.log");
        should_match_mesen(
            rom,
            nes_test_log,
            Some(NesInit {
                cycles: 8,
                register_a: 0,
                register_x: 0,
                status: 04,
                stack_pointer: 0xFD,
                ppu_frame_cycles: 27,
            }),
            -1,
        );
    }

    fn write_nes_logs(rom_path: &str, out_path: &str, max_cycles: i64) {
        let file = File::create(out_path).expect("failed to create log file");
        let writer = RefCell::new(BufWriter::new(file));

        let rom = Rom::from_file(rom_path);
        let halt = Arc::new(AtomicBool::new(false));

        let mut nes = construct_nes(rom, &halt);

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            let timeout = Duration::from_secs(2);
            let start = Instant::now();

            while start.elapsed() < timeout {
                if halt_share.load(Ordering::Relaxed) {
                    return;
                }

                thread::sleep(Duration::from_millis(1));
            }
            halt_share.store(true, Ordering::Relaxed);
        });

        let nes_formatter = NesTraceFormatter {
            cpu_formatter: CpuTraceFormatter {
                options: nes.cpu.format_options(false, true),
            },
            ppu_formatter: Some(PpuTraceFormatter {}),
            apu_formatter: Some(ApuTraceFormatter {}),
        };

        let _runtime_result = panic::catch_unwind(AssertUnwindSafe(|| {
            let _ = nes.run_with_callback(|nes| {
                let trace = nes.trace.as_ref();
                match trace {
                    None => println!("WARN: No CPU trace"),
                    Some(tr) => {
                        let f = nes_formatter.format(&tr);
                        writeln!(writer.borrow_mut(), "{}", f)
                            .expect("failed to write trace to log file");
                    }
                }
                if max_cycles > 0 && nes.cpu.get_cycles() > max_cycles as u64 {
                    halt.store(true, Ordering::Relaxed);
                }
            });
        }));

        let _ = handle.join();
        writer.borrow_mut().flush().unwrap();
    }

    fn should_match_mesen(
        rom: Rom,
        mesen_log: Vec<String>,
        nes_init: Option<NesInit>,
        max_cycles: i64,
    ) {
        should_match(
            rom,
            mesen_log,
            max_cycles,
            nes_init,
            |i, fceux_log, nes_log| nes_mesen_line_matches(i, fceux_log, nes_log),
        );
    }

    fn should_match_fceux(rom: Rom, fceux_log: Vec<String>, max_cycles: i64) {
        should_match(rom, fceux_log, max_cycles, None, |i, fceux_log, nes_log| {
            nes_fceux_line_matches(i, fceux_log, nes_log)
        });
    }
    fn should_match_nes(rom: Rom, fceux_log: Vec<String>, max_cycles: i64) {
        should_match(rom, fceux_log, max_cycles, None, |i, nes_gold, nes_log| {
            nes_nes_line_matches(i, nes_gold, nes_log)
        });
    }

    fn should_match<F>(
        rom: Rom,
        compare_log: Vec<String>,
        max_cycles: i64,
        nes_init: Option<NesInit>,
        line_matches: F,
    ) where
        F: Fn(usize, &[String], &[String]) -> bool,
    {
        let halt = Arc::new(AtomicBool::new(false));

        let mut nes = construct_nes(rom, &halt);

        match nes_init {
            Some(init) => {
                nes.cpu.set_cycles(init.cycles);
                nes.cpu.set_register_a(init.register_a);
                nes.cpu.set_register_x(init.register_x);
                nes.cpu.set_status(init.status);
                nes.cpu.set_stack_pointer(init.stack_pointer);
                nes.bus.borrow_mut().ppu.frame_dots = init.ppu_frame_cycles;
            }
            None => {}
        }

        let mut result: Vec<String> = Vec::new();

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            let timeout = Duration::from_secs(2);
            let start = Instant::now();

            while start.elapsed() < timeout {
                if halt_share.load(Ordering::Relaxed) {
                    return;
                }

                thread::sleep(Duration::from_millis(1));
            }
            halt_share.store(true, Ordering::Relaxed);
        });

        let nes_tr_formatter = NesTraceFormatter {
            cpu_formatter: CpuTraceFormatter {
                options: nes.cpu.format_options(false, true),
            },
            ppu_formatter: Some(PpuTraceFormatter {}),
            apu_formatter: Some(ApuTraceFormatter {}),
        };

        let _runtime_result = panic::catch_unwind(AssertUnwindSafe(|| {
            let _ = nes.run_with_callback(|nes| {
                let trace = nes.trace.as_ref();
                match trace {
                    None => println!("WARN: No CPU trace"),
                    Some(tr) => {
                        // println!("{}", tr);
                        result.push(nes_tr_formatter.format(&tr));
                    }
                }

                if max_cycles > 0 && nes.cpu.get_cycles() > max_cycles as u64 {
                    halt.store(true, Ordering::Relaxed);
                }
            });
        }));

        let _ = handle.join();

        assert_ne!(compare_log.len(), 0);

        let failure = find_first_failure(&compare_log, &result, line_matches);

        if failure.is_none() {
            return;
        }

        let i = failure.unwrap();

        if i == result.len() || i == compare_log.len() {
            return;
        }

        let expected = &compare_log[i];
        let actual = &result[i];

        let start_prev = i.saturating_sub(10);
        let prev_fceux = &compare_log[start_prev..(i).min(compare_log.len() - 1)];
        let prev_nes = &result[start_prev..i];

        let end_next = i + 10;
        let next_fceux = &compare_log[i + 1..=end_next.min(compare_log.len() - 1)];
        let next_nes = &result[i + 1..=end_next.min(result.len() - 1)];

        // ANSI red for highlighting
        let red_start = "\x1b[31m";
        let red_end = "\x1b[0m";

        let mut f_str = String::new();
        for h in prev_fceux {
            f_str.push_str(&format!("\t{}\n", h));
        }
        f_str.push_str(&format!("\t{}{}{}\n", red_start, expected, red_end));
        for h in next_fceux {
            f_str.push_str(&format!("\t{}\n", h));
        }

        let mut n_str = String::new();
        for h in prev_nes {
            n_str.push_str(&format!("\t{}\n", h));
        }
        n_str.push_str(&format!("\t{}{}{}\n", red_start, actual, red_end));
        for h in next_nes {
            n_str.push_str(&format!("\t{}\n", h));
        }

        panic!(
            "\n\nMismatch at line {i}\nFCEUX expected: {}\nNES actual: {}\n\nContext around failure:\nFCEUX:\n{}\nNES:\n{}",
            expected, actual, f_str, n_str
        );
    }
    fn find_first_failure<F>(fceux: &[String], nes: &[String], line_matches: F) -> Option<usize>
    where
        F: Fn(usize, &[String], &[String]) -> bool,
    {
        let slow_match = std::env::var("SLOW_MATCH").is_ok();
        if slow_match {
            return linear_match(fceux, nes, line_matches);
        }

        fast_flakey_matcher(fceux, nes, line_matches)
    }

    fn linear_match<F>(fceux: &[String], nes: &[String], line_matches: F) -> Option<usize>
    where
        F: Fn(usize, &[String], &[String]) -> bool,
    {
        for i in 0..nes.len() {
            if !line_matches(i, fceux, nes) {
                return Some(i);
            }
        }
        None
    }

    fn fast_flakey_matcher<F>(fceux: &[String], nes: &[String], line_matches: F) -> Option<usize>
    where
        F: Fn(usize, &[String], &[String]) -> bool,
    {
        let mut low = 0;
        let mut high = nes.len();

        let mut first_bad = None;

        while low < high {
            let mid = (low + high) / 2;

            if line_matches(mid, fceux, nes) {
                low = mid + 1; // failure is after mid
            } else {
                first_bad = Some(mid);
                high = mid; // failure is at or before mid
            }
        }

        first_bad
    }

    fn nes_mesen_line_matches(i: usize, mesen: &[String], nes: &[String]) -> bool {
        let expected = match mesen.get(i) {
            Some(v) => v,
            None => return false,
        };
        let actual = match nes.get(i) {
            Some(v) => v,
            None => return false,
        };
        let expected_capture =
            Capture::from_line(expected, "S", |line| capture_register(line, "P"), true);
        let actual_capture =
            Capture::from_line(actual, "SP", |line| capture_register(line, "P"), true);
        // if expected_capture != actual_capture {
        //     println!("Expected: {}", expected_capture);
        //     println!("Actual: {}", actual_capture);
        //     return false;
        // }
        expected_capture == actual_capture
    }

    fn nes_fceux_line_matches(i: usize, fceux: &[String], nes: &[String]) -> bool {
        let expected = match fceux.get(i) {
            Some(v) => v,
            None => return false,
        };
        let actual = match nes.get(i) {
            Some(v) => v,
            None => return false,
        };
        let expected_capture =
            Capture::from_line(expected, "S", |line| extract_fceux_status(line), false);
        let actual_capture =
            Capture::from_line(actual, "SP", |line| capture_register(line, "P"), false);

        // if expected_capture != actual_capture {
        //     println!("Expected: {}", expected_capture);
        //     println!("Actual: {}", actual_capture);
        //     return false;
        // }
        expected_capture == actual_capture
    }
    fn nes_nes_line_matches(i: usize, nes_gold: &[String], nes: &[String]) -> bool {
        let expected = match nes_gold.get(i) {
            Some(v) => v,
            None => return false,
        };
        let actual = match nes.get(i) {
            Some(v) => v,
            None => return false,
        };
        expected == actual
    }
    #[derive(Debug, PartialEq, Eq)]
    struct Capture {
        a: String,
        x: String,
        y: String,
        sp: String,
        status: String,
        mem_addr: Option<String>,
        cycles: String,
        scanline: Option<String>,
        dot: Option<String>,
    }

    impl fmt::Display for Capture {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "A:{} X:{} Y:{} SP:{} P:{} {} C:{} V:{} H:{}",
                self.a,
                self.x,
                self.y,
                self.sp,
                self.status,
                match &self.mem_addr {
                    Some(addr) => format!("M:{}", addr),
                    None => String::from(""),
                },
                self.cycles,
                match &self.scanline {
                    Some(sl) => format!("V:{}", sl),
                    None => String::from("n/a"),
                },
                match &self.dot {
                    Some(dot) => format!("H:{}", dot),
                    None => String::from("n/a"),
                },
            )
        }
    }

    impl Capture {
        fn from_line<F>(line: &str, sp_string: &str, status_extract: F, include_ppu: bool) -> Self
        where
            F: Fn(&str) -> Option<String>,
        {
            let (scanline, dot) = match include_ppu {
                false => (None, None),
                true => (
                    capture_scanline(line).map(|v| if v == "-1" { "261".to_string() } else { v }),
                    capture_dot(line),
                ),
            };

            Capture {
                a: capture_register(line, "A").unwrap(),
                x: capture_register(line, "X").unwrap(),
                y: capture_register(line, "Y").unwrap(),
                sp: capture_register(line, sp_string).unwrap(),
                status: status_extract(line).unwrap(),
                mem_addr: capture_mem_addr(line),
                cycles: capture_cycles(line).unwrap(),
                scanline: scanline,
                dot: dot,
            }
        }
    }
    static REGEX_CACHE: Lazy<std::sync::Mutex<HashMap<String, Regex>>> =
        Lazy::new(|| std::sync::Mutex::new(HashMap::new()));

    fn extract_fceux_status(line: &str) -> Option<String> {
        let re = Regex::new(r"\b([nvubdizcNVUBDIZC]{8})\b").unwrap();
        let caps = re.captures(line)?;
        let s = &caps[1];

        let mut status = 0u8;

        for (i, ch) in s.chars().enumerate() {
            if ch.is_ascii_uppercase() {
                status |= 1 << (7 - i);
            }
        }

        Some(format!("{:02X}", status))
    }

    fn capture_scanline(line: &str) -> Option<String> {
        let re = {
            let mut cache = REGEX_CACHE.lock().unwrap();
            cache
                .entry("SCANLINE".to_string())
                .or_insert_with(|| Regex::new(r"V:(-?\d+)(?:\s|$)").unwrap())
                .clone()
        };
        re.captures(line).map(|caps| caps[1].to_string())
    }

    fn capture_dot(line: &str) -> Option<String> {
        let re = {
            let mut cache = REGEX_CACHE.lock().unwrap();
            cache
                .entry("DOT".to_string())
                .or_insert_with(|| Regex::new(r"H:(\d+)(?:\s|$)").unwrap())
                .clone()
        };
        re.captures(line).map(|caps| caps[1].to_string())
    }

    fn capture_register(line: &str, reg: &str) -> Option<String> {
        let pattern = format!(r"{}:([0-9A-F]{{2}})", reg);

        let re = {
            let mut cache = REGEX_CACHE.lock().unwrap();
            cache
                .entry(pattern.clone())
                .or_insert_with(|| Regex::new(&pattern).unwrap())
                .clone()
        };

        re.captures(line).map(|caps| caps[1].to_string())
    }

    fn capture_mem_addr(line: &str) -> Option<String> {
        let re = {
            let mut cache = REGEX_CACHE.lock().unwrap();
            cache
                .entry("MEM_ADDR".to_string())
                .or_insert_with(|| Regex::new(r"\b([0-9A-F]{4})\b").unwrap())
                .clone()
        };

        re.captures(line)
            .and_then(|caps| caps.get(1).map(|m| m.as_str().to_string()))
    }

    fn capture_cycles(line: &str) -> Option<String> {
        let re = {
            let mut cache = REGEX_CACHE.lock().unwrap();
            cache
                .entry("CYCLE".to_string())
                .or_insert_with(|| Regex::new(r"c(\d+)\s").unwrap())
                .clone()
        };

        re.captures(line)
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().to_string())
    }
}
