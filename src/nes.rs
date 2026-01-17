use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use crate::apu::APU;
use crate::bus::BusImpl;
use crate::cpu::CPU;
use crate::interrupt::InterruptImpl;
use crate::io::joypad::Joypad;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::trace::{CpuTraceFormatOptions, CpuTraceFormatter, NesTrace, NesTraceFormatter};

pub struct NES<'call> {
    tracing: bool,
    trace_options: CpuTraceFormatOptions,
    pub trace: Option<NesTrace>,

    pub cpu: CPU<BusImpl<'call>>,
    pub bus: Rc<RefCell<BusImpl<'call>>>,
}

impl<'call> fmt::Display for NES<'call> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\ncpu: {}, \nbus: {}", self.cpu, self.bus.borrow())
    }
}

impl<'call> NES<'call> {
    pub fn new<'cl, F>(rom: Rom, halt: Arc<AtomicBool>, gameloop_callback: F) -> NES<'cl>
    where
        F: FnMut(&PPU, &APU, &mut Joypad) + 'cl,
    {
        let interrupt = Rc::new(RefCell::new(InterruptImpl::new()));
        let interrupt_cpu = interrupt.clone();
        let bus = Rc::new(RefCell::new(BusImpl::new(
            rom,
            interrupt,
            gameloop_callback,
        )));
        let mut cpu = CPU::new(Rc::clone(&bus), interrupt_cpu, halt);
        cpu.reset();
        NES {
            tracing: false,
            trace_options: CpuTraceFormatOptions {
                write_break_2_flag: true,
                write_cpu_cycles: true,
            },
            trace: Option::None,
            cpu: cpu,
            bus: bus,
        }
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: FnMut(&mut NES),
    {
        let tracing = self.tracing;
        let trace_formatter = CpuTraceFormatter {
            options: self.trace_options,
        };
        let mut combined_callback = |nes: &mut NES| {
            if tracing {
                match nes.trace.as_ref() {
                    Option::None => println!("NULL Trace"),
                    Option::Some(s) => println!("{}", trace_formatter.format(&s.cpu_trace)),
                };
            }
            callback(nes);
        };

        loop {
            let ppu_tr = self.bus.borrow_mut().ppu.trace();
            match self.cpu.step_with_callback(&mut |_| {}) {
                Ok(b) => match b {
                    true => {}
                    false => break,
                },
                Err(s) => {
                    return Err(s);
                }
            }
            self.trace = Option::Some(NesTrace {
                cpu_trace: self.cpu.trace.take().unwrap(),
                ppu_trace: ppu_tr,
            });
            combined_callback(self);
        }
        Result::Ok(())
    }
}

#[cfg(test)]
mod test {
    use clap::error::Result;
    use once_cell::sync::Lazy;
    use regex::Regex;

    use crate::apu::APU;
    use crate::cpu::CPU;
    use crate::io::joypad::Joypad;
    use crate::ppu::PPU;
    use crate::rom::{self, Rom};
    use crate::trace::{
        CpuTraceFormatOptions, CpuTraceFormatter, NesTraceFormatter, PpuTraceFormatter,
    };
    use crate::traits::mem::Mem;
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

    use super::NES;

    fn read_file(path: &str) -> Vec<String> {
        let file_result = std::fs::File::open(path);

        let file = match file_result {
            Ok(f) => f,
            Err(e) => panic!("Failed to open file: {}", e),
        };

        let reader = BufReader::new(file);
        reader.lines().filter_map(Result::ok).collect()
    }
    #[test]
    fn test_format_string() {
        let mut program = vec![];
        program.resize(2 * rom::PRG_ROM_PAGE_SIZE, 0);
        program[2 * rom::PRG_ROM_PAGE_SIZE - 2] = 0xFF;
        program[2 * rom::PRG_ROM_PAGE_SIZE - 1] = 0x00; //Redundant, but let's be explicit

        let rom = crate::rom::test::test_rom(program);

        let halt = Arc::new(AtomicBool::new(false));
        let mut nes = NES::new(
            rom,
            Arc::clone(&halt),
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );
        let mut result: Vec<String> = Vec::new();

        // nes.setDebug(true);
        nes.bus.borrow_mut().mem_write(100, 0xA2);
        nes.bus.borrow_mut().mem_write(101, 0x01);
        nes.bus.borrow_mut().mem_write(102, 0xCA);
        nes.bus.borrow_mut().mem_write(103, 0x88);
        nes.bus.borrow_mut().mem_write(104, 0x00);

        nes.cpu.program_counter = 0x64;
        nes.cpu.register_a = 1;
        nes.cpu.register_x = 2;
        nes.cpu.register_y = 3;

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: CpuTraceFormatOptions {
                write_break_2_flag: false,
                write_cpu_cycles: false,
            },
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
        let mut nes = NES::new(
            rom,
            Arc::clone(&halt),
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );
        let mut result: Vec<String> = Vec::new();

        nes.bus.borrow_mut().mem_write(100, 0x11);
        nes.bus.borrow_mut().mem_write(101, 0x33);

        //data
        nes.bus.borrow_mut().mem_write(0x33, 00);
        nes.bus.borrow_mut().mem_write(0x34, 04);

        //target cell
        nes.bus.borrow_mut().mem_write(0x400, 0xAA);

        nes.cpu.program_counter = 0x64;
        nes.cpu.register_y = 0;

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: CpuTraceFormatOptions {
                write_break_2_flag: false,
                write_cpu_cycles: false,
            },
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
        let mut nes = NES::new(
            Rom::from_file("assets/nestest.nes"),
            Arc::clone(&halt),
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );
        let mut result: Vec<String> = Vec::new();
        let nes_test_log = read_file("assets/nestest.log");

        nes.cpu.reset();
        nes.cpu.trace_format_options.write_break_2_flag = true;

        //        nes.setDebug(true);
        nes.cpu.program_counter = 0xC000;

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let formatter = CpuTraceFormatter {
            options: CpuTraceFormatOptions {
                write_break_2_flag: true,
                write_cpu_cycles: false,
            },
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
        let rom = Rom::from_file("nestest/ppu/palette_ram.nes");
        let nes_test_log = read_file("nestest/ppu/palette_ram_fceux.log");
        should_match_fceux(rom, nes_test_log, 38315);
    }

    #[test]
    fn nestest_regression_palette_ram() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/palette_ram.nes",
                "nestest/ppu/palette_ram_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/palette_ram.nes");
        let nes_test_log = read_file("nestest/ppu/palette_ram_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    // I'm not convinced we really care about this test, as it's asserting on the state of the
    // palette before it's been written to after power up, which we don't care about
    // #[test]
    // fn nestest_blargg_power_up_palette() {
    //     let rom = Rom::from_file("nestest/ppu/power_up_palette.nes");
    //     let nes_test_log = read_file("nestest/ppu/power_up_palette_fceux.log");
    //     should_match_fceux(rom, nes_test_log, -1);
    // }

    #[test]
    fn nestest_blargg_sprite_ram() {
        let rom = Rom::from_file("nestest/ppu/sprite_ram.nes");
        let nes_test_log = read_file("nestest/ppu/sprite_ram_fceux.log");
        should_match_fceux(rom, nes_test_log, 38315);
    }

    #[test]
    fn nestest_regression_sprite_ram() {
        let regen_logs = std::env::var("REGEN_LOGS").is_ok();

        if regen_logs {
            write_nes_logs(
                "nestest/ppu/sprite_ram.nes",
                "nestest/ppu/sprite_ram_nes.log",
                1_000_000,
            );
        }

        let rom = Rom::from_file("nestest/ppu/sprite_ram.nes");
        let nes_test_log = read_file("nestest/ppu/sprite_ram_nes.log");
        should_match_nes(rom, nes_test_log, 1_000_000);
    }

    #[test]
    fn nestest_blargg_vbl_clear_time() {
        let rom = Rom::from_file("nestest/ppu/vbl_clear_time.nes");
        let nes_test_log = read_file("nestest/ppu/vbl_clear_time_fceux.log");
        should_match_fceux(rom, nes_test_log, 84393);
    }

    #[test]
    fn nestest_blargg_vbl_clear_time_mesen() {
        let rom = Rom::from_file("nestest/ppu/vbl_clear_time.nes");
        let nes_test_log = read_file("nestest/ppu/vbl_clear_time_mesen.log");
        should_match_mesen(rom, nes_test_log, 104528);
    }

    #[test]
    fn nestest_blargg_vram_access() {
        let rom = Rom::from_file("nestest/ppu/vram_access.nes");
        let nes_test_log = read_file("nestest/ppu/vram_access_fceux.log");
        should_match_fceux(rom, nes_test_log, -1);
    }
    /*
     * BLARGG APU TESTS
     */

    #[test]
    fn nestest_blargg_01_len_ctr() {
        let rom = Rom::from_file("nestest/apu/01.len_ctr.nes");
        let nes_test_log = read_file("nestest/apu/01_fceux.log");
        should_match_fceux(rom, nes_test_log, 270300);
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

    #[test]
    fn nestest_blargg_04_clock_jitter() {
        let rom = Rom::from_file("nestest/apu/04.clock_jitter.nes");
        let nes_test_log = read_file("nestest/apu/04_fceux.log");
        should_match_fceux(rom, nes_test_log, 11976);
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

    fn write_nes_logs(rom_path: &str, out_path: &str, max_cycles: i64) {
        let file = File::create(out_path).expect("failed to create log file");
        let writer = RefCell::new(BufWriter::new(file));

        let rom = Rom::from_file(rom_path);
        let halt = Arc::new(AtomicBool::new(false));

        let mut nes = NES::new(
            rom,
            Arc::clone(&halt),
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );

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
                options: CpuTraceFormatOptions {
                    write_break_2_flag: false,
                    write_cpu_cycles: true,
                },
            },
            ppu_formatter: PpuTraceFormatter {},
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
                if max_cycles > 0 && nes.cpu.total_cycles > max_cycles as u64 {
                    halt.store(true, Ordering::Relaxed);
                }
            });
        }));

        let _ = handle.join();
        writer.borrow_mut().flush().unwrap();
    }
    fn should_match_mesen(rom: Rom, mesen_log: Vec<String>, max_cycles: i64) {
        should_match(
            rom,
            mesen_log,
            max_cycles,
            |nes: &mut NES| {
                nes.cpu.total_cycles = 8;
                nes.cpu.register_x = 1;
                nes.cpu.status = 0x07;
                nes.cpu.stack_pointer = 0xF4;
                nes.bus.borrow_mut().ppu.frame_cycles = 27
            },
            |i, fceux_log, nes_log| nes_mesen_line_matches(i, fceux_log, nes_log),
        );
    }

    fn should_match_fceux(rom: Rom, fceux_log: Vec<String>, max_cycles: i64) {
        should_match(
            rom,
            fceux_log,
            max_cycles,
            |nes| (),
            |i, fceux_log, nes_log| nes_fceux_line_matches(i, fceux_log, nes_log),
        );
    }
    fn should_match_nes(rom: Rom, fceux_log: Vec<String>, max_cycles: i64) {
        should_match(
            rom,
            fceux_log,
            max_cycles,
            |nes| (),
            |i, nes_gold, nes_log| nes_nes_line_matches(i, nes_gold, nes_log),
        );
    }

    fn should_match<T, F>(
        rom: Rom,
        compare_log: Vec<String>,
        max_cycles: i64,
        nes_init: T,
        line_matches: F,
    ) where
        T: Fn(&mut NES) -> (),
        F: Fn(usize, &[String], &[String]) -> bool,
    {
        let halt = Arc::new(AtomicBool::new(false));
        let mut nes = NES::new(
            rom,
            Arc::clone(&halt),
            |_ppu: &PPU, _apu: &APU, _joypad: &mut Joypad| {},
        );

        nes_init(&mut nes);

        nes.cpu.trace_format_options.write_cpu_cycles = true;

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
                options: CpuTraceFormatOptions {
                    write_break_2_flag: false,
                    write_cpu_cycles: true,
                },
            },
            ppu_formatter: PpuTraceFormatter {},
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

                if max_cycles > 0 && nes.cpu.total_cycles > max_cycles as u64 {
                    halt.store(true, Ordering::Relaxed);
                }
            });
        }));

        let _ = handle.join();

        assert_ne!(compare_log.len(), 0);

        if let Some(i) = find_first_failure(&compare_log, &result, line_matches) {
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
    }
    fn find_first_failure<F>(fceux: &[String], nes: &[String], line_matches: F) -> Option<usize>
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
        if expected_capture != actual_capture {
            println!("Expected: {}", expected_capture);
            println!("Actual: {}", actual_capture);
            return false;
        }
        true
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
                "A:{} X:{} Y:{} SP:{} P:{} {} C:{}",
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
                true => (capture_register(line, "V"), capture_register(line, "H")),
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
        let re = Regex::new(r"(?i)([^c]|^)([0-9A-F]{4})").unwrap();
        re.captures(line)
            .and_then(|caps| caps.get(1).map(|m| m.as_str().to_string()))
    }

    fn capture_cycles(line: &str) -> Option<String> {
        let re = Regex::new(r"c(\d+)\s").unwrap();

        re.captures(line)
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().to_string())
    }
}
