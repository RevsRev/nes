use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use crate::bus::BusImpl;
use crate::cpu::CPU;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::traits;

pub struct NES<'call> {
    tracing: bool,

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
        F: FnMut(&PPU) + 'cl,
    {
        let bus = Rc::new(RefCell::new(BusImpl::new(rom, gameloop_callback)));
        let mut cpu = CPU::new(Rc::clone(&bus), halt);
        cpu.reset();
        NES {
            tracing: false,
            cpu: cpu,
            bus: bus,
        }
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Result<(), String>
    where
        F: FnMut(&mut CPU<BusImpl>),
    {
        if self.tracing {
            let combined_callback = |cpu: &mut CPU<BusImpl>| {
                match cpu.get_trace_str() {
                    Option::None => println!("NULL Trace"),
                    Option::Some(s) => println!("{}", s),
                };
                callback(cpu);
            };
            self.cpu.run_with_callback(combined_callback)
        } else {
            self.cpu.run_with_callback(callback)
        }
    }
}

#[cfg(test)]
mod test {
    use clap::error::Result;

    use crate::ppu::PPU;
    use crate::rom::{self, Rom};
    use crate::traits::mem::Mem;
    use std::io::{BufRead, BufReader};
    use std::panic::AssertUnwindSafe;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::time::Duration;
    use std::{panic, thread};

    use super::NES;

    fn nestest_rom() -> Rom {
        Rom::from_file("assets/nestest.nes")
    }

    fn nestest_log() -> Vec<String> {
        let file_result = std::fs::File::open("assets/nestest.log");

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
        let mut nes = NES::new(rom, Arc::clone(&halt), |ppu: &PPU| {});
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

        let _ = nes.run_with_callback(|cpu| {
            let trace = cpu.get_trace_str();
            match trace {
                None => println!("WARN: No CPU trace"),
                Some(tr) => result.push(tr),
            }
        });

        handle.join().unwrap();

        if !nes.cpu.graceful_shutdown() {
            println!("{:}", result.join("\n"));
            panic!("CPU did not gracefully shutdown in NES test");
        }

        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
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
        let mut nes = NES::new(rom, Arc::clone(&halt), |ppu: &PPU| {});
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

        let _ = nes.run_with_callback(|cpu| {
            let trace = cpu.get_trace_str();
            match trace {
                None => println!("WARN: No CPU trace"),
                Some(tr) => result.push(tr),
            }
        });

        handle.join().unwrap();

        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
            result[0]
        );
    }

    #[test]
    fn test_nestest() {
        let halt = Arc::new(AtomicBool::new(false));
        let mut nes = NES::new(nestest_rom(), Arc::clone(&halt), |ppu: &PPU| {});
        let mut result: Vec<String> = Vec::new();
        let nes_test_log = nestest_log();

        nes.cpu.reset();

        //        nes.setDebug(true);
        nes.cpu.program_counter = 0xC000;

        let halt_share = halt.clone();
        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            halt_share.store(true, Ordering::Relaxed);
        });

        let runtime_result = panic::catch_unwind(AssertUnwindSafe(|| {
            let _ = nes.run_with_callback(|cpu| {
                let trace = cpu.get_trace_str();
                match trace {
                    None => println!("WARN: No CPU trace"),
                    Some(tr) => {
                        // println!("{}", tr);
                        result.push(tr)
                    }
                }
            });
        }));

        match runtime_result {
            Ok(_) => println!("NES executed gracefully"),
            Err(_) => println!("Error during NES execution"),
        }

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

        assert_eq!(
            nes_test_log.len(),
            result.len(),
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

        if !nes.cpu.graceful_shutdown() {
            // println!("{:?}", result);
            panic!("CPU did not gracefully shutdown in NES test");
        }
    }
}
