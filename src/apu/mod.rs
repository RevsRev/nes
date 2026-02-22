use std::{cell::RefCell, rc::Rc};

use crate::{
    apu::{
        channel::{
            dmc::DmcChannel,
            noise::NoiseChannel,
            square::SquareChannel,
            sweep::SweepChangeMethod::{ONES_COMPLIMENT, TWOS_COMPLIMENT},
            triangle::TriangleChannel,
        },
        mixer::Mixer,
        registers::{frame::FrameCounter, status::Status},
    },
    interrupt::{Interrupt, InterruptImpl},
    trace::{ApuTrace, PulseTrace},
    traits::tick::Tick,
};

pub mod channel;
pub mod mixer;
pub mod registers;

pub struct APU {
    pub pulse_1: Rc<RefCell<SquareChannel>>,
    pub pulse_2: Rc<RefCell<SquareChannel>>,
    pub triangle: Rc<RefCell<TriangleChannel>>,
    pub noise: Rc<RefCell<NoiseChannel>>,
    pub dmc: Rc<RefCell<DmcChannel>>,

    pub status: Status,
    pub frame: Rc<RefCell<FrameCounter>>,
    interrupt: Rc<RefCell<InterruptImpl>>,
    mixer: Mixer,

    cpu_cycles: u8,
    sequencer_cycles: u16,
    tracing: bool,
}

impl APU {
    pub fn new(interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        let pulse_1 = Rc::new(RefCell::new(SquareChannel::new(ONES_COMPLIMENT)));
        let pulse_2 = Rc::new(RefCell::new(SquareChannel::new(TWOS_COMPLIMENT)));
        let triangle = Rc::new(RefCell::new(TriangleChannel::new()));
        let noise = Rc::new(RefCell::new(NoiseChannel::new()));
        let dmc = Rc::new(RefCell::new(DmcChannel::new()));
        let frame = Rc::new(RefCell::new(FrameCounter::new(interrupt.clone())));

        let status = Status::new(
            pulse_1.clone(),
            pulse_2.clone(),
            triangle.clone(),
            noise.clone(),
            dmc.clone(),
            frame.clone(),
        );
        APU {
            pulse_1: pulse_1.clone(),
            pulse_2: pulse_2.clone(),
            triangle: triangle.clone(),
            noise: noise.clone(),
            dmc: dmc.clone(),
            status,
            frame: frame.clone(),
            interrupt: interrupt,
            mixer: Mixer::new(),
            cpu_cycles: 0,
            sequencer_cycles: 0,
            tracing: false,
        }
    }

    pub fn write_to_status(&mut self, data: u8) -> u8 {
        self.interrupt.borrow_mut().set_dmc(false);
        self.status.write(data)
    }

    pub fn write_to_frame_counter(&mut self, data: u8) -> u8 {
        let r = self.frame.borrow_mut().write(data);
        let countdown = if self.cpu_cycles % 2 == 0 { 2 } else { 3 };
        self.frame.borrow_mut().set_reset_timer_countdown(countdown);
        r
    }

    pub fn output(&self) -> f32 {
        self.mixer.output
    }

    pub fn read_status(&mut self) -> Result<u8, String> {
        self.status.read()
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.tracing = tracing;
    }

    pub fn trace(&self) -> Option<ApuTrace> {
        if !self.tracing {
            return None;
        }

        Some(ApuTrace {
            pulse_1: self.pulse_1.borrow().trace(),
            pulse_2: self.pulse_2.borrow().trace(),
            frame_trace: self.frame.borrow().trace(),
        })
    }
}

impl Tick for APU {
    fn tick(&mut self) {
        self.frame.borrow_mut().step(1);
        let on_apu_clock_cycle = self.cpu_cycles.wrapping_add(1) % 2 == 0;
        if on_apu_clock_cycle {
            let emit_clock = self.frame.borrow_mut().emit_clock();

            match emit_clock {
                Some(clock) => {
                    self.pulse_1.borrow_mut().frame_clock(&clock);
                    self.pulse_2.borrow_mut().frame_clock(&clock);
                    self.triangle.borrow_mut().frame_clock(&clock);
                }
                None => {}
            }

            self.pulse_1.borrow_mut().decrement_timer();
            self.pulse_2.borrow_mut().decrement_timer();
        }
        self.triangle.borrow_mut().decrement_timer();
        self.mixer.output(
            self.pulse_1.borrow_mut().get_out(),
            self.pulse_2.borrow_mut().get_out(),
            self.triangle.borrow_mut().get_out(),
        );

        let num_apu_cycles = ((self.cpu_cycles % 2 + 1) / 2) as u16;
        self.cpu_cycles = self.cpu_cycles.wrapping_add(1);
        self.sequencer_cycles = self.sequencer_cycles.wrapping_add(num_apu_cycles);
    }
}
