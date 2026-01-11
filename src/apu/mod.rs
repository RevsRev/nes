use std::{cell::RefCell, rc::Rc};

use crate::{
    apu::{
        channel::{
            dmc::DmcChannel, noise::NoiseChannel, square::SquareChannel, triangle::TriangleChannel,
        },
        mixer::Mixer,
        registers::{frame::FrameCounter, status::Status},
    },
    interrupt::{Interrupt, InterruptImpl},
    traits::tick::Tick,
};

pub mod channel;
pub mod mixer;
pub mod registers;

pub struct APU {
    pub pulse_1: SquareChannel,
    pub pulse_2: SquareChannel,
    pub triangle: TriangleChannel,
    pub noise: NoiseChannel,
    pub dmc: DmcChannel,
    pub status: Status,
    frame: FrameCounter,
    interrupt: Rc<RefCell<InterruptImpl>>,
    mixer: Mixer,

    cpu_cycles: u8,
    sequencer_cycles: u16,
}

impl APU {
    pub fn new(interrupt: Rc<RefCell<InterruptImpl>>) -> Self {
        APU {
            pulse_1: SquareChannel::new(),
            pulse_2: SquareChannel::new(),
            triangle: TriangleChannel::new(),
            noise: NoiseChannel::new(),
            dmc: DmcChannel::new(),
            status: Status::new(),
            frame: FrameCounter::new(interrupt.clone()),
            interrupt: interrupt,
            mixer: Mixer::new(),
            cpu_cycles: 0,
            sequencer_cycles: 0,
        }
    }

    pub fn write_to_status(&mut self, data: u8) -> u8 {
        self.interrupt.borrow_mut().set_dmc(false);
        self.status.write(data)
    }

    pub fn write_to_frame_counter(&mut self, data: u8) -> u8 {
        // self.interrupt
        //     .borrow_mut()
        //     .set_irq(data & 0b1100_0000 == 0b0);

        let old_irq_inhibit = self.frame.get_data() & 0b1000_0000 == 0b1000_0000;
        let new_irq_inhibit = data & 0b1000_0000 == 0b1000_0000;

        if !old_irq_inhibit && new_irq_inhibit {
            self.status.set_irq_flag(false);
            self.interrupt.borrow_mut().set_irq(false);
        }
        self.frame.write(data)
    }

    fn recompute_irq(&mut self) {
        self.interrupt
            .borrow_mut()
            .set_irq(self.status.get_irq_flag());
    }

    pub fn output(&self) -> f32 {
        self.mixer.output
    }

    pub fn read_status(&self) -> Result<u8, String> {
        let status_data = self.status.read();
        let pulse_1_expired = self.pulse_1.len_counter_expired();
        let pulse_2_expired = self.pulse_2.len_counter_expired();
        let triangle_expired = self.triangle.len_counter_expired();

        let pulse_1_flag = if pulse_1_expired {
            !0b0000_0001
        } else {
            0b1111_1111
        };
        let pulse_2_flag = if pulse_2_expired {
            !0b0000_0010
        } else {
            0b1111_1111
        };
        let triangle_flag = if triangle_expired {
            !0b0000_0100
        } else {
            0b1111_1111
        };

        status_data.map(|d| d & pulse_1_flag & pulse_2_flag & triangle_flag)
    }
}

impl Tick for APU {
    fn tick(&mut self, cycles: u8) {
        for c in 0..cycles {
            let on_apu_clock_cycle = self.cpu_cycles.wrapping_add(c) % 2 == 0;
            let frame_tick = if on_apu_clock_cycle { 1 } else { 0 };
            self.frame.tick(frame_tick);
            if on_apu_clock_cycle {
                let emit_clock = self.frame.emit_clock();

                match emit_clock {
                    Some(clock) => {
                        self.pulse_1.frame_clock(&clock);
                        self.pulse_2.frame_clock(&clock);
                        self.triangle.frame_clock(&clock);
                    }
                    None => {}
                }

                let irq_set = self.frame.step();
                if irq_set {
                    self.status.set_irq_flag(true);
                    self.recompute_irq();
                }

                self.pulse_1.decrement_timer();
                self.pulse_2.decrement_timer();
            }
            self.triangle.decrement_timer();
            self.mixer.output(
                self.pulse_1.get_out(),
                self.pulse_2.get_out(),
                self.triangle.get_out(),
            );
        }

        let num_apu_cycles = ((self.cpu_cycles % 2 + cycles) / 2) as u16;
        self.cpu_cycles = self.cpu_cycles.wrapping_add(cycles);
        self.sequencer_cycles = self.sequencer_cycles.wrapping_add(num_apu_cycles);
    }
}
