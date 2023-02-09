use crate::save_state::{LoadStateError, SaveState};

#[derive(Default, Debug, Eq, Clone)]
pub struct Timer {
    /// FF04: DIV register
    pub div: u16,
    /// FF05: TIMA register
    pub tima: u8,
    /// FF06: TMA register
    pub tma: u8,
    /// FF07: TAC register
    ///
    /// Bit  2   - Timer Enable
    /// Bits 1-0 - Input Clock Select
    ///     00: CPU Clock / 1024 (DMG:   4096 Hz)
    ///     01: CPU Clock / 16   (DMG: 262144 Hz)
    ///     10: CPU Clock / 64   (DMG:  65536 Hz)
    ///     11: CPU Clock / 256  (DMG:  16384 Hz)
    pub tac: u8,

    /// The value of the counter bit in the last cycle, for edge detection.
    pub last_counter_bit: bool,
    /// The last clock cycle where Timer was updated
    pub last_clock_count: u64,
    /// Keep track of TIMA reloading. TIMA is reloading if < 4, reloading is scheduled if >= 4, and
    /// there is no reload if = 0.
    pub loading: u8,

    /// The estimated time where the next interrupt may happen.
    pub next_interrupt: u64,
}

impl PartialEq for Timer {
    fn eq(&self, other: &Self) -> bool {
        self.div == other.div
            && self.tima == other.tima
            && self.tma == other.tma
            && self.tac == other.tac
            && self.last_counter_bit == other.last_counter_bit
            && self.last_clock_count == other.last_clock_count
            && self.loading == other.loading
        // && self.next_interrupt == other.next_interrupt
    }
}
crate::save_state!(Timer, self, ctx, data {
    on_save debug_assert_eq!(self.last_clock_count, ctx.clock_count.unwrap());
    on_load self.next_interrupt = 0;

    self.div;
    self.tima;
    self.tma;
    self.tac;
    bitset [self.last_counter_bit];
    self.last_clock_count;
    self.loading;
});

// TODO: At some point, I want this timer to be lazy evaluated.
impl Timer {
    pub fn new() -> Self {
        Self {
            // Starting div with this value to make it have the correct DIV phase after boot
            // has finished. May change depending on changes in timing precision.
            div: 0xFFE8,
            tima: 0x00,
            tma: 0x00,
            tac: 0x00,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 0,
        }
    }

    /// Return the state of timer just after the boot finished.
    pub fn after_boot(clock_count: u64) -> Self {
        Timer {
            div: 0xabcc,
            tima: 0x00,
            tma: 0x00,
            tac: 0xf8,
            last_counter_bit: false,
            last_clock_count: clock_count,
            loading: 0,
            next_interrupt: 0,
        }
    }

    /// Advance the timer by one cycle
    /// Return true if there is a interrupt
    pub(crate) fn update(&mut self, clock_count: u64) -> bool {
        let mut interrupt = false;

        for _clock in self.last_clock_count..clock_count {
            self.div = self.div.wrapping_add(1);

            let freq = [9, 3, 5, 7][(self.tac & 0b11) as usize];
            let counter_bit = ((self.div >> freq) as u8 & (self.tac >> 2)) & 0b1 != 0;

            // faling edge
            if self.last_counter_bit && !counter_bit {
                // dbg!(self.tima);
                let (v, overflow) = self.tima.overflowing_add(1);
                self.tima = v;
                // TIMA, on overflow, keep the value 00 for 4 cycles before the overflow is
                // detected. A write in this interval would cancel it.
                if overflow {
                    self.loading = 8;
                }
            }

            // loading TIMA
            if self.loading != 0 {
                self.loading -= 1;
                if self.loading <= 4 {
                    self.tima = self.tma;
                    interrupt = true;
                }
            }

            self.last_counter_bit = counter_bit;
        }

        self.last_clock_count = clock_count;
        self.next_interrupt = self.estimate_next_interrupt();

        interrupt
    }

    pub fn read(&self, address: u8) -> u8 {
        match address {
            0x04 => (self.div >> 8) as u8,
            0x05 => self.tima,
            0x06 => self.tma,
            0x07 => self.tac | 0xF8,
            _ => unreachable!("out of Timer memory map"),
        }
    }

    pub fn write(&mut self, address: u8, value: u8) {
        match address {
            0x04 => self.div = 0,
            0x05 => {
                if self.loading > 4 {
                    // cancel reloading
                    self.loading = 0;
                    self.tima = value
                } else if self.loading == 0 {
                    self.tima = value
                } else {
                    // ignored while reloading
                }
            }
            0x06 => self.tma = value,

            0x07 => self.tac = value,
            _ => unreachable!("out of Timer memory map"),
        }
        self.next_interrupt = self.estimate_next_interrupt();
    }

    pub fn estimate_next_interrupt(&self) -> u64 {
        if self.loading != 0 {
            // The interrupt signal is currently on
            if self.loading - 1 <= 4 {
                return self.last_clock_count;
            }
            // or will be on
            return self.last_clock_count + self.loading as u64 - 5;
        }

        // if the timer is not enabled, there will never be a interrupt.
        if self.tac & 0b100 == 0 {
            // unless the faling edge of the timer enabled bit causes a interrupt.
            if self.last_counter_bit && self.tima == 255 {
                return self.last_clock_count + 4;
            }
            return u64::MAX;
        }

        let freq = [9, 3, 5, 7][(self.tac & 0b11) as usize];
        let period = 1 << (freq as u64 + 1);

        let until_falling_edge = period - (self.div as u64 & (period - 1));
        debug_assert!((self.div as u64 + until_falling_edge) & (period - 1) == 0);

        let counter_bit = (self.div >> freq) as u8 & 0b1 != 0;
        let tima = if self.last_counter_bit && !counter_bit {
            self.tima as u64 + 2
        } else {
            self.tima as u64 + 1
        };

        let remaining_time = if tima == 256 {
            until_falling_edge + 2
        } else if tima == 257 {
            3
        } else {
            (256 - (tima + 1)) * period + until_falling_edge + 2
        };

        self.last_clock_count + remaining_time
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;

    const DIV: u8 = 0x04;
    const TIMA: u8 = 0x05;
    const TMA: u8 = 0x06;
    const TAC: u8 = 0x07;

    #[test]
    fn fuzz() {
        let mut timer = Timer::new();
        let mut rng = rand::thread_rng();
        for _ in 0..50 {
            if rng.gen_bool(0.25) {
                timer.write(DIV, rng.gen());
            }
            if rng.gen_bool(0.25) {
                timer.write(TIMA, rng.gen());
            }
            if rng.gen_bool(0.25) {
                timer.write(TMA, rng.gen());
            }
            if rng.gen_bool(0.25) {
                timer.write(TAC, rng.gen());
            }

            let r: f64 = rng.gen();
            let cycles = (2.0f64.powf(r * r * 10.0)) as u64;

            'test: for _ in 0..cycles {
                let error_timer = timer.clone();
                let next_interrupt = timer.estimate_next_interrupt();

                let target_clock = if next_interrupt == u64::MAX {
                    let r: f64 = rng.gen();
                    let cycles = (2.0f64.powf(2.0 + r * 21.0)) as u64;
                    println!("random target: {}", cycles);
                    timer.last_clock_count + cycles
                } else if next_interrupt <= timer.last_clock_count + 1 {
                    for _ in 0..10_000_000 {
                        let interrupt = timer.update(timer.last_clock_count + 4);
                        if interrupt {
                            continue 'test;
                        }
                    }
                    panic!("interrupt never happens!?: {:?}", error_timer)
                } else {
                    next_interrupt - 1
                };

                let interrupt = timer.update(target_clock);
                if interrupt {
                    panic!("interrupt is on early? {:?}", error_timer);
                }
            }
        }
    }

    #[test]
    fn case1() {
        let mut timer = Timer {
            div: 0b11001,
            tima: 228,
            tma: 92,
            tac: 0b110,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 0,
        };

        let next_interrupt = timer.estimate_next_interrupt();

        let target_clock = if next_interrupt == u64::MAX {
            unimplemented!()
        } else if next_interrupt == timer.last_clock_count {
            println!("running until interrupt: tac = {:03b}", timer.tac & 0b111);
            for _ in 0..10_000_000 {
                let interrupt = timer.update(timer.last_clock_count + 4);
                if interrupt {
                    return;
                }
            }
            panic!("interrupt never happens!?")
        } else {
            println!("next_interrupt: {}", next_interrupt);
            next_interrupt - 1
        };

        let interrupt = timer.update(target_clock);
        assert!(!interrupt);
    }

    #[test]
    fn case2() {
        let mut timer = Timer {
            div: 0,
            tima: 255,
            tma: 94,
            tac: 0b111,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 0,
        };

        let next_interrupt = timer.estimate_next_interrupt();

        let target_clock = if next_interrupt == u64::MAX {
            unimplemented!()
        } else if next_interrupt == timer.last_clock_count {
            println!("running until interrupt: tac = {:03b}", timer.tac & 0b111);
            for _ in 0..10_000_000 {
                let interrupt = timer.update(timer.last_clock_count + 4);
                if interrupt {
                    return;
                }
            }
            panic!("interrupt never happens!?")
        } else {
            println!("next_interrupt: {}", next_interrupt);
            next_interrupt - 1
        };

        let interrupt = timer.update(target_clock);
        assert!(!interrupt);
    }

    #[test]
    fn case3() {
        let mut timer = Timer {
            div: 59393,
            tima: 0,
            tma: 20,
            tac: 0b011,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 0,
        };

        let next_interrupt = timer.estimate_next_interrupt();

        let target_clock = if next_interrupt == u64::MAX {
            timer.last_clock_count + 7984
        } else if next_interrupt == timer.last_clock_count {
            println!("running until interrupt: tac = {:03b}", timer.tac & 0b111);
            for _ in 0..10_000_000 {
                let interrupt = timer.update(timer.last_clock_count + 4);
                if interrupt {
                    return;
                }
            }
            panic!("interrupt never happens!?")
        } else {
            println!("next_interrupt: {}", next_interrupt);
            next_interrupt - 1
        };

        let interrupt = timer.update(target_clock);
        assert!(!interrupt);
    }
}
