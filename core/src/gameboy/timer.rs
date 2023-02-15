use crate::save_state::{LoadStateError, SaveState};

#[derive(Default, Debug, Eq, Clone)]
pub struct Timer {
    /// FF04: DIV register
    ///
    /// The intern counter of the Timer. Increases every cycle. Only the upper byte is readible.
    pub div: u16,
    /// FF05: TIMA register
    ///
    /// Count upwards. Generates a interrupt on overflow.
    pub tima: u8,
    /// FF06: TMA register
    ///
    /// TIMA is reset to this value when it overflows.
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

    /// Return true if there is a interrupt.
    ///
    /// Update the Timer to the given `clock_count`, in O(1). See [Timer::update_cycle_by_cycle] for
    /// a more straigh-forward implementation.
    pub fn update(&mut self, clock_count: u64) -> bool {
        debug_assert!(clock_count >= self.last_clock_count);
        if clock_count <= self.last_clock_count {
            self.next_interrupt = self.estimate_next_interrupt();
            return false;
        }

        self.next_interrupt = self.estimate_next_interrupt();

        let elapsed = clock_count - self.last_clock_count;
        self.last_clock_count = clock_count;

        let div = self.div.wrapping_add(1);

        let freq = [9, 3, 5, 7][(self.tac & 0b11) as usize];
        let period = 1 << (freq as u64 + 1);

        let until_falling_edge = period - (div as u64 & (period - 1));
        debug_assert!((div as u64 + until_falling_edge) & (period - 1) == 0);

        let counter_bit = (div >> freq) as u8 & (self.tac >> 2) & 0b1 != 0;

        let mut interrupt = false;
        let mut remain = elapsed;

        remain -= 1;
        if self.last_counter_bit && !counter_bit {
            self.tima = self.tima.wrapping_add(1);
            if self.tima == 0 {
                self.loading = 8;
            }
        };

        // A overflow will reset `loading` before the reload.
        let overflow_happens_early = self.tac & 0b100 != 0
            && self.tima == 255
            && (until_falling_edge + 1) + 4 <= self.loading as u64;

        // A overflow will reset `loading` after a reload
        let overflow_happens_during_load = self.tma == 255
            && (self.loading <= 4 || self.loading as u64 - 4 <= elapsed)
            && (until_falling_edge + 1) + 4 > self.loading as u64
            && (until_falling_edge + 1) <= self.loading as u64;

        // the load of TMA overwrite the first increase.
        let first_increase_overwritten = !overflow_happens_early
            && !overflow_happens_during_load
            && self.loading as u64 > until_falling_edge
            && (self.loading <= 4 || self.loading as u64 - 4 <= elapsed);

        if self.loading != 0 {
            self.loading = (self.loading as u64).saturating_sub(elapsed) as u8;
            if self.loading <= 4 && !overflow_happens_early {
                self.tima = self.tma;
                interrupt = true;
            }
        }

        let increase_tima = |this: &mut Timer, remain: u64, x: u8| -> bool {
            this.tima = this.tima.wrapping_add(x);
            if this.tima == 0 {
                this.loading = (7u64.saturating_sub(remain)) as u8;
                if this.loading <= 4 {
                    this.tima = this.tma;
                    return true;
                }
            }
            false
        };

        if self.tac & 0b100 != 0 && remain >= until_falling_edge {
            remain -= until_falling_edge;

            if !first_increase_overwritten {
                interrupt |= increase_tima(self, remain, 1);
            }

            while remain >= period {
                remain -= period;
                interrupt |= increase_tima(self, remain, 1);
            }
        }

        self.div = self.div.wrapping_add(elapsed as u16);
        self.last_counter_bit = (self.div >> freq) as u8 & (self.tac >> 2) & 0b1 != 0;

        self.next_interrupt = self.estimate_next_interrupt();

        interrupt
    }

    /// Return true if there is a interrupt
    ///
    /// Reference implementation to [Self::update]. Slower, but less prone to bugs.
    pub fn update_cycle_by_cycle(&mut self, clock_count: u64) -> bool {
        let mut interrupt = false;

        for _clock in self.last_clock_count..clock_count {
            self.div = self.div.wrapping_add(1);

            let freq = [9, 3, 5, 7][(self.tac & 0b11) as usize];
            let counter_bit = ((self.div >> freq) as u8 & (self.tac >> 2)) & 0b1 != 0;

            // faling edge
            if self.last_counter_bit && !counter_bit {
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

        let div = self.div.wrapping_add(1);

        let freq = [9, 3, 5, 7][(self.tac & 0b11) as usize];
        let period = 1 << (freq as u64 + 1);

        let until_falling_edge = period - (div as u64 & (period - 1));
        debug_assert!((div as u64 + until_falling_edge) & (period - 1) == 0);

        let counter_bit = (div >> freq) as u8 & 0b1 != 0;
        let tima = if self.last_counter_bit && !counter_bit {
            self.tima as u64 + 2
        } else {
            self.tima as u64 + 1
        };

        let remaining_time = if tima == 256 {
            until_falling_edge + 4
        } else if tima == 257 {
            4
        } else {
            (256 - tima) * period + until_falling_edge + 4
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
    fn fuzz_interrupt_prediction() {
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
                let timer_start = timer.clone();
                let next_interrupt = timer.estimate_next_interrupt();

                if next_interrupt == u64::MAX {
                    let r: f64 = rng.gen();
                    let cycles = (2.0f64.powf(1.0 + r * 21.0)) as u64;
                    println!("random target: {}", cycles);

                    let interrupt = timer.update(timer.last_clock_count + cycles);
                    check_with_ref(&timer_start, &timer, interrupt);
                    if interrupt {
                        panic!("interrupt is on? {:?}", timer_start);
                    }
                } else if next_interrupt <= timer.last_clock_count + 1 {
                    for _ in 0..10_000_000 {
                        let interrupt = timer.update(timer.last_clock_count + 4);
                        check_with_ref(&timer_start, &timer, interrupt);
                        if interrupt {
                            continue 'test;
                        }
                    }
                    panic!("interrupt never happens!?: {:?}", timer_start)
                } else {
                    let interrupt = timer.update(next_interrupt - 1);
                    check_with_ref(&timer_start, &timer, interrupt);
                    if interrupt {
                        panic!("interrupt is on early? {:?}", timer_start);
                    }
                    let interrupt = timer.update(next_interrupt);
                    check_with_ref(&timer_start, &timer, interrupt);
                    if !interrupt {
                        panic!("interrupt is not on? {:?}", timer_start);
                    }
                };
            }
        }
    }

    #[test]
    fn fuzz_with_ref() {
        let start = std::time::Instant::now();
        let mut rng = rand::thread_rng();

        while start.elapsed().as_secs() < 3 {
            let timer_start = Timer {
                div: rng.gen(),
                tima: rng.gen(),
                tma: rng.gen(),
                tac: rng.gen(),
                last_counter_bit: rng.gen(),
                last_clock_count: 0,
                loading: rng.gen_range(0..=7),
                next_interrupt: rng.gen(),
            };
            let mut timer = timer_start.clone();

            let r: f64 = rng.gen();
            let cycles = (2.0f64.powf(r * r * 10.0)) as u64;

            let interrupt = timer.update(cycles);
            check_with_ref(&timer_start, &timer, interrupt);
        }
    }

    fn test_case(mut timer: Timer, random_target: u64) {
        timer.last_clock_count = 0;
        let timer_start = timer.clone();
        dbg!(&timer);
        let next_interrupt = timer.estimate_next_interrupt();
        if next_interrupt == u64::MAX {
            let interrupt = timer.update(random_target);
            check_with_ref(&timer_start, &timer, interrupt);
            if interrupt {
                panic!("interrupt should never happen!");
            }
        } else if next_interrupt <= timer.last_clock_count + 1 {
            for _ in 0..10_000_000 {
                let interrupt = timer.update(timer.last_clock_count + 4);
                check_with_ref(&timer_start, &timer, interrupt);
                if interrupt {
                    return;
                }
            }
            panic!("interrupt never happens!?")
        } else {
            let interrupt = timer.update(next_interrupt - 1);
            check_with_ref(&timer_start, &timer, interrupt);
            if interrupt {
                panic!("interrupt is on early?");
            }
            let interrupt = timer.update(next_interrupt);
            check_with_ref(&timer_start, &timer, interrupt);
            if !interrupt {
                panic!("interrupt is not on?");
            }
        };
    }

    pub fn check_with_ref(timer_start: &Timer, timer: &Timer, interrupt: bool) {
        let mut timer_ref = timer_start.clone();
        let interrupt_ref = timer_ref.update_cycle_by_cycle(timer.last_clock_count);
        if interrupt != interrupt_ref || *timer != timer_ref {
            println!(
                "updated {} cycles",
                timer.last_clock_count - timer_start.last_clock_count
            );
            println!("interrupts: ref: {}, got: {}", interrupt_ref, interrupt);
            println!("start:     {:?}", timer_start);
            println!("reference: {:?}", timer_ref);
            println!("fast:      {:?}", timer);
            panic!("Don't match with reference!")
        }
    }

    macro_rules! test_cases {
        { $( $(#[$($attrib:meta)*])* $test:ident: $timer:expr; )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_case($timer, 10_000);
            })*
        };
    }

    test_cases!(
        case1: Timer {
            div: 0b11001,
            tima: 228,
            tma: 92,
            tac: 0b110,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 0,
        };
        case2: Timer {
            div: 0,
            tima: 255,
            tma: 94,
            tac: 0b111,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 0,
        };
        case3: Timer {
            div: 59393,
            tima: 0,
            tma: 20,
            tac: 0b011,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 0,
        };
        case4: Timer {
            div: 0x3D4F,
            tima: 0,
            tma: 0xC1,
            tac: 0b1001100,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 260787,
        };
        case5: Timer {
            div: 0b11000001001111,
            // div: 12367,
            tima: 60,
            tma: 0,
            tac: 0b1101101,
            last_counter_bit: false,
            last_clock_count: 17510503,
            loading: 0,
            next_interrupt: 17513627,
        };
        case6: Timer { div: 17351, tima: 255, tma: 255, tac: 205, last_counter_bit: false, last_clock_count: 691228312, loading: 0, next_interrupt: 691228323 };
        case7: Timer { div: 11463, tima: 255, tma: 77, tac: 103, last_counter_bit: false, last_clock_count: 1261064037, loading: 0, next_interrupt: 1261064096 };
        case8: Timer { div: 36591, tima: 0, tma: 0, tac: 133, last_counter_bit: false, last_clock_count: 320704263, loading: 0, next_interrupt: 320708347 };
        case9: Timer { div: 12367, tima: 60, tma: 0, tac: 109, last_counter_bit: false, last_clock_count: 17510503, loading: 0, next_interrupt: 17513627 };
        case10: Timer { div: 65512, tima: 0, tma: 0, tac: 0, last_counter_bit: false, last_clock_count: 0, loading: 0, next_interrupt: 0 };
    );

    #[test]
    fn with_ref1() {
        let timer_start = Timer {
            div: 52,
            tima: 4,
            tma: 4,
            tac: 5,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 0,
            next_interrupt: 23444475,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(4);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref2() {
        let timer_start = Timer {
            div: 9350,
            tima: 63,
            tma: 235,
            tac: 139,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 1,
            next_interrupt: 11422743145167053399,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(1);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref3() {
        let timer_start = Timer {
            div: 31772,
            tima: 255,
            tma: 141,
            tac: 204,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 3,
            next_interrupt: 16220173124440547041,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(1);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref4() {
        let timer_start = Timer {
            div: 34910,
            tima: 50,
            tma: 227,
            tac: 165,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 7,
            next_interrupt: 17646046326310135957,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(60);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref5() {
        let timer_start = Timer {
            div: 25789,
            tima: 5,
            tma: 148,
            tac: 101,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 2,
            next_interrupt: 14621974948106595554,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(94);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref6() {
        let timer_start = Timer {
            div: 158,
            tima: 214,
            tma: 98,
            tac: 45,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 5,
            next_interrupt: 4681648085878461225,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(3);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref7() {
        let timer_start = Timer {
            div: 58574,
            tima: 61,
            tma: 19,
            tac: 53,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 14866078717898697690,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(2);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref8() {
        let timer_start = Timer {
            div: 10126,
            tima: 255,
            tma: 39,
            tac: 157,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 11877710213502878665,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(3);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref9() {
        let timer_start = Timer {
            div: 4206,
            tima: 42,
            tma: 255,
            tac: 85,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 7,
            next_interrupt: 15356067585205845017,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(1);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref10() {
        let timer_start = Timer {
            div: 23374,
            tima: 233,
            tma: 255,
            tac: 157,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 5517617767262456490,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(3);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref11() {
        let timer_start = Timer {
            div: 57934,
            tima: 255,
            tma: 226,
            tac: 0b10010101,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 3,
            next_interrupt: 5440094674562551980,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(18);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref12() {
        let timer_start = Timer {
            div: 0b1001110011111101,
            tima: 254,
            tma: 155,
            tac: 0b11000011,
            last_counter_bit: true,
            last_clock_count: 0,
            loading: 7,
            next_interrupt: 1945807476102360953,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(5);
        check_with_ref(&timer_start, &timer, interrupt);
    }

    #[test]
    fn with_ref13() {
        let timer_start = Timer {
            div: 57706,
            tima: 215,
            tma: 255,
            tac: 0b1100101,
            last_counter_bit: false,
            last_clock_count: 0,
            loading: 6,
            next_interrupt: 13843092793995000582,
        };
        let mut timer = timer_start.clone();
        let interrupt = timer.update(6);
        check_with_ref(&timer_start, &timer, interrupt);
    }
}
