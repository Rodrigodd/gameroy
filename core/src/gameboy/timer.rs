use crate::save_state::{LoadStateError, SaveState};

#[derive(Default, Debug, PartialEq, Eq)]
pub struct Timer {
    /// FF04: DIV register
    pub div: u16,
    /// FF05: TIMA register
    pub tima: u8,
    /// FF06: TMA register
    pub tma: u8,
    /// FF07: TAC register
    pub tac: u8,

    /// The value of the counter bit in the last cycle, for edge detection.
    pub last_counter_bit: bool,
    /// The last clock cycle where Timer was updated
    pub last_clock_count: u64,
    /// Keep track of TIMA reloading. TIMA is reloading if < 4, reloading is scheduled if >= 4, and
    /// there is no reload if = 0.
    pub loading: u8,
}
crate::save_state!(Timer, self, ctx, data {
    on_save debug_assert_eq!(self.last_clock_count, ctx.clock_count.unwrap());

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
        }
    }

    /// Advance the timer by one cycle
    /// Return true if there is a interrupt
    pub(crate) fn update(&mut self, clock_count: u64) -> bool {
        let mut interrupt = false;
        for _clock in self.last_clock_count..clock_count {
            self.div = self.div.wrapping_add(1);

            let f = [9, 3, 5, 7][(self.tac & 0b11) as usize];
            let counter_bit = ((self.div >> f) as u8 & (self.tac >> 2)) & 0b1 != 0;

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
    }
}
