use std::io::{Read, Write};

use crate::save_state::{LoadStateError, SaveState};

#[derive(Default, Debug, PartialEq, Eq)]
pub struct Timer {
    pub div: u16,
    pub tima: u8,
    pub tma: u8,
    pub tac: u8,
    pub last_counter_bit: bool,
    /// The last clock cycle where Timer was updated
    pub last_clock_count: u64,
}

impl SaveState for Timer {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        self.div.save_state(data)?;
        self.tima.save_state(data)?;
        self.tma.save_state(data)?;
        self.tac.save_state(data)?;
        [&self.last_counter_bit].save_state(data)?;
        self.last_clock_count.save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.div.load_state(data)?;
        self.tima.load_state(data)?;
        self.tma.load_state(data)?;
        self.tac.load_state(data)?;
        [&mut self.last_counter_bit].load_state(data)?;
        self.last_clock_count.load_state(data)?;
        Ok(())
    }
}

// TODO: At some point, I want this timer to be lazy evaluated.
impl Timer {
    /// Advance the timer by one cycle
    /// Return true if there is a interrupt
    pub(crate) fn update(&mut self, clock_count: u64) -> bool {
        let mut interrupt = false;
        for _ in self.last_clock_count..clock_count {
            self.div = self.div.wrapping_add(1);

            let f = [9, 3, 5, 7][(self.tac & 0b11) as usize];
            let counter_bit = ((self.div >> f) as u8 & (self.tac >> 2)) & 0b1 != 0;

            // faling edge
            if self.last_counter_bit && !counter_bit {
                let (v, o) = self.tima.overflowing_add(1);
                self.tima = v;
                // TODO: TIMA, on overflow, should keep the value 0 for 4 cycles
                // before the overflow be detected. A write in this interval would cancel it.
                if o {
                    self.tima = self.tma;
                    interrupt = true;
                }
            }

            self.last_counter_bit = counter_bit;
        }
        self.last_clock_count = clock_count;

        interrupt
    }

    pub(crate) fn read_div(&self) -> u8 {
        (self.div >> 8) as u8
    }
    pub(crate) fn write_div(&mut self, _div: u8) {
        self.div = 0;
    }

    pub(crate) fn read_tima(&self) -> u8 {
        self.tima
    }
    pub(crate) fn write_tima(&mut self, tima: u8) {
        self.tima = tima;
    }

    pub(crate) fn read_tma(&self) -> u8 {
        self.tma
    }
    pub(crate) fn write_tma(&mut self, tma: u8) {
        self.tma = tma;
    }

    pub(crate) fn read_tac(&self) -> u8 {
        self.tac
    }
    pub(crate) fn write_tac(&mut self, tac: u8) {
        self.tac = tac;
    }
}
