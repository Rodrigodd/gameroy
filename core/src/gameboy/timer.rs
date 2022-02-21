use std::io::{Read, Write};

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
            0x05 => self.tima = value,
            0x06 => self.tma = value,
            0x07 => self.tac = value,
            _ => unreachable!("out of Timer memory map"),
        }
    }
}
