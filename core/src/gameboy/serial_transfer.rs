use crate::save_state::{LoadStateError, SaveState};

use super::GameBoy;

/// The offset between `clock_count` and the serial transfer clock, in cycles. This is choose
/// arbitrarily, in a way that pass the serial_boot_sclk_align_dmg_abc_mgb test.
const SERIAL_OFFSET: u64 = 8;

pub struct Serial {
    /// FF01: SB
    pub serial_data: u8,
    /// FF02: SC
    pub serial_control: u8,

    /// The instant, in 2^13 Hz clock count (T-clock count >> 9), in which the first bit of current
    /// serial transfer was send. It is 0 if there is no transfer happening.
    pub serial_transfer_started: u64,

    #[cfg(not(target_arch = "wasm32"))]
    pub serial_transfer_callback: Option<Box<dyn FnMut(u8) + Send>>,
    #[cfg(target_arch = "wasm32")]
    pub serial_transfer_callback: Option<Box<dyn FnMut(u8)>>,

    /// The estimated time where the next interrupt may happen.
    pub next_interrupt: u64,
}

impl Eq for Serial {}
impl PartialEq for Serial {
    fn eq(&self, other: &Self) -> bool {
        self.serial_data == other.serial_data
            && self.serial_control == other.serial_control
            && self.serial_transfer_started == other.serial_transfer_started
        // && self.serial_transfer_callback == other.serial_transfer_callback
        // && self.next_interrupt == other.next_interrupt
    }
}

crate::save_state!(Serial, self, data {
    self.serial_data;
    self.serial_control;
    self.serial_transfer_started;

    on_load self.next_interrupt = 0;
});

impl Serial {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            serial_data: 0,
            serial_control: 0x7E,
            serial_transfer_started: 0,
            serial_transfer_callback: Some(Box::new(|c| {
                eprint!("{}", c as char);
            })),
            next_interrupt: 0,
        }
    }

    pub fn reset(&mut self) {
        *self = Self {
            serial_transfer_callback: self.serial_transfer_callback.take(),
            ..Self::new()
        }
    }

    pub fn update(&mut self, clock_count: u64) -> bool {
        if self.serial_transfer_started != 0
            && self.serial_transfer_started + 7 < (clock_count + SERIAL_OFFSET) >> 9
        {
            // clear transfer flag bit
            self.serial_control &= !0x80;
            self.serial_transfer_started = 0;

            self.next_interrupt = self.estimate_next_interrupt();

            // interrupt
            return true;
        }
        false
    }

    pub fn write(gb: &mut GameBoy, address: u8, value: u8) {
        match address {
            0x01 => gb.serial.get_mut().serial_data = value,
            0x02 => {
                gb.update_serial();
                let this = &mut *gb.serial.get_mut();
                this.serial_control = value | 0x7E;
                if value & 0x81 == 0x81 {
                    // serial transfer is aligned to a 8192Hz (2^13 Hz) clock.
                    this.serial_transfer_started = (gb.clock_count + SERIAL_OFFSET) >> 9;
                    let data = this.serial_data;
                    if let Some(x) = this.serial_transfer_callback.as_mut() {
                        x(data)
                    }
                }
            }
            _ => unreachable!(),
        }

        let this = &mut *gb.serial.get_mut();
        this.next_interrupt = this.estimate_next_interrupt();
    }

    pub fn read(gb: &GameBoy, address: u8) -> u8 {
        match address {
            0x01 => gb.serial.borrow().serial_data,
            0x02 => {
                gb.update_serial();
                gb.serial.borrow().serial_control
            }
            _ => unreachable!(),
        }
    }

    pub fn estimate_next_interrupt(&self) -> u64 {
        if self.serial_transfer_started == 0 {
            // will never happen
            u64::MAX
        } else {
            // from update:
            // serial_transfer_started + 7 < (clock_count + SERIAL_OFFSET) >> 9
            // => ((serial_transfer_started + 7) << 9) - SERIAL_OFFSET < clock_count
            ((self.serial_transfer_started + 7) << 9) - SERIAL_OFFSET
        }
    }
}
