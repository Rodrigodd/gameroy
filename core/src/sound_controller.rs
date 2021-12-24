use std::process::exit;

#[derive(Default)]
pub struct SoundController {
    // Sound Channel 1 - Tone & Sweep
    /// FF10: Channel 1 Sweep register (R/W)
    nr10: u8,
    /// FF11: Channel 1 Sound length/Wave pattern duty (R/W)
    nr11: u8,
    /// FF12: Channel 1 Volume Envelope (R/W)
    nr12: u8,
    /// FF13: Channel 1 Frequency lo (Write Only)
    nr13: u8,
    /// FF14: Channel 1 Frequency hi (R/W)
    nr14: u8,

    // Sound Channel 2 - Tone
    /// FF16: Channel 2 Sound Length/Wave Pattern Duty (R/W)
    nr21: u8,
    /// FF17: Channel 2 Volume Envelope (R/W)
    nr22: u8,
    /// FF18: Channel 2 Frequency lo data (W)
    nr23: u8,
    /// FF19: Channel 2 Frequency hi data (R/W)
    nr24: u8,

    // Sound Channel 3 - Wave Output
    /// FF1A - NR30 - Channel 3 Sound on/off (R/W)
    nr30: u8,
    /// FF1B - NR31 - Channel 3 Sound Length (W)
    nr31: u8,
    /// FF1C - NR32 - Channel 3 Select output level (R/W)
    nr32: u8,
    /// FF1D - NR33 - Channel 3 Frequency’s lower data (W)
    nr33: u8,
    /// FF1E - NR34 - Channel 3 Frequency’s higher data (R/W)
    nr34: u8,
    /// FF30-FF3F - Wave Pattern RAM
    wave_pattern: [u8; 16],

    // Sound Channel 4 - Noise
    /// FF20 - NR41 - Channel 4 Sound Length (W)
    nr41: u8,
    /// FF21 - NR42 - Channel 4 Volume Envelope (R/W)
    nr42: u8,
    /// FF22 - NR43 - Channel 4 Polynomial Counter (R/W)
    nr43: u8,
    /// FF23 - NR44 - Channel 4 Counter/consecutive; Inital (R/W)
    nr44: u8,

    // Sound Control Registers
    /// FF24 - NR50 - Channel control / ON-OFF / Volume (R/W)
    nr50: u8,
    /// FF25 - NR51 - Selection of Sound output terminal (R/W)
    nr51: u8,
    /// FF26 - NR52 - Sound on/off
    nr52: u8,

    /// All sound on/off
    on: bool,

    /// Output Buffer
    output: Vec<u8>,
    /// Clock count at the last sound output
    last_clock: u64,

    frequency_timer: u16,
    wave_duty_position: u8,
}

const WAVE_DUTY_TABLE: [u8; 4] = [0b0000_0001, 0b0000_0011, 0b0000_1111, 0b1111_1100];

impl SoundController {
    /// Return the currently generated audio output. The buffer is cleared.
    pub fn get_output(&mut self, clock_count: u64) -> Vec<u8> {
        self.update(clock_count);
        std::mem::take(&mut self.output)
    }

    pub fn update(&mut self, clock_count: u64) {
        let sample_rate = crate::consts::CLOCK_SPEED / 48_000;
        // if it is off, there is no need for audio generation
        if !self.on {
            //  n = r/k - (l - 1)/k <- underflow
            //  n = r/k - l/k + (l%k) == 0
            let n = clock_count / sample_rate - self.last_clock / sample_rate
                + ((self.last_clock % sample_rate) == 0) as u64;
            self.last_clock = clock_count;
            self.output.extend((0..n).map(|_| 0));
            return;
        }
        // channel 2
        let duty = (self.nr21 >> 6) & 0x3;
        let freq = u16::from_be_bytes([self.nr24, self.nr23]) & 0x07FF;

        // mixing
        let volume_left = (self.nr50 & 0x70) >> 4;
        let channel2_left = (self.nr51 & 0x20) != 0;
        let volume_right = self.nr50 & 0x7;
        let channel2_right = (self.nr51 & 0x02) != 0;
        for clock in self.last_clock..clock_count {
            // The frequency timer decreases in one every clock. When it reaches 0, it is reloaded.
            if self.frequency_timer <= 1 {
                // Frequency Timer = (2048 - Frequency) * 4;
                self.frequency_timer = (2048 - freq) * 4;
                self.wave_duty_position = (self.wave_duty_position + 1) % 8;
            } else {
                self.frequency_timer -= 1;
            }
            // collect a sample
            // TODO: here CLOCK_SPEED / 48_000 is rounded down, wich could cause inaccuracies.
            if (clock % sample_rate) == 0 {
                let mut left = 0;
                if channel2_left {
                    let amp = (WAVE_DUTY_TABLE[duty as usize] >> self.wave_duty_position) & 0x1;
                    left += amp;
                };
                let mut right = 0;
                if channel2_right {
                    let amp = (WAVE_DUTY_TABLE[duty as usize] >> self.wave_duty_position) & 0x1;
                    right += amp;
                };
                self.output.push(8 * left * volume_left);
                self.output.push(8 * right * volume_right);
            }
        }
        self.last_clock = clock_count;
        if self.output.len() > 2 * 1200_000 {
            std::fs::write("audio.pcm", &self.output).unwrap();
            exit(0);
        }
    }

    // TODO: Check for read or write only registers and bits.
    pub fn write(&mut self, clock_count: u64, address: u8, value: u8) {
        self.update(clock_count);
        match address {
            0x10 => self.nr10 = value,
            0x11 => self.nr11 = value,
            0x12 => self.nr12 = value,
            0x13 => self.nr13 = value,
            0x14 => self.nr14 = value,
            0x16 => {
                self.nr21 = value;
                eprintln!("write nr21: {:02x}", value)
            }
            0x17 => self.nr22 = value,
            0x18 => {
                self.nr23 = value;
                eprintln!("write nr23: {:02x}", value)
            }
            0x19 => self.nr24 = value,
            0x1A => self.nr30 = value,
            0x1B => self.nr31 = value,
            0x1C => self.nr32 = value,
            0x1D => self.nr33 = value,
            0x1E => self.nr34 = value,
            0x20 => self.nr41 = value,
            0x21 => self.nr42 = value,
            0x22 => self.nr43 = value,
            0x23 => self.nr44 = value,
            0x24 => {
                self.nr50 = value;
                eprintln!("write nr50: {:02x}", value)
            }
            0x25 => {
                self.nr51 = value;
                eprintln!("write nr51: {:02x}", value)
            }
            0x26 => {
                eprintln!("write nr52: {:02x}", value);
                // Bit 7 stop all sounds
                if value & 0x80 == 0 {
                    self.on = false;
                    // and reset all registers
                    *self = Self::default();
                } else {
                    self.on = true;
                }
            }
            0x30..=0x3F => self.wave_pattern[address as usize - 0x30] = value,
            _ => unreachable!(),
        }
    }

    pub fn read(&mut self, address: u8) -> u8 {
        match address {
            0x10 => self.nr10,
            0x11 => self.nr11,
            0x12 => self.nr12,
            0x13 => self.nr13,
            0x14 => self.nr14,
            0x16 => self.nr21,
            0x17 => self.nr22,
            0x18 => self.nr23,
            0x19 => self.nr24,
            0x1A => self.nr30,
            0x1B => self.nr31,
            0x1C => self.nr32,
            0x1D => self.nr33,
            0x1E => self.nr34,
            0x20 => self.nr41,
            0x21 => self.nr42,
            0x22 => self.nr43,
            0x23 => self.nr44,
            0x24 => self.nr50,
            0x25 => self.nr51,
            0x26 => self.nr52,
            0x30..=0x3F => self.wave_pattern[address as usize - 0x30],
            _ => unreachable!(),
        }
    }
}
