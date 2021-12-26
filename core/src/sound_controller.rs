use std::process::exit;

use crate::consts::CLOCK_SPEED;

// based mostly on https://nightshade256.github.io/2021/03/27/gb-sound-emulation.html#fnref:2

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

    ch1_frequency_timer: u16,
    ch1_wave_duty_position: u8,
    ch1_current_volume: u8,
    ch1_period_timer: u8,
    ch2_frequency_timer: u16,
    ch2_wave_duty_position: u8,
    ch2_current_volume: u8,
    ch2_period_timer: u8,

    /// Output Buffer
    output: Vec<u8>,
    /// Clock count at the last sound output
    last_clock: u64,
    /// The frequency in Hertz at which the sound controller is sampled.
    pub sample_frequency: u64,
    sample_mod: u64,
}

impl Default for SoundController {
    fn default() -> Self {
        Self {
            nr10: 0,
            nr11: 0,
            nr12: 0,
            nr13: 0,
            nr14: 0,
            nr21: 0,
            nr22: 0,
            nr23: 0,
            nr24: 0,
            nr30: 0,
            nr31: 0,
            nr32: 0,
            nr33: 0,
            nr34: 0,
            wave_pattern: [0; 16],
            nr41: 0,
            nr42: 0,
            nr43: 0,
            nr44: 0,
            nr50: 0,
            nr51: 0,
            nr52: 0,
            on: false,
            ch1_frequency_timer: 0,
            ch1_wave_duty_position: 0,
            ch1_current_volume: 0,
            ch1_period_timer: 0,
            ch2_frequency_timer: 0,
            ch2_wave_duty_position: 0,
            ch2_current_volume: 0,
            ch2_period_timer: 0,
            output: Vec::default(),
            last_clock: 0,
            sample_frequency: 48_000,
            sample_mod: 0,
        }
    }
}

const WAVE_DUTY_TABLE: [u8; 4] = [0b0000_0001, 0b0000_0011, 0b0000_1111, 0b1111_1100];

impl SoundController {
    /// Return the currently generated audio output. The buffer is cleared.
    pub fn get_output(&mut self, clock_count: u64) -> Vec<u8> {
        self.update(clock_count);
        std::mem::take(&mut self.output)
    }

    pub fn update(&mut self, clock_count: u64) {
        // if it is off, there is no need for audio generation
        if !self.on {
            // compute the number of samples (multiples of k) beetween l (inclusive) and r (exclusive)
            //  k = fc/fs
            //  n = r/k - l/k + (l%k == 0) <- for r, l and k integers
            //  => n = r*fs/fc - l*fs/fc + (l*fs % fc < fs) <- for r, l, fs and fc integers

            // map clock_count to a smaller value, to avoid multiplication overflows in the
            // distance future
            let anchor = self.last_clock - (self.last_clock % CLOCK_SPEED);
            let l = self.last_clock - anchor;
            let r = clock_count - anchor;

            let n = r * self.sample_frequency / CLOCK_SPEED
                - l * self.sample_frequency / CLOCK_SPEED
                + ((l * self.sample_frequency) % CLOCK_SPEED < self.sample_frequency) as u64;
            // for each sample, there is two values (left and right channels)
            self.output.extend((0..2 * n).map(|_| 0));

            self.last_clock = clock_count;
            let elapsed_clock = clock_count - self.last_clock;
            self.sample_mod =
                (self.sample_mod + elapsed_clock * self.sample_frequency) % CLOCK_SPEED;
            return;
        }
        // channel 1
        let ch1_duty = (self.nr11 >> 6) & 0x3;
        let ch1_freq = u16::from_be_bytes([self.nr14, self.nr13]) & 0x07FF;
        let ch1_period = self.nr12 & 0x7;
        let ch1_env_direction = (self.nr12 & 0x08) != 0;
        // channel 2
        let ch2_duty = (self.nr21 >> 6) & 0x3;
        let ch2_freq = u16::from_be_bytes([self.nr24, self.nr23]) & 0x07FF;
        let ch2_period = self.nr22 & 0x7;
        let ch2_env_direction = (self.nr22 & 0x08) != 0;

        // mixing
        let volume_left = (self.nr50 & 0x70) >> 4;
        let ch1_left = (self.nr51 & 0x10) != 0;
        let ch2_left = (self.nr51 & 0x20) != 0;
        let volume_right = self.nr50 & 0x7;
        let ch1_right = (self.nr51 & 0x01) != 0;
        let ch2_right = (self.nr51 & 0x02) != 0;
        for clock in self.last_clock..clock_count {
            // The frequency timer decreases in one every clock. When it reaches 0, it is reloaded.
            //
            if self.ch1_frequency_timer <= 1 {
                // Frequency Timer = (2048 - Frequency) * 4;
                self.ch1_frequency_timer = (2048 - ch1_freq) * 4;
                self.ch1_wave_duty_position = (self.ch1_wave_duty_position + 1) % 8;
            } else {
                self.ch1_frequency_timer -= 1;
            }

            if self.ch2_frequency_timer <= 1 {
                // Frequency Timer = (2048 - Frequency) * 4;
                self.ch2_frequency_timer = (2048 - ch2_freq) * 4;
                self.ch2_wave_duty_position = (self.ch2_wave_duty_position + 1) % 8;
            } else {
                self.ch2_frequency_timer -= 1;
            }

            // frame sequencer
            
            // TODO: a step should happens in a falling edge of the bit 5 of the DIV timer.
            if clock % (CLOCK_SPEED / 512) == 0 { // step
                let lenght_ctr = (clock % (CLOCK_SPEED / 256)) == 0;
                let volume_env = (clock % (CLOCK_SPEED / 64)) == 0;
                let sweep = ((clock + CLOCK_SPEED / 256) % (CLOCK_SPEED / 128)) == 0;

                fn env(period: u8, period_timer: &mut u8, current_volume: &mut u8, is_upwards: bool) {
                    if period != 0 {
                        if *period_timer > 0 {
                            *period_timer -= 1;
                        }

                        if *period_timer == 0 {
                            *period_timer = period;

                            if (*current_volume < 0xF && is_upwards) || (*current_volume > 0x0 && !is_upwards) {
                                if is_upwards {
                                    *current_volume += 1;
                                } else {
                                    *current_volume -= 1;
                                }
                            }
                        }
                    }
                }

                if volume_env {
                    env(ch1_period, &mut self.ch1_period_timer, &mut self.ch1_current_volume, ch1_env_direction);
                    env(ch2_period, &mut self.ch2_period_timer, &mut self.ch2_current_volume, ch2_env_direction);
                }
            }

            // collect a sample

            // c % (fc/fs) == 0 ~> c % (fc/fs) < 1 => (c*fs) % fc < fs
            // => ((c-1)*fs) % fc + fs) % fc < fs
            // => (last + fs) % fc < fs
            self.sample_mod = (self.sample_mod + self.sample_frequency) % CLOCK_SPEED;
            if self.sample_mod < self.sample_frequency {
                let ch1_amp = ((WAVE_DUTY_TABLE[ch1_duty as usize] >> self.ch1_wave_duty_position) & 0x1) * self.ch1_current_volume;
                let ch2_amp = ((WAVE_DUTY_TABLE[ch2_duty as usize] >> self.ch2_wave_duty_position) & 0x1) * self.ch2_current_volume;
                let mut left = 0;
                if ch1_left {
                    left += ch1_amp;
                };
                if ch2_left {
                    left += ch2_amp;
                };
                let mut right = 0;
                if ch1_right {
                    right += ch1_amp;
                };
                if ch2_right {
                    right += ch2_amp;
                };
                self.output.push(left * volume_left);
                self.output.push(right * volume_right);
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
            0x14 => {
                if value & 0x80 != 0 {
                    // restart sound
                    self.ch1_period_timer = self.nr12 & 0x07;
                    self.ch1_current_volume = (self.nr12 & 0xF0) >> 4;
                    self.ch1_frequency_timer = 0;
                    self.ch1_wave_duty_position = 0;
                }
                self.nr14 = value;
            },
            0x16 => {
                self.nr21 = value;
                eprintln!("write nr21: {:02x}", value)
            }
            0x17 => self.nr22 = value,
            0x18 => {
                self.nr23 = value;
                eprintln!("write nr23: {:02x}", value)
            }
            0x19 => {
                if value & 0x80 != 0 {
                    // restart sound
                    self.ch2_period_timer = self.nr22 & 0x07;
                    self.ch2_current_volume = (self.nr22 & 0xF0) >> 4;
                    self.ch2_frequency_timer = 0;
                    self.ch2_wave_duty_position = 0;
                }
                self.nr24 = value;
            },
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
