use crate::consts::CLOCK_SPEED;

// based on https://nightshade256.github.io/2021/03/27/gb-sound-emulation.html, https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware
// and https://github.com/LIJI32/SameBoy source code.

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
    ch3_wave_pattern: [u8; 16],

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
    // FF26 - NR52 - Sound on/off
    // nr52: u8, this is replaced by `on` and `chX_channel_enable`s
    /// All sound on/off
    on: bool,

    /// The currenty step of the frame sequencer
    frame_sequencer_step: u8,
    // From SameBoy source code: "When turning the APU on while DIV's bit 4 (or 5 in
    // double speed mode) is on, the first DIV/APU event is skipped."
    // frame_sequencer_skip: u8,
    ch1_channel_enable: bool,
    ch1_length_timer: u8,
    ch1_sweep_enabled: bool,
    ch1_shadow_freq: u16,
    ch1_sweep_timer: u8,
    /// Tell if there was at last one sweep calculation with negate mode since the last trigger
    ch1_has_done_sweep_calculation: bool,
    ch1_frequency_timer: u16,
    ch1_wave_duty_position: u8,
    ch1_current_volume: u8,
    ch1_env_period_timer: u8,

    ch2_channel_enable: bool,
    ch2_length_timer: u8,
    ch2_frequency_timer: u16,
    ch2_wave_duty_position: u8,
    ch2_current_volume: u8,
    ch2_env_period_timer: u8,

    ch3_channel_enable: bool,
    ch3_length_timer: u16,
    ch3_frequency_timer: u16,
    ch3_wave_position: u8,
    ch3_sample_buffer: u8,
    ch3_wave_just_read: bool,

    ch4_channel_enable: bool,
    ch4_length_timer: u8,
    ch4_current_volume: u8,
    ch4_env_period_timer: u8,
    ch4_lfsr: u16,
    ch4_frequency_timer: u16,

    /// Output Buffer
    output: Vec<u16>,
    /// Clock count at the last sound update
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
            ch3_wave_pattern: [
                0x84, 0x40, 0x43, 0xAA, 0x2D, 0x78, 0x92, 0x3C, 0x60, 0x59, 0x59, 0xB0, 0x34, 0xB8,
                0x2E, 0xDA,
            ],
            nr41: 0,
            nr42: 0,
            nr43: 0,
            nr44: 0,
            nr50: 0,
            nr51: 0,
            on: false,
            frame_sequencer_step: 0,
            ch1_channel_enable: false,
            ch1_length_timer: 0,
            ch1_sweep_enabled: false,
            ch1_shadow_freq: 0,
            ch1_sweep_timer: 0,
            ch1_has_done_sweep_calculation: false,
            ch1_frequency_timer: 0,
            ch1_wave_duty_position: 0,
            ch1_current_volume: 0,
            ch1_env_period_timer: 0,
            ch2_channel_enable: false,
            ch2_length_timer: 0,
            ch2_frequency_timer: 0,
            ch2_wave_duty_position: 0,
            ch2_current_volume: 0,
            ch2_env_period_timer: 0,
            ch3_channel_enable: false,
            ch3_length_timer: 0,
            ch3_frequency_timer: 0,
            ch3_wave_position: 0,
            ch3_sample_buffer: 0,
            ch3_wave_just_read: false,
            ch4_channel_enable: false,
            ch4_length_timer: 0,
            ch4_current_volume: 0,
            ch4_env_period_timer: 0,
            ch4_lfsr: 0,
            ch4_frequency_timer: 0,
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
    pub fn get_output(&mut self, clock_count: u64) -> Vec<u16> {
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
        let mut ch1_freq = u16::from_be_bytes([self.nr14, self.nr13]) & 0x07FF;
        let ch1_sweep_period = (self.nr10 & 0x70) >> 4;
        let ch1_sweep_direction = (self.nr10 & 0x08) != 0;
        let ch1_sweep_shift = self.nr10 & 0x7;
        let ch1_env_period = self.nr12 & 0x7;
        let ch1_env_direction = (self.nr12 & 0x08) != 0;

        // channel 2
        let ch2_duty = (self.nr21 >> 6) & 0x3;
        let ch2_freq = u16::from_be_bytes([self.nr24, self.nr23]) & 0x07FF;
        let ch2_env_period = self.nr22 & 0x7;
        let ch2_env_direction = (self.nr22 & 0x08) != 0;

        // channel 3
        let ch3_output_level = [4, 0, 1, 2][(self.nr32 as usize & 0x60) >> 5];
        let ch3_freq = u16::from_be_bytes([self.nr34, self.nr33]) & 0x07FF;

        // channel 4
        let ch4_env_period = self.nr42 & 0x7;
        let ch4_env_direction = (self.nr42 & 0x08) != 0;
        let ch4_shift_amount = (self.nr43 & 0xF0) >> 4;
        let ch4_counter_width = (self.nr43 & 0x08) != 0;
        let ch4_divisor: u16 = [8, 16, 32, 48, 64, 80, 96, 112][self.nr43 as usize & 0x07];

        // mixing
        let volume_left = (self.nr50 & 0x70) >> 4;
        let ch1_left = (self.nr51 & 0x10) != 0;
        let ch2_left = (self.nr51 & 0x20) != 0;
        let ch3_left = (self.nr51 & 0x40) != 0;
        let ch4_left = (self.nr51 & 0x80) != 0;
        let volume_right = self.nr50 & 0x7;
        let ch1_right = (self.nr51 & 0x01) != 0;
        let ch2_right = (self.nr51 & 0x02) != 0;
        let ch3_right = (self.nr51 & 0x04) != 0;
        let ch4_right = (self.nr51 & 0x08) != 0;
        for clock in (self.last_clock..clock_count).filter(|x| x % 2 == 0) {
            // The frequency timer decreases in one every clock. When it reaches 0, it is reloaded.
            if self.ch1_channel_enable {
                if self.ch1_frequency_timer == 0 {
                    self.ch1_frequency_timer = (0x07FF ^ ch1_freq) * 2;
                    self.ch1_wave_duty_position = (self.ch1_wave_duty_position + 1) % 8;
                } else {
                    self.ch1_frequency_timer -= 1;
                }
            }

            if self.ch2_channel_enable {
                if self.ch2_frequency_timer == 0 {
                    self.ch2_frequency_timer = (0x07FF ^ ch2_freq) * 2;
                    self.ch2_wave_duty_position = (self.ch2_wave_duty_position + 1) % 8;
                } else {
                    self.ch2_frequency_timer -= 1;
                }
            }

            if self.ch3_channel_enable {
                if self.ch3_frequency_timer == 0 {
                    self.ch3_wave_position = (self.ch3_wave_position + 1) % 32;
                    self.ch3_sample_buffer = (self.ch3_wave_pattern
                                              [self.ch3_wave_position as usize / 2]
                                              >> [4, 0][self.ch3_wave_position as usize % 2])
                        & 0xF;
                    self.ch3_frequency_timer = 0x07FF ^ ch3_freq;
                    self.ch3_wave_just_read = true;
                } else {
                    self.ch3_frequency_timer -= 1;
                    self.ch3_wave_just_read = false;
                }
            } else {
                self.ch3_wave_just_read = false;
            }

            if self.ch4_channel_enable {
                if self.ch4_frequency_timer == 0 {
                    self.ch4_frequency_timer = ch4_divisor << ch4_shift_amount;
                    let xor = (self.ch4_lfsr & 0x1 != 0) ^ (self.ch4_lfsr & 0x2 != 0);
                    self.ch4_lfsr = (self.ch4_lfsr >> 1) | ((xor as u16) << 14);
                    if ch4_counter_width {
                        self.ch4_lfsr &= !(1 << 6);
                        self.ch4_lfsr |= (xor as u16) << 6;
                    }
                } else {
                    self.ch4_frequency_timer -= 1;
                }
            }

            // frame sequencer

            // TODO: a step should happens in a falling edge of the bit 5 of the DIV timer.
            if clock % (CLOCK_SPEED / 512) == 0 {
                // step
                let lenght_ctr = self.frame_sequencer_step % 2 == 0;
                let volume_env = self.frame_sequencer_step % 8 == 7;
                let sweep = self.frame_sequencer_step % 4 == 2;
                self.frame_sequencer_step = (self.frame_sequencer_step + 1) % 8;

                if lenght_ctr {
                    if self.nr14 & 0x40 != 0 && self.ch1_length_timer != 0 {
                        eprintln!("drecrease ch1 len from {}", self.ch1_length_timer);
                        self.ch1_length_timer -= 1;
                        if self.ch1_length_timer == 0 {
                            self.ch1_channel_enable = false;
                        }
                    }
                    if self.nr24 & 0x40 != 0 && self.ch2_length_timer != 0 {
                        self.ch2_length_timer -= 1;
                        if self.ch2_length_timer == 0 {
                            self.ch2_channel_enable = false;
                        }
                    }
                    if self.nr34 & 0x40 != 0 && self.ch3_length_timer != 0 {
                        self.ch3_length_timer -= 1;
                        if self.ch3_length_timer == 0 {
                            self.ch3_channel_enable = false;
                        }
                    }
                    if self.nr44 & 0x40 != 0 && self.ch4_length_timer != 0 {
                        self.ch4_length_timer -= 1;
                        if self.ch4_length_timer == 0 {
                            self.ch4_channel_enable = false;
                        }
                    }
                }

                if volume_env {
                    fn env(
                        period: u8,
                        period_timer: &mut u8,
                        current_volume: &mut u8,
                        is_upwards: bool,
                    ) {
                        if period != 0 {
                            if *period_timer > 0 {
                                *period_timer -= 1;
                            }

                            if *period_timer == 0 {
                                *period_timer = period;

                                if (*current_volume < 0xF && is_upwards)
                                    || (*current_volume > 0x0 && !is_upwards)
                                {
                                    if is_upwards {
                                        *current_volume += 1;
                                    } else {
                                        *current_volume -= 1;
                                    }
                                }
                            }
                        }
                    }

                    env(
                        ch1_env_period,
                        &mut self.ch1_env_period_timer,
                        &mut self.ch1_current_volume,
                        ch1_env_direction,
                    );
                    env(
                        ch2_env_period,
                        &mut self.ch2_env_period_timer,
                        &mut self.ch2_current_volume,
                        ch2_env_direction,
                    );
                    env(
                        ch4_env_period,
                        &mut self.ch4_env_period_timer,
                        &mut self.ch4_current_volume,
                        ch4_env_direction,
                    );
                }

                if sweep {
                    if self.ch1_sweep_timer > 0 {
                        self.ch1_sweep_timer -= 1;
                    }
                    if self.ch1_sweep_timer == 0 {
                        self.ch1_sweep_timer = if ch1_sweep_period == 0 {
                            8
                        } else {
                            ch1_sweep_period
                        };
                        if self.ch1_sweep_enabled && ch1_sweep_period != 0 {
                            let new_freq =
                                self.calculate_frequency(ch1_sweep_shift, ch1_sweep_direction);
                            if new_freq < 2048 && ch1_sweep_shift > 0 {
                                ch1_freq = new_freq & 0x07FF;
                                let [upper, lower] = ch1_freq.to_be_bytes();
                                self.nr14 = (self.nr14 & 0xF8) | (upper & 0x7);
                                self.nr13 = lower;

                                self.ch1_shadow_freq = new_freq;

                                // do overflow check again
                                self.calculate_frequency(ch1_sweep_shift, ch1_sweep_direction);
                            }
                        }
                    }
                }
            }

            // collect a sample

            // c % (fc/fs) == 0 ~> c % (fc/fs) < 1 => (c*fs) % fc < fs
            // => ( (c-1)*fs)%fc + fs) % fc < fs
            // => (    last      + fs) % fc < fs

            // I multiple fs by 2, because I increase clock by two by two
            self.sample_mod = (self.sample_mod + 2*self.sample_frequency) % CLOCK_SPEED;
            if self.sample_mod < 2*self.sample_frequency {
                let ch1_amp = ((WAVE_DUTY_TABLE[ch1_duty as usize] >> self.ch1_wave_duty_position)
                    & 0x1)
                    * self.ch1_current_volume;
                let ch2_amp = ((WAVE_DUTY_TABLE[ch2_duty as usize] >> self.ch2_wave_duty_position)
                    & 0x1)
                    * self.ch2_current_volume;
                let ch3_amp = self.ch3_sample_buffer >> ch3_output_level;
                let ch4_amp = ((!self.ch4_lfsr as u8) & 0x01) * self.ch4_current_volume;
                let mut left = 0;
                let mut right = 0;
                if self.ch1_channel_enable {
                    if ch1_left {
                        left += ch1_amp as u16;
                    }
                    if ch1_right {
                        right += ch1_amp as u16;
                    }
                }
                if self.ch2_channel_enable {
                    if ch2_left {
                        left += ch2_amp as u16;
                    }
                    if ch2_right {
                        right += ch2_amp as u16;
                    }
                }
                if self.ch3_channel_enable && self.nr30 & 0x80 != 0 {
                    if ch3_left {
                        left += ch3_amp as u16;
                    }
                    if ch3_right {
                        right += ch3_amp as u16;
                    }
                }
                if self.ch4_channel_enable {
                    if ch4_left {
                        left += ch4_amp as u16;
                    }
                    if ch4_right {
                        right += ch4_amp as u16;
                    }
                }
                self.output.push(left * volume_left as u16);
                self.output.push(right * volume_right as u16);
            }
        }
        self.last_clock = clock_count;
    }

    fn calculate_frequency(&mut self, ch1_sweep_shift: u8, is_downwards: bool) -> u16 {
        if is_downwards {
            self.ch1_has_done_sweep_calculation = true;
        }
        let mut new_freq = self.ch1_shadow_freq >> ch1_sweep_shift;
        if is_downwards {
            new_freq = self.ch1_shadow_freq - new_freq;
        } else {
            new_freq = self.ch1_shadow_freq + new_freq;
        };
        if new_freq >= 2048 {
            self.ch1_channel_enable = false;
        }
        new_freq
    }

    // TODO: Check for read or write only registers and bits.
    pub fn write(&mut self, clock_count: u64, address: u8, value: u8) {
        self.update(clock_count);
        if let 0x30..=0x3F = address {
            let ch3_freq = u16::from_be_bytes([self.nr34, self.nr33]) & 0x07FF;
            println!(
                "writ wave: pos {}, tim {}/{}, just {}, enabled {}/{}",
                self.ch3_wave_position,
                self.ch3_frequency_timer,
                ch3_freq ^ 0x07FF,
                self.ch3_wave_just_read as u8,
                self.ch3_channel_enable as u8,
                self.on as u8
            );
        }
        if !self.on {
            match address {
                0x26 => {
                    // writes to nr52 works
                }
                // On DMG, load counters can be written to, while off
                0x11 => {
                    self.nr11 = value & 0x3F;
                    self.ch1_length_timer = 64 - (value & 0x3F);
                    eprintln!("write nr11: {:02x}", value);
                    return;
                }
                0x16 => {
                    self.nr21 = value & 0x3F;
                    self.ch2_length_timer = 64 - (value & 0x3F);
                    eprintln!("write nr21: {:02x}", value);
                    return;
                }
                0x1B => {
                    self.nr31 = value;
                    self.ch3_length_timer = 256 - value as u16;
                    eprintln!("write nr31: {:02x}", value);
                    return;
                }
                0x20 => {
                    self.nr41 = value & 0x3F;
                    self.ch4_length_timer = 64 - (value & 0x3F);
                    eprintln!("write nr41: {:02x}", value);
                    return;
                }
                0x30..=0x3F => {
                    self.ch3_wave_pattern[address as usize - 0x30] = value;
                    eprintln!("write wave {:02x}: {:02x}", address, value);
                    return;
                }
                _ => return,
            }
        }
        match address {
            0x10 => {
                if self.nr10 & 0x08 != 0 && value & 0x08 == 0 && self.ch1_has_done_sweep_calculation
                {
                    // Clearing the sweep negate mode bit after at last one sweep calculation using
                    // the negate mode since the last trigger, disable the channel
                    self.ch1_channel_enable = false;
                }
                self.nr10 = value;
                eprintln!("write nr10: {:02x}", value)
            }
            0x11 => {
                self.nr11 = value;
                eprintln!("write nr11: {:02x}", value);
                let ch1_length_data = self.nr11 & 0x3F;
                self.ch1_length_timer = 64 - ch1_length_data;
                eprintln!("set ch1 len to {}", self.ch1_length_timer);
            }
            0x12 => {
                self.nr12 = value;
                if self.nr12 & 0xF8 == 0 {
                    self.ch1_channel_enable = false;
                }
                eprintln!("write nr12: {:02x}", value);
            }
            0x13 => {
                self.nr13 = value;
                eprintln!("write nr13: {:02x}", value);
            }
            0x14 => {
                let length_previous_enabled = self.nr14 & 0x40 != 0;
                let length_now_enabled = value & 0x40 != 0;
                let extra_clock = self.frame_sequencer_step % 2 == 1;

                // extra length clocking occurs when frame sequencer's next step don't clock the
                // length.
                if extra_clock {
                    // if was PREVIOUSLY disabled and now enabled and the length counter is not
                    // zero, the counter decreases
                    eprintln!(
                        "drecrease ch1 len from {} in extra clock",
                        self.ch1_length_timer
                    );
                    if !length_previous_enabled && length_now_enabled && self.ch1_length_timer != 0
                    {
                        self.ch1_length_timer -= 1;
                        // if became zero, and trigger is clear, disable channel
                        if self.ch1_length_timer == 0 && value & 0x80 == 0 {
                            self.ch1_channel_enable = false;
                        }
                    }
                }
                self.nr14 = value;

                if value & 0x80 != 0 {
                    // Trigger event
                    eprintln!("trigger ch1");
                    let ch1_freq = u16::from_be_bytes([self.nr14, self.nr13]) & 0x07FF;
                    let ch1_sweep_period = (self.nr10 & 0x70) >> 4;
                    let ch1_sweep_shift = self.nr10 & 0x7;
                    let ch1_sweep_direction = (self.nr10 & 0x08) != 0;
                    self.ch1_channel_enable = true;
                    if self.ch1_length_timer == 0 {
                        if extra_clock && length_now_enabled {
                            self.ch1_length_timer = 63;
                        } else {
                            self.ch1_length_timer = 64;
                        }
                        eprintln!("set ch1 len from 0 to {}", self.ch1_length_timer);
                    }
                    self.ch1_frequency_timer = (0x07FF ^ ch1_freq) * 2;
                    self.ch1_wave_duty_position = 0;
                    self.ch1_sweep_timer = if ch1_sweep_period == 0 {
                        8
                    } else {
                        ch1_sweep_period
                    };
                    self.ch1_shadow_freq = ch1_freq;
                    self.ch1_sweep_enabled = ch1_sweep_period != 0 || ch1_sweep_shift != 0;
                    self.ch1_has_done_sweep_calculation = false;
                    if ch1_sweep_shift != 0 {
                        self.calculate_frequency(ch1_sweep_shift, ch1_sweep_direction);
                    }

                    self.ch1_env_period_timer = self.nr12 & 0x07;
                    self.ch1_current_volume = (self.nr12 & 0xF0) >> 4;
                    if self.nr12 & 0xF8 == 0 {
                        self.ch1_channel_enable = false;
                    }
                }
                eprintln!("write nr14: {:02x}", value)
            }
            0x16 => {
                self.nr21 = value;
                let ch2_length_data = self.nr21 & 0x3F;
                self.ch2_length_timer = 64 - ch2_length_data;
                eprintln!("write nr21: {:02x}", value)
            }
            0x17 => {
                self.nr22 = value;
                if self.nr22 & 0xF8 == 0 {
                    self.ch2_channel_enable = false;
                }
                eprintln!("write nr22: {:02x}", value)
            }
            0x18 => {
                self.nr23 = value;
                eprintln!("write nr23: {:02x}", value)
            }
            0x19 => {
                let length_previou_enabled = self.nr24 & 0x40 != 0;
                let length_now_enabled = value & 0x40 != 0;
                // TODO: replace this step period with frame_sequencer_step
                let extra_clock = self.frame_sequencer_step % 2 == 1;

                // extra length clocking occurs when frame sequencer's next step don't clock the
                // length.
                if extra_clock {
                    // if was PREVIOUSLY disabled and now enabled and the length counter is not
                    // zero, the counter decreases
                    if !length_previou_enabled && length_now_enabled && self.ch2_length_timer != 0 {
                        self.ch2_length_timer -= 1;
                        // if became zero, and trigger is clear, disable channel
                        if self.ch2_length_timer == 0 && value & 0x80 == 0 {
                            self.ch2_channel_enable = false;
                        }
                    }
                }

                if value & 0x80 != 0 {
                    // Trigger event
                    eprintln!("trigger ch2");
                    let ch2_freq = u16::from_be_bytes([self.nr24, self.nr23]) & 0x07FF;
                    self.ch2_channel_enable = true;
                    if self.ch2_length_timer == 0 {
                        if extra_clock && length_now_enabled {
                            self.ch2_length_timer = 63;
                        } else {
                            self.ch2_length_timer = 64;
                        }
                    }
                    self.ch2_env_period_timer = self.nr22 & 0x07;
                    self.ch2_current_volume = (self.nr22 & 0xF0) >> 4;
                    self.ch2_frequency_timer = (0x07FF ^ ch2_freq) * 2;
                    self.ch2_wave_duty_position = 0;
                    if self.nr22 & 0xF8 == 0 {
                        self.ch2_channel_enable = false;
                    }
                }
                self.nr24 = value;
                eprintln!("write nr24: {:02x}", value)
            }
            0x1A => {
                self.nr30 = value;
                if self.nr30 & 0x80 == 0 {
                    self.ch3_channel_enable = false;
                }
                eprintln!("write nr30: {:02x}", value)
            }
            0x1B => {
                self.nr31 = value;
                let ch3_length_data = self.nr31;
                self.ch3_length_timer = 256 - ch3_length_data as u16;
                eprintln!("write nr31: {:02x}", value)
            }
            0x1C => {
                self.nr32 = value;
                eprintln!("write nr32: {:02x}", value)
            }
            0x1D => {
                self.nr33 = value;
                eprintln!("write nr33: {:02x}", value)
            }
            0x1E => {
                let length_previou_enabled = self.nr34 & 0x40 != 0;
                let length_now_enabled = value & 0x40 != 0;
                let extra_clock = self.frame_sequencer_step % 2 == 1;

                // extra length clocking occurs when frame sequencer's next step don't clock the
                // length.
                if extra_clock {
                    // if was PREVIOUSLY disabled and now enabled and the length counter is not
                    // zero, the counter decreases
                    if !length_previou_enabled && length_now_enabled && self.ch3_length_timer != 0 {
                        self.ch3_length_timer -= 1;
                        // if became zero, and trigger is clear, disable channel
                        if self.ch3_length_timer == 0 && value & 0x80 == 0 {
                            self.ch3_channel_enable = false;
                        }
                    }
                }
                if value & 0x80 != 0 {
                    // Trigger event
                    eprintln!("trigger ch3");

                    if self.ch3_channel_enable
                        && self.nr30 & 0x80 != 0
                        && self.ch3_frequency_timer == 0
                    {
                        let pos = ((self.ch3_wave_position as usize + 1) % 32) / 2;
                        if pos < 4 {
                            self.ch3_wave_pattern[0] = self.ch3_wave_pattern[pos];
                        } else {
                            let p = pos - (pos % 4) - 4;
                            let (a, b) = self.ch3_wave_pattern.split_at_mut(4);
                            a.clone_from_slice(&b[p..p + 4]);
                        }
                    }

                    let ch3_freq = u16::from_be_bytes([self.nr34, self.nr33]) & 0x07FF;
                    self.ch3_channel_enable = true;
                    if self.ch3_length_timer == 0 {
                        if extra_clock && length_now_enabled {
                            self.ch3_length_timer = 255;
                        } else {
                            self.ch3_length_timer = 256;
                        }
                    }
                    self.ch3_frequency_timer = (ch3_freq ^ 0x07FF) + 3;
                    self.ch3_wave_position = 0;
                    if self.nr30 & 0x80 == 0 {
                        self.ch3_channel_enable = false;
                    }
                }
                self.nr34 = value;
                eprintln!("write nr34: {:02x}", value)
            }
            0x20 => {
                self.nr41 = value;
                let ch4_length_data = self.nr41 & 0x3F;
                self.ch4_length_timer = 64 - ch4_length_data;
                eprintln!("write nr41: {:02x}", value)
            }
            0x21 => {
                self.nr42 = value;
                if self.nr42 & 0xF8 == 0 {
                    self.ch4_channel_enable = false;
                }
                eprintln!("write nr42: {:02x}", value)
            }
            0x22 => {
                self.nr43 = value;
                eprintln!("write nr43: {:02x}", value)
            }
            0x23 => {
                let length_previou_enabled = self.nr44 & 0x40 != 0;
                let length_now_enabled = value & 0x40 != 0;
                let extra_clock = self.frame_sequencer_step % 2 == 1;

                // extra length clocking occurs when frame sequencer's next step don't clock the
                // length.
                if extra_clock {
                    // if was PREVIOUSLY disabled and now enabled and the length counter is not
                    // zero, the counter decreases
                    if !length_previou_enabled && length_now_enabled && self.ch4_length_timer != 0 {
                        self.ch4_length_timer -= 1;
                        // if became zero, and trigger is clear, disable channel
                        if self.ch4_length_timer == 0 && value & 0x80 == 0 {
                            self.ch4_channel_enable = false;
                        }
                    }
                }
                if value & 0x80 != 0 {
                    // Trigger event
                    eprintln!("trigger ch4");
                    let ch4_divisor = [8, 16, 32, 48, 64, 80, 96, 112][self.nr43 as usize & 0x07];
                    let ch4_shift_amount = (self.nr43 & 0xF0) >> 4;
                    self.ch4_channel_enable = true;
                    if self.ch4_length_timer == 0 {
                        if extra_clock && length_now_enabled {
                            self.ch4_length_timer = 63;
                        } else {
                            self.ch4_length_timer = 64;
                        }
                    }
                    self.ch4_frequency_timer = ch4_divisor << ch4_shift_amount;
                    self.ch4_lfsr = 0x7FFF;
                    self.ch4_env_period_timer = self.nr42 & 0x07;
                    self.ch4_current_volume = (self.nr42 & 0xF0) >> 4;
                    if self.nr42 & 0xF8 == 0 {
                        self.ch4_channel_enable = false;
                    }
                }
                self.nr44 = value;
                eprintln!("write nr44: {:02x}", value)
            }
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
                // Bit 7 turn off the sound
                if value & 0x80 == 0 && self.on {
                    eprintln!("turn off");
                    // Most register were reset while the sound was off
                    *self = Self {
                        on: false,
                        ch3_wave_pattern: self.ch3_wave_pattern,
                        // On DMG, load counters can be written to, while off
                        nr11: self.nr11 & 0x3F,
                        ch1_length_timer: self.ch1_length_timer,
                        nr21: self.nr21 & 0x3F,
                        ch2_length_timer: self.ch2_length_timer,
                        nr31: self.nr31 & 0xFF,
                        ch3_length_timer: self.ch3_length_timer,
                        nr41: self.nr41 & 0x3F,
                        ch4_length_timer: self.ch4_length_timer,

                        output: std::mem::take(&mut self.output),
                        last_clock: self.last_clock,
                        sample_frequency: self.sample_frequency,
                        sample_mod: self.sample_mod,
                        ..Self::default()
                    };
                } else if value & 0x80 != 0 && !self.on {
                    eprintln!("turn on");
                    self.on = true;
                }
            }
            0x30..=0x3F => {
                if self.ch3_channel_enable {
                    eprintln!("wave position: {}", self.ch3_wave_position);
                    // if it had read recently, write to the currently read
                    if self.ch3_wave_just_read {
                        self.ch3_wave_pattern[self.ch3_wave_position as usize / 2] = value;
                    }
                } else {
                    self.ch3_wave_pattern[address as usize - 0x30] = value;
                }
                eprintln!("write wave {:02x}: {:02x}", address, value);
            }
            _ => unreachable!(),
        }
    }

    pub fn read(&mut self, clock_count: u64, address: u8) -> u8 {
        // if let 0x30..=0x3F = address {
        //     self.update(clock_count);
        //     let ch3_freq = u16::from_be_bytes([self.nr34, self.nr33]) & 0x07FF;
        //     println!("read wave: pos {}, tim {}/{}, just {}, enabled {}/{}",
        //              self.ch3_wave_position, self.ch3_frequency_timer, ch3_freq ^ 0x07FF,
        //              self.ch3_wave_just_read as u8,
        //              self.ch3_channel_enable as u8, self.on as u8);
        // }
        let value = if self.on {
            match address {
                0x10 => self.nr10 | 0x80,
                0x11 => self.nr11 | 0x3F,
                0x12 => self.nr12 | 0x00,
                0x13 => self.nr13 | 0xFF,
                0x14 => self.nr14 | 0xBF,
                0x16 => self.nr21 | 0x3F,
                0x17 => self.nr22 | 0x00,
                0x18 => self.nr23 | 0xFF,
                0x19 => self.nr24 | 0xBF,
                0x1A => self.nr30 | 0x7F,
                0x1B => self.nr31 | 0xFF,
                0x1C => self.nr32 | 0x9F,
                0x1D => self.nr33 | 0xFF,
                0x1E => self.nr34 | 0xBF,
                0x20 => self.nr41 | 0xFF,
                0x21 => self.nr42 | 0x00,
                0x22 => self.nr43 | 0x00,
                0x23 => self.nr44 | 0xBF,
                0x24 => self.nr50 | 0x00,
                0x25 => self.nr51 | 0x00,
                0x26 => {
                    self.update(clock_count);
                    let r = ((self.on as u8) << 7)
                        | ((self.ch4_channel_enable as u8) << 3)
                        | ((self.ch3_channel_enable as u8) << 2)
                        | ((self.ch2_channel_enable as u8) << 1)
                        | ((self.ch1_channel_enable as u8) << 0)
                        | 0x70;
                    r
                }
                0x30..=0x3F => {
                    self.update(clock_count);
                    if self.ch3_channel_enable {
                        eprintln!("wave position: {}", self.ch3_wave_position);
                        // if it had read recently, return the currently value, otherwise 0xFF
                        if self.ch3_wave_just_read {
                            self.ch3_wave_pattern[self.ch3_wave_position as usize / 2]
                        } else {
                            0xFF
                        }
                    } else {
                        self.ch3_wave_pattern[address as usize - 0x30]
                    }
                }
                _ => unreachable!(),
            }
        } else {
            match address {
                0x10 => 0x80,
                0x11 => 0x3F,
                0x12 => 0x00,
                0x13 => 0xFF,
                0x14 => 0xBF,
                0x16 => 0x3F,
                0x17 => 0x00,
                0x18 => 0xFF,
                0x19 => 0xBF,
                0x1A => 0x7F,
                0x1B => 0xFF,
                0x1C => 0x9F,
                0x1D => 0xFF,
                0x1E => 0xBF,
                0x20 => 0xFF,
                0x21 => 0x00,
                0x22 => 0x00,
                0x23 => 0xBF,
                0x24 => 0x00,
                0x25 => 0x00,
                0x26 => 0x70,
                0x30..=0x3F => self.ch3_wave_pattern[address as usize - 0x30],
                _ => unreachable!(),
            }
        };
        eprintln!("read {:02x}: {:02x}", address, value);
        value
    }
}
