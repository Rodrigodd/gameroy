//! Audio output.
//!
//! The server synthesizes samples; this owns the device that plays them. The two
//! are joined by a ring buffer shared with the device's callback thread — the only
//! shared mutable state left in the program, and unavoidable, since the callback
//! runs on the driver's own thread.
//!
//! The interesting problem here is drift. The old front-end pumped emulation from
//! the render loop, which implicitly slaved the Game Boy to vsync and hid the fact
//! that three clocks disagree: the emulator's 59.7 Hz, the display's refresh, and
//! the audio device's crystal. Now the server paces itself, so the correction has
//! to be explicit — hence the water marks below.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

/// Drop samples once the queue is longer than this. Latency the player can feel is
/// worse than a discontinuity they cannot hear.
const HIGH_WATER_MS: usize = 150;
/// Pad with silence below this, to stay ahead of an underrun.
const LOW_WATER_MS: usize = 25;

const CHANNELS: usize = 2;

/// Lets the device be swapped — for cpal directly, or WebAudio — in one file.
pub trait AudioSink {
    fn sample_rate(&self) -> u32;
    fn push(&mut self, samples: &[i16]);
}

fn samples_for_ms(rate: u32, ms: usize) -> usize {
    (rate as usize) * CHANNELS * ms / 1000
}

/// The shared queue, written here and drained by the device callback.
#[derive(Clone)]
struct Shared(Arc<Mutex<VecDeque<i16>>>);

pub struct AudioEngineSink {
    // Kept alive: dropping the engine stops playback.
    _engine: audio_engine::AudioEngine,
    shared: Shared,
    sample_rate: u32,
    high_water: usize,
    low_water: usize,
    /// Counts drops and pads, so drift shows up in the log rather than only by ear.
    dropped: usize,
    padded: usize,
}

impl AudioEngineSink {
    /// Returns `None` if no audio device could be opened, which is not fatal: the
    /// emulator runs fine without sound, and headless machines are common.
    pub fn new() -> Option<Self> {
        let engine = match audio_engine::AudioEngine::new() {
            Ok(e) => e,
            Err(e) => {
                log::warn!("no audio device ({e}); continuing without sound");
                return None;
            }
        };
        let sample_rate = engine.sample_rate();
        let shared = Shared(Arc::new(Mutex::new(VecDeque::new())));

        let source = RingSource {
            shared: shared.clone(),
            sample_rate,
        };
        let mut sound = match engine.new_sound(source) {
            Ok(s) => s,
            Err(e) => {
                log::warn!("could not start audio playback ({e}); continuing without sound");
                return None;
            }
        };
        sound.set_loop(true);
        sound.play();
        // The handle stops playback when dropped, and we want it to run for the
        // lifetime of the process.
        std::mem::forget(sound);

        Some(Self {
            _engine: engine,
            shared,
            sample_rate,
            high_water: samples_for_ms(sample_rate, HIGH_WATER_MS),
            low_water: samples_for_ms(sample_rate, LOW_WATER_MS),
            dropped: 0,
            padded: 0,
        })
    }
}

impl AudioSink for AudioEngineSink {
    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn push(&mut self, samples: &[i16]) {
        let mut queue = self.shared.0.lock().unwrap();

        if queue.len() < self.low_water {
            // Running dry. Pad with silence rather than let the device underrun,
            // which is audible as a click.
            let pad = self.low_water - queue.len();
            queue.extend(std::iter::repeat_n(0, pad));
            self.padded += pad;
        }

        queue.extend(samples.iter().copied());

        if queue.len() > self.high_water {
            // Too far ahead: the emulator is outrunning the device. Drop from the
            // front, which discards the stalest audio.
            let excess = queue.len() - self.high_water;
            queue.drain(0..excess);
            self.dropped += excess;
        }

        if (self.dropped + self.padded) > 0 && (self.dropped + self.padded) % 100_000 < 4096 {
            log::debug!(
                "audio drift: {} samples dropped, {} padded, queue {}",
                self.dropped,
                self.padded,
                queue.len()
            );
        }
    }
}

struct RingSource {
    shared: Shared,
    sample_rate: u32,
}

impl audio_engine::SoundSource for RingSource {
    fn channels(&self) -> u16 {
        CHANNELS as u16
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn reset(&mut self) {
        self.shared.0.lock().unwrap().clear();
    }

    fn write_samples(&mut self, out: &mut [i16]) -> usize {
        let mut queue = self.shared.0.lock().unwrap();
        let n = queue.len().min(out.len());
        for (dst, src) in out[..n].iter_mut().zip(queue.iter()) {
            *dst = *src;
        }
        queue.drain(0..n);
        // Anything we could not fill stays as the caller left it. Report the whole
        // buffer as written regardless: returning a short count makes audio-engine
        // treat the source as finished and reset it.
        out.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn water_marks_scale_with_rate() {
        assert_eq!(samples_for_ms(48_000, 1000), 96_000);
        assert!(samples_for_ms(48_000, HIGH_WATER_MS) > samples_for_ms(48_000, LOW_WATER_MS));
    }
}
