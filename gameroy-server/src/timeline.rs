//! Per-frame input record and the rewind ring.
//!
//! Ported from the old front-end's `src/emulator.rs`. It lives entirely on the
//! server and never crosses the protocol boundary: the 32 MiB of delta-compressed
//! save states would be absurd to ship, and nothing on the front-end needs them.

use std::io::Write;
use std::time::{SystemTime, UNIX_EPOCH};

use gameroy::{diff_stack::DiffStack, gameboy::GameBoy};

/// Milliseconds since the UNIX epoch, stamped into save states.
pub fn timestamp() -> Option<u64> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .ok()?
        .as_millis()
        .try_into()
        .ok()
}

pub struct Timeline {
    /// Scratch space for serializing one save state.
    buffer: Vec<u8>,

    /// Delta-compressed save states, most recent on top.
    save_states: DiffStack,
    /// A second stack. When `save_states` fills, the two are swapped and the full
    /// one cleared, which discards the older half of history. Inherited from the
    /// old implementation; it means a long rewind hits a wall at an unpredictable
    /// point, and a real circular buffer would be the fix.
    save_states2: DiffStack,

    /// The keys currently held by the user.
    pub current_joypad: u8,
    /// The frame being emulated.
    pub current_frame: u32,
    /// Input for each frame. Pre-populating this is how movie replay works.
    joypad_timeline: Vec<u8>,

    pub rewinding: bool,
}

impl Timeline {
    pub fn new(current_frame: u32, joypad_timeline: Vec<u8>, capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(64 * 1024),
            current_frame,
            joypad_timeline,
            save_states: DiffStack::new(capacity / 2),
            save_states2: DiffStack::new(capacity / 2),
            current_joypad: 0xff,
            rewinding: false,
        }
    }

    fn save_state(&mut self, gb: &GameBoy) {
        // With rewinding disabled the stacks have zero capacity.
        if self.save_states.capacity() == 0 {
            return;
        }

        self.buffer.clear();
        self.buffer
            .write_all(&gb.clock_count.to_le_bytes())
            .unwrap();
        self.buffer
            .write_all(&self.current_frame.to_le_bytes())
            .unwrap();
        gb.save_state(timestamp(), &mut self.buffer).unwrap();

        if !self.save_states.push(&self.buffer) {
            std::mem::swap(&mut self.save_states, &mut self.save_states2);
            log::debug!(
                "rewind buffer cleared after {} frames",
                self.save_states.count()
            );
            self.save_states.clear();
            self.save_states.push(&self.buffer);
        }
    }

    /// Restore the most recent frame without consuming it.
    // Used by rewind, which is wired up in a later milestone.
    #[allow(dead_code)]
    pub fn load_last_frame(&mut self, gb: &mut GameBoy) -> bool {
        let Some(last_frame) = self.save_states.top() else {
            return false;
        };
        // The first 12 bytes are our own clock/frame header.
        gb.load_state(&mut &last_frame[12..]).unwrap();
        true
    }

    /// Drop the most recent frame.
    #[allow(dead_code)]
    pub fn pop_last_frame(&mut self) -> bool {
        let Some(last_frame) = self.save_states.top() else {
            return false;
        };
        self.current_frame = u32::from_le_bytes(last_frame[8..12].try_into().unwrap());
        self.save_states.pop();
        if self.save_states.is_empty() {
            std::mem::swap(&mut self.save_states, &mut self.save_states2);
        }
        true
    }

    /// Snapshot the current frame and return the joypad state for the next one.
    ///
    /// Order matters: the snapshot is taken *before* the caller assigns the
    /// returned value to `gb.joypad`, so a restored state replays the same input it
    /// originally saw. Reversing this desynchronizes movie replay in a way that is
    /// very hard to spot.
    pub fn next_frame(&mut self, gb: &GameBoy) -> u8 {
        let joy = if (self.current_frame as usize) < self.joypad_timeline.len() {
            self.joypad_timeline[self.current_frame as usize]
        } else {
            // Fill any gap if we are several frames ahead of the record.
            let diff = self.current_frame as usize - self.joypad_timeline.len();
            self.joypad_timeline.extend((0..diff).map(|_| 0xff));
            self.joypad_timeline.push(self.current_joypad);
            self.current_joypad
        };
        self.save_state(gb);
        self.current_frame += 1;
        joy
    }
}
