use serde::{Deserialize, Serialize};

use super::FrameBuffer;
use super::debug::{DebugViews, DisasmAnchor};

pub type RequestId = u64;

/// Incremented by the server on every reset, state load or ROM open. Debug data
/// carries it so a front-end can discard a snapshot that describes a machine that
/// no longer exists.
pub type SessionEpoch = u64;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[non_exhaustive]
pub enum Command {
    // --- lifecycle ---
    OpenRom(Box<OpenRom>),
    Shutdown,

    // --- run control ---
    SetRunMode(RunMode),
    /// `false` disables real-time pacing, i.e. fast-forward.
    SetFrameLimit(bool),
    Reset,

    // --- input ---
    /// Joypad bitmask, where a 0 bit means pressed. Bits 0-3 are the direction keys
    /// (right, left, up, down), bits 4-7 the buttons (A, B, select, start).
    SetJoypad(u8),

    /// Hold-to-rewind: replays the delta-compressed save-state ring backwards.
    SetRewinding(bool),

    // --- persistence: bytes only; the client owns the filesystem ---
    /// Asks for a save state. Answered with [`super::Event::SaveStateData`].
    RequestSaveState,
    /// Restores a save state produced earlier by this emulator.
    LoadSaveState(#[serde(with = "serde_bytes")] Vec<u8>),
    /// Asks for cartridge RAM now, rather than waiting for the periodic write.
    RequestPersistRam,

    // --- debugger ---
    Step,
    /// Undoes one instruction. Needs the rewind ring, so it fails when
    /// `rewind_capacity_bytes` is 0.
    ///
    /// A command of its own rather than only a console action: it is bound to a
    /// key, and a key press should not have to synthesize the line `"stepback"`
    /// and then filter the console echo it causes.
    StepBack,
    /// Run until a breakpoint, as the debugger's "continue".
    RunToBreakpoint,
    AddBreakpoint {
        address: u16,
        /// Any of `break_flags`: WRITE | READ | EXECUTE | JUMP.
        flags: u8,
    },
    RemoveBreakpoint {
        address: u16,
    },
    AddWatch {
        address: u16,
    },
    RemoveWatch {
        address: u16,
    },
    /// One line typed into the debugger console.
    ExecuteDebugCommand {
        line: String,
    },
    SetDebugViews(DebugViews),
    /// Asks for an arbitrary window of the disassembly, answered with
    /// [`super::Event::Disassembly`].
    ///
    /// The PC-centred window in [`DebugViews::disassembly_lines`] is a
    /// subscription pushed on every pause; this is the cursor a scrollbar and a
    /// clicked jump target drive.
    RequestDisassembly {
        id: RequestId,
        anchor: DisasmAnchor,
        /// Rows wanted. Clamped server-side, so a client cannot ask for a message
        /// the size of the whole trace.
        count: u16,
    },

    // --- flow control ---
    /// Hands a frame buffer back for reuse. A no-op over a serialized transport.
    ReturnFrame(FrameBuffer),
}

/// What the emulator should be doing.
///
/// This replaces the old `Pause`/`Resume`/`Run`/`RunFrame`/`Debug(bool)` events,
/// which encoded the same state across five messages and an implicit state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RunMode {
    /// Not running. The debugger surface is live and coherent.
    Paused,
    /// Running in real time, breakpoints ignored. The normal playing mode.
    Running,
    /// Running, checking breakpoints — the debugger's "continue".
    RunningWithBreakpoints,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenRom {
    #[serde(with = "serde_bytes")]
    pub rom: Vec<u8>,
    /// Battery-backed cartridge RAM, if a save file exists. The *client* reads it;
    /// the server has no filesystem.
    pub save_ram: Option<Vec<u8>>,
    /// A 256-byte boot ROM. `Vec` rather than an array to avoid `serde_arrays`.
    pub boot_rom: Option<Vec<u8>>,
    pub config: EmulatorConfig,
    pub start_paused: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmulatorConfig {
    /// The client's audio device rate. 0 disables audio synthesis entirely, which
    /// is what a client with no working audio backend should send.
    pub audio_sample_rate: u32,
    /// Rewind ring capacity. 0 disables rewinding entirely.
    pub rewind_capacity_bytes: usize,
    pub interrupt_prediction: bool,
    /// Use the x86_64 recompiler when this build has it. Ignored elsewhere.
    pub jit: bool,
    /// Start with real-time pacing on.
    pub frame_limit: bool,
}

impl Default for EmulatorConfig {
    fn default() -> Self {
        Self {
            // Matches the old front-end's `32 * mib`.
            audio_sample_rate: 0,
            rewind_capacity_bytes: 32 * 1024 * 1024,
            interrupt_prediction: true,
            jit: true,
            frame_limit: true,
        }
    }
}
