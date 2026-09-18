use serde::{Deserialize, Serialize};

use super::{AudioChunk, DebugUpdate, FramePacket, RunMode, SessionEpoch};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[non_exhaustive]
pub enum Event {
    /// First message the server sends. The front-end waits for this before issuing
    /// `OpenRom`, which gives CLI launch, drag-and-drop and the ROM picker one code
    /// path instead of three.
    Ready {
        protocol_version: u32,
    },
    RomOpened(Box<RomOpened>),
    Frame(FramePacket),
    /// Synthesized audio. Not emitted while fast-forwarding, since the samples
    /// cannot be played at real-time rate anyway.
    Audio(AudioChunk),
    /// Emitted on every run-mode transition. Replaces the old content-free
    /// `EmulatorPaused` / `EmulatorStarted` pings.
    RunState {
        epoch: SessionEpoch,
        mode: RunMode,
        reason: StopReason,
        clock_count: u64,
        frame: u32,
    },
    DebugUpdate(Box<DebugUpdate>),
    /// A save state, in the core's own binary format. Opaque to the front-end,
    /// which only has to store and return the bytes.
    SaveStateData {
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    },
    /// The server wants cartridge RAM persisted. It cannot write files itself.
    PersistRam {
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    },
    Fatal(ServerError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StopReason {
    None,
    UserRequest,
    StepComplete,
    Reset,
    LoadedState,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ServerError {
    LoadRom(String),
    LoadState(String),
    /// No ROM is open, but the command needed one.
    NoRom,
    Internal(String),
}

impl std::fmt::Display for ServerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadRom(m) => write!(f, "failed to load ROM: {m}"),
            Self::LoadState(m) => write!(f, "failed to load save state: {m}"),
            Self::NoRom => write!(f, "no ROM is open"),
            Self::Internal(m) => write!(f, "internal server error: {m}"),
        }
    }
}

impl std::error::Error for ServerError {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RomOpened {
    pub epoch: SessionEpoch,
    pub header: CartridgeInfo,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct CartridgeInfo {
    pub title: String,
    pub kind_name: String,
    pub rom_size: u32,
    pub ram_size: u32,
}
