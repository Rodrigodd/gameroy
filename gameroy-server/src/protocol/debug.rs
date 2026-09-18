use serde::{Deserialize, Serialize};

use super::SessionEpoch;

/// Which debug panels the front-end currently has open.
///
/// The server assembles exactly these on each pause and pushes one
/// [`DebugUpdate`]. Declaring the subscription once, rather than answering a query
/// per panel, avoids a round trip on every single-step and means a closed panel
/// costs nothing — an improvement on the old front-end, which rendered the PPU
/// viewer's textures every frame whether or not the tab was visible.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DebugViews {
    pub cpu: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DebugUpdate {
    pub epoch: SessionEpoch,
    pub clock_count: u64,
    pub frame: u32,
    pub cpu: Option<CpuState>,
}

/// A mirror of the core's `Cpu`, because `gameroy-core` has no serde and this
/// module must not depend on it.
///
/// Formatting stays on the front-end: the server sends numbers, not
/// thousands-separated strings or `Z N H C` flag text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct CpuState {
    pub a: u8,
    pub f: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
    pub ime: ImeState,
    /// `cartridge.curr_bank()`: (lower, upper). Needed to resolve a PC to a
    /// bank-qualified address.
    pub banks: (u16, u16),
    pub interrupt_flag: u8,
    pub interrupt_enabled: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ImeState {
    Disabled,
    Enabled,
    ToBeEnabled,
}
