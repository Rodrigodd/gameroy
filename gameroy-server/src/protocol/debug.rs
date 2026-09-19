use serde::{Deserialize, Serialize};

use super::{RequestId, SessionEpoch};

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
    pub ppu: bool,
    pub watches: bool,
    /// Generate the PPU viewer's images and sprite tables. Expensive -- four
    /// buffers redrawn per pause -- so it is opt-in, and costs nothing when the
    /// tab is closed. The old front-end redrew its textures every frame whether
    /// or not the tab was visible.
    pub ppu_viewer: bool,
    /// Ship the partially-drawn screen. Cheap next to `ppu_viewer`, and it is
    /// what anyone single-stepping actually wants to see, so it gets its own
    /// flag rather than riding along with the tilemaps.
    pub screen: bool,
    /// Number of disassembly lines to push around the PC on each pause. 0 is off.
    ///
    /// A window rather than the whole trace: `Trace::directives` grows into the
    /// tens of thousands, and the old front-end cloned the entire set on every
    /// pause. That was merely wasteful in-process; across a serialized boundary it
    /// would be fatal.
    pub disassembly_lines: u16,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DebugUpdate {
    pub epoch: SessionEpoch,
    /// Set when this snapshot answers a [`super::Command::RequestDebugUpdate`].
    /// `None` on the ones the server pushes unasked: on pause, and on a
    /// subscription change.
    ///
    /// A front-end pacing itself on one answer per request tells the two apart
    /// by this, so an unsolicited push cannot be mistaken for its reply.
    pub request_id: Option<RequestId>,
    pub clock_count: u64,
    pub frame: u32,
    pub cpu: Option<CpuState>,
    pub ppu: Option<PpuState>,
    /// Current value of each watched address.
    pub watches: Option<Vec<WatchValue>>,
    pub disassembly: Option<DisassemblyWindow>,
    pub ppu_viewer: Option<PpuViewer>,
    pub screen: Option<PartialScreen>,
}

/// The screen as it stands mid-frame.
///
/// While paused part-way through a frame the client's most recent
/// [`super::Event::Frame`] is the *previous complete* frame, so it holds the
/// stale tail but not the fresh head -- it cannot assemble this itself.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PartialScreen {
    /// The PPU's own buffer. Its tail is whatever the previous frame left there,
    /// which is what `drawn_pixels` is for.
    pub pixels: IndexedImage,
    /// How many pixels of `pixels`, in raster order, the frame in progress has
    /// drawn. The rest is stale; the old viewer tinted it blue.
    ///
    /// A count rather than a second image, so which two palettes to use stays a
    /// front-end decision, as for every other image here.
    pub drawn_pixels: u32,
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

/// Exactly the PPU fields the old debugger panel displayed, plus `screen_x`,
/// which the partially-drawn-screen visualization needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct PpuState {
    pub lcdc: u8,
    pub stat: u8,
    pub scy: u8,
    pub scx: u8,
    pub ly: u8,
    pub lyc: u8,
    /// Dot position within the current line, computed server-side because it
    /// needs the line's start clock.
    pub lx: u16,
    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,
    pub wyc: u8,
    pub wy: u8,
    pub wx: u8,
    /// The PPU state machine's current state.
    pub state: u8,
    pub screen_x: u8,
    pub next_clock_count: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct WatchValue {
    pub address: u16,
    pub value: u8,
}

/// A bank-qualified ROM address, mirroring `gameroy::disassembler::Address`.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Address {
    pub bank: u16,
    pub address: u16,
}

/// Where a requested disassembly window sits within the trace.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DisasmAnchor {
    /// The window *starts* here. What a scrolling list asks for.
    Index(u32),
    /// The window is centred on this address, or on the first row after it. What
    /// a clicked jump target asks for, and what a client re-anchors on after the
    /// trace grew and shifted the indices under it.
    Address(Address),
    /// Centred on the PC.
    Pc,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DisassemblyWindow {
    /// Index of `lines[0]` within the whole trace.
    pub start_index: u32,
    pub total: u32,
    /// Bumped whenever the trace grows, so the client knows cached rows are stale.
    pub generation: u32,
    pub lines: Vec<DisasmLine>,
    /// Index of the current PC within the whole trace, when it is traced at all.
    pub pc_index: Option<u32>,
    /// The row the anchor resolved to, which is the one to scroll into view.
    pub focus_index: Option<u32>,
}

/// One rendered instruction, split into the parts a front-end colours separately.
///
/// Rendering happens server-side because it needs the trace's label and jump
/// maps; shipping raw opcodes instead would make every alternative front-end
/// reimplement a Game Boy disassembler. It arrives pre-split rather than as one
/// string with offsets because the operand has to be a separate element to be
/// clickable at all -- text has no per-run hit testing.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DisasmLine {
    pub address: Address,
    /// The CPU-visible address, which is what the user reads and what
    /// `break`/`runto` accept.
    pub pc: u16,
    /// Label attached to this address, if the trace named one.
    pub label: Option<String>,
    /// The opcode's name, e.g. `jp`.
    pub mnemonic: String,
    /// Operand text before the jump target, e.g. `nz, `. Holds the whole operand
    /// when there is no jump.
    pub operand_prefix: String,
    /// The clickable run: the destination's label, or `$xxxx` when the trace
    /// named none. Empty when this instruction does not jump.
    pub link_text: String,
    pub operand_suffix: String,
    /// Where the link goes. The stable anchor -- it survives the trace growing.
    pub jump_target: Option<Address>,
    /// The destination's row index, valid only for this window's `generation`.
    /// Lets the client scroll without a round trip while it still matches.
    pub jump_index: Option<u32>,
    pub is_current: bool,
}

/// The PPU viewer's contents.
///
/// Images are one byte per pixel holding a palette index, not RGBA: a quarter of
/// the bytes, and the palette stays a front-end decision, so a green-screen theme
/// costs nothing.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PpuViewer {
    /// All 384 tiles, 16 across: 128x192.
    pub tiles: IndexedImage,
    /// The background tile map, 256x256.
    pub background: IndexedImage,
    /// The window tile map, 256x256.
    pub window: IndexedImage,
    /// The 40 OAM entries, in order.
    pub oam: Vec<SpriteInfo>,
    /// The sprites selected for the current scanline.
    pub sprite_buffer: Vec<SpriteInfo>,
    pub sprite_buffer_len: u8,
    /// LCDC bit 2: sprites are 8x16 rather than 8x8. Decides how tall a slice of
    /// `tiles` one sprite's preview is.
    pub tall_sprites: bool,
    /// Index into `tiles` for each of the 32x32 background map cells, with the
    /// 0x8800 signed addressing already applied. Resolved here so a front-end can
    /// index the atlas without a copy of VRAM and the LCDC rules.
    ///
    /// `u16` because the resolved index runs to 0x17F, past what a byte holds.
    pub background_tiles: Vec<u16>,
    /// The same for the window map.
    pub window_tiles: Vec<u16>,
    /// The two pixel FIFOs, as the old viewer's hex dump showed them.
    pub background_fifo: Vec<u8>,
    pub sprite_fifo: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IndexedImage {
    pub width: u16,
    pub height: u16,
    #[serde(with = "serde_bytes")]
    pub data: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpriteInfo {
    pub sx: u8,
    pub sy: u8,
    pub tile: u8,
    pub flags: u8,
}

impl SpriteInfo {
    /// Off-screen sprites are parked outside the visible area; the old viewer
    /// tinted them red.
    pub fn is_offscreen(&self) -> bool {
        self.sy < 16 || self.sy >= 160 || self.sx == 0 || self.sx >= 168
    }
}
