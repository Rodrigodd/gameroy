//! The GameRoy emulation server.
//!
//! This crate owns everything about *running* a Game Boy and nothing about showing
//! one. It has no windowing, rendering, audio-device or filesystem dependency; it
//! receives [`protocol::Command`]s and emits [`protocol::Event`]s, and the bytes of
//! ROMs and save files are handed to it by whoever drives it.
//!
//! The split exists so the server can eventually live in another process or a web
//! worker. Two properties keep that door open, and both are load-bearing:
//!
//! 1. Every type crossing the boundary is plain owned data and is serde-serializable.
//!    No handles, no callbacks, no `Arc`.
//! 2. [`Session::step`] is a *step function*, not a loop: it does a bounded slice of
//!    work and returns a [`Schedule`] saying when it wants to be called again. It
//!    never blocks. Blocking is the driver's business, and a web worker — which
//!    cannot block — supplies a different driver without changing `Session`.
//!
//! The `protocol` module never references the emulator, so building with
//! `--no-default-features` compiles the wire types alone. That build is the guard
//! on this whole arrangement; keep it working.

pub mod protocol;

#[cfg(feature = "emulation")]
mod debug_console;
#[cfg(feature = "emulation")]
mod disasm;
#[cfg(feature = "emulation")]
mod ppu_view;
#[cfg(feature = "emulation")]
mod session;
#[cfg(feature = "emulation")]
mod timeline;

#[cfg(feature = "emulation")]
pub use session::{Schedule, Session};

#[cfg(feature = "thread-driver")]
pub mod driver;
