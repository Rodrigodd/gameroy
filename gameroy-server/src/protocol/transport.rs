//! Moving [`Command`]s and [`Event`]s between a front-end and a server.
//!
//! These traits are deliberately about *moving values*, not about the protocol.
//! The in-process implementation performs no serialization at all, so an 8 MiB ROM
//! or a 23 KiB frame costs a pointer copy; a future pipe or `postMessage`
//! implementation serializes the very same types without the server noticing.
//!
//! There is no async here on purpose. The server is a CPU-bound state machine with
//! a real-time deadline: an `.await` point inside the interpreter loop would mean
//! nothing, and an async runtime would buy complexity and a hostile wasm story. The
//! front-end is already an event loop, and what it actually wants is a callback —
//! which [`ClientTransport::set_waker`] provides.

use std::time::Instant;

use super::{Command, Event};

#[cfg(feature = "transport-channel")]
pub mod channel;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransportError {
    /// The peer is gone. Terminal.
    Closed,
    /// The queue is full and the message was not sent.
    Full,
}

impl std::fmt::Display for TransportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Closed => write!(f, "transport closed"),
            Self::Full => write!(f, "transport queue full"),
        }
    }
}

impl std::error::Error for TransportError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WaitResult {
    /// Something is ready to receive.
    Ready,
    TimedOut,
    /// This platform cannot block. A web-worker driver returns this always; the
    /// caller must re-arm with a timer instead of spinning.
    CannotBlock,
    Closed,
}

pub trait ClientTransport: Send + Sync + 'static {
    fn send(&self, cmd: Command) -> Result<(), TransportError>;

    /// Never blocks. `Ok(None)` means "nothing right now", which is not an error
    /// and is not the same as [`TransportError::Closed`].
    fn try_recv(&self) -> Result<Option<Event>, TransportError>;

    /// Called on an arbitrary thread when an event becomes available. This is what
    /// drives `slint::invoke_from_event_loop` on the front-end side.
    fn set_waker(&self, waker: Box<dyn Fn() + Send + Sync>);
}

pub trait ServerTransport: 'static {
    /// Reliable send, for messages whose loss would desynchronize the front-end:
    /// run-state transitions, debug snapshots, save data, fatal errors.
    fn send(&mut self, ev: Event) -> Result<(), TransportError>;

    /// Send, but drop the message if the queue is full, returning it so the server
    /// can recycle its buffer.
    ///
    /// Used for frames and audio, where stale is worse than absent: a slow or
    /// stalled front-end must never stall emulation.
    fn send_lossy(&mut self, ev: Event) -> Result<Option<Event>, TransportError>;

    fn try_recv(&mut self) -> Result<Option<Command>, TransportError>;

    /// Block until a command arrives or `deadline` passes.
    fn wait(&mut self, deadline: Option<Instant>) -> WaitResult;
}
