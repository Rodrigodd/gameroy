//! Owns the emulation server thread and the transport to it.
//!
//! The front-end owns the thread, not the other way round: the server is a library
//! that gets driven, so that a different front-end — or a different driver, such as
//! a web worker — can host it without changes.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use gameroy_server::protocol::transport::{ClientTransport, TransportError, channel};
use gameroy_server::protocol::{Command, EmulatorConfig, Event};

/// Command queue depth. Commands are small and rare (input changes, run control).
const COMMAND_CAPACITY: usize = 64;
/// Event queue depth. Deeper, because the server emits frames steadily while the
/// front-end drains in bursts, once per repaint.
const EVENT_CAPACITY: usize = 256;

pub struct ServerHandle {
    transport: Arc<dyn ClientTransport>,
    thread: Option<std::thread::JoinHandle<()>>,
    /// True once a wake-up has been posted to the front-end and not yet consumed.
    pending: Arc<AtomicBool>,
}

impl ServerHandle {
    /// Spawns the server on its own thread.
    ///
    /// `on_event` is called on the *server's* thread whenever an event becomes
    /// available; it should do nothing but poke the front-end's event loop. The
    /// actual draining happens on the UI thread via [`ServerHandle::try_recv`].
    pub fn spawn(
        config: EmulatorConfig,
        on_event: impl Fn() + Send + Sync + 'static,
    ) -> std::io::Result<Self> {
        let (client, server) = channel::pair(COMMAND_CAPACITY, EVENT_CAPACITY);

        // Coalesce: a burst of N events must produce one wake-up, not N. The flag
        // is cleared by the front-end in `begin_drain`, not here — clearing it
        // straight after `on_event` would mean every event posts its own closure,
        // which is the thing this exists to prevent.
        let pending = Arc::new(AtomicBool::new(false));
        client.set_waker(Box::new({
            let pending = pending.clone();
            move || {
                if !pending.swap(true, Ordering::AcqRel) {
                    on_event();
                }
            }
        }));

        let thread = std::thread::Builder::new()
            .name("gameroy-server".into())
            // `GameBoy` is large (VRAM + WRAM + screen + trace) and the interpreter
            // recurses through memory accesses; the old front-end bumped this too.
            .stack_size(8 * 1024 * 1024)
            .spawn(move || gameroy_server::driver::run(server, config))?;

        Ok(Self {
            transport: Arc::new(client),
            thread: Some(thread),
            pending,
        })
    }

    /// Call on the front-end thread before draining.
    ///
    /// Clearing first, then draining to empty, is the order that cannot lose a
    /// wake-up: anything the server pushes mid-drain re-arms the flag and posts
    /// again, costing one redundant drain rather than a missed frame.
    pub fn begin_drain(&self) {
        self.pending.store(false, Ordering::Release);
    }

    pub fn send(&self, cmd: Command) {
        if let Err(e) = self.transport.send(cmd) {
            log::warn!("dropping command: {e}");
        }
    }

    pub fn try_recv(&self) -> Result<Option<Event>, TransportError> {
        self.transport.try_recv()
    }

    /// Asks the server to stop, drains what is left — the final `PersistRam` is in
    /// there, so draining before joining is what saves the player's game — and then
    /// waits for the thread.
    pub fn shutdown(&mut self, mut on_event: impl FnMut(Event)) {
        let Some(thread) = self.thread.take() else {
            return;
        };
        self.send(Command::Shutdown);
        while let Ok(Some(ev)) = self.transport.try_recv() {
            on_event(ev);
        }
        // `Shutdown` handling is bounded (it only serializes cartridge RAM), so an
        // unconditional join cannot hang here.
        if thread.join().is_err() {
            log::error!("emulation server thread panicked");
        }
        // Anything the server emitted between the last drain and exiting.
        while let Ok(Some(ev)) = self.transport.try_recv() {
            on_event(ev);
        }
    }
}
