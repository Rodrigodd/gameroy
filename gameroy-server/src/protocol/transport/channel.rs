//! In-process transport over `flume` channels.
//!
//! Values are moved, never serialized.

use std::sync::{Arc, Mutex};
use std::time::Instant;

use super::{ClientTransport, ServerTransport, TransportError, WaitResult};
use crate::protocol::{Command, Event};

type Waker = Box<dyn Fn() + Send + Sync>;

/// Creates a connected client/server pair.
///
/// Both queues are bounded. The event queue is the larger of the two because the
/// server produces frames steadily while the front-end drains in bursts, once per
/// repaint.
pub fn pair(command_capacity: usize, event_capacity: usize) -> (ChannelClient, ChannelServer) {
    let (cmd_tx, cmd_rx) = flume::bounded(command_capacity);
    let (ev_tx, ev_rx) = flume::bounded(event_capacity);
    let waker: Arc<Mutex<Option<Waker>>> = Arc::new(Mutex::new(None));
    (
        ChannelClient {
            tx: cmd_tx,
            rx: ev_rx,
            waker: waker.clone(),
        },
        ChannelServer {
            tx: ev_tx,
            rx: cmd_rx,
            waker,
            stash: None,
        },
    )
}

pub struct ChannelClient {
    tx: flume::Sender<Command>,
    rx: flume::Receiver<Event>,
    waker: Arc<Mutex<Option<Waker>>>,
}

impl ClientTransport for ChannelClient {
    fn send(&self, cmd: Command) -> Result<(), TransportError> {
        self.tx.send(cmd).map_err(|_| TransportError::Closed)
    }

    fn try_recv(&self) -> Result<Option<Event>, TransportError> {
        match self.rx.try_recv() {
            Ok(ev) => Ok(Some(ev)),
            Err(flume::TryRecvError::Empty) => Ok(None),
            Err(flume::TryRecvError::Disconnected) => Err(TransportError::Closed),
        }
    }

    fn set_waker(&self, waker: Waker) {
        *self.waker.lock().unwrap() = Some(waker);
    }
}

pub struct ChannelServer {
    tx: flume::Sender<Event>,
    rx: flume::Receiver<Command>,
    waker: Arc<Mutex<Option<Waker>>>,
    /// `flume` has no peek, so `wait` takes a command off the channel and parks it
    /// here for the next `try_recv`.
    stash: Option<Command>,
}

impl ChannelServer {
    fn wake(&self) {
        // Held only long enough to call the waker. The waker itself must not send
        // on this transport, or it would deadlock; the front-end's waker only
        // posts to its event loop.
        if let Some(w) = self.waker.lock().unwrap().as_ref() {
            w();
        }
    }
}

impl ServerTransport for ChannelServer {
    fn send(&mut self, ev: Event) -> Result<(), TransportError> {
        self.tx.send(ev).map_err(|_| TransportError::Closed)?;
        self.wake();
        Ok(())
    }

    fn send_lossy(&mut self, ev: Event) -> Result<Option<Event>, TransportError> {
        match self.tx.try_send(ev) {
            Ok(()) => {
                self.wake();
                Ok(None)
            }
            Err(flume::TrySendError::Full(ev)) => Ok(Some(ev)),
            Err(flume::TrySendError::Disconnected(_)) => Err(TransportError::Closed),
        }
    }

    fn try_recv(&mut self) -> Result<Option<Command>, TransportError> {
        if let Some(cmd) = self.stash.take() {
            return Ok(Some(cmd));
        }
        match self.rx.try_recv() {
            Ok(cmd) => Ok(Some(cmd)),
            Err(flume::TryRecvError::Empty) => Ok(None),
            Err(flume::TryRecvError::Disconnected) => Err(TransportError::Closed),
        }
    }

    fn wait(&mut self, deadline: Option<Instant>) -> WaitResult {
        if self.stash.is_some() {
            return WaitResult::Ready;
        }
        let r = match deadline {
            Some(d) => self.rx.recv_deadline(d),
            None => self
                .rx
                .recv()
                .map_err(|_| flume::RecvTimeoutError::Disconnected),
        };
        match r {
            // `recv` consumes the command, so park it for the `try_recv` that the
            // caller makes next; otherwise waiting would swallow a command.
            Ok(cmd) => {
                self.stash = Some(cmd);
                WaitResult::Ready
            }
            Err(flume::RecvTimeoutError::Timeout) => WaitResult::TimedOut,
            Err(flume::RecvTimeoutError::Disconnected) => WaitResult::Closed,
        }
    }
}
