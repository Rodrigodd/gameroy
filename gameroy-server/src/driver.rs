//! The `std::thread` driver.
//!
//! Deliberately tiny: all it does is turn [`Schedule`] into blocking. A web-worker
//! driver would call [`Session::step`] once per `onmessage` and re-arm with
//! `setTimeout` on [`Schedule::At`], with no change to `Session` itself. Keeping the
//! blocking `recv` out here, rather than inside the session, is the whole of that
//! portability story.

use crate::protocol::transport::{ServerTransport, WaitResult};
use crate::protocol::{EmulatorConfig, Event, PROTOCOL_VERSION};
use crate::{Schedule, Session};

pub fn run(mut transport: impl ServerTransport, config: EmulatorConfig) {
    let mut session = Session::new(config);

    if transport
        .send(Event::Ready {
            protocol_version: PROTOCOL_VERSION,
        })
        .is_err()
    {
        return;
    }

    loop {
        match session.step(&mut transport) {
            Schedule::Now => {}
            Schedule::At(deadline) => {
                transport.wait(Some(deadline));
            }
            Schedule::Idle => {
                if transport.wait(None) == WaitResult::Closed {
                    break;
                }
            }
            Schedule::Exit => break,
        }
    }
}
