//! Pacing for the debug snapshots pulled while the machine is running.
//!
//! The debugger used to refresh only when the emulator stopped, because that is
//! when the server pushes a [`DebugUpdate`]. Refreshing while it runs needs an
//! answer to *when*, and the rate cannot come from the emulation: under
//! fast-forward the machine runs hundreds of times faster than real time, and
//! the debugger only has to keep up with the eye.
//!
//! So the front-end pulls. It holds exactly one request in flight and asks for
//! the next one when the answer has been drawn, which paces the updates to the
//! round trip. Driven from a frame callback, that round trip is the display's
//! refresh; and if drawing an update ever becomes slow, the render rate drops
//! and the request rate drops with it. There is no rate constant to tune.
//!
//! [`DebugUpdate`]: gameroy_server::protocol::DebugUpdate

use std::time::{Duration, Instant};

use gameroy_server::protocol::{RequestId, RunMode};

/// How long to wait for an answer before assuming it was lost.
///
/// The server answers every request, so this should never fire. It is here
/// because the alternative to a watchdog is a debugger that stops updating
/// until the user happens to pause.
const LOST_AFTER: Duration = Duration::from_millis(500);

/// Floor on the interval between requests.
///
/// The pump is driven by whatever the renderer does, and a renderer that does
/// not wait for vblank would otherwise drive the emulator's snapshot path
/// thousands of times a second.
const MIN_INTERVAL: Duration = Duration::from_millis(4);

/// How often to pull a snapshot when the renderer cannot say when it drew one.
///
/// A constant is exactly what the rendering notifier exists to avoid; this is
/// the consolation prize for the renderer that has none.
pub const FALLBACK_INTERVAL: Duration = Duration::from_millis(16);

/// Which side owns `EmulatorState.frame`.
///
/// Two writers at two unrelated rates is a flicker, so exactly one of them is
/// in charge at a time. While the debugger is open that is the snapshot, which
/// is the only source coherent with the register panels beside it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScreenSource {
    /// `Event::Frame`, at the emulation's frame rate. The normal playing path.
    Frames,
    /// `DebugUpdate::screen`, which is the same instant as everything else in
    /// the snapshot -- including the part of the frame not drawn yet.
    DebugSnapshot,
}

pub fn screen_source(debugging: bool) -> ScreenSource {
    if debugging {
        ScreenSource::DebugSnapshot
    } else {
        ScreenSource::Frames
    }
}

/// Whether the debugger should be pulling snapshots at all.
///
/// Not while paused: the server already pushes one on every stop, and pulling
/// more would only re-send a state that cannot change.
///
/// `has_session` is "there is a machine and someone to ask" -- a ROM and a live
/// server. Without the second half a front-end whose server has gone would keep
/// asking questions nobody answers.
pub fn wants_live(debugging: bool, mode: RunMode, has_session: bool) -> bool {
    debugging && has_session && mode != RunMode::Paused
}

/// Keeps exactly one snapshot request in flight.
#[derive(Debug, Default)]
pub struct LiveUpdates {
    /// The outstanding request and when it went out.
    inflight: Option<(RequestId, Instant)>,
    next_id: RequestId,
    last_sent: Option<Instant>,
}

impl LiveUpdates {
    /// The id to request now, or `None` when nothing is due.
    ///
    /// Safe to call as often as frames are drawn; it is the throttle, not the
    /// caller.
    pub fn poll(&mut self, now: Instant, active: bool) -> Option<RequestId> {
        if !active {
            self.reset();
            return None;
        }
        if let Some((_, sent)) = self.inflight {
            if now.duration_since(sent) < LOST_AFTER {
                return None;
            }
            // Reissue under a *new* id rather than resending the old one, so a
            // straggler arriving afterwards is discarded instead of being taken
            // for the answer to the new request.
            self.inflight = None;
        }
        if self
            .last_sent
            .is_some_and(|at| now.duration_since(at) < MIN_INTERVAL)
        {
            return None;
        }
        self.next_id = self.next_id.wrapping_add(1);
        self.inflight = Some((self.next_id, now));
        self.last_sent = Some(now);
        Some(self.next_id)
    }

    /// Folds in an arriving update, returning true when it answers the
    /// outstanding request.
    ///
    /// False for the snapshots the server pushes unasked -- on pause, and on a
    /// subscription change -- which is what keeps one of those from releasing a
    /// request that is still in flight.
    pub fn answered(&mut self, request_id: Option<RequestId>) -> bool {
        let Some(id) = request_id else { return false };
        if self.inflight.is_some_and(|(want, _)| want == id) {
            self.inflight = None;
            return true;
        }
        false
    }

    /// Forgets the outstanding request.
    ///
    /// Called whenever live updating stops, so resuming does not first have to
    /// wait out the watchdog on an answer that is no longer wanted.
    pub fn reset(&mut self) {
        self.inflight = None;
        self.last_sent = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Far enough apart that `MIN_INTERVAL` never decides anything.
    fn later(now: Instant, ms: u64) -> Instant {
        now + Duration::from_millis(ms)
    }

    #[test]
    fn pulls_only_while_running_with_the_debugger_open_and_a_server() {
        assert!(wants_live(true, RunMode::Running, true));
        assert!(wants_live(true, RunMode::RunningWithBreakpoints, true));
        assert!(!wants_live(true, RunMode::Paused, true));
        assert!(!wants_live(false, RunMode::Running, true));
        assert!(!wants_live(true, RunMode::Running, false));
    }

    #[test]
    fn holds_one_request_in_flight() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        let id = live.poll(t, true).expect("the first request goes out");
        assert_eq!(live.poll(later(t, 10), true), None);
        assert_eq!(live.poll(later(t, 20), true), None);

        assert!(live.answered(Some(id)));
        let next = live
            .poll(later(t, 30), true)
            .expect("the answer releases it");
        assert_ne!(next, id, "ids must not repeat");
    }

    /// An id we are not waiting for is somebody else's answer.
    #[test]
    fn a_foreign_answer_does_not_release_the_request() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        let id = live.poll(t, true).unwrap();
        assert!(!live.answered(Some(id.wrapping_add(7))));
        assert_eq!(live.poll(later(t, 10), true), None);
    }

    /// The server pushes a snapshot on every pause; taking one of those for the
    /// answer would let a second request go out while the first is still in
    /// flight.
    #[test]
    fn an_unsolicited_update_does_not_release_the_request() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        live.poll(t, true).unwrap();
        assert!(!live.answered(None));
        assert_eq!(live.poll(later(t, 10), true), None);
    }

    #[test]
    fn a_lost_answer_is_reissued_under_a_new_id() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        let id = live.poll(t, true).unwrap();
        assert_eq!(live.poll(t + LOST_AFTER / 2, true), None);

        let reissued = live
            .poll(t + LOST_AFTER + Duration::from_millis(1), true)
            .expect("the watchdog fires");
        assert_ne!(reissued, id);
        // The straggler must not be mistaken for the answer to the new request.
        assert!(!live.answered(Some(id)));
    }

    #[test]
    fn requests_respect_the_minimum_interval() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        let id = live.poll(t, true).unwrap();
        assert!(live.answered(Some(id)));
        assert_eq!(
            live.poll(t + MIN_INTERVAL / 2, true),
            None,
            "a renderer that does not wait for vblank must not run away with it"
        );
        assert!(live.poll(t + MIN_INTERVAL, true).is_some());
    }

    /// Otherwise resuming would sit out the watchdog before asking again.
    #[test]
    fn leaving_live_clears_the_outstanding_request() {
        let mut live = LiveUpdates::default();
        let t = Instant::now();
        live.poll(t, true).unwrap();
        assert_eq!(live.poll(later(t, 10), false), None);
        assert!(live.poll(later(t, 20), true).is_some());
    }

    #[test]
    fn the_debugger_owns_the_screen_while_it_is_open() {
        assert_eq!(screen_source(true), ScreenSource::DebugSnapshot);
        assert_eq!(screen_source(false), ScreenSource::Frames);
    }
}
