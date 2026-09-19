//! The wire contract between a front-end and the emulation server.
//!
//! Nothing in this module may reference `gameroy-core`, or the
//! `--no-default-features` build breaks and with it the ability to write a
//! front-end that does not link the emulator.

mod buffer;
mod command;
mod debug;
mod event;
pub mod transport;

pub use buffer::{AudioChunk, FrameBuffer, FramePacket};
pub use command::{Command, EmulatorConfig, OpenRom, RequestId, RunMode, SessionEpoch};
pub use debug::{
    Address, CpuState, DebugUpdate, DebugViews, DisasmLine, DisassemblyWindow, ImeState,
    IndexedImage, PpuState, PpuViewer, SpriteInfo, WatchValue,
};
pub use event::{CartridgeInfo, Event, RomOpened, ServerError, StopReason};

/// Bumped on any breaking change to [`Command`] or [`Event`]. The server reports it
/// in [`Event::Ready`] so a mismatched peer can be detected rather than misparsed.
pub const PROTOCOL_VERSION: u32 = 2;

/// Duplicated from `gameroy_core::consts` so this module stays emulator-free.
/// `session.rs` statically asserts that they still agree.
pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;
pub const SCREEN_PIXELS: usize = SCREEN_WIDTH * SCREEN_HEIGHT;
pub const CLOCK_SPEED: u64 = 4_194_304;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::StopReason;

    /// Every type crossing the boundary must be plain owned data. If this stops
    /// compiling, something grew a handle, a lifetime or a callback, and the
    /// out-of-process story is broken.
    #[test]
    fn wire_types_are_serializable_and_sendable() {
        fn assert_wire<T: serde::Serialize + serde::de::DeserializeOwned + Send + 'static>() {}
        assert_wire::<Command>();
        assert_wire::<Event>();
    }

    #[test]
    fn commands_round_trip() {
        let cases = vec![
            Command::Shutdown,
            Command::SetRunMode(RunMode::RunningWithBreakpoints),
            Command::SetFrameLimit(false),
            Command::SetJoypad(0b1111_0111),
            Command::Step,
            Command::StepBack,
            Command::ReturnFrame(FrameBuffer(vec![3; SCREEN_PIXELS])),
            Command::OpenRom(Box::new(OpenRom {
                rom: vec![1, 2, 3],
                save_ram: Some(vec![4, 5]),
                boot_rom: None,
                config: EmulatorConfig::default(),
                start_paused: true,
            })),
        ];
        for cmd in cases {
            let json = serde_json::to_string(&cmd).unwrap();
            let back: Command = serde_json::from_str(&json).unwrap();
            assert_eq!(
                format!("{cmd:?}"),
                format!("{back:?}"),
                "round trip changed {cmd:?}"
            );
        }
    }

    /// Every stop reason must survive the wire: a new variant that does not is a
    /// pause the client silently fails to parse.
    #[test]
    fn stop_reasons_round_trip() {
        let cases = [
            StopReason::None,
            StopReason::UserRequest,
            StopReason::StepComplete,
            StopReason::StepBackComplete,
            StopReason::Reset,
            StopReason::LoadedState,
            StopReason::Breakpoint {
                address: 0x150,
                flags: 0b0100,
            },
            StopReason::TargetAddress(0x40),
            StopReason::TargetClock(1234),
        ];
        for reason in cases {
            let json = serde_json::to_string(&reason).unwrap();
            let back: StopReason = serde_json::from_str(&json).unwrap();
            assert_eq!(reason, back);
        }
    }

    #[test]
    fn events_round_trip() {
        let ev = Event::Frame(FramePacket {
            seq: 7,
            frame: 3,
            clock_count: 1234,
            width: SCREEN_WIDTH as u16,
            height: SCREEN_HEIGHT as u16,
            pixels: FrameBuffer(vec![2; SCREEN_PIXELS]),
        });
        let json = serde_json::to_string(&ev).unwrap();
        let back: Event = serde_json::from_str(&json).unwrap();
        match back {
            Event::Frame(p) => {
                assert_eq!(p.seq, 7);
                assert_eq!(p.pixels.0.len(), SCREEN_PIXELS);
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }
}
