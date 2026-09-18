//! End-to-end tests driving a real session over the real transport.
//!
//! These run headless, which matters: the front-end needs a display, so this is
//! where the emulation path actually gets exercised in CI.

use std::time::{Duration, Instant};

use gameroy_server::protocol::transport::{ClientTransport, channel};
use gameroy_server::protocol::{
    Command, DebugViews, EmulatorConfig, Event, OpenRom, RunMode, SCREEN_PIXELS,
};

/// The test ROM is gitignored, so skip rather than fail when it is absent.
fn test_rom() -> Option<Vec<u8>> {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../roms/zelda.gb");
    std::fs::read(path).ok()
}

struct Harness {
    client: channel::ChannelClient,
    thread: Option<std::thread::JoinHandle<()>>,
}

impl Harness {
    fn start(config: EmulatorConfig) -> Self {
        let (client, server) = channel::pair(64, 256);
        let thread = std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(move || gameroy_server::driver::run(server, config))
            .unwrap();
        Self {
            client,
            thread: Some(thread),
        }
    }

    /// Waits for the first event satisfying `f`, up to a timeout.
    fn wait_for<T>(&self, what: &str, mut f: impl FnMut(Event) -> Option<T>) -> T {
        let deadline = Instant::now() + Duration::from_secs(20);
        while Instant::now() < deadline {
            match self.client.try_recv() {
                Ok(Some(ev)) => {
                    if let Some(v) = f(ev) {
                        return v;
                    }
                }
                Ok(None) => std::thread::sleep(Duration::from_millis(1)),
                Err(e) => panic!("transport closed while waiting for {what}: {e}"),
            }
        }
        panic!("timed out waiting for {what}");
    }

    fn send(&self, cmd: Command) {
        self.client.send(cmd).unwrap();
    }
}

impl Drop for Harness {
    fn drop(&mut self) {
        let _ = self.client.send(Command::Shutdown);
        if let Some(t) = self.thread.take() {
            let _ = t.join();
        }
    }
}

fn open(h: &Harness, rom: Vec<u8>, start_paused: bool) {
    h.wait_for("Ready", |ev| {
        matches!(ev, Event::Ready { .. }).then_some(())
    });
    h.send(Command::OpenRom(Box::new(OpenRom {
        rom,
        save_ram: None,
        boot_rom: None,
        config: EmulatorConfig::default(),
        start_paused,
    })));
}

#[test]
fn runs_a_rom_and_produces_frames() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);

    let title = h.wait_for("RomOpened", |ev| match ev {
        Event::RomOpened(o) => Some(o.header.title.clone()),
        _ => None,
    });
    assert!(
        title.to_uppercase().contains("ZELDA"),
        "unexpected cartridge title {title:?}"
    );

    // Collect frames and check the invariants the front-end relies on. The first
    // frames after boot are legitimately a blank screen, so the interesting
    // assertion is that *some* frame carries real picture data.
    let mut seqs = Vec::new();
    let mut saw_picture = false;
    while seqs.len() < 240 && !(saw_picture && seqs.len() >= 5) {
        let (seq, pixels) = h.wait_for("Frame", |ev| match ev {
            Event::Frame(p) => Some((p.seq, p.pixels.0)),
            _ => None,
        });
        assert_eq!(pixels.len(), SCREEN_PIXELS, "wrong frame size");
        assert!(
            pixels.iter().all(|&p| p < 4),
            "pixels must be 2-bit shade indices"
        );
        seqs.push(seq);
        if pixels
            .iter()
            .collect::<std::collections::HashSet<_>>()
            .len()
            > 1
        {
            saw_picture = true;
        }
    }

    assert!(
        seqs.windows(2).all(|w| w[1] > w[0]),
        "seq must be strictly monotonic, got {seqs:?}"
    );
    assert!(
        saw_picture,
        "every one of {} frames was a single flat shade; PPU output is not reaching the client",
        seqs.len()
    );
}

#[test]
fn pauses_and_reports_cpu_state() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);

    // Let it run a little so the CPU is somewhere interesting.
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    h.send(Command::SetDebugViews(DebugViews { cpu: true }));
    h.send(Command::SetRunMode(RunMode::Paused));

    let first = h.wait_for("DebugUpdate", |ev| match ev {
        Event::DebugUpdate(u) => u.cpu.map(|cpu| (cpu, u.clock_count, u.epoch)),
        _ => None,
    });
    let (cpu, clock, epoch) = first;
    assert!(clock > 0, "clock should have advanced before pausing");
    assert!(epoch > 0, "epoch should be bumped by opening a ROM");
    // The boot-ROM-less start jumps to the cartridge entry point, so the PC must be
    // somewhere in the ROM rather than at reset.
    assert!(cpu.pc >= 0x0100, "pc {:#06x} looks unstarted", cpu.pc);

    // Stepping must advance the clock and come back paused.
    h.send(Command::Step);
    let stepped = h.wait_for("DebugUpdate after Step", |ev| match ev {
        Event::DebugUpdate(u) if u.clock_count > clock => Some(u.clock_count),
        _ => None,
    });
    assert!(stepped > clock, "Step did not advance the clock");
}

#[test]
fn persists_ram_on_shutdown() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    // Zelda is a battery-backed cart, so shutting down must hand its RAM back for
    // the front-end to write; the server cannot touch the filesystem itself.
    h.send(Command::Shutdown);
    let data = h.wait_for("PersistRam", |ev| match ev {
        Event::PersistRam { data } => Some(data),
        _ => None,
    });
    assert!(!data.is_empty(), "expected non-empty cartridge RAM");
}
