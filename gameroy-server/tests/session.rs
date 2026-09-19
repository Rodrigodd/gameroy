//! End-to-end tests driving a real session over the real transport.
//!
//! These run headless, which matters: the front-end needs a display, so this is
//! where the emulation path actually gets exercised in CI.

use std::time::{Duration, Instant};

use gameroy_server::protocol::transport::{ClientTransport, channel};
use gameroy_server::protocol::{
    Command, DebugViews, EmulatorConfig, Event, OpenRom, RunMode, SCREEN_PIXELS, StopReason,
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

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
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
fn produces_audio_at_roughly_real_time() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    const RATE: u32 = 48_000;
    let config = EmulatorConfig {
        audio_sample_rate: RATE,
        ..EmulatorConfig::default()
    };
    let h = Harness::start(config.clone());

    h.wait_for("Ready", |ev| {
        matches!(ev, Event::Ready { .. }).then_some(())
    });
    h.send(Command::OpenRom(Box::new(OpenRom {
        rom,
        save_ram: None,
        boot_rom: None,
        config,
        start_paused: false,
    })));

    // Accumulate samples over a known wall-clock window. The emulator paces itself
    // against real time, so the sample count must track the device rate; a rate
    // that is badly wrong is exactly what produces crackle on a real device.
    let start = Instant::now();
    let window = Duration::from_millis(1500);
    let mut samples = 0usize;
    let mut chunks = 0usize;
    while start.elapsed() < window {
        match h.client.try_recv() {
            Ok(Some(Event::Audio(chunk))) => {
                assert_eq!(chunk.sample_rate, RATE);
                samples += chunk.samples.len();
                chunks += 1;
            }
            Ok(Some(_)) => {}
            Ok(None) => std::thread::sleep(Duration::from_millis(1)),
            Err(e) => panic!("transport closed: {e}"),
        }
    }

    assert!(
        chunks > 10,
        "expected a steady stream of chunks, got {chunks}"
    );
    // Stereo, so two samples per frame of audio.
    let expected = RATE as f64 * 2.0 * window.as_secs_f64();
    let ratio = samples as f64 / expected;
    assert!(
        (0.5..1.5).contains(&ratio),
        "produced {samples} samples in {:?}; expected about {expected:.0} \
         (ratio {ratio:.2}) -- audio is not tracking real time",
        window
    );
}

#[test]
fn no_audio_while_fast_forwarding() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let config = EmulatorConfig {
        audio_sample_rate: 48_000,
        ..EmulatorConfig::default()
    };
    let h = Harness::start(config.clone());
    h.wait_for("Ready", |ev| {
        matches!(ev, Event::Ready { .. }).then_some(())
    });
    h.send(Command::OpenRom(Box::new(OpenRom {
        rom,
        save_ram: None,
        boot_rom: None,
        config,
        start_paused: false,
    })));
    h.wait_for("first audio", |ev| {
        matches!(ev, Event::Audio(_)).then_some(())
    });

    h.send(Command::SetFrameLimit(false));
    // Let the switch take effect and flush anything already queued.
    std::thread::sleep(Duration::from_millis(200));
    while matches!(h.client.try_recv(), Ok(Some(_))) {}

    let start = Instant::now();
    let mut audio_chunks = 0;
    while start.elapsed() < Duration::from_millis(400) {
        match h.client.try_recv() {
            Ok(Some(Event::Audio(_))) => audio_chunks += 1,
            Ok(Some(_)) => {}
            Ok(None) => std::thread::sleep(Duration::from_millis(1)),
            Err(_) => break,
        }
    }
    assert_eq!(
        audio_chunks, 0,
        "fast-forward must not emit audio; it cannot be played at real-time rate"
    );
}

#[test]
fn save_state_round_trips() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));
    let clock_at_save = h.wait_for("DebugUpdate", |ev| match ev {
        Event::DebugUpdate(u) => Some(u.clock_count),
        _ => None,
    });

    h.send(Command::RequestSaveState);
    let state = h.wait_for("SaveStateData", |ev| match ev {
        Event::SaveStateData { data } => Some(data),
        _ => None,
    });
    assert!(!state.is_empty());

    // Run on, then rewind to the saved point.
    h.send(Command::SetRunMode(RunMode::Running));
    h.wait_for("progress", |ev| match ev {
        Event::DebugUpdate(u) if u.clock_count > clock_at_save => Some(()),
        Event::Frame(_) => Some(()),
        _ => None,
    });

    h.send(Command::LoadSaveState(state));
    h.send(Command::SetRunMode(RunMode::Paused));
    let restored = h.wait_for("DebugUpdate after load", |ev| match ev {
        Event::DebugUpdate(u) => Some(u.clock_count),
        _ => None,
    });
    assert_eq!(
        restored, clock_at_save,
        "loading a state must restore the exact clock it was taken at"
    );
}

#[test]
fn rejects_a_corrupt_save_state_without_losing_the_session() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    h.send(Command::LoadSaveState(vec![0xde, 0xad, 0xbe, 0xef]));
    h.wait_for("Fatal", |ev| match ev {
        Event::Fatal(e) => Some(e),
        _ => None,
    });

    // The session must still be alive and emulating.
    h.wait_for("frame after bad load", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });
}

#[test]
fn rewinds_backwards_through_frames() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);

    // Build up some history.
    let mut last_clock = 0;
    for _ in 0..60 {
        last_clock = h.wait_for("Frame", |ev| match ev {
            Event::Frame(p) => Some(p.clock_count),
            _ => None,
        });
    }

    h.send(Command::SetRewinding(true));
    let mut clocks = Vec::new();
    while clocks.len() < 5 {
        let c = h.wait_for("Frame while rewinding", |ev| match ev {
            Event::Frame(p) => Some(p.clock_count),
            _ => None,
        });
        clocks.push(c);
    }
    h.send(Command::SetRewinding(false));

    assert!(
        clocks.last().unwrap() < &last_clock,
        "rewinding must move the clock backwards: {last_clock} -> {clocks:?}"
    );
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

#[test]
fn breaks_on_an_execute_breakpoint() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, true);
    h.wait_for("paused at start", |ev| {
        matches!(
            ev,
            Event::RunState {
                mode: RunMode::Paused,
                ..
            }
        )
        .then_some(())
    });

    // 0x0150 is the usual cartridge entry point, reached almost immediately.
    h.send(Command::AddBreakpoint {
        address: 0x0150,
        flags: 4, // EXECUTE
    });
    let breakpoints = h.wait_for("BreakpointsChanged", |ev| match ev {
        Event::BreakpointsChanged { breakpoints } => Some(breakpoints),
        _ => None,
    });
    assert_eq!(breakpoints, vec![(0x0150, 4)]);

    h.send(Command::RunToBreakpoint);
    let reason = h.wait_for("stop at breakpoint", |ev| match ev {
        Event::RunState {
            mode: RunMode::Paused,
            reason,
            ..
        } => Some(reason),
        _ => None,
    });
    assert!(
        matches!(
            reason,
            StopReason::Breakpoint {
                address: 0x0150,
                ..
            }
        ),
        "expected a breakpoint stop at 0150, got {reason:?}"
    );
}

#[test]
fn watches_report_live_memory() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ppu: true,
        watches: true,
        ..Default::default()
    }));
    // 0xff47 is BGP, the background palette. Deliberately not LY: LY advances
    // every scanline and is updated lazily, so cross-checking it against the panel
    // would be testing the emulator's update timing rather than snapshot coherence.
    h.send(Command::AddWatch { address: 0xff47 });
    h.send(Command::SetRunMode(RunMode::Paused));

    // SetDebugViews itself triggers a snapshot, which is emitted before AddWatch
    // has been handled, so the first update legitimately carries an empty list.
    // Wait for one that actually has the watch in it.
    let update = h.wait_for("DebugUpdate carrying the watch", |ev| match ev {
        Event::DebugUpdate(u)
            if u.ppu.is_some() && u.watches.as_ref().is_some_and(|w| !w.is_empty()) =>
        {
            Some(u)
        }
        _ => None,
    });
    let watches = update.watches.unwrap();
    assert_eq!(watches.len(), 1);
    assert_eq!(watches[0].address, 0xff47);

    let ppu = update.ppu.unwrap();
    assert!(ppu.ly < 154, "LY out of range: {}", ppu.ly);
    assert!(ppu.lx < 456, "LX out of range: {}", ppu.lx);
    // The watch and the PPU panel must describe the same instant.
    assert_eq!(
        watches[0].value, ppu.bgp,
        "watch on BGP disagrees with the PPU panel in the same snapshot"
    );
}

#[test]
fn console_reports_errors_and_runs_commands() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, true);

    h.send(Command::ExecuteDebugCommand {
        line: "brk 0150".into(),
    });
    let (ok, text) = h.wait_for("console error", |ev| match ev {
        Event::DebugConsole { ok, text } => Some((ok, text)),
        _ => None,
    });
    assert!(!ok, "a bad command must report failure");
    assert!(
        text.contains("unknown command"),
        "unhelpful message: {text}"
    );

    // A valid one must still work afterwards: a bad command cannot wedge the console.
    h.send(Command::ExecuteDebugCommand {
        line: "break x 0150".into(),
    });
    let breakpoints = h.wait_for("BreakpointsChanged", |ev| match ev {
        Event::BreakpointsChanged { breakpoints } => Some(breakpoints),
        _ => None,
    });
    assert_eq!(breakpoints, vec![(0x0150, 4)]);

    h.send(Command::ExecuteDebugCommand {
        line: "watch ff05".into(),
    });
    let watches = h.wait_for("WatchesChanged", |ev| match ev {
        Event::WatchesChanged { watches } => Some(watches),
        _ => None,
    });
    assert_eq!(watches, vec![0xff05]);
}

#[test]
fn disassembles_a_window_around_the_pc() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    // Let it execute enough for the tracer to discover code.
    for _ in 0..30 {
        h.wait_for("Frame", |ev| matches!(ev, Event::Frame(_)).then_some(()));
    }

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        disassembly_lines: 40,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));

    let update = h.wait_for("DebugUpdate with disassembly", |ev| match ev {
        Event::DebugUpdate(u) if u.disassembly.as_ref().is_some_and(|d| !d.lines.is_empty()) => {
            Some(u)
        }
        _ => None,
    });
    let disasm = update.disassembly.unwrap();
    let cpu = update.cpu.unwrap();

    assert!(
        disasm.total > 100,
        "trace looks empty: {} entries",
        disasm.total
    );
    assert!(
        disasm.lines.len() <= 40,
        "window must be bounded, got {} lines",
        disasm.lines.len()
    );
    // The window must be a window: a slice of the whole, not the whole thing.
    assert!(disasm.total as usize > disasm.lines.len());

    for line in &disasm.lines {
        assert!(!line.text.is_empty(), "every line must render some text");
    }

    // The PC row must be marked, and must agree with the CPU snapshot.
    if let Some(current) = disasm.lines.iter().find(|l| l.is_current) {
        assert_eq!(
            current.pc, cpu.pc,
            "the highlighted row must be the row the CPU is on"
        );
    }
}

#[test]
fn disassembly_generation_is_stable_while_no_new_code_runs() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    for _ in 0..30 {
        h.wait_for("Frame", |ev| matches!(ev, Event::Frame(_)).then_some(()));
    }
    h.send(Command::SetDebugViews(DebugViews {
        disassembly_lines: 20,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));

    let first = h.wait_for("first disassembly", |ev| match ev {
        Event::DebugUpdate(u) => u.disassembly.map(|d| d.generation),
        _ => None,
    });

    // Paused: nothing new is traced, so the index must not be rebuilt and the
    // generation must not move. If it does, clients would invalidate caches for
    // nothing on every single pause.
    h.send(Command::SetDebugViews(DebugViews {
        disassembly_lines: 20,
        ..Default::default()
    }));
    let second = h.wait_for("second disassembly", |ev| match ev {
        Event::DebugUpdate(u) => u.disassembly.map(|d| d.generation),
        _ => None,
    });
    assert_eq!(
        first, second,
        "generation moved without new code being traced"
    );
}

#[test]
fn ppu_viewer_renders_images_only_when_asked() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, false);
    for _ in 0..40 {
        h.wait_for("Frame", |ev| matches!(ev, Event::Frame(_)).then_some(()));
    }

    // With only the CPU panel open, the expensive images must not be produced.
    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));
    let cheap = h.wait_for("DebugUpdate", |ev| match ev {
        Event::DebugUpdate(u) if u.cpu.is_some() => Some(u),
        _ => None,
    });
    assert!(
        cheap.ppu_viewer.is_none(),
        "the PPU viewer must cost nothing while its tab is closed"
    );

    // Now open it.
    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ppu_viewer: true,
        ..Default::default()
    }));
    let update = h.wait_for("DebugUpdate with ppu viewer", |ev| match ev {
        Event::DebugUpdate(u) if u.ppu_viewer.is_some() => Some(u),
        _ => None,
    });
    let view = update.ppu_viewer.unwrap();

    assert_eq!(view.tiles.width, 128);
    assert_eq!(view.tiles.height, 192);
    assert_eq!(view.tiles.data.len(), 128 * 192);
    assert_eq!(view.background.data.len(), 256 * 256);
    assert_eq!(view.window.data.len(), 256 * 256);
    assert_eq!(view.oam.len(), 40, "OAM always holds 40 entries");
    assert_eq!(view.sprite_buffer.len(), 10);

    for img in [&view.tiles, &view.background, &view.window] {
        assert!(
            img.data.iter().all(|&p| p < 4),
            "images must be 2-bit palette indices, not RGBA"
        );
    }
    // Zelda has drawn something by now, so the tile sheet cannot be blank.
    assert!(
        view.tiles.data.iter().any(|&p| p != view.tiles.data[0]),
        "tile sheet is a single flat value; VRAM is not being read"
    );
}

/// A single `Step` must advance exactly one instruction.
///
/// Regression: `Step` used to switch into a running mode and stop at the next
/// frame boundary, so each press advanced ~70000 clocks. It also went through the
/// recompiler, which executes whole compiled blocks and so cannot express a
/// single-instruction step at all.
#[test]
fn step_advances_one_instruction_not_one_frame() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    // Explicitly with the JIT enabled, since that is the configuration the bug
    // needed: a debugging step must use the interpreter regardless.
    let config = EmulatorConfig {
        jit: true,
        ..EmulatorConfig::default()
    };
    let h = Harness::start(config.clone());
    h.wait_for("Ready", |ev| {
        matches!(ev, Event::Ready { .. }).then_some(())
    });
    h.send(Command::OpenRom(Box::new(OpenRom {
        rom,
        save_ram: None,
        boot_rom: None,
        config,
        start_paused: false,
    })));
    h.wait_for("first frame", |ev| {
        matches!(ev, Event::Frame(_)).then_some(())
    });

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));

    let mut last = h.wait_for("paused snapshot", |ev| match ev {
        Event::DebugUpdate(u) => u.cpu.map(|cpu| (u.clock_count, cpu.pc)),
        _ => None,
    });

    // One Game Boy instruction is 4-24 clocks; a frame is 70224. Step several
    // times so a single lucky short step cannot pass by accident.
    for i in 0..8 {
        h.send(Command::Step);
        let now = h.wait_for("snapshot after Step", |ev| match ev {
            Event::DebugUpdate(u) => match u.cpu {
                Some(cpu) if u.clock_count != last.0 => Some((u.clock_count, cpu.pc)),
                _ => None,
            },
            _ => None,
        });

        let advanced = now.0 - last.0;
        assert!(
            advanced > 0 && advanced <= 32,
            "step {i} advanced {advanced} clocks; one instruction is 4-24, a frame is 70224"
        );
        assert_ne!(now.1, last.1, "step {i} left the PC unchanged");
        last = now;
    }
}

/// Stepping must leave the emulator paused, not running.
#[test]
fn step_leaves_the_emulator_paused() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    open(&h, rom, true);
    h.wait_for("paused at start", |ev| {
        matches!(
            ev,
            Event::RunState {
                mode: RunMode::Paused,
                ..
            }
        )
        .then_some(())
    });

    h.send(Command::Step);
    let (mode, reason) = h.wait_for("RunState after Step", |ev| match ev {
        Event::RunState { mode, reason, .. } => Some((mode, reason)),
        _ => None,
    });
    assert_eq!(
        mode,
        RunMode::Paused,
        "Step must not leave the emulator running"
    );
    assert!(
        matches!(reason, StopReason::StepComplete),
        "expected StepComplete, got {reason:?}"
    );
}

/// Runs the ROM long enough to fill the rewind ring, then pauses with the CPU
/// view on and returns the paused state. Every stepback test needs this: a
/// stepback can only reach as far as there is rewind history.
fn paused_with_history(h: &Harness, rom: Vec<u8>, config: EmulatorConfig) -> (u64, u16) {
    h.wait_for("Ready", |ev| {
        matches!(ev, Event::Ready { .. }).then_some(())
    });
    h.send(Command::OpenRom(Box::new(OpenRom {
        rom,
        save_ram: None,
        boot_rom: None,
        config,
        start_paused: false,
    })));
    // Several frames, so there is more than one snapshot to unwind through.
    let mut frames = 0;
    h.wait_for("a few frames", |ev| {
        if matches!(ev, Event::Frame(_)) {
            frames += 1;
        }
        (frames >= 5).then_some(())
    });

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    h.send(Command::SetRunMode(RunMode::Paused));
    // `SetDebugViews` answers with a snapshot of its own, taken while the machine
    // was still running. Waiting for the pause first discards it, so what comes
    // back is the state the machine actually stopped in.
    await_stop(h, |r| matches!(r, StopReason::UserRequest));
    await_snapshot(h)
}

/// Waits for a pause whose reason satisfies `f`, and asserts nothing else
/// stopped us first.
fn await_stop(h: &Harness, f: impl Fn(StopReason) -> bool) {
    let reason = h.wait_for("a pause", |ev| match ev {
        Event::RunState {
            mode: RunMode::Paused,
            reason,
            ..
        } => Some(reason),
        _ => None,
    });
    assert!(f(reason), "stopped for the wrong reason: {reason:?}");
}

/// The `(clock, pc)` of the next CPU snapshot.
fn await_snapshot(h: &Harness) -> (u64, u16) {
    h.wait_for("a CPU snapshot", |ev| match ev {
        Event::DebugUpdate(u) => u.cpu.map(|cpu| (u.clock_count, cpu.pc)),
        _ => None,
    })
}

/// One instruction forward. Each pause emits a `RunState` and then exactly one
/// `DebugUpdate`, so consuming both keeps the queue balanced for the next call.
fn step(h: &Harness) -> (u64, u16) {
    h.send(Command::Step);
    await_stop(h, |r| matches!(r, StopReason::StepComplete));
    await_snapshot(h)
}

#[test]
fn stepback_returns_to_the_previous_instruction() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    // Two steps, so the landing point is a state actually observed rather than
    // whatever the pause happened to catch.
    let landing = step(&h);
    let departed = step(&h);
    assert_ne!(landing, departed, "the second Step did not move");

    h.send(Command::StepBack);
    await_stop(&h, |r| matches!(r, StopReason::StepBackComplete));
    assert_eq!(
        await_snapshot(&h),
        landing,
        "stepback did not land on the previous instruction"
    );
}

/// The console spelling must do what the command does; it was a stub replying
/// "stepback is not implemented yet".
#[test]
fn stepback_from_the_console_matches_the_command() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    let landing = step(&h);
    step(&h);

    h.send(Command::ExecuteDebugCommand {
        line: "stepback".into(),
    });
    await_stop(&h, |r| matches!(r, StopReason::StepBackComplete));
    assert_eq!(await_snapshot(&h), landing);
}

#[test]
fn stepback_without_a_rewind_buffer_reports_instead_of_moving() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let config = EmulatorConfig {
        rewind_capacity_bytes: 0,
        ..EmulatorConfig::default()
    };
    let h = Harness::start(config.clone());
    paused_with_history(&h, rom, config);
    let before = step(&h);

    h.send(Command::StepBack);
    let text = h.wait_for("a console complaint", |ev| match ev {
        Event::DebugConsole { ok: false, text } => Some(text),
        _ => None,
    });
    assert!(
        text.contains("rewind"),
        "the message should say why: {text:?}"
    );

    // Re-declaring the views is the cheap way to ask for a fresh snapshot.
    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    assert_eq!(
        await_snapshot(&h),
        before,
        "a refused stepback moved the machine"
    );
}

#[test]
fn stepback_before_any_step_reports_instead_of_moving() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    let before = paused_with_history(&h, rom, EmulatorConfig::default());

    // Nothing has been stepped, so there is no previous instruction to name.
    h.send(Command::StepBack);
    let text = h.wait_for("a console complaint", |ev| match ev {
        Event::DebugConsole { ok: false, text } => Some(text),
        _ => None,
    });
    assert!(
        text.contains("previous instruction"),
        "the message should say why: {text:?}"
    );

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    assert_eq!(await_snapshot(&h), before);
}

/// The replay re-executes code that may contain a breakpoint. Stopping there
/// would strand the user somewhere they never asked to be, which is the only
/// thing `skip_breakpoints_until_target_clock` exists for.
#[test]
fn stepback_ignores_breakpoints_during_the_replay() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    let landing = step(&h);
    step(&h);

    // Break on the instruction we are about to step back onto, so the replay is
    // guaranteed to run straight through it.
    h.send(Command::AddBreakpoint {
        address: landing.1,
        flags: 1 << 2, // break_flags::EXECUTE
    });
    h.wait_for("the breakpoint to register", |ev| match ev {
        Event::BreakpointsChanged { breakpoints } => breakpoints
            .iter()
            .any(|&(a, _)| a == landing.1)
            .then_some(()),
        _ => None,
    });

    h.send(Command::StepBack);
    await_stop(&h, |r| matches!(r, StopReason::StepBackComplete));
    assert_eq!(await_snapshot(&h), landing);
}

#[test]
fn partial_screen_is_absent_unless_subscribed() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    // `paused_with_history` subscribes to the CPU view and nothing else.
    // Re-declaring it is the cheap way to ask for a fresh snapshot.
    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        ..Default::default()
    }));
    let update = h.wait_for("a DebugUpdate", |ev| match ev {
        Event::DebugUpdate(u) if u.cpu.is_some() => Some(u),
        _ => None,
    });
    assert!(
        update.screen.is_none(),
        "the screen was sent without being asked for"
    );
}

/// `drawn_pixels` must be the real boundary between this frame's output and the
/// previous frame's leftovers. The assertion is that it agrees with `ly` and
/// `screen_x` from the very same update -- two numbers derived independently.
#[test]
fn partial_screen_marks_the_drawn_prefix() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    h.send(Command::SetDebugViews(DebugViews {
        ppu: true,
        screen: true,
        ..Default::default()
    }));
    let update = h.wait_for("a DebugUpdate with the screen", |ev| match ev {
        Event::DebugUpdate(u) if u.screen.is_some() && u.ppu.is_some() => Some(u),
        _ => None,
    });

    let screen = update.screen.as_ref().unwrap();
    let ppu = update.ppu.unwrap();

    assert_eq!(screen.pixels.data.len(), SCREEN_PIXELS);
    assert_eq!(
        screen.pixels.width as usize * screen.pixels.height as usize,
        SCREEN_PIXELS
    );
    assert!(
        screen.pixels.data.iter().all(|&p| p < 4),
        "the Game Boy has four shades; something other than a palette index got in"
    );

    let expected = (ppu.ly as usize * 160 + (ppu.screen_x as usize).min(160)).min(SCREEN_PIXELS);
    assert_eq!(
        screen.drawn_pixels as usize, expected,
        "drawn_pixels disagrees with ly={} screen_x={}",
        ppu.ly, ppu.screen_x
    );
}

/// It has to actually track the PPU, not sit at a constant.
#[test]
fn partial_screen_advances_while_stepping() {
    let Some(rom) = test_rom() else {
        eprintln!("skipping: roms/zelda.gb not present");
        return;
    };
    let h = Harness::start(EmulatorConfig::default());
    paused_with_history(&h, rom, EmulatorConfig::default());

    h.send(Command::SetDebugViews(DebugViews {
        cpu: true,
        screen: true,
        ..Default::default()
    }));

    let mut seen = std::collections::BTreeSet::new();
    for _ in 0..400 {
        h.send(Command::Step);
        await_stop(&h, |r| matches!(r, StopReason::StepComplete));
        let update = h.wait_for("a DebugUpdate with the screen", |ev| match ev {
            Event::DebugUpdate(u) if u.screen.is_some() => Some(u),
            _ => None,
        });
        seen.insert(update.screen.as_ref().unwrap().drawn_pixels);
    }
    // Not "non-decreasing": it wraps to 0 at every frame boundary.
    assert!(
        seen.len() > 1,
        "drawn_pixels never moved across 400 instructions: {seen:?}"
    );
}
