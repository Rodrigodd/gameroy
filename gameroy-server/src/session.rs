//! The emulator session: one open ROM, driven a slice at a time.

use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{GameBoy, cartridge::Cartridge},
    interpreter::Interpreter,
};

use crate::protocol::{
    CartridgeInfo, Command, CpuState, DebugUpdate, DebugViews, EmulatorConfig, Event, FrameBuffer,
    FramePacket, ImeState, OpenRom, RomOpened, RunMode, ServerError, SessionEpoch, StopReason,
    transport::{ServerTransport, TransportError},
};
use crate::timeline::Timeline;

// The protocol duplicates these so it can compile without the emulator. If the core
// ever changes them, this is where the duplication is caught.
const _: () = assert!(crate::protocol::SCREEN_WIDTH == SCREEN_WIDTH);
const _: () = assert!(crate::protocol::SCREEN_HEIGHT == SCREEN_HEIGHT);
const _: () = assert!(crate::protocol::CLOCK_SPEED == CLOCK_SPEED);

/// One slice of emulation, matching the old front-end's `CLOCK_SPEED / 600`
/// (~1.6 ms). Short enough that commands are handled promptly, long enough that the
/// per-slice overhead is noise.
const SLICE_CLOCKS: u64 = CLOCK_SPEED / 600;

/// How far behind real time the emulator is allowed to fall before it gives up on
/// catching up. Without this, a stalled process returns and then sprints.
const MAX_CATCHUP_CLOCKS: u64 = CLOCK_SPEED / 30;

/// What the driver should do after [`Session::step`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Schedule {
    /// More work is pending; call `step` again immediately.
    Now,
    /// Call `step` again at this instant. This is real-time frame pacing.
    At(Instant),
    /// Nothing to do until a command arrives.
    Idle,
    /// Shut down.
    Exit,
}

/// State shared with the `v_blank` callback that lives inside `GameBoy`.
///
/// The callback is `Box<dyn FnMut(&mut GameBoy) + Send>` stored *inside* the struct
/// it mutates, so whatever it captures cannot also be `&mut`-borrowed by the owner.
/// That is the real reason the old code reached for a mutex on a thread that never
/// shared anything. `Send` on the callback rules out `Rc<RefCell<_>>`, so this is an
/// uncontended `std::sync::Mutex` — cheap, and never held across emulation.
struct VBlank {
    timeline: Timeline,
    /// Frames produced since the last drain, oldest first.
    produced: Vec<Vec<u8>>,
    /// Recycled frame allocations.
    pool: Vec<Vec<u8>>,
}

impl VBlank {
    fn take_buffer(&mut self) -> Vec<u8> {
        self.pool
            .pop()
            .unwrap_or_else(|| vec![0u8; SCREEN_WIDTH * SCREEN_HEIGHT])
    }
}

pub struct Session {
    gb: Option<Box<GameBoy>>,
    vblank: Arc<Mutex<VBlank>>,
    config: EmulatorConfig,

    mode: RunMode,
    frame_limit: bool,
    /// Set by `Step`: run until one more frame is produced, then pause.
    step_pending: bool,

    epoch: SessionEpoch,
    seq: u64,
    views: DebugViews,

    /// Wall-clock/emulated-clock reference for real-time pacing.
    last_start_time: Instant,
    last_start_clock: u64,

    shutting_down: bool,
}

impl Session {
    pub fn new(config: EmulatorConfig) -> Self {
        Self {
            gb: None,
            vblank: Arc::new(Mutex::new(VBlank {
                timeline: Timeline::new(0, Vec::new(), 0),
                produced: Vec::new(),
                pool: Vec::new(),
            })),
            frame_limit: config.frame_limit,
            config,
            mode: RunMode::Paused,
            step_pending: false,
            epoch: 0,
            seq: 0,
            views: DebugViews::default(),
            last_start_time: Instant::now(),
            last_start_clock: 0,
            shutting_down: false,
        }
    }

    /// Do a bounded slice of work. Never blocks, never loops indefinitely.
    pub fn step(&mut self, t: &mut impl ServerTransport) -> Schedule {
        // 1. Drain every pending command first, so input and run-mode changes take
        //    effect before this slice rather than after it.
        loop {
            match t.try_recv() {
                Ok(Some(cmd)) => {
                    if self.handle_command(cmd, t) {
                        return Schedule::Exit;
                    }
                }
                Ok(None) => break,
                Err(TransportError::Closed) => return Schedule::Exit,
                Err(TransportError::Full) => break,
            }
        }

        if self.shutting_down {
            return Schedule::Exit;
        }

        // 2. Emulate.
        let schedule = self.run_slice();

        // 3. Ship whatever the vblank callback produced.
        self.flush_frames(t);

        schedule
    }

    fn run_slice(&mut self) -> Schedule {
        if self.mode == RunMode::Paused {
            return Schedule::Idle;
        }
        let Some(gb) = self.gb.as_mut() else {
            return Schedule::Idle;
        };

        if !self.frame_limit {
            // Fast-forward: no real-time pacing, just keep going a slice at a time.
            let target = gb.clock_count + SLICE_CLOCKS;
            while gb.clock_count < target {
                Interpreter(gb).interpret_op();
            }
            // Audio would be generated and immediately discarded here; it cannot
            // keep up with real time and is not worth shipping.
            let clock = gb.clock_count;
            let _ = gb.sound.get_mut().get_output(clock);
            return Schedule::Now;
        }

        // Real-time pacing: derive a clock target from elapsed wall time.
        let elapsed = self.last_start_time.elapsed();
        let elapsed_clocks = CLOCK_SPEED * elapsed.as_secs()
            + (CLOCK_SPEED as f64 * (elapsed.subsec_nanos() as f64 * 1e-9)) as u64;
        let mut target = self.last_start_clock + elapsed_clocks;

        if target > gb.clock_count + MAX_CATCHUP_CLOCKS {
            target = gb.clock_count + MAX_CATCHUP_CLOCKS;
            self.last_start_time = Instant::now();
            self.last_start_clock = gb.clock_count;
        }

        if gb.clock_count >= target {
            // Ahead of schedule. Sleep until the next slice is due rather than spin.
            return Schedule::At(Instant::now() + Duration::from_micros(500));
        }

        // Cap the work per call so commands stay responsive even after a long stall.
        let capped = target.min(gb.clock_count + SLICE_CLOCKS);
        while gb.clock_count < capped {
            Interpreter(gb).interpret_op();
        }

        let clock = gb.clock_count;
        let _ = gb.sound.get_mut().get_output(clock);

        Schedule::Now
    }

    fn flush_frames(&mut self, t: &mut impl ServerTransport) {
        let (frames, clock, frame_no) = {
            let vb = &mut *self.vblank.lock().unwrap();
            if vb.produced.is_empty() {
                return;
            }
            let frames: Vec<Vec<u8>> = std::mem::take(&mut vb.produced);
            (
                frames,
                self.gb.as_ref().map_or(0, |gb| gb.clock_count),
                vb.timeline.current_frame,
            )
        };

        let produced_any = !frames.is_empty();
        for pixels in frames {
            self.seq += 1;
            let packet = FramePacket {
                seq: self.seq,
                frame: frame_no,
                clock_count: clock,
                width: SCREEN_WIDTH as u16,
                height: SCREEN_HEIGHT as u16,
                pixels: FrameBuffer(pixels),
            };
            // Lossy: a front-end that cannot keep up should drop frames, not stall
            // the emulator. Recycle the allocation of anything that got dropped.
            match t.send_lossy(Event::Frame(packet)) {
                Ok(Some(Event::Frame(p))) => self.recycle(p.pixels.0),
                Ok(_) => {}
                Err(_) => return,
            }
        }

        // `Step` completes when a frame boundary is reached, which is the same
        // granularity the old front-end's single-frame stepping used.
        if produced_any && self.step_pending {
            self.step_pending = false;
            self.set_mode(RunMode::Paused, StopReason::StepComplete, t);
        }
    }

    fn recycle(&mut self, buf: Vec<u8>) {
        let vb = &mut *self.vblank.lock().unwrap();
        if vb.pool.len() < 4 {
            vb.pool.push(buf);
        }
    }

    /// Returns true when the server should exit.
    fn handle_command(&mut self, cmd: Command, t: &mut impl ServerTransport) -> bool {
        match cmd {
            Command::Shutdown => {
                self.persist_ram(t);
                self.shutting_down = true;
                return true;
            }
            Command::OpenRom(open) => self.open_rom(*open, t),
            Command::SetRunMode(mode) => self.set_mode(mode, StopReason::UserRequest, t),
            Command::SetFrameLimit(on) => {
                self.frame_limit = on;
                self.resync_pacing();
            }
            Command::Reset => {
                if let Some(gb) = self.gb.as_mut() {
                    gb.reset();
                    self.epoch += 1;
                    self.resync_pacing();
                    self.set_mode(self.mode, StopReason::Reset, t);
                }
            }
            Command::SetJoypad(joypad) => {
                self.vblank.lock().unwrap().timeline.current_joypad = joypad;
            }
            Command::Step => {
                self.step_pending = true;
                self.set_mode(RunMode::Running, StopReason::UserRequest, t);
            }
            Command::SetDebugViews(views) => {
                self.views = views;
                self.send_debug_update(t);
            }
            Command::ReturnFrame(buf) => self.recycle(buf.0),
        }
        false
    }

    fn open_rom(&mut self, open: OpenRom, t: &mut impl ServerTransport) {
        let cartridge = match Cartridge::new(open.rom) {
            Ok(c) => c,
            // `Cartridge::new` reports (message, partially-parsed cartridge); the
            // partial value is only useful for the old debugger's MBC override.
            Err((message, _partial)) => {
                let _ = t.send(Event::Fatal(ServerError::LoadRom(message)));
                return;
            }
        };
        let header = CartridgeInfo {
            title: cartridge.header.title_as_string(),
            kind_name: cartridge.kind_name().to_string(),
            rom_size: cartridge.rom.len() as u32,
            ram_size: cartridge.ram.len() as u32,
        };

        let boot_rom = open
            .boot_rom
            .and_then(|b| <[u8; 256]>::try_from(b.as_slice()).ok());
        let mut gb = Box::new(GameBoy::new(boot_rom, cartridge));
        gb.predict_interrupt = open.config.interrupt_prediction;

        if let Some(ram) = open.save_ram {
            if ram.len() == gb.cartridge.ram.len() {
                gb.cartridge.ram.copy_from_slice(&ram);
            } else {
                log::warn!(
                    "ignoring save RAM: got {} bytes, cartridge wants {}",
                    ram.len(),
                    gb.cartridge.ram.len()
                );
            }
        }

        *self.vblank.lock().unwrap() = VBlank {
            timeline: Timeline::new(0, Vec::new(), open.config.rewind_capacity_bytes),
            produced: Vec::new(),
            pool: Vec::new(),
        };

        // Advance the input record and capture the screen on every vblank. This is
        // the one place the emulator calls back into us.
        let shared = self.vblank.clone();
        gb.v_blank = Some(Box::new(move |gb| {
            let vb = &mut *shared.lock().unwrap();
            if !vb.timeline.rewinding {
                // `next_frame` snapshots for rewind before returning the input, so
                // this assignment must come after it.
                gb.joypad = vb.timeline.next_frame(gb);
            }
            let mut buf = vb.take_buffer();
            buf.copy_from_slice(&gb.ppu.borrow().screen.packed());
            vb.produced.push(buf);
        }));

        self.config = open.config;
        self.frame_limit = self.config.frame_limit;
        self.gb = Some(gb);
        self.epoch += 1;
        self.resync_pacing();

        let _ = t.send(Event::RomOpened(Box::new(RomOpened {
            epoch: self.epoch,
            header,
        })));

        let mode = if open.start_paused {
            RunMode::Paused
        } else {
            RunMode::Running
        };
        self.set_mode(mode, StopReason::None, t);
    }

    fn set_mode(&mut self, mode: RunMode, reason: StopReason, t: &mut impl ServerTransport) {
        self.mode = mode;
        if mode != RunMode::Paused {
            self.resync_pacing();
        }
        let (clock, frame) = self.clock_and_frame();
        let _ = t.send(Event::RunState {
            epoch: self.epoch,
            mode,
            reason,
            clock_count: clock,
            frame,
        });
        if mode == RunMode::Paused {
            self.send_debug_update(t);
        }
    }

    /// Re-anchor the pacing reference, so wall time that passed while paused or
    /// fast-forwarding is not mistaken for emulation debt.
    fn resync_pacing(&mut self) {
        self.last_start_time = Instant::now();
        self.last_start_clock = self.gb.as_ref().map_or(0, |gb| gb.clock_count);
    }

    fn clock_and_frame(&self) -> (u64, u32) {
        let clock = self.gb.as_ref().map_or(0, |gb| gb.clock_count);
        let frame = self.vblank.lock().unwrap().timeline.current_frame;
        (clock, frame)
    }

    fn send_debug_update(&mut self, t: &mut impl ServerTransport) {
        if !self.views.cpu {
            return;
        }
        let Some(gb) = self.gb.as_ref() else { return };
        let (clock, frame) = (
            gb.clock_count,
            self.vblank.lock().unwrap().timeline.current_frame,
        );
        let cpu = &gb.cpu;
        let update = DebugUpdate {
            epoch: self.epoch,
            clock_count: clock,
            frame,
            cpu: Some(CpuState {
                a: cpu.a,
                f: cpu.f.0,
                b: cpu.b,
                c: cpu.c,
                d: cpu.d,
                e: cpu.e,
                h: cpu.h,
                l: cpu.l,
                sp: cpu.sp,
                pc: cpu.pc,
                ime: match cpu.ime {
                    gameroy::gameboy::cpu::ImeState::Disabled => ImeState::Disabled,
                    gameroy::gameboy::cpu::ImeState::Enabled => ImeState::Enabled,
                    gameroy::gameboy::cpu::ImeState::ToBeEnable => ImeState::ToBeEnabled,
                },
                banks: gb.cartridge.curr_bank(),
                interrupt_flag: gb.interrupt_flag.get(),
                interrupt_enabled: gb.interrupt_enabled,
            }),
        };
        let _ = t.send(Event::DebugUpdate(Box::new(update)));
    }

    fn persist_ram(&mut self, t: &mut impl ServerTransport) {
        if let Some(gb) = self.gb.as_ref() {
            if !gb.cartridge.ram.is_empty() {
                let _ = t.send(Event::PersistRam {
                    data: gb.cartridge.ram.clone(),
                });
            }
        }
    }
}
