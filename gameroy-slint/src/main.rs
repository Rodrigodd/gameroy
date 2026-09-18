// Prevent console window in addition to Slint window in Windows release builds when, e.g., starting the app via file manager. Ignored on other platforms.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod audio;
mod config;
mod input;
mod library;
mod server_handle;
mod storage;

use std::cell::RefCell;
use std::error::Error;

use gameroy_server::protocol::{
    Command, CpuState, DebugViews, EmulatorConfig, Event, OpenRom, PpuState, RunMode, StopReason,
};
use slint::{ModelRc, Rgb8Pixel, SharedPixelBuffer, SharedString, VecModel};

use audio::{AudioEngineSink, AudioSink};
use config::Config;
use input::{Hotkey, Joypad};
use library::{RomEntry, Sort, SortColumn};
use server_handle::ServerHandle;
use storage::RomFile;

slint::include_modules!();

/// How many disassembly lines to keep around the PC. Enough to fill a tall panel
/// without shipping a large slice on every single step.
const DISASM_WINDOW: u16 = 64;

/// The four Game Boy shades, lightest first, as the old front-end had them.
const PALETTE: [u8; 4] = [255, 180, 100, 0];

/// UI-thread state. The server thread never touches this; it only posts a wake-up
/// and the draining happens here.
struct App {
    server: Option<ServerHandle>,
    audio: Option<AudioEngineSink>,
    config: EmulatorConfig,
    settings: Config,

    entries: Vec<RomEntry>,
    sort: Sort,
    selected: Option<usize>,

    /// None while the library is showing.
    rom: Option<RomFile>,
    joypad: Joypad,
    debugging: bool,
    mode: RunMode,
    menu_open: bool,
    /// Console scrollback. Bounded so a long session cannot grow without limit.
    console: Vec<String>,
    /// Run mode to restore when the pause menu closes.
    mode_before_menu: RunMode,
    /// The server announces itself with Ready; a ROM chosen before that (from the
    /// command line) waits in `rom` until then.
    server_ready: bool,
}

impl App {
    fn send(&self, cmd: Command) {
        if let Some(server) = self.server.as_ref() {
            server.send(cmd);
        }
    }
}

thread_local! {
    static APP: RefCell<Option<App>> = const { RefCell::new(None) };
}

fn with_app<T>(f: impl FnOnce(&mut App) -> T) -> Option<T> {
    APP.with(|cell| cell.borrow_mut().as_mut().map(f))
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let settings = Config::load();
    let cli_rom = std::env::args().nth(1).map(RomFile::new);

    let ui = AppWindow::new()?;
    let weak = ui.as_weak();

    // Waking the UI loop from the server thread. `invoke_from_event_loop` returns
    // Err once the loop has quit, which is the natural "front-end is gone" signal.
    let wake = {
        let weak = weak.clone();
        move || {
            let weak = weak.clone();
            let _ = slint::invoke_from_event_loop(move || {
                if let Some(ui) = weak.upgrade() {
                    drain_events(&ui);
                }
            });
        }
    };

    // Open the audio device first: its rate is what the emulator must synthesize
    // at, and the server needs it in the config it starts with.
    let audio = AudioEngineSink::new();
    let config = EmulatorConfig {
        audio_sample_rate: audio.as_ref().map_or(0, |a| a.sample_rate()),
        rewind_capacity_bytes: if settings.rewinding {
            EmulatorConfig::default().rewind_capacity_bytes
        } else {
            0
        },
        jit: settings.jit,
        ..EmulatorConfig::default()
    };
    if let Some(a) = audio.as_ref() {
        log::info!("audio device at {} Hz", a.sample_rate());
    }

    let server = ServerHandle::spawn(config.clone(), wake)?;

    APP.with(|cell| {
        *cell.borrow_mut() = Some(App {
            server: Some(server),
            audio,
            config,
            sort: settings.sort(),
            settings,
            entries: Vec::new(),
            selected: None,
            joypad: Joypad::default(),
            debugging: false,
            mode: RunMode::Paused,
            menu_open: false,
            console: Vec::new(),
            mode_before_menu: RunMode::Running,
            rom: cli_rom,
            server_ready: false,
        })
    });

    ui.set_integer_scaling(with_app(|app| app.settings.only_integer_scaling).unwrap_or(false));

    wire_callbacks(&ui);
    refresh_library(&ui);

    ui.run()?;

    // Draining during shutdown is what persists the save: the server emits its
    // final PersistRam after receiving Shutdown and before exiting.
    let taken = with_app(|app| {
        (
            app.server.take(),
            app.rom.as_ref().map(|r| r.save_ram_path()),
        )
    });
    if let Some((Some(mut server), save_path)) = taken {
        server.shutdown(|ev| {
            if let (Event::PersistRam { data }, Some(path)) = (ev, save_path.as_ref()) {
                match std::fs::write(path, &data) {
                    Ok(()) => log::info!("wrote {} ({} bytes)", path.display(), data.len()),
                    Err(e) => log::error!("failed to write {}: {e}", path.display()),
                }
            }
        });
    }

    Ok(())
}

// --- library ---------------------------------------------------------------

/// Rescans the configured folder and republishes the table model.
fn refresh_library(ui: &AppWindow) {
    let folder = with_app(|app| app.settings.rom_folder.clone()).flatten();

    let entries = match folder.as_deref() {
        Some(dir) => match library::scan(std::path::Path::new(dir)) {
            Ok(e) => e,
            Err(e) => {
                log::warn!("could not scan {dir}: {e}");
                Vec::new()
            }
        },
        None => Vec::new(),
    };

    with_app(|app| {
        app.entries = entries;
        let sort = app.sort;
        library::sort_entries(&mut app.entries, sort);
        app.selected = None;
    });

    publish_library(ui);
    ui.set_folder_text(
        folder
            .map(SharedString::from)
            .unwrap_or_else(|| "no ROM folder set".into()),
    );
}

/// Pushes the current entries into the Slint model.
fn publish_library(ui: &AppWindow) {
    let rows: Vec<RomRow> = with_app(|app| {
        app.entries
            .iter()
            .map(|e| RomRow {
                file: e.file_name.clone().into(),
                title: e.title_text().into(),
                size: e.size_text().into(),
                last_played: e.last_played_text().into(),
            })
            .collect()
    })
    .unwrap_or_default();

    ui.set_roms(ModelRc::new(VecModel::from(rows)));
    let (col, asc, sel) = with_app(|app| {
        (
            app.sort.column.index(),
            app.sort.ascending,
            app.selected.map_or(-1, |i| i as i32),
        )
    })
    .unwrap_or((0, true, -1));
    ui.set_sort_column(col);
    ui.set_sort_ascending(asc);
    ui.set_selected_rom(sel);
}

/// Starts playing the ROM at `index` in the library.
fn activate_rom(ui: &AppWindow, index: usize) {
    let rom =
        with_app(|app| app.entries.get(index).map(|e| RomFile::new(e.path.clone()))).flatten();
    let Some(rom) = rom else { return };
    start_rom(ui, rom);
}

fn start_rom(ui: &AppWindow, rom: RomFile) {
    let ready = with_app(|app| {
        app.rom = Some(rom);
        app.server_ready
    })
    .unwrap_or(false);

    // If the server has not announced itself yet, the ROM simply waits in
    // `app.rom` and the Ready handler opens it.
    if ready {
        open_pending_rom(ui);
    }
}

/// Reads the ROM and hands its bytes to the server.
fn open_pending_rom(ui: &AppWindow) {
    let request = with_app(|app| {
        let rom = app.rom.as_ref()?;
        let bytes = match rom.read_rom() {
            Ok(b) => b,
            Err(e) => {
                log::error!("failed to read {}: {e}", rom.path().display());
                return Some(Err(format!("cannot read {}: {e}", rom.path().display())));
            }
        };
        let save_ram = rom.read_save_ram();
        Some(Ok(Command::OpenRom(Box::new(OpenRom {
            rom: bytes,
            save_ram,
            boot_rom: None,
            config: app.config.clone(),
            start_paused: false,
        }))))
    })
    .flatten();

    match request {
        Some(Ok(cmd)) => {
            with_app(|app| app.send(cmd));
            ui.set_playing(true);
        }
        Some(Err(message)) => ui.set_status_text(message.into()),
        None => {}
    }
}

/// Returns to the library, pausing whatever was running.
fn exit_to_library(ui: &AppWindow) {
    with_app(|app| {
        app.send(Command::RequestPersistRam);
        app.send(Command::SetRunMode(RunMode::Paused));
        app.menu_open = false;
        app.rom = None;
    });
    ui.set_menu_open(false);
    ui.set_playing(false);
    ui.set_debugging(false);
    refresh_library(ui);
}

// --- callbacks -------------------------------------------------------------

fn wire_callbacks(ui: &AppWindow) {
    ui.on_is_handled_key(|text| input::is_handled(&text));

    let weak = ui.as_weak();
    ui.on_key_pressed(move |event| {
        let Some(ui) = weak.upgrade() else { return };
        // The library screen has no joypad and no hotkeys of its own.
        if !ui.get_playing() {
            return;
        }
        let hotkey = input::hotkey(&event.text);
        with_app(|app| {
            if let Some(mask) = app.joypad.press(&event.text) {
                app.send(Command::SetJoypad(mask));
            }
        });
        if let Some(key) = hotkey {
            press_hotkey(&ui, key);
        }
    });

    ui.on_key_release(move |event| {
        with_app(|app| {
            if let Some(mask) = app.joypad.release(&event.text) {
                app.send(Command::SetJoypad(mask));
                return;
            }
            // Only the held actions do anything on release.
            match input::hotkey(&event.text) {
                Some(Hotkey::FastForward) => app.send(Command::SetFrameLimit(true)),
                Some(Hotkey::Rewind) => app.send(Command::SetRewinding(false)),
                _ => {}
            }
        });
    });

    let weak = ui.as_weak();
    ui.on_sort_by(move |index| {
        let Some(ui) = weak.upgrade() else { return };
        let Some(column) = SortColumn::from_index(index) else {
            return;
        };
        with_app(|app| {
            // Clicking the active column reverses it; a new column starts ascending.
            app.sort = if app.sort.column == column {
                Sort {
                    column,
                    ascending: !app.sort.ascending,
                }
            } else {
                Sort {
                    column,
                    ascending: true,
                }
            };
            let sort = app.sort;
            library::sort_entries(&mut app.entries, sort);
            app.selected = None;
            app.settings.set_sort(sort);
            if let Err(e) = app.settings.save() {
                log::warn!("could not save config: {e}");
            }
        });
        publish_library(&ui);
    });

    let weak = ui.as_weak();
    ui.on_select_rom(move |index| {
        with_app(|app| app.selected = usize::try_from(index).ok());
        if let Some(ui) = weak.upgrade() {
            ui.set_selected_rom(index);
        }
    });

    let weak = ui.as_weak();
    ui.on_activate_rom(move |index| {
        let Some(ui) = weak.upgrade() else { return };
        if let Ok(i) = usize::try_from(index) {
            activate_rom(&ui, i);
        }
    });

    let weak = ui.as_weak();
    ui.on_pick_rom(move || {
        let Some(ui) = weak.upgrade() else { return };
        let picked = rfd::FileDialog::new()
            .add_filter("Game Boy ROM", &["gb"])
            .pick_file();
        if let Some(path) = picked {
            start_rom(&ui, RomFile::new(path));
        }
    });

    let weak = ui.as_weak();
    ui.on_pick_folder(move || {
        let Some(ui) = weak.upgrade() else { return };
        let picked = rfd::FileDialog::new().pick_folder();
        if let Some(path) = picked {
            with_app(|app| {
                app.settings.rom_folder = Some(path.to_string_lossy().into_owned());
                if let Err(e) = app.settings.save() {
                    log::warn!("could not save config: {e}");
                }
            });
            refresh_library(&ui);
        }
    });

    ui.on_remove_breakpoint(|address| {
        if let Ok(address) = u16::try_from(address) {
            with_app(|app| app.send(Command::RemoveBreakpoint { address }));
        }
    });

    ui.on_remove_watch(|address| {
        if let Ok(address) = u16::try_from(address) {
            with_app(|app| app.send(Command::RemoveWatch { address }));
        }
    });

    let weak = ui.as_weak();
    ui.on_submit_command(move |line| {
        let Some(ui) = weak.upgrade() else { return };
        let line = line.trim().to_string();
        // Echo the input so the log reads as a transcript.
        push_console(&ui, format!("> {line}"));
        with_app(|app| app.send(Command::ExecuteDebugCommand { line }));
    });

    let weak = ui.as_weak();
    ui.on_menu_save_state(move || {
        with_app(|app| app.send(Command::RequestSaveState));
        if let Some(ui) = weak.upgrade() {
            close_menu(&ui);
        }
    });

    let weak = ui.as_weak();
    ui.on_menu_load_state(move || {
        load_state();
        if let Some(ui) = weak.upgrade() {
            close_menu(&ui);
        }
    });

    let weak = ui.as_weak();
    ui.on_menu_reset(move || {
        with_app(|app| app.send(Command::Reset));
        if let Some(ui) = weak.upgrade() {
            close_menu(&ui);
        }
    });

    let weak = ui.as_weak();
    ui.on_menu_exit(move || {
        if let Some(ui) = weak.upgrade() {
            exit_to_library(&ui);
        }
    });

    let weak = ui.as_weak();
    ui.on_menu_close(move || {
        if let Some(ui) = weak.upgrade() {
            close_menu(&ui);
        }
    });
}

fn load_state() {
    with_app(|app| {
        let Some(rom) = app.rom.as_ref() else { return };
        match rom.read_save_state() {
            Ok(data) => app.send(Command::LoadSaveState(data)),
            Err(e) => log::warn!("no save state to load: {e}"),
        }
    });
}

fn press_hotkey(ui: &AppWindow, key: Hotkey) {
    match key {
        // Held, not toggled: the key repeat re-sends, which is harmless.
        Hotkey::FastForward => {
            with_app(|app| app.send(Command::SetFrameLimit(false)));
        }
        Hotkey::Rewind => {
            with_app(|app| app.send(Command::SetRewinding(true)));
        }
        Hotkey::SaveState => {
            with_app(|app| app.send(Command::RequestSaveState));
        }
        Hotkey::LoadState => load_state(),
        Hotkey::ToggleDebugger => {
            let debugging = with_app(|app| {
                app.debugging = !app.debugging;
                app.send(Command::SetDebugViews(DebugViews {
                    cpu: app.debugging,
                    ppu: app.debugging,
                    watches: app.debugging,
                    // A window, not the whole trace. The server centres it on the
                    // PC, so following the PC costs the client nothing.
                    disassembly_lines: if app.debugging { DISASM_WINDOW } else { 0 },
                }));
                app.send(Command::SetRunMode(if app.debugging {
                    RunMode::Paused
                } else {
                    RunMode::Running
                }));
                app.debugging
            })
            .unwrap_or(false);
            ui.set_debugging(debugging);
        }
        Hotkey::DebugStep => {
            with_app(|app| app.send(Command::Step));
        }
        Hotkey::ToggleRun => {
            with_app(|app| {
                let mode = match app.mode {
                    // Continuing from the debugger must honour breakpoints;
                    // continuing while just playing must not pay for checking them.
                    RunMode::Paused if app.debugging => RunMode::RunningWithBreakpoints,
                    RunMode::Paused => RunMode::Running,
                    _ => RunMode::Paused,
                };
                app.send(Command::SetRunMode(mode));
            });
        }
        Hotkey::ToggleMenu => {
            let open = with_app(|app| {
                if app.menu_open {
                    app.menu_open = false;
                    app.send(Command::SetRunMode(app.mode_before_menu));
                } else {
                    app.menu_open = true;
                    app.mode_before_menu = app.mode;
                    app.send(Command::SetRunMode(RunMode::Paused));
                }
                app.menu_open
            })
            .unwrap_or(false);
            ui.set_menu_open(open);
        }
    }
}

/// Closes the pause menu and resumes whatever was running before it opened.
fn close_menu(ui: &AppWindow) {
    with_app(|app| {
        if app.menu_open {
            app.menu_open = false;
            app.send(Command::SetRunMode(app.mode_before_menu));
        }
    });
    ui.set_menu_open(false);
}

// --- events ----------------------------------------------------------------

/// Pulls everything the server has queued and folds it into the UI. Runs on the UI
/// thread, holds no locks, and never blocks.
fn drain_events(ui: &AppWindow) {
    with_app(|app| {
        if let Some(server) = app.server.as_ref() {
            server.begin_drain();
        }
    });
    loop {
        let next = APP.with(|cell| {
            cell.borrow()
                .as_ref()
                .and_then(|app| app.server.as_ref()?.try_recv().ok().flatten())
        });
        let Some(event) = next else { return };
        handle_event(ui, event);
    }
}

fn handle_event(ui: &AppWindow, event: Event) {
    match event {
        Event::Ready { protocol_version } => {
            log::info!("server ready, protocol v{protocol_version}");
            let has_rom = with_app(|app| {
                app.server_ready = true;
                app.rom.is_some()
            })
            .unwrap_or(false);
            if has_rom {
                open_pending_rom(ui);
            }
        }
        Event::RomOpened(opened) => {
            let title = opened.header.title.trim().to_string();
            ui.set_window_title(if title.is_empty() {
                "GameRoy".into()
            } else {
                SharedString::from(format!("{title} - GameRoy"))
            });
            log::info!(
                "loaded {:?} ({}, rom {} B, ram {} B)",
                opened.header.title,
                opened.header.kind_name,
                opened.header.rom_size,
                opened.header.ram_size
            );
        }
        Event::Frame(packet) => {
            let mut buffer =
                SharedPixelBuffer::<Rgb8Pixel>::new(packet.width as u32, packet.height as u32);
            for (out, &shade) in buffer
                .make_mut_slice()
                .iter_mut()
                .zip(packet.pixels.0.iter())
            {
                let l = PALETTE[(shade & 3) as usize];
                *out = Rgb8Pixel::new(l, l, l);
            }
            ui.set_screen(slint::Image::from_rgb8(buffer));
            // Hand the allocation back so the server can reuse it.
            with_app(|app| app.send(Command::ReturnFrame(packet.pixels)));
        }
        Event::Audio(chunk) => {
            with_app(|app| {
                if let Some(sink) = app.audio.as_mut() {
                    sink.push(&chunk.samples);
                }
            });
        }
        Event::RunState {
            mode,
            clock_count,
            reason,
            ..
        } => {
            with_app(|app| app.mode = mode);
            ui.set_status_text(format!("{mode:?} @ {clock_count}").into());
            // A breakpoint hit is worth saying out loud; the user asked for it.
            if let StopReason::Breakpoint { address, flags } = reason {
                push_console(
                    ui,
                    format!("stopped at {address:04x} ({})", format_break_flags(flags)),
                );
            }
        }
        Event::DebugUpdate(update) => {
            if let Some(cpu) = update.cpu.as_ref() {
                ui.set_cpu_text(format_cpu(cpu, update.clock_count, update.frame).into());
            }
            if let Some(ppu) = update.ppu.as_ref() {
                ui.set_ppu_text(format_ppu(ppu).into());
            }
            if let Some(disasm) = update.disassembly.as_ref() {
                let rows: Vec<DisasmRow> = disasm
                    .lines
                    .iter()
                    .map(|l| DisasmRow {
                        address_text: format!("{:04x}", l.pc).into(),
                        label: l.label.clone().unwrap_or_default().into(),
                        text: l.text.clone().into(),
                        is_current: l.is_current,
                    })
                    .collect();
                ui.set_disasm_rows(ModelRc::new(VecModel::from(rows)));
                ui.set_disasm_total(disasm.total as i32);
                ui.set_disasm_start(disasm.start_index as i32);
            }
            if let Some(watches) = update.watches.as_ref() {
                let rows: Vec<WatchRow> = watches
                    .iter()
                    .map(|w| WatchRow {
                        address: w.address as i32,
                        address_text: format!("{:04x}", w.address).into(),
                        value_text: format!("{:02x}", w.value).into(),
                    })
                    .collect();
                ui.set_watches(ModelRc::new(VecModel::from(rows)));
            }
        }
        Event::BreakpointsChanged { breakpoints } => {
            let rows: Vec<BreakRow> = breakpoints
                .iter()
                .map(|&(address, flags)| BreakRow {
                    address: address as i32,
                    address_text: format!("{address:04x}").into(),
                    flags_text: format_break_flags(flags).into(),
                })
                .collect();
            ui.set_breakpoints(ModelRc::new(VecModel::from(rows)));
        }
        Event::WatchesChanged { .. } => {
            // The values arrive with the next DebugUpdate; nothing to do here.
        }
        Event::DebugConsole { ok, text } => {
            push_console(ui, if ok { text } else { format!("error: {text}") });
        }
        Event::SaveStateData { data } => {
            with_app(|app| {
                let Some(rom) = app.rom.as_ref() else { return };
                match rom.write_save_state(&data) {
                    Ok(()) => log::info!(
                        "wrote save state to {} ({} bytes)",
                        rom.save_state_path().display(),
                        data.len()
                    ),
                    Err(e) => log::error!("failed to write save state: {e}"),
                }
            });
        }
        Event::PersistRam { data } => {
            with_app(|app| {
                let Some(rom) = app.rom.as_ref() else { return };
                match rom.write_save_ram(&data) {
                    Ok(()) => log::info!("saved {} bytes of cartridge RAM", data.len()),
                    Err(e) => log::error!("failed to save cartridge RAM: {e}"),
                }
            });
        }
        Event::Fatal(err) => {
            log::error!("server error: {err}");
            ui.set_status_text(format!("error: {err}").into());
        }
        other => log::debug!("unhandled event: {other:?}"),
    }
}

fn format_cpu(cpu: &CpuState, clock: u64, frame: u32) -> String {
    let flag = |bit: u8, name: char| if cpu.f & (1 << bit) != 0 { name } else { '-' };
    format!(
        "clock {clock}\n\
         frame {frame}\n\
         \n\
         AF {:02x}{:02x}  {}{}{}{}\n\
         BC {:02x}{:02x}\n\
         DE {:02x}{:02x}\n\
         HL {:02x}{:02x}\n\
         SP {:04x}\n\
         PC {:04x}\n\
         \n\
         IME {:?}\n\
         IF {:02x}  IE {:02x}\n\
         bank {:x}/{:x}",
        cpu.a,
        cpu.f,
        flag(7, 'Z'),
        flag(6, 'N'),
        flag(5, 'H'),
        flag(4, 'C'),
        cpu.b,
        cpu.c,
        cpu.d,
        cpu.e,
        cpu.h,
        cpu.l,
        cpu.sp,
        cpu.pc,
        cpu.ime,
        cpu.interrupt_flag,
        cpu.interrupt_enabled,
        cpu.banks.0,
        cpu.banks.1,
    )
}

/// Renders breakpoint flags as `wrxj`, dashes for the ones that are off. Formatting
/// is a front-end concern: the server sends the bitmask.
fn format_break_flags(flags: u8) -> String {
    // Mirrors gameroy::debugger::break_flags.
    const WRITE: u8 = 1 << 0;
    const READ: u8 = 1 << 1;
    const EXECUTE: u8 = 1 << 2;
    const JUMP: u8 = 1 << 3;
    let bit = |f: u8, c: char| if flags & f != 0 { c } else { '-' };
    [
        bit(WRITE, 'w'),
        bit(READ, 'r'),
        bit(EXECUTE, 'x'),
        bit(JUMP, 'j'),
    ]
    .iter()
    .collect()
}

/// Maximum console scrollback. Beyond this the oldest lines are dropped, so a long
/// debugging session cannot grow the model without bound.
const CONSOLE_LIMIT: usize = 200;

fn push_console(ui: &AppWindow, line: String) {
    let lines = with_app(|app| {
        app.console.push(line);
        let overflow = app.console.len().saturating_sub(CONSOLE_LIMIT);
        if overflow > 0 {
            app.console.drain(0..overflow);
        }
        app.console
            .iter()
            .map(SharedString::from)
            .collect::<Vec<_>>()
    })
    .unwrap_or_default();
    ui.set_console_lines(ModelRc::new(VecModel::from(lines)));
}

fn format_ppu(ppu: &PpuState) -> String {
    format!(
        "LCDC {:02x}  STAT {:02x}\n\
         SCY  {:02x}  SCX  {:02x}\n\
         LY   {:02x}  LYC  {:02x}\n\
         LX   {:>3}\n\
         BGP  {:02x}\n\
         OBP0 {:02x}  OBP1 {:02x}\n\
         WY   {:02x}  WX   {:02x}\n\
         WYC  {:02x}\n\
         state {}\n\
         next  {}",
        ppu.lcdc,
        ppu.stat,
        ppu.scy,
        ppu.scx,
        ppu.ly,
        ppu.lyc,
        ppu.lx,
        ppu.bgp,
        ppu.obp0,
        ppu.obp1,
        ppu.wy,
        ppu.wx,
        ppu.wyc,
        ppu.state,
        ppu.next_clock_count,
    )
}
