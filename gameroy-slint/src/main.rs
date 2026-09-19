// Prevent console window in addition to Slint window in Windows release builds when, e.g., starting the app via file manager. Ignored on other platforms.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod audio;
mod config;
mod input;
mod library;
mod library_loader;
mod server_handle;
mod storage;
mod thumbs;

use std::cell::RefCell;
use std::error::Error;

use gameroy_server::protocol::{
    Command, CpuState, DebugViews, EmulatorConfig, Event, IndexedImage, OpenRom, PpuState, RunMode,
    SpriteInfo, StopReason,
};
use slint::{ModelRc, Rgb8Pixel, Rgba8Pixel, SharedPixelBuffer, SharedString, VecModel};

use audio::{AudioEngineSink, AudioSink};
use config::Config;
use input::{HoldKey, Joypad};
use library::{RomEntry, Sort, SortColumn};
use library_loader::{LibraryLoader, LibraryMsg};
use server_handle::ServerHandle;
use storage::RomFile;

slint::include_modules!();

/// How many disassembly lines to keep around the PC. Enough to fill a tall panel
/// without shipping a large slice on every single step.
const DISASM_WINDOW: u16 = 64;

/// The four Game Boy shades, lightest first, as the old front-end had them.
const PALETTE: [u8; 4] = [255, 180, 100, 0];

/// The same four shades tinted blue, for the part of a paused screen that the
/// frame in progress has not reached yet and so still holds the previous frame.
/// Blue because the Game Boy's own output is grey, so nothing else can be.
const STALE_PALETTE: [(u8, u8, u8); 4] =
    [(120, 140, 255), (85, 100, 190), (48, 58, 125), (14, 18, 60)];

/// UI-thread state. The server thread never touches this; it only posts a wake-up
/// and the draining happens here.
struct App {
    server: Option<ServerHandle>,
    audio: Option<AudioEngineSink>,
    config: EmulatorConfig,
    settings: Config,

    /// In scan order, never permuted: the loader addresses rows by their index
    /// here, and a message in flight must not land on somebody else's row.
    entries: Vec<RomEntry>,
    /// Indices into `entries`, in the order the table shows them.
    order: Vec<usize>,
    /// Box art per entry, parallel to `entries`. Kept out of `RomEntry` so the
    /// library module stays free of Slint and testable without a window.
    thumbs: Vec<Option<slint::Image>>,
    /// The scan in progress. Dropping it cancels the workers.
    loader: Option<LibraryLoader>,
    sort: Sort,
    /// A position in `order`, not an index into `entries`; it is reset whenever
    /// the order changes.
    selected: Option<usize>,

    /// None while the library is showing.
    rom: Option<RomFile>,
    joypad: Joypad,
    debugging: bool,
    mode: RunMode,
    menu_open: bool,
    /// Console scrollback. Bounded so a long session cannot grow without limit.
    console: Vec<String>,
    /// The PPU viewer tab is open, so its images are worth generating.
    ppu_viewer: bool,
    /// Run mode to restore when the pause menu closes.
    mode_before_menu: RunMode,
    /// The server announces itself with Ready; a ROM chosen before that (from the
    /// command line) waits in `rom` until then.
    server_ready: bool,
}

impl App {
    /// Recomputes the display order from the current entries and sort.
    fn reorder(&mut self) {
        self.order = library::sort_order(&self.entries, self.sort);
    }

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

    // Pin the backend before the first window exists. `on_winit_window_event`
    // below is a silent no-op under any other backend, and a drag-and-drop that
    // quietly does nothing is worse than refusing to start.
    slint::BackendSelector::new()
        .backend_name("winit".into())
        .select()?;

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
            order: Vec::new(),
            thumbs: Vec::new(),
            loader: None,
            selected: None,
            joypad: Joypad::default(),
            debugging: false,
            mode: RunMode::Paused,
            menu_open: false,
            console: Vec::new(),
            ppu_viewer: false,
            mode_before_menu: RunMode::Running,
            rom: cli_rom,
            server_ready: false,
        })
    });

    ui.set_integer_scaling(with_app(|app| app.settings.only_integer_scaling).unwrap_or(false));

    wire_callbacks(&ui);
    wire_file_drop(&ui);
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

/// Accepts a ROM dragged onto the window.
///
/// This reaches through to winit rather than using Slint's `DropArea`, because in
/// Slint 1.18 only the Qt backend ever feeds that element an OS drop -- the winit
/// backend never calls `process_drag_event`, and there is no public API to inject
/// one. `unstable-winit-030` is documented as removable in a future minor release,
/// so a Slint upgrade may well break this function; nothing else depends on it.
fn wire_file_drop(ui: &AppWindow) {
    use slint::winit_030::{EventResult, WinitWindowAccessor, winit};

    let weak = ui.as_weak();
    ui.window().on_winit_window_event(move |_window, event| {
        let Some(ui) = weak.upgrade() else {
            return EventResult::Propagate;
        };
        match event {
            winit::event::WindowEvent::DroppedFile(path) => {
                ui.set_drop_hover(false);
                if storage::is_rom_path(path) {
                    // `start_rom` parks the ROM until `Event::Ready`, so a drop
                    // before the server announces itself works like any other way
                    // of choosing one.
                    start_rom(&ui, RomFile::new(path.clone()));
                } else {
                    ui.set_status_text(format!("{} is not a .gb ROM", path.display()).into());
                }
            }
            winit::event::WindowEvent::HoveredFile(path) => {
                ui.set_drop_hover(storage::is_rom_path(path));
            }
            winit::event::WindowEvent::HoveredFileCancelled => ui.set_drop_hover(false),
            _ => {}
        }
        EventResult::Propagate
    });
}

// --- library ---------------------------------------------------------------

/// Rescans the configured folder and republishes the table model.
///
/// Only the folder listing happens here. Headers and thumbnails arrive from
/// [`LibraryLoader`], so a folder of several hundred ROMs draws immediately
/// instead of after every one of them has been opened.
fn refresh_library(ui: &AppWindow) {
    let folder = with_app(|app| app.settings.rom_folder.clone()).flatten();

    let paths = match folder.as_deref() {
        Some(dir) => match library::list_roms(std::path::Path::new(dir)) {
            Ok(p) => p,
            Err(e) => {
                log::warn!("could not scan {dir}: {e}");
                Vec::new()
            }
        },
        None => Vec::new(),
    };

    with_app(|app| {
        // Assigning the new loader drops the old one, which cancels its workers.
        // Anything they still post lands in the queue they own, which dies with
        // them, so a stale message can never reach the new list.
        app.loader = None;
        app.thumbs = vec![None; paths.len()];
        app.entries = paths.iter().cloned().map(RomEntry::pending).collect();
        app.reorder();
        app.selected = None;
    });

    publish_library(ui);
    ui.set_folder_text(
        folder
            .map(SharedString::from)
            .unwrap_or_else(|| "no ROM folder set".into()),
    );

    if !paths.is_empty() {
        let weak = ui.as_weak();
        let wake = move || {
            let weak = weak.clone();
            let _ = slint::invoke_from_event_loop(move || {
                if let Some(ui) = weak.upgrade() {
                    drain_library(&ui);
                }
            });
        };
        with_app(|app| app.loader = Some(LibraryLoader::spawn(paths, wake)));
    }
}

/// Folds in whatever the loader has found since the last drain.
fn drain_library(ui: &AppWindow) {
    with_app(|app| {
        let Some(loader) = app.loader.as_ref() else {
            return;
        };
        loader.begin_drain();
        let messages = loader.drain();
        if messages.is_empty() {
            return;
        }
        for message in messages {
            match message {
                LibraryMsg::Header {
                    index,
                    header,
                    last_played,
                } => {
                    if let Some(entry) = app.entries.get_mut(index) {
                        entry.apply(header, last_played);
                    }
                }
                LibraryMsg::Thumb {
                    index,
                    width,
                    height,
                    rgba,
                } => {
                    if let Some(slot) = app.thumbs.get_mut(index) {
                        let buffer =
                            SharedPixelBuffer::<Rgba8Pixel>::clone_from_slice(&rgba, width, height);
                        *slot = Some(slint::Image::from_rgba8(buffer));
                    }
                }
            }
        }
        // A title or size that only just arrived can change where its row
        // belongs, so the order is recomputed rather than left stale. Rows do
        // shuffle while a folder loads; a list that is sorted only once the
        // data is complete would be worse.
        app.reorder();
    });
    publish_library(ui);
}

/// Pushes the current entries into the Slint model, in display order.
fn publish_library(ui: &AppWindow) {
    let rows: Vec<RomRow> = with_app(|app| {
        app.order
            .iter()
            .map(|&i| {
                let e = &app.entries[i];
                RomRow {
                    file: e.file_name.clone().into(),
                    title: e.title_text().into(),
                    size: e.size_text().into(),
                    last_played: e.last_played_text().into(),
                    thumb: app.thumbs[i].clone().unwrap_or_default(),
                    loading: e.loading,
                }
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

/// Starts playing the ROM at display position `row`.
fn activate_rom(ui: &AppWindow, row: usize) {
    let rom = with_app(|app| {
        let &index = app.order.get(row)?;
        Some(RomFile::new(app.entries[index].path.clone()))
    })
    .flatten();
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

    // --- library ---

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
            app.reorder();
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

    // --- input ---

    // Joypad and hold actions need to know whether the key is down, so they stay
    // on the press/release callbacks. Everything momentary is a KeyBinding.
    ui.on_key_pressed(move |event| {
        with_app(|app| {
            if let Some(mask) = app.joypad.press(&event.text) {
                app.send(Command::SetJoypad(mask));
                return;
            }
            // Auto-repeat re-sends these; harmless, since both are idempotent.
            match input::hold_key(&event.text) {
                Some(HoldKey::FastForward) => app.send(Command::SetFrameLimit(false)),
                Some(HoldKey::Rewind) => app.send(Command::SetRewinding(true)),
                None => {}
            }
        });
    });

    ui.on_key_release(move |event| {
        with_app(|app| {
            if let Some(mask) = app.joypad.release(&event.text) {
                app.send(Command::SetJoypad(mask));
                return;
            }
            match input::hold_key(&event.text) {
                Some(HoldKey::FastForward) => app.send(Command::SetFrameLimit(true)),
                Some(HoldKey::Rewind) => app.send(Command::SetRewinding(false)),
                None => {}
            }
        });
    });

    // --- shortcuts, driven by KeyBinding in the .slint file ---

    ui.on_save_state(|| {
        with_app(|app| app.send(Command::RequestSaveState));
    });

    ui.on_load_state(load_state);

    ui.on_debug_step(|| {
        with_app(|app| app.send(Command::Step));
    });

    ui.on_debug_step_back(|| {
        with_app(|app| app.send(Command::StepBack));
    });

    ui.on_toggle_run(|| {
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
    });

    let weak = ui.as_weak();
    ui.on_toggle_debugger(move || {
        let Some(ui) = weak.upgrade() else { return };
        let debugging = with_app(|app| {
            app.debugging = !app.debugging;
            app.send(Command::SetDebugViews(debug_views(app)));
            app.send(Command::SetRunMode(if app.debugging {
                RunMode::Paused
            } else {
                RunMode::Running
            }));
            app.debugging
        })
        .unwrap_or(false);
        ui.set_debugging(debugging);
    });

    let weak = ui.as_weak();
    ui.on_toggle_menu(move || {
        let Some(ui) = weak.upgrade() else { return };
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
    });

    ui.on_ppu_viewer_toggled(|shown| {
        with_app(|app| {
            app.ppu_viewer = shown;
            // Re-declare the subscription so the server starts or stops paying for
            // the four image buffers.
            app.send(Command::SetDebugViews(debug_views(app)));
        });
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

/// The view subscription implied by the current UI state.
fn debug_views(app: &App) -> DebugViews {
    DebugViews {
        cpu: app.debugging,
        ppu: app.debugging,
        watches: app.debugging,
        // Whoever has the debugger open is the person who wants to watch the
        // frame come out a scanline at a time; it costs one screen buffer.
        screen: app.debugging,
        disassembly_lines: if app.debugging { DISASM_WINDOW } else { 0 },
        ppu_viewer: app.debugging && app.ppu_viewer,
    }
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
            // A mismatch is not survivable: the two sides would misparse each
            // other's messages rather than fail, which is the whole reason the
            // version is announced. Logging it and carrying on, as this used to,
            // turns a clear error into a puzzling one.
            if protocol_version != gameroy_server::protocol::PROTOCOL_VERSION {
                let message = format!(
                    "server speaks protocol v{protocol_version}, this front-end speaks v{}",
                    gameroy_server::protocol::PROTOCOL_VERSION
                );
                log::error!("{message}");
                ui.set_status_text(message.into());
                with_app(|app| app.server = None);
                return;
            }
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
            // Only while stopped. A views change made mid-play also produces an
            // update, and painting a half-drawn frame over the live one would be
            // a visible flicker for no reason.
            if let Some(screen) = update.screen.as_ref()
                && with_app(|app| app.mode == RunMode::Paused).unwrap_or(false)
            {
                ui.set_screen(partial_screen_image(screen));
            }
            if let Some(cpu) = update.cpu.as_ref() {
                ui.set_cpu_text(format_cpu(cpu, update.clock_count, update.frame).into());
            }
            if let Some(ppu) = update.ppu.as_ref() {
                ui.set_ppu_text(format_ppu(ppu).into());
            }
            if let Some(view) = update.ppu_viewer.as_ref() {
                ui.set_ppu_tiles(indexed_to_image(&view.tiles));
                ui.set_ppu_background(indexed_to_image(&view.background));
                ui.set_ppu_window(indexed_to_image(&view.window));
                ui.set_ppu_oam(ModelRc::new(VecModel::from(sprite_cells(&view.oam))));
                ui.set_ppu_sprite_buffer(ModelRc::new(VecModel::from(sprite_cells(
                    &view.sprite_buffer,
                ))));
                ui.set_ppu_sprite_buffer_len(view.sprite_buffer_len as i32);
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

/// The paused screen, with the pixels this frame has not reached yet tinted.
///
/// Those hold whatever the previous frame left there, so showing them in the
/// normal palette would be showing stale output as if it were current -- the
/// distinction is the entire point of the view.
fn partial_screen_image(screen: &gameroy_server::protocol::PartialScreen) -> slint::Image {
    let img = &screen.pixels;
    let mut buffer = SharedPixelBuffer::<Rgb8Pixel>::new(img.width as u32, img.height as u32);
    let drawn = screen.drawn_pixels as usize;
    for (i, (out, &shade)) in buffer
        .make_mut_slice()
        .iter_mut()
        .zip(img.data.iter())
        .enumerate()
    {
        let shade = (shade & 3) as usize;
        *out = if i < drawn {
            let l = PALETTE[shade];
            Rgb8Pixel::new(l, l, l)
        } else {
            let (r, g, b) = STALE_PALETTE[shade];
            Rgb8Pixel::new(r, g, b)
        };
    }
    slint::Image::from_rgb8(buffer)
}

/// Expands a palette-index image into RGB for display. The server ships indices,
/// so the palette stays a front-end decision.
fn indexed_to_image(img: &IndexedImage) -> slint::Image {
    let mut buffer = SharedPixelBuffer::<Rgb8Pixel>::new(img.width as u32, img.height as u32);
    for (out, &shade) in buffer.make_mut_slice().iter_mut().zip(img.data.iter()) {
        let l = PALETTE[(shade & 3) as usize];
        *out = Rgb8Pixel::new(l, l, l);
    }
    slint::Image::from_rgb8(buffer)
}

fn sprite_cells(sprites: &[SpriteInfo]) -> Vec<SpriteCell> {
    sprites
        .iter()
        .enumerate()
        .map(|(i, s)| SpriteCell {
            text: format!(
                "{i:>2}  x {:>3}  y {:>3}  tile {:02x}  flags {:02x}",
                s.sx, s.sy, s.tile, s.flags
            )
            .into(),
            offscreen: s.is_offscreen(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use gameroy_server::protocol::{IndexedImage, PartialScreen};

    /// The boundary is the whole point: everything before `drawn_pixels` is this
    /// frame's output and must read as the Game Boy's grey, everything after is
    /// the previous frame's and must not.
    #[test]
    fn a_partial_screen_tints_only_what_the_frame_has_not_reached() {
        let screen = PartialScreen {
            pixels: IndexedImage {
                width: 4,
                height: 2,
                // Same shade throughout, so only the palette can differ.
                data: vec![0; 8],
            },
            drawn_pixels: 5,
        };

        let image = partial_screen_image(&screen);
        let buffer = image.to_rgb8().expect("built from rgb8");
        let pixels: Vec<_> = buffer.as_slice().to_vec();
        assert_eq!(pixels.len(), 8);

        for (i, p) in pixels.iter().enumerate() {
            let grey = p.r == p.g && p.g == p.b;
            if i < 5 {
                assert!(grey, "pixel {i} is inside the drawn prefix but was tinted");
            } else {
                assert!(!grey, "pixel {i} is stale but was left untinted");
            }
        }
    }
}
