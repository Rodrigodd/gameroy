// Prevent console window in addition to Slint window in Windows release builds when, e.g., starting the app via file manager. Ignored on other platforms.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod audio;
mod config;
mod disasm;
mod format;
mod input;
mod library;
mod library_loader;
mod server_handle;
mod storage;
mod thumbs;

use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;

use gameroy_server::protocol::{
    Address, Command, DebugViews, DisasmAnchor, EmulatorConfig, Event, OpenRom, RunMode, StopReason,
};
use slint::{ModelRc, Rgb8Pixel, Rgba8Pixel, SharedPixelBuffer, SharedString, VecModel};

use audio::{AudioEngineSink, AudioSink};
use config::Config;
use disasm::DisasmModel;
use format::{
    PALETTE, format_break_flags, format_cpu, format_ppu, indexed_to_image, partial_screen_image,
    sprite_cells,
};
use input::{HoldKey, Joypad};
use library::{RomEntry, Sort, SortColumn};
use library_loader::{LibraryLoader, LibraryMsg};
use server_handle::ServerHandle;
use storage::RomFile;

slint::include_modules!();

/// How many disassembly lines to keep around the PC. Enough to fill a tall panel
/// without shipping a large slice on every single step.
const DISASM_WINDOW: u16 = 64;

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
    /// Submitted command lines, oldest first, for the Up/Down keys.
    history: Vec<String>,
    /// Where the Up/Down keys currently sit in `history`; `len()` means "at the
    /// empty line below the newest entry".
    history_pos: usize,
    /// Where a pending `dump` should write, once the server sends the text.
    pending_dump: Option<std::path::PathBuf>,
    /// The PPU viewer tab is open, so its images are worth generating.
    ppu_viewer: bool,
    /// The disassembly list's backing model, shared with Slint.
    disasm: Rc<DisasmModel>,
    /// Bumped to make the view re-centre; see `focus-token` in disassembly.slint.
    disasm_focus_token: i32,
    /// The view follows the PC on each pause. Cleared when the user scrolls or
    /// follows a link, so browsing is not yanked away by the next step, and
    /// re-armed by stepping.
    disasm_following_pc: bool,
    /// Identifies our disassembly requests; only the newest answer is applied.
    disasm_request: u64,
    /// The session the front-end believes it is looking at. The server bumps this
    /// on every reset, state load and ROM open, and anything older describes a
    /// machine that no longer exists.
    epoch: u64,
    breakpoints: Vec<u16>,
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
            history: Vec::new(),
            history_pos: 0,
            pending_dump: None,
            ppu_viewer: false,
            disasm: Rc::new(DisasmModel::default()),
            disasm_focus_token: 0,
            disasm_following_pc: true,
            disasm_request: 0,
            epoch: 0,
            breakpoints: Vec::new(),
            mode_before_menu: RunMode::Running,
            rom: cli_rom,
            server_ready: false,
        })
    });

    ui.set_integer_scaling(with_app(|app| app.settings.only_integer_scaling).unwrap_or(false));

    // Handed over once: the model is mutated in place afterwards and notifies
    // Slint itself, so the rows are never republished as a fresh VecModel.
    if let Some(model) = with_app(|app| app.disasm.clone()) {
        ui.set_disasm_rows(ModelRc::from(model));
    }

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
        // The listing describes a machine that is about to go away.
        app.disasm.clear();
        app.breakpoints.clear();
        app.send(Command::RequestPersistRam);
        app.send(Command::SetRunMode(RunMode::Paused));
        app.menu_open = false;
        app.rom = None;
    });
    ui.set_menu_open(false);
    ui.set_playing(false);
    ui.set_debugging(false);
    ui.set_disasm_total(0);
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

    // Stepping is a statement about where you want to be looking, so it re-arms
    // the follow-the-PC behaviour a scroll or a jump turned off.
    ui.on_debug_step(|| {
        with_app(|app| {
            app.disasm_following_pc = true;
            app.send(Command::Step);
        });
    });

    ui.on_debug_step_back(|| {
        with_app(|app| {
            app.disasm_following_pc = true;
            app.send(Command::StepBack);
        });
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

    let weak = ui.as_weak();
    ui.on_disasm_jump_to(move |index, bank, address| {
        let Some(ui) = weak.upgrade() else { return };
        with_app(|app| {
            // Following a link means the user has chosen where to look.
            app.disasm_following_pc = false;
            app.disasm_request += 1;
            // The index is only valid while the trace has not grown under us, so
            // the address is what gets sent; the index just lets the view move
            // now rather than after the round trip.
            app.send(Command::RequestDisassembly {
                id: app.disasm_request,
                anchor: DisasmAnchor::Address(Address {
                    bank: bank.max(0) as u16,
                    address: address.max(0) as u16,
                }),
                count: disasm::CHUNK,
            });
        });
        if index >= 0 {
            focus_disassembly(&ui, index);
        }
    });

    ui.on_disasm_toggle_breakpoint(|address| {
        let Ok(address) = u16::try_from(address) else {
            return;
        };
        with_app(|app| {
            if app.breakpoints.contains(&address) {
                app.send(Command::RemoveBreakpoint { address });
            } else {
                // Execute: the only flag that means anything for a row in the
                // listing, which is an instruction rather than a data address.
                app.send(Command::AddBreakpoint {
                    address,
                    flags: BREAK_EXECUTE,
                });
            }
        });
    });

    ui.on_disasm_visible_range_changed(|first, count| {
        with_app(|app| {
            let (first, count) = (first.max(0) as u32, count.max(0) as u32);
            let Some((start, want)) =
                disasm::window_needed(first, count, app.disasm.cached(), app.disasm.total())
            else {
                return;
            };
            app.disasm_request += 1;
            app.send(Command::RequestDisassembly {
                id: app.disasm_request,
                anchor: DisasmAnchor::Index(start),
                count: want,
            });
        });
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
        with_app(|app| {
            if !line.is_empty() && app.history.last() != Some(&line) {
                app.history.push(line.clone());
            }
            app.history_pos = app.history.len();
        });
        // `dump` and `save` write files, which is this side's job; everything
        // else is the server's.
        if let Some(rest) = line.strip_prefix("dump") {
            dump_disassembly(&ui, rest.trim());
            return;
        }
        with_app(|app| app.send(Command::ExecuteDebugCommand { line }));
    });

    ui.on_history(|direction| {
        with_app(|app| {
            if app.history.is_empty() {
                return SharedString::new();
            }
            if direction < 0 {
                app.history_pos = app.history_pos.saturating_sub(1);
            } else {
                // Stepping past the newest entry lands on the empty line, which
                // is how you get back to a blank prompt without clearing.
                app.history_pos = (app.history_pos + 1).min(app.history.len());
            }
            app.history
                .get(app.history_pos)
                .map(SharedString::from)
                .unwrap_or_default()
        })
        .unwrap_or_default()
    });

    ui.on_add_breakpoint(|text| {
        // Reuse the console's parser rather than growing a second one here.
        let line = format!("break {}", text.trim());
        with_app(|app| app.send(Command::ExecuteDebugCommand { line }));
    });

    ui.on_add_watch(|text| {
        let line = format!("watch {}", text.trim());
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

/// Asks the server for the whole listing, to be written once it arrives.
///
/// The path defaults beside the ROM, so a bare `dump` does something sensible.
fn dump_disassembly(ui: &AppWindow, path: &str) {
    let target = with_app(|app| {
        if !path.is_empty() {
            return Some(std::path::PathBuf::from(path));
        }
        Some(app.rom.as_ref()?.path().with_extension("asm"))
    })
    .flatten();

    let Some(target) = target else {
        push_console(ui, "error: no ROM open, and no path given".into());
        return;
    };
    with_app(|app| {
        app.pending_dump = Some(target);
        app.send(Command::RequestDisassemblyDump);
    });
}

/// Mirrors `gameroy::debugger::break_flags::EXECUTE`.
const BREAK_EXECUTE: u8 = 1 << 2;

/// Scrolls the listing to `index`.
///
/// The token is what actually triggers the scroll, because the index alone does
/// not change when stepping inside a one-instruction loop, and that still has to
/// re-centre.
fn focus_disassembly(ui: &AppWindow, index: i32) {
    ui.set_disasm_focus_index(index);
    let token = with_app(|app| {
        app.disasm_focus_token = app.disasm_focus_token.wrapping_add(1);
        app.disasm_focus_token
    })
    .unwrap_or_default();
    ui.set_disasm_focus_token(token);
}

fn tile_numbers(tiles: &[u16]) -> ModelRc<i32> {
    ModelRc::new(VecModel::from(
        tiles.iter().map(|&t| t as i32).collect::<Vec<_>>(),
    ))
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
            with_app(|app| app.epoch = opened.epoch);
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
            epoch,
            mode,
            clock_count,
            reason,
            ..
        } => {
            with_app(|app| {
                app.epoch = epoch;
                app.mode = mode;
            });
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
            // A snapshot of a machine that has since been reset or reloaded is
            // not stale data to show briefly; it is wrong data.
            if with_app(|app| update.epoch < app.epoch).unwrap_or(false) {
                return;
            }
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
                ui.set_ppu_tall_sprites(view.tall_sprites);
                ui.set_ppu_background_tiles(tile_numbers(&view.background_tiles));
                ui.set_ppu_window_tiles(tile_numbers(&view.window_tiles));
                ui.set_ppu_background_fifo(
                    format::format_fifo("bg ", &view.background_fifo).into(),
                );
                ui.set_ppu_sprite_fifo(format::format_fifo("obj", &view.sprite_fifo).into());
            }
            if let Some(disasm) = update.disassembly.as_ref() {
                with_app(|app| app.disasm.set_window(disasm));
                ui.set_disasm_total(disasm.total as i32);
                // Re-centre on the PC only while the user is following it.
                if let Some(pc_index) = disasm.pc_index {
                    let following = with_app(|app| app.disasm_following_pc).unwrap_or(false);
                    if following {
                        focus_disassembly(ui, pc_index as i32);
                    }
                }
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
        Event::Disassembly { id, epoch, window } => {
            if with_app(|app| epoch < app.epoch).unwrap_or(false) {
                return;
            }
            // Only the newest request's answer is interesting; an older one
            // describes a scroll position the user has already left.
            let current = with_app(|app| app.disasm_request).unwrap_or(0);
            if id != current {
                return;
            }
            with_app(|app| app.disasm.set_window(&window));
            ui.set_disasm_total(window.total as i32);
            if let Some(focus) = window.focus_index {
                focus_disassembly(ui, focus as i32);
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
            with_app(|app| {
                app.breakpoints = breakpoints.iter().map(|&(a, _)| a).collect();
                app.disasm.set_breakpoints(app.breakpoints.clone());
            });
        }
        Event::WatchesChanged { .. } => {
            // The values arrive with the next DebugUpdate; nothing to do here.
        }
        Event::DisassemblyDump { text } => {
            let target = with_app(|app| app.pending_dump.take()).flatten();
            let Some(target) = target else { return };
            match std::fs::write(&target, text) {
                Ok(()) => push_console(ui, format!("wrote {}", target.display())),
                Err(e) => push_console(
                    ui,
                    format!("error: could not write {}: {e}", target.display()),
                ),
            }
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
