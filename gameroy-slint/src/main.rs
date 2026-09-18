// Prevent console window in addition to Slint window in Windows release builds when, e.g., starting the app via file manager. Ignored on other platforms.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod audio;
mod input;
mod server_handle;
mod storage;

use std::cell::RefCell;
use std::error::Error;

use gameroy_server::protocol::{
    Command, CpuState, DebugViews, EmulatorConfig, Event, OpenRom, RunMode,
};
use slint::{Rgb8Pixel, SharedPixelBuffer};

use audio::{AudioEngineSink, AudioSink};
use input::{Hotkey, Joypad};
use server_handle::ServerHandle;
use storage::RomFile;

slint::include_modules!();

/// The four Game Boy shades, darkest last, as the old front-end had them.
const PALETTE: [u8; 4] = [255, 180, 100, 0];

/// UI-thread state. Single-threaded: the server thread never touches this, it only
/// posts a wake-up and the draining happens here.
struct App {
    server: Option<ServerHandle>,
    audio: Option<AudioEngineSink>,
    config: EmulatorConfig,
    rom: RomFile,
    joypad: Joypad,
    debugging: bool,
    mode: RunMode,
    opened: bool,
    menu_open: bool,
    /// Run mode to restore when the pause menu closes.
    mode_before_menu: RunMode,
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    // The server has no filesystem, so the ROM is resolved and read here.
    let rom_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "roms/zelda.gb".to_string());
    let rom = RomFile::new(&rom_path);

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

    // Open the device first: its rate is what the emulator must synthesize at, and
    // the server needs it in the config it starts with.
    let audio = AudioEngineSink::new();
    let config = EmulatorConfig {
        audio_sample_rate: audio.as_ref().map_or(0, |a| a.sample_rate()),
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
            rom,
            joypad: Joypad::default(),
            debugging: false,
            mode: RunMode::Paused,
            opened: false,
            menu_open: false,
            mode_before_menu: RunMode::Running,
        })
    });

    wire_callbacks(&ui);

    ui.run()?;

    // Draining during shutdown is what persists the save: the server emits its
    // final PersistRam after receiving Shutdown and before exiting.
    let taken = APP.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let app = borrow.as_mut()?;
        Some((app.server.take()?, app.rom.save_ram_path()))
    });
    if let Some((mut server, save_path)) = taken {
        server.shutdown(|ev| {
            if let Event::PersistRam { data } = ev {
                match std::fs::write(&save_path, &data) {
                    Ok(()) => log::info!("wrote {} ({} bytes)", save_path.display(), data.len()),
                    Err(e) => log::error!("failed to write {}: {e}", save_path.display()),
                }
            }
        });
    }

    Ok(())
}

thread_local! {
    static APP: RefCell<Option<App>> = const { RefCell::new(None) };
}

fn wire_callbacks(ui: &AppWindow) {
    ui.on_is_handled_key(|text| input::is_handled(&text));

    let weak = ui.as_weak();
    ui.on_key_pressed(move |event| {
        let Some(ui) = weak.upgrade() else { return };
        with_app(|app| {
            if let Some(mask) = app.joypad.press(&event.text) {
                app.send(Command::SetJoypad(mask));
                return;
            }
            if let Some(key) = input::hotkey(&event.text) {
                press_hotkey(app, &ui, key);
            }
        })
    });

    let weak = ui.as_weak();
    ui.on_key_release(move |event| {
        let Some(ui) = weak.upgrade() else { return };
        let _ = &ui;
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
        })
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
        with_app(|app| match app.rom.read_save_state() {
            Ok(data) => app.send(Command::LoadSaveState(data)),
            Err(e) => log::warn!("no save state to load: {e}"),
        });
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
    ui.on_menu_close(move || {
        if let Some(ui) = weak.upgrade() {
            close_menu(&ui);
        }
    });
}

fn press_hotkey(app: &mut App, ui: &AppWindow, key: Hotkey) {
    match key {
        // Held, not toggled: repeats while the key is down, which is harmless.
        Hotkey::FastForward => app.send(Command::SetFrameLimit(false)),
        Hotkey::Rewind => app.send(Command::SetRewinding(true)),
        Hotkey::SaveState => app.send(Command::RequestSaveState),
        Hotkey::LoadState => match app.rom.read_save_state() {
            Ok(data) => app.send(Command::LoadSaveState(data)),
            Err(e) => log::warn!("no save state to load: {e}"),
        },
        Hotkey::ToggleDebugger => {
            app.debugging = !app.debugging;
            app.send(Command::SetDebugViews(DebugViews { cpu: app.debugging }));
            app.send(Command::SetRunMode(if app.debugging {
                RunMode::Paused
            } else {
                RunMode::Running
            }));
            if !app.debugging {
                ui.set_debug_text("".into());
            }
        }
        Hotkey::DebugStep => app.send(Command::Step),
        Hotkey::ToggleRun => {
            let mode = match app.mode {
                RunMode::Paused => RunMode::Running,
                _ => RunMode::Paused,
            };
            app.send(Command::SetRunMode(mode));
        }
        Hotkey::ToggleMenu => {
            if app.menu_open {
                app.menu_open = false;
                app.send(Command::SetRunMode(app.mode_before_menu));
            } else {
                app.menu_open = true;
                app.mode_before_menu = app.mode;
                app.send(Command::SetRunMode(RunMode::Paused));
            }
            ui.set_menu_open(app.menu_open);
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

impl App {
    fn send(&self, cmd: Command) {
        if let Some(server) = self.server.as_ref() {
            server.send(cmd);
        }
    }
}

fn with_app(f: impl FnOnce(&mut App)) {
    APP.with(|cell| {
        if let Some(app) = cell.borrow_mut().as_mut() {
            f(app)
        }
    })
}

/// Pulls everything the server has queued and folds it into the UI. Runs on the UI
/// thread, holds no locks, and never blocks.
fn drain_events(ui: &AppWindow) {
    APP.with(|cell| {
        if let Some(server) = cell.borrow().as_ref().and_then(|a| a.server.as_ref()) {
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
            open_rom(ui);
        }
        Event::RomOpened(opened) => {
            let title = opened.header.title.trim().to_string();
            ui.set_window_title(if title.is_empty() {
                "GameRoy".into()
            } else {
                slint::SharedString::from(format!("{title} - GameRoy"))
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
        Event::RunState {
            mode, clock_count, ..
        } => {
            with_app(|app| app.mode = mode);
            ui.set_status_text(format!("{mode:?} @ {clock_count}").into());
        }
        Event::DebugUpdate(update) => {
            if let Some(cpu) = update.cpu {
                ui.set_debug_text(format_cpu(&cpu, update.clock_count, update.frame).into());
            }
        }
        Event::Audio(chunk) => {
            with_app(|app| {
                if let Some(sink) = app.audio.as_mut() {
                    sink.push(&chunk.samples);
                }
            });
        }
        Event::SaveStateData { data } => {
            with_app(|app| match app.rom.write_save_state(&data) {
                Ok(()) => log::info!(
                    "wrote save state to {} ({} bytes)",
                    app.rom.save_state_path().display(),
                    data.len()
                ),
                Err(e) => log::error!("failed to write save state: {e}"),
            });
        }
        Event::PersistRam { data } => {
            with_app(|app| match app.rom.write_save_ram(&data) {
                Ok(()) => log::info!("saved {} bytes of cartridge RAM", data.len()),
                Err(e) => log::error!("failed to save cartridge RAM: {e}"),
            });
        }
        Event::Fatal(err) => {
            log::error!("server error: {err}");
            ui.set_status_text(format!("error: {err}").into());
        }
        other => log::debug!("unhandled event: {other:?}"),
    }
}

fn open_rom(ui: &AppWindow) {
    let request = APP.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let app = borrow.as_mut()?;
        if app.opened {
            return None;
        }
        let rom = match app.rom.read_rom() {
            Ok(bytes) => bytes,
            Err(e) => {
                log::error!("failed to read {}: {e}", app.rom.path().display());
                return Some(Err(format!(
                    "cannot read {}: {e}",
                    app.rom.path().display()
                )));
            }
        };
        let save_ram = app.rom.read_save_ram();
        app.opened = true;
        Some(Ok(Command::OpenRom(Box::new(OpenRom {
            rom,
            save_ram,
            boot_rom: None,
            config: app.config.clone(),
            start_paused: false,
        }))))
    });

    match request {
        Some(Ok(cmd)) => with_app(|app| app.send(cmd)),
        Some(Err(message)) => ui.set_status_text(message.into()),
        None => {}
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
