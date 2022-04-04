#![cfg_attr(not(feature = "console"), windows_subsystem = "windows")]
#![cfg_attr(feature = "console", windows_subsystem = "console")]

use std::{
    path::{Path, PathBuf},
    rc::Rc,
    sync::{mpsc::sync_channel, Arc},
    thread,
};

use gameroy::{
    debugger::{Debugger, DebuggerEvent},
    gameboy::{self, cartridge::Cartridge, GameBoy},
    parser::Vbm,
};
use parking_lot::Mutex;

mod emulator;
mod event_table;
mod fold_view;
mod layout;
mod split_view;
mod style;
mod ui;

pub use emulator::{Emulator, EmulatorEvent};

#[macro_use]
extern crate giui;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

use clap::{arg, Command};

mod config;
use config::{config, normalize_config_path};

fn main() {
    let _logger = flexi_logger::Logger::try_with_env_or_str("gameroy=info")
        .unwrap()
        .log_to_file(
            flexi_logger::FileSpec::default()
                .directory(config::base_folder().unwrap_or_default())
                .suppress_timestamp(),
        )
        .duplicate_to_stderr(flexi_logger::Duplicate::All)
        .start()
        .unwrap();

    let matches = Command::new("GameRoy")
        .version("0.1")
        .author("Rodrigo Moraes")
        .about("A Game Boy emulator and debugger (and disassembler?).")
        .arg(arg!(-d - -debug "start the emulation in debug mode").required(false))
        .arg(
            arg!(--disassembly "output to stdout the dissasembly of the rom (it isn't working nor complete assembly)")
                .required(false)
                .requires("ROM_PATH"),
        )
        .arg(arg!(--movie <PATH> "play the given .vbm file").required(false))
        .arg(arg!(--boot_rom <PATH> "dump of the bootrom to be used").required(false))
        .arg(arg!(--rom_folder <PATH> "specify the path of the folder for listing .gb roms").required(false))
        .arg(arg!(<ROM_PATH> "path to the game rom to be emulated").required(false))
        .get_matches();

    let debug = matches.is_present("debug");
    let diss = matches.is_present("disassembly");
    let boot_rom_path = matches.value_of("boot_rom");
    let rom_folder = matches.value_of("rom_folder");
    let rom_path = matches.value_of("ROM_PATH");
    let movie = matches.value_of("movie").map(|path| {
        let mut file = std::fs::File::open(path).unwrap();
        let vbm = gameroy::parser::vbm(&mut file).unwrap();
        vbm
    });

    // dissasembly and return early
    if diss {
        if let Some(rom_path) = rom_path {
            let rom_path = PathBuf::from(rom_path);

            let gb = load_gameboy(&rom_path, boot_rom_path.as_ref().map(|x| Path::new(x)));
            let (_, mut gb) = match gb {
                Ok(x) => x,
                Err(e) => return eprintln!("{}", e),
            };
            gb.boot_rom_active = false;

            let mut string = String::new();
            gb.trace.borrow_mut().fmt(&gb, &mut string).unwrap();
            println!("{}", string);

            return;
        } else {
            unreachable!("the --disassembly flag already requires <ROM_PATH>");
        }
    }

    config::init_config({
        let mut config = config::Config::load()
            .map_err(|e| log::error!("error loading config file 'gameroy.toml': {}", e))
            .unwrap_or_default();
        config.start_in_debug |= debug;
        config.rom_folder = config
            .rom_folder
            .or_else(|| rom_folder.map(|x| x.to_string()));
        config
    });

    // load rom if necesary
    let gb = if let Some(rom_path) = rom_path {
        let rom_path = PathBuf::from(rom_path);

        let gb = load_gameboy(&rom_path, boot_rom_path.as_ref().map(|x| Path::new(x)));
        match gb {
            Ok((s, g)) => Some((s, g, rom_path)),
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        }
    } else {
        None
    };

    #[allow(unused_assignments)]
    let mut icon = None;
    #[cfg(target_os = "windows")]
    {
        use winit::platform::windows::IconExtWindows;
        icon = Icon::from_resource(101, None)
            .map_err(|e| log::error!("{}", e))
            .ok();
    };

    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let window = WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(768u32, 400))
        // wait every thing to be loaded before showing the window
        .with_visible(false)
        .with_window_icon(icon)
        .with_title("gameroy")
        .build(&event_loop)
        .unwrap();

    let proxy = event_loop.create_proxy();
    let mut ui = ui::Ui::new(&window, proxy);

    let window = Rc::new(window);
    ui.gui.set(window.clone());

    let proxy = event_loop.create_proxy();

    // initiate in the apropriated screen
    match gb {
        Some((save_path, gb, rom_path)) => {
            window.set_title(&format!(
                "{} - gameroy",
                rom_path.file_name().unwrap().to_string_lossy()
            ));
            let emu = EmulatorApp::new(
                gb,
                proxy,
                config::config().start_in_debug,
                &mut ui,
                movie,
                rom_path,
                save_path,
            );
            start_event_loop(event_loop, window, ui, Box::new(emu));
        }
        _ => {
            let rom_loading = RomLoadingApp::new();
            start_event_loop(event_loop, window, ui, Box::new(rom_loading))
        }
    };
}

fn load_gameboy(
    rom_path: &Path,
    boot_rom_path: Option<&Path>,
) -> Result<(PathBuf, GameBoy), String> {
    log::info!("loading rom: {:?}", rom_path);
    let rom = {
        let mut rom = Vec::new();
        open_and_read(rom_path, &mut rom)?;
        rom
    };

    let boot_rom = load_boot_rom(boot_rom_path);

    let mut cartridge = Cartridge::new(rom).unwrap();
    log::info!("Cartridge type: {}", cartridge.kind_name());

    let mut save_path = rom_path.to_path_buf();
    if save_path.set_extension("sav") {
        log::info!("loading save at {}", save_path.display());
        let saved_ram = std::fs::read(&save_path);
        match saved_ram {
            Ok(save) => cartridge.ram = save,
            Err(err) => {
                log::error!("load save failed: {}", err);
            }
        }
    }

    let game_boy = gameboy::GameBoy::new(boot_rom, cartridge);
    {
        let mut trace = game_boy.trace.borrow_mut();

        trace.trace_starting_at(&game_boy, 0, 0x100, Some("entry point".into()));
        trace.trace_starting_at(&game_boy, 0, 0x40, Some("RST_0x40".into()));
        trace.trace_starting_at(&game_boy, 0, 0x48, Some("RST_0x48".into()));
        trace.trace_starting_at(&game_boy, 0, 0x50, Some("RST_0x50".into()));
        trace.trace_starting_at(&game_boy, 0, 0x58, Some("RST_0x58".into()));
        trace.trace_starting_at(&game_boy, 0, 0x60, Some("RST_0x60".into()));
    }
    Ok((save_path, game_boy))
}

fn load_boot_rom(boot_rom_path: Option<&Path>) -> Option<[u8; 256]> {
    let boot_rom_path = if let Some(x) = boot_rom_path {
        x
    } else {
        return None;
    };

    let mut boot_rom = [0; 0x100];
    match open_and_read(boot_rom_path, &mut &mut boot_rom[..]) {
        Err(e) => {
            eprintln!("{}", e);
            return None;
        }
        Ok(_) => Some(boot_rom),
    }
}

fn open_and_read(rom_path: &Path, writer: &mut impl std::io::Write) -> Result<usize, String> {
    let file = &mut std::fs::File::open(&rom_path)
        .map_err(|x| format!("error loading '{}': {}", rom_path.display(), x))?;

    Ok(std::io::copy(file, writer)
        .map_err(|x| format!("error reading '{}': {}", rom_path.display(), x))? as usize)
}

use winit::{
    dpi::PhysicalSize,
    event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopProxy},
    window::{Icon, Window, WindowBuilder},
};

pub struct AppState {
    /// The current state of the joypad. It is a bitmask, where 0 means pressed, and 1 released.
    pub joypad: u8,
    /// If the emulation is in debug mode.
    pub debug: bool,
}
impl AppState {
    fn new(debug: bool) -> Self {
        Self {
            debug,
            joypad: 0xFF,
        }
    }
}

fn start_event_loop(
    event_loop: EventLoop<UserEvent>,
    window: Rc<Window>,
    mut ui: ui::Ui,
    mut app: Box<dyn App>,
) -> ! {
    window.set_visible(true);
    app.build_ui(&mut ui);
    let proxy = event_loop.create_proxy();
    // winit event loop
    event_loop.run(move |event, _, control| {
        match event {
            Event::NewEvents(_) => {
                ui.new_events(control, &window);
            }
            Event::WindowEvent {
                ref event,
                window_id,
                ..
            } => {
                ui.window_event(&event, &window);
                match event {
                    WindowEvent::CloseRequested => {
                        *control = ControlFlow::Exit;
                    }
                    WindowEvent::Resized(size) => {
                        ui.resize(size.clone(), window_id);
                    }
                    // Rebuild the UI
                    WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                virtual_keycode: Some(VirtualKeyCode::Escape),
                                state: ElementState::Pressed,
                                ..
                            },
                        ..
                    } => {
                        ui.clear();
                        ui.reload_style();
                        app.build_ui(&mut ui);
                    }
                    _ => {}
                }
            }
            Event::MainEventsCleared => {}
            Event::RedrawRequested(window_id) => {
                // render the gui
                ui.render(window_id);

                if ui.is_animating {
                    *control = ControlFlow::Poll;
                }
            }
            Event::UserEvent(UserEvent::LoadRom(rom_path)) => {
                ui.clear();
                let gb = load_gameboy(&rom_path, None);
                let (save_path, gb) = match gb {
                    Ok(x) => x,
                    Err(e) => return eprintln!("{}", e),
                };
                window.set_title(&format!(
                    "{} - gameroy",
                    rom_path.file_name().unwrap().to_string_lossy()
                ));
                let emu = EmulatorApp::new(
                    gb,
                    proxy.clone(),
                    config::config().start_in_debug,
                    &mut ui,
                    None,
                    rom_path,
                    save_path,
                );
                app = Box::new(emu);
                app.build_ui(&mut ui);
                return;
            }
            _ => {}
        }
        app.handle_event(event, &mut ui, &window, &proxy);
    })
}

trait App {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        proxy: &EventLoopProxy<UserEvent>,
    );

    fn build_ui(&self, ui: &mut ui::Ui);
}

struct RomLoadingApp;
impl RomLoadingApp {
    fn new() -> Self {
        Self
    }
}

impl App for RomLoadingApp {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        _ui: &mut ui::Ui,
        _window: &winit::window::Window,
        proxy: &EventLoopProxy<UserEvent>,
    ) {
        match event {
            Event::WindowEvent {
                event: WindowEvent::DroppedFile(path),
                ..
            } => {
                log::info!("The file {:?} was dropped", path);
                proxy.send_event(UserEvent::LoadRom(path)).unwrap();
            }
            _ => {}
        }
    }

    fn build_ui(&self, ui: &mut ui::Ui) {
        let gui = &mut ui.gui;
        let style = &gui.get::<style::Style>().clone();
        ui::create_rom_loading_ui(gui, style);
    }
}

struct EmulatorApp {
    lcd_screen: Arc<
        parking_lot::lock_api::Mutex<parking_lot::RawMutex, [u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    >,
    emu_channel: std::sync::mpsc::SyncSender<EmulatorEvent>,
    emu_thread: Option<thread::JoinHandle<()>>,
}
impl EmulatorApp {
    fn new(
        mut gb: GameBoy,
        proxy: winit::event_loop::EventLoopProxy<UserEvent>,
        debug: bool,
        ui: &mut ui::Ui,
        movie: Option<Vbm>,
        rom_path: PathBuf,
        save_path: PathBuf,
    ) -> EmulatorApp {
        let lcd_screen: Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>> =
            Arc::new(Mutex::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]));
        gb.v_blank = Some(Box::new({
            let lcd_screen = lcd_screen.clone();
            let proxy = proxy.clone();
            move |gb| {
                {
                    let img_data = &mut lcd_screen.lock();
                    img_data.copy_from_slice(&gb.ppu.borrow().screen);
                }
                let _ = proxy.send_event(UserEvent::FrameUpdated);
            }
        }));
        let gb = Arc::new(Mutex::new(gb));
        let (emu_channel, recv) = sync_channel(3);
        if debug {
            proxy.send_event(UserEvent::Debug(debug)).unwrap();
        } else {
            emu_channel.send(EmulatorEvent::RunFrame).unwrap();
        }
        let debugger = Arc::new(Mutex::new(Debugger::default()));
        {
            let proxy = proxy.clone();
            let emu_channel = emu_channel.clone();
            debugger.lock().callback = Some(Box::new(move |_, event| {
                use DebuggerEvent::*;
                match event {
                    Step => emu_channel.send(EmulatorEvent::Step).unwrap(),
                    StepBack => emu_channel.send(EmulatorEvent::StepBack).unwrap(),
                    Reset => emu_channel.send(EmulatorEvent::Reset).unwrap(),
                    Run => emu_channel.send(EmulatorEvent::Run).unwrap(),
                    BreakpointsUpdate => proxy.send_event(UserEvent::BreakpointsUpdated).unwrap(),
                    WatchsUpdate => proxy.send_event(UserEvent::WatchsUpdated).unwrap(),
                }
            }));
        }
        ui.gui.set::<Arc<Mutex<GameBoy>>>(gb.clone());
        ui.gui.set::<Arc<Mutex<Debugger>>>(debugger.clone());
        ui.gui.set(emu_channel.clone());
        ui.gui.set(AppState::new(debug));
        let emu_thread = {
            let join_handle = thread::Builder::new()
                .name("emulator".to_string())
                .spawn(move || Emulator::run(gb, debugger, recv, proxy, movie, rom_path, save_path))
                .unwrap();
            Some(join_handle)
        };
        EmulatorApp {
            lcd_screen,
            emu_channel,
            emu_thread,
        }
    }
}
impl App for EmulatorApp {
    fn build_ui(&self, ui: &mut ui::Ui) {
        let debug = ui.get::<AppState>().debug;
        ui::create_emulator_ui(ui, debug);
    }
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        _proxy: &EventLoopProxy<UserEvent>,
    ) {
        match event {
            Event::RedrawRequested(_) => {
                let joypad = ui.get::<AppState>().joypad;
                self.emu_channel
                    .send(EmulatorEvent::SetJoypad(joypad))
                    .unwrap();
                self.emu_channel.send(EmulatorEvent::RunFrame).unwrap();
            }
            Event::LoopDestroyed => {
                self.emu_channel.send(EmulatorEvent::Kill).unwrap();
                self.emu_thread.take().unwrap().join().unwrap();
            }
            Event::UserEvent(event) => {
                use UserEvent::*;
                match event {
                    FrameUpdated => {
                        let screen: &[u8] = &{
                            let lock = self.lcd_screen.lock();
                            lock.clone()
                        };
                        const COLOR: [[u8; 3]; 4] =
                            [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                        let mut img_data = vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4];
                        for y in 0..SCREEN_HEIGHT {
                            for x in 0..SCREEN_WIDTH {
                                let i = (x + y * SCREEN_WIDTH) as usize * 4;
                                let c = screen[i / 4];
                                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                            }
                        }
                        ui.update_screen_texture(&img_data);

                        ui.notify(event_table::FrameUpdated);
                        window.request_redraw();
                    }
                    EmulatorStarted => {
                        ui.force_render = true;
                        window.request_redraw();
                    }
                    EmulatorPaused => {
                        ui.notify(event_table::EmulatorUpdated);
                        ui.force_render = false;
                    }
                    BreakpointsUpdated => ui.notify(event_table::BreakpointsUpdated),
                    WatchsUpdated => ui.notify(event_table::WatchsUpdated),
                    Debug(value) => {
                        ui.get::<AppState>().debug = value;
                        ui.notify(event_table::Debug(value));
                        self.emu_channel.send(EmulatorEvent::Debug(value)).unwrap();
                    }
                    UpdateTexture(texture, data) => ui.update_texture(texture, &data),
                    LoadRom(_) => unreachable!(),
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
pub enum UserEvent {
    FrameUpdated,
    EmulatorPaused,
    EmulatorStarted,
    BreakpointsUpdated,
    WatchsUpdated,
    Debug(bool),
    UpdateTexture(u32, Box<[u8]>),
    LoadRom(PathBuf),
}
