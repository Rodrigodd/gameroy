use std::{path::PathBuf, rc::Rc, sync::Arc, thread};

use gameroy::{
    debugger::{Debugger, DebuggerEvent},
    gameboy::GameBoy,
    parser::Vbm,
};
use parking_lot::Mutex;

#[cfg(target_arch = "wasm32")]
mod wasm;

mod waker_fn;

#[cfg(not(target_arch = "wasm32"))]
mod bench;

mod emulator;
mod event_table;
mod widget {
    pub mod fold_view;
    mod pixel_perfect_layout;
    mod split_view;
    pub mod table_item;

    pub use pixel_perfect_layout::PixelPerfectLayout;
    pub use split_view::SplitView;
}
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

fn log_panic() {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let current = thread::current();
        let thread = current.name().unwrap_or("unnamed");

        let msg = if let Some(s) = info.payload().downcast_ref::<&'static str>() {
            *s
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            &**s
        } else {
            "Box<Any>"
        };

        match info.location() {
            Some(location) => {
                log::error!(
                    "thread '{}' panicked at '{}': {}:{}",
                    thread,
                    msg,
                    location.file(),
                    location.line(),
                );
            }
            None => {
                log::error!("thread '{}' panicked at '{}'", thread, msg,)
            }
        }

        default_hook(info);
    }));
}

pub fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    let _logger = flexi_logger::Logger::try_with_env_or_str("gameroy=info")
        .unwrap()
        .log_to_file(
            flexi_logger::FileSpec::default()
                .directory(config::base_folder().unwrap_or_default())
                .suppress_timestamp(),
        )
        .duplicate_to_stderr(if cfg!(debug_assertions) {
            flexi_logger::Duplicate::All
        } else {
            flexi_logger::Duplicate::None
        })
        .start()
        .unwrap();
    #[cfg(target_arch = "wasm32")]
    let _logger = wasm_logger::init(wasm_logger::Config::default().module_prefix("gameroy"));

    log_panic();

    const VERSION: &'static str = "0.1.1";

    log::info!("GameRoy {}", VERSION);

    let matches = Command::new("GameRoy")
        .version(VERSION)
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
        .subcommand(Command::new("bench")
            .about("Emulate a given rom for some ammount of frames, and give back the time runned.")
            .arg(arg!(-f --frames <NUMBER> "the number of frames to run for each run")
                 .required(false)
                 .default_value("600")
                 .validator(|x| x.parse::<u64>())
            )
            .arg(arg!(-t --times <NUMBER> "the number of times run")
                 .required(false)
                 .default_value("10")
                 .validator(|x| x.parse::<u64>())
            )
            .arg(arg!(<ROM_PATH> "path to the game rom to be emulated").required(true)))
        .get_matches();

    #[cfg(not(target_arch = "wasm32"))]
    match matches.subcommand() {
        Some(("bench", matches)) => {
            let rom_path = matches.value_of("ROM_PATH").unwrap();
            let frames: u64 = matches
                .value_of("frames")
                .and_then(|x| x.parse().ok())
                .unwrap();
            let len: usize = matches
                .value_of("times")
                .and_then(|x| x.parse().ok())
                .unwrap();
            return bench::benchmark(rom_path, frames * gameroy::consts::FRAME_CYCLES, len);
        }
        _ => {}
    }

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
    #[cfg(not(target_arch = "wasm32"))]
    if diss {
        if let Some(rom_path) = rom_path {
            let rom_path = PathBuf::from(rom_path);

            let gb = RomEntry::from_path(rom_path).and_then(|x| x.load_gameboy());
            let mut gb = match gb {
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
        config.boot_rom = boot_rom_path.map(|x| x.to_string());
        config
    });

    // load rom if necesary
    #[cfg(not(target_arch = "wasm32"))]
    let gb = if let Some(rom_path) = rom_path {
        let rom_path = PathBuf::from(rom_path);

        let gb = RomEntry::from_path(rom_path).and_then(|x| Ok((x.load_gameboy()?, x)));
        match gb {
            Ok(g) => Some(g),
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        }
    } else {
        None
    };
    #[cfg(target_arch = "wasm32")]
    let gb = Some(0);

    #[allow(unused_assignments, unused_mut)]
    let mut icon: Option<Icon> = None;
    #[cfg(target_os = "windows")]
    {
        use winit::platform::windows::IconExtWindows;
        icon = Icon::from_resource(101, None)
            .map_err(|e| log::error!("{}", e))
            .ok();
    };

    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let wb = WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(768u32, 400))
        // wait every thing to be loaded before showing the window
        .with_visible(false)
        .with_window_icon(icon)
        .with_title("gameroy");

    #[cfg(target_arch = "wasm32")]
    let wb = {
        use wasm_bindgen::JsCast;
        use winit::platform::web::WindowBuilderExtWebSys;

        let document = web_sys::window().unwrap().document().unwrap();
        let canvas = document.get_element_by_id("main_canvas").unwrap();
        let canvas: web_sys::HtmlCanvasElement = canvas
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .map_err(|_| ())
            .unwrap();

        wb.with_canvas(Some(canvas))
    };

    let window = wb.build(&event_loop).unwrap();

    let proxy = event_loop.create_proxy();
    let mut ui = ui::Ui::new(&window, proxy);

    let window = Rc::new(window);
    ui.gui.set(window.clone());

    let proxy = event_loop.create_proxy();

    // initiate in the apropriated screen
    match gb {
        #[cfg(not(target_arch = "wasm32"))]
        Some((gb, entry)) => {
            window.set_title(&format!("{} - gameroy", entry.file_name()));
            let emu = EmulatorApp::new(
                gb,
                proxy,
                config::config().start_in_debug,
                &mut ui,
                movie,
                entry,
            );
            start_event_loop(event_loop, window, ui, Box::new(emu));
        }
        _ => {
            let rom_loading = RomLoadingApp::new();
            start_event_loop(event_loop, window, ui, Box::new(rom_loading))
        }
    };
}

use winit::{
    dpi::PhysicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopProxy},
    window::{Icon, Window, WindowBuilder},
};

use self::ui::RomEntry;

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
                    #[cfg(not(feature = "static"))]
                    WindowEvent::KeyboardInput {
                        input:
                            winit::event::KeyboardInput {
                                virtual_keycode: Some(winit::event::VirtualKeyCode::Escape),
                                state: winit::event::ElementState::Pressed,
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
                #[cfg(target_arch = "wasm32")]
                {
                    if let Some((width, height)) = wasm::RESIZE.lock().take() {
                        let size = winit::dpi::PhysicalSize { width, height };
                        ui.resize(size.clone(), window_id);
                        window.set_inner_size(size);
                    }
                }
                // render the gui
                ui.render(window_id);

                if ui.is_animating {
                    *control = ControlFlow::Poll;
                }
            }
            Event::UserEvent(UserEvent::LoadRom(entry)) => {
                let gb = entry.load_gameboy();
                let gb = match gb {
                    Ok(x) => x,
                    Err(e) => return log::error!("{}", e),
                };
                window.set_title(&format!("{} - gameroy", entry.file_name(),));
                let emu = EmulatorApp::new(
                    gb,
                    proxy.clone(),
                    config::config().start_in_debug,
                    &mut ui,
                    None,
                    entry,
                );
                app = Box::new(emu);
                ui.clear();
                app.build_ui(&mut ui);
                return;
            }
            Event::UserEvent(UserEvent::SpawnTask(task_id)) => {
                use std::future::Future;
                use std::pin::Pin;
                let p = Arc::new(Mutex::new(proxy.clone()));
                let waker = waker_fn::waker_fn(move || {
                    p.lock().send_event(UserEvent::SpawnTask(task_id)).unwrap()
                });
                let task = ui.get::<Pin<Box<dyn Future<Output = ()>>>>();
                let mut cx = std::task::Context::from_waker(&waker);
                match Future::poll(task.as_mut(), &mut cx) {
                    std::task::Poll::Ready(_) => println!("woke!"),
                    std::task::Poll::Pending => println!("wait..."),
                }
            }
            _ => {}
        }
        app.handle_event(event, &mut ui, &window, control, &proxy);
    })
}

trait App {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        control: &mut ControlFlow,
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
        _control: &mut ControlFlow,
        proxy: &EventLoopProxy<UserEvent>,
    ) {
        match event {
            #[cfg(not(target_arch = "wasm32"))]
            Event::WindowEvent {
                event: WindowEvent::DroppedFile(path),
                ..
            } => {
                log::info!("The file {:?} was dropped", path);
                let entry = match RomEntry::from_path(path.clone()) {
                    Ok(x) => x,
                    Err(e) => {
                        log::error!("failed to load rom from {}: {}", path.display(), e);
                        return;
                    }
                };
                proxy.send_event(UserEvent::LoadRom(entry)).unwrap();
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
    emu_channel: flume::Sender<EmulatorEvent>,
    #[cfg(not(target_arch = "wasm32"))]
    emu_thread: Option<thread::JoinHandle<()>>,
    #[cfg(target_arch = "wasm32")]
    emulator: Emulator,
    #[cfg(target_arch = "wasm32")]
    recv: flume::Receiver<emulator::EmulatorEvent>,
}
impl EmulatorApp {
    fn new(
        mut gb: GameBoy,
        proxy: winit::event_loop::EventLoopProxy<UserEvent>,
        debug: bool,
        ui: &mut ui::Ui,
        movie: Option<Vbm>,
        rom: RomEntry,
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
        let (emu_channel, recv) = flume::bounded(8);
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

        #[cfg(not(target_arch = "wasm32"))]
        let emu_thread = {
            let join_handle = thread::Builder::new()
                .name("emulator".to_string())
                .spawn(move || Emulator::run(gb, debugger, recv, proxy, movie, rom))
                .unwrap();
            Some(join_handle)
        };

        EmulatorApp {
            lcd_screen,
            emu_channel,
            #[cfg(not(target_arch = "wasm32"))]
            emu_thread,
            #[cfg(target_arch = "wasm32")]
            emulator: Emulator::new(gb, debugger, proxy, movie, rom),
            #[cfg(target_arch = "wasm32")]
            recv,
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
        _control: &mut ControlFlow,
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
            #[cfg(not(target_arch = "wasm32"))]
            Event::LoopDestroyed => {
                self.emu_channel.send(EmulatorEvent::Kill).unwrap();
                self.emu_thread.take().unwrap().join().unwrap();
            }
            #[cfg(target_arch = "wasm32")]
            Event::MainEventsCleared => {
                if let Ok(mut event) = self.recv.try_recv() {
                    loop {
                        if self.emulator.handle_event(event) {
                            // break 'event_loop;
                        }
                        match self.emulator.poll() {
                            _ => {}
                        }
                        match self.recv.try_recv() {
                            Ok(x) => event = x,
                            _ => break,
                        }
                    }
                } else {
                    match self.emulator.poll() {
                        emulator::Control::Poll => *_control = ControlFlow::Poll,
                        emulator::Control::Wait => {}
                    }
                }
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
                    SpawnTask(_) => {}
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
    LoadRom(RomEntry),
    SpawnTask(usize),
}
