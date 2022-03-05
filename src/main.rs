use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
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
extern crate crui;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

fn main() {
    env_logger::init();

    let mut diss = false;
    let mut debug = false;
    let mut rom_path = None;
    let mut boot_rom_path = None; //"bootrom/dmg_boot.bin";
    let mut movie = None;

    // Skip the command name
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-d" | "--disassembly" => diss = true,
            "-b" | "--debug" => debug = true,
            "-m" | "--movie" => {
                let path = args.next().expect("expected path to the movie");
                let mut file = std::fs::File::open(path).unwrap();
                let vbm = gameroy::parser::vbm(&mut file).unwrap();
                movie = Some(vbm);
            }
            "--boot-rom" => {
                let path = args.next().expect("expected path to the movie");
                boot_rom_path = Some(path);
            }
            _ if arg.starts_with("-") => {
                eprintln!("unknown argument {}", arg);
                return;
            }
            _ => {
                println!("rom path is {}", arg);
                rom_path = Some(arg);
            }
        }
    }

    if let Some(rom_path) = rom_path {
        let rom_path = PathBuf::from(rom_path);

        if diss {
            let (_, mut game_boy) =
                load_gameboy(&rom_path, boot_rom_path.as_ref().map(|x| Path::new(x)));
            game_boy.boot_rom_active = false;

            let mut string = String::new();
            game_boy
                .trace
                .borrow_mut()
                .fmt(&game_boy, &mut string)
                .unwrap();
            println!("{}", string);

            return;
        }

        // create winit's window and event_loop
        let event_loop = EventLoop::with_user_event();
        let window = WindowBuilder::new()
            .with_inner_size(PhysicalSize::new(768u32, 400))
            .build(&event_loop)
            .unwrap();

        let mut ui = ui::Ui::new(&window);

        let proxy = event_loop.create_proxy();

        let (save_path, game_boy) =
            load_gameboy(&rom_path, boot_rom_path.as_ref().map(|x| Path::new(x)));
        let emu = EmulatorApp::new(game_boy, proxy, debug, &mut ui, movie, rom_path, save_path);
        start_event_loop(event_loop, window, ui, Box::new(emu));
    } else {
        // create winit's window and event_loop
        let event_loop = EventLoop::with_user_event();
        let window = WindowBuilder::new()
            .with_inner_size(PhysicalSize::new(768u32, 400))
            .build(&event_loop)
            .unwrap();

        let mut ui = ui::Ui::new(&window);

        // let proxy = event_loop.create_proxy();

        let rom_loading = RomLoadingApp::new(&mut ui);
        start_event_loop(event_loop, window, ui, Box::new(rom_loading));
    }
}

fn load_gameboy(rom_path: &Path, boot_rom_path: Option<&Path>) -> (PathBuf, GameBoy) {
    let rom = std::fs::read(&rom_path).unwrap();

    let boot_rom = if let Some(boot_rom_path) = boot_rom_path {
        match File::open(boot_rom_path) {
            Ok(mut boot_rom_file) => {
                let mut boot_rom = [0; 0x100];
                boot_rom_file.read(&mut boot_rom).unwrap();
                Some(boot_rom)
            }
            Err(e) => {
                eprintln!("error loading boot rom: {}", e);
                None
            }
        }
    } else {
        None
    };
    let mut cartridge = Cartridge::new(rom).unwrap();
    let mut save_path = rom_path.to_path_buf();
    if save_path.set_extension("sav") {
        println!("loading save at {}", save_path.display());
        let saved_ram = std::fs::read(&save_path);
        match saved_ram {
            Ok(save) => cartridge.ram = save,
            Err(err) => {
                println!("load save failed: {}", err);
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
    (save_path, game_boy)
}

use winit::{
    dpi::PhysicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopProxy},
    window::{Window, WindowBuilder},
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
    window: Window,
    mut ui: ui::Ui,
    mut app: Box<dyn App>,
) {
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
                let (save_path, game_boy) = load_gameboy(&rom_path, None);
                let emu = EmulatorApp::new(
                    game_boy,
                    proxy.clone(),
                    false,
                    &mut ui,
                    None,
                    rom_path,
                    save_path,
                );
                app = Box::new(emu);
                return;
            }
            _ => {}
        }
        app.handle_event(event, &mut ui, &window, &proxy);
    });
}

trait App {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        proxy: &EventLoopProxy<UserEvent>,
    );
}

struct RomLoadingApp;
impl RomLoadingApp {
    fn new(ui: &mut ui::Ui) -> Self {
        let gui = &mut ui.gui;
        gui.create_control()
            .graphic(crui::text::Text::new(
                "Drop the game rom file here to load it".to_string(),
                (0, 0),
                ui.style.text_style.clone(),
            ))
            .build(gui);
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
            // Event::WindowEvent {
            //     event: WindowEvent::HoveredFile(path),
            //     ..
            // } => {
            //     println!("The file {:?} is hovering", path);
            // }
            // Event::WindowEvent {
            //     event: WindowEvent::HoveredFileCancelled,
            //     ..
            // } => {
            //     println!("The file hovering was cancel");
            // }
            Event::WindowEvent {
                event: WindowEvent::DroppedFile(path),
                ..
            } => {
                println!("The file {:?} was dropped", path);
                proxy.send_event(UserEvent::LoadRom(path)).unwrap();
            }
            _ => {}
        }
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
        ui::create_emulator_ui(
            ui,
            proxy.clone(),
            gb.clone(),
            debugger.clone(),
            emu_channel.clone(),
            AppState::new(debug),
        );
        let emu_thread = {
            let gb = gb.clone();
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
