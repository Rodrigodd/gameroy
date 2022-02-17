use std::{
    fs::File,
    io::Read,
    path::PathBuf,
    sync::{mpsc::sync_channel, Arc},
    thread,
};

use parking_lot::Mutex;

use gameroy::{
    cartridge::Cartridge,
    debugger::{Debugger, DebuggerEvent},
    gameboy::{self, GameBoy},
    parser::Vbm,
};

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
    let mut rom_path = "roms/test.gb".to_string();
    let mut boot_rom_path = None; //"bootrom/dmg_boot.bin";
    let mut movie = None;

    let mut args = std::env::args();
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
                rom_path = arg;
            }
        }
    }
    let rom_path = PathBuf::from(rom_path);

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
    let mut save_path = rom_path.clone();
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
    let mut game_boy = gameboy::GameBoy::new(boot_rom, cartridge);

    {
        let mut trace = game_boy.trace.borrow_mut();

        trace.trace_starting_at(&game_boy, 0, 0x100, Some("entry point".into()));
        trace.trace_starting_at(&game_boy, 0, 0x40, Some("RST_0x40".into()));
        trace.trace_starting_at(&game_boy, 0, 0x48, Some("RST_0x48".into()));
        trace.trace_starting_at(&game_boy, 0, 0x50, Some("RST_0x50".into()));
        trace.trace_starting_at(&game_boy, 0, 0x58, Some("RST_0x58".into()));
        trace.trace_starting_at(&game_boy, 0, 0x60, Some("RST_0x60".into()));

        if diss {
            game_boy.boot_rom_active = false;

            let mut string = String::new();
            trace.fmt(&game_boy, &mut string).unwrap();
            println!("{}", string);

            return;
        }
    }

    create_window(game_boy, movie, rom_path, save_path, debug);
}

use winit::{
    dpi::PhysicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
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

fn create_window(
    mut gb: GameBoy,
    movie: Option<Vbm>,
    rom_path: PathBuf,
    save_path: PathBuf,
    debug: bool,
) {
    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let window = WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(600, 400))
        .build(&event_loop)
        .unwrap();
    let proxy = event_loop.create_proxy();

    let lcd_screen: Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>> =
        Arc::new(Mutex::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]));
    let lcd_screen_clone = lcd_screen.clone();
    gb.v_blank = Some(Box::new(move |gb| {
        {
            let img_data = &mut lcd_screen_clone.lock();
            img_data.copy_from_slice(&gb.ppu.borrow().screen);
        }
        let _ = proxy.send_event(UserEvent::FrameUpdated);
    }));

    let gb = Arc::new(Mutex::new(gb));
    let proxy = event_loop.create_proxy();

    let (emu_channel, recv) = sync_channel(3);
    if debug {
        proxy.send_event(UserEvent::Debug(debug)).unwrap();
    }
    emu_channel.send(EmulatorEvent::RunFrame).unwrap();

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

    let mut ui = ui::Ui::new(
        &window,
        proxy.clone(),
        gb.clone(),
        debugger.clone(),
        emu_channel.clone(),
        AppState::new(debug),
    );

    let mut emu_thread = {
        let gb = gb.clone();
        let join_handle = thread::Builder::new()
            .name("emulator".to_string())
            .spawn(move || Emulator::run(gb, debugger, recv, proxy, movie, rom_path, save_path))
            .unwrap();
        Some(join_handle)
    };

    // winit event loop
    event_loop.run(move |event, _, control| {
        match event {
            Event::NewEvents(_) => {
                ui.new_events(control, &window);
            }
            Event::LoopDestroyed => {
                emu_channel.send(EmulatorEvent::Kill).unwrap();
                emu_thread.take().unwrap().join().unwrap();
            }
            Event::WindowEvent {
                event, window_id, ..
            } => {
                ui.window_event(&event, &window);
                match event {
                    WindowEvent::CloseRequested => {
                        *control = ControlFlow::Exit;
                    }
                    WindowEvent::Resized(size) => {
                        ui.resize(size, window_id);
                    }
                    _ => {}
                }
            }
            Event::UserEvent(event) => {
                use UserEvent::*;
                match event {
                    FrameUpdated => {
                        let screen: &[u8] = &{
                            let lock = lcd_screen.lock();
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
                        emu_channel.send(EmulatorEvent::Debug(value)).unwrap();
                    }
                    UpdateTexture(texture, data) => ui.update_texture(texture, &data),
                }
            }
            Event::MainEventsCleared => {}
            Event::RedrawRequested(window_id) => {
                // render the gui
                ui.render(window_id);

                if ui.is_animating {
                    *control = ControlFlow::Poll;
                }

                let joypad = ui.get::<AppState>().joypad;
                emu_channel.send(EmulatorEvent::SetJoypad(joypad)).unwrap();
                emu_channel.send(EmulatorEvent::RunFrame).unwrap();
            }
            _ => {}
        }
    });
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
}
