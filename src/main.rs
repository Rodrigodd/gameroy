use std::{
    fs::File,
    sync::{mpsc::sync_channel, Arc},
    thread,
};

use parking_lot::Mutex;

use gameroy::{
    cartridge::Cartridge,
    gameboy,
    interpreter::{self, Interpreter},
};

mod disassembler_viewer;
mod emulator;
mod event_table;
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

    for arg in std::env::args() {
        match arg.as_str() {
            "-d" | "--disassembly" => diss = true,
            "-b" | "--debug" => debug = true,
            _ if arg.starts_with("-") => {
                eprintln!("unknown argument {}", arg);
                return;
            }
            _ => {
                rom_path = arg;
            }
        }
    }

    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("bootrom/dmg_boot.bin").unwrap();

    let cartridge = Cartridge::new(rom_file).unwrap();
    let mut game_boy = gameboy::GameBoy::new(boot_rom_file, cartridge).unwrap();

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

    let inter = interpreter::Interpreter(game_boy);

    create_window(inter, debug);
}

use winit::{
    dpi::PhysicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopProxy},
    window::WindowBuilder,
};

struct AppState {
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

fn create_window(mut inter: Interpreter, debug: bool) {
    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let wb = WindowBuilder::new().with_inner_size(PhysicalSize::new(600, 400));

    let (mut ui, window) = ui::Ui::new(wb, &event_loop);

    let ppu_screen: Arc<Mutex<Vec<u8>>> =
        Arc::new(Mutex::new(vec![0; SCREEN_WIDTH * SCREEN_HEIGHT]));
    let ppu_screen_clone = ppu_screen.clone();
    let proxy = event_loop.create_proxy();
    inter.0.v_blank = Box::new(move |ppu| {
        {
            let img_data = &mut ppu_screen_clone.lock();
            img_data.copy_from_slice(&ppu.screen);
        }
        let _ = proxy.send_event(UserEvent::FrameUpdated);
    });

    let inter = Arc::new(Mutex::new(inter));
    let proxy = event_loop.create_proxy();

    let (emu_channel, recv) = sync_channel(3);
    if debug {
        proxy.send_event(UserEvent::Debug(debug)).unwrap();
    }
    emu_channel.send(EmulatorEvent::RunFrame).unwrap();

    ui.insert(inter.clone());
    ui.insert(emu_channel.clone());
    ui.insert::<EventLoopProxy<UserEvent>>(proxy.clone());
    ui.insert(AppState::new(debug));

    thread::Builder::new()
        .name("emulator".to_string())
        .spawn(move || Emulator::run(inter, recv, proxy))
        .unwrap();

    // undeclare
    let debug = ();

    // winit event loop
    event_loop.run(move |event, _, control| {
        match event {
            Event::NewEvents(_) => {
                ui.new_events(control, &window);
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
                            let lock = ppu_screen.lock();
                            lock.clone()
                        };
                        let mut img_data = vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4];
                        for y in 0..SCREEN_HEIGHT {
                            for x in 0..SCREEN_WIDTH {
                                let i = (x + y * SCREEN_WIDTH) as usize * 4;
                                let c = screen[i / 4];
                                const COLOR: [[u8; 3]; 4] =
                                    [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                            }
                        }
                        ui.frame_update(&img_data);
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
                    Debug(value) => {
                        ui.get::<AppState>().debug = value;
                        ui.notify(event_table::Debug(value));
                        emu_channel.send(EmulatorEvent::Debug(value)).unwrap();
                    }
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
                emu_channel.send(EmulatorEvent::SetKeys(joypad)).unwrap();
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
    Debug(bool),
}
