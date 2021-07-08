use std::{
    fs::File,
    sync::{
        mpsc::{sync_channel, Receiver, TryRecvError},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use gameroy::{
    cartridge::Cartridge,
    gameboy,
    interpreter::{self, Interpreter},
};

mod layout;
mod ui;

#[macro_use]
extern crate crui;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

fn main() {
    let mut diss = false;
    let mut debug = false;
    let mut rom_path = "roms/test.gb".to_string();

    for arg in std::env::args() {
        match arg.as_str() {
            "-d" | "--dissasembly" => diss = true,
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

    let game_boy = gameboy::GameBoy::new(boot_rom_file, cartridge).unwrap();

    let mut inter = interpreter::Interpreter(game_boy);

    if diss {
        inter.0.boot_rom_active = false;

        let mut trace = inter.0.trace.borrow_mut();

        trace.trace_starting_at(&inter.0, 0x100);
        trace.trace_starting_at(&inter.0, 0x40);
        trace.trace_starting_at(&inter.0, 0x48);
        trace.trace_starting_at(&inter.0, 0x50);
        trace.trace_starting_at(&inter.0, 0x58);
        trace.trace_starting_at(&inter.0, 0x60);

        let mut string = String::new();
        trace.fmt(&inter.0, &mut string).unwrap();
        println!("{}", string);

        return;
    }

    create_window(inter, debug);
}

use winit::{
    dpi::PhysicalSize,
    event::{ElementState, Event, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

fn create_window(mut inter: Interpreter, debug: bool) {
    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let wb = WindowBuilder::new().with_inner_size(PhysicalSize::new(600, 400));

    let (mut ui, window) = ui::Ui::new(wb, &event_loop);

    let img_data: Arc<Mutex<Vec<u8>>> =
        Arc::new(Mutex::new(vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4]));
    let img_data_clone = img_data.clone();
    let proxy = event_loop.create_proxy();
    inter.0.v_blank = Box::new(move |ppu| {
        let img_data: &mut [u8] = &mut img_data_clone.lock().unwrap();
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let i = (x + y * SCREEN_WIDTH) as usize * 4;
                let c = ppu.screen[i / 4];
                const COLOR: [[u8; 3]; 4] =
                    [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
            }
        }
        let _ = proxy.send_event(UserEvent::FrameUpdated);
    });

    let (sender, recv) = sync_channel(3);
    if debug {
        sender.send(EmulatorEvent::Debug).unwrap();
    }
    sender.send(EmulatorEvent::Run).unwrap();

    thread::Builder::new()
        .name("emulator".to_string())
        .spawn(move || emulator_thread(inter, recv))
        .unwrap();

    let mut joypad = 0xFF;

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
                    WindowEvent::KeyboardInput { input, .. } => {
                        let mut set_key = |key: u8| {
                            joypad = (joypad & !(1 << key))
                                | (((input.state == ElementState::Released) as u8) << key)
                        };
                        match input.virtual_keycode {
                            Some(VirtualKeyCode::Right) => set_key(0),
                            Some(VirtualKeyCode::Left) => set_key(1),
                            Some(VirtualKeyCode::Up) => set_key(2),
                            Some(VirtualKeyCode::Down) => set_key(3),
                            Some(VirtualKeyCode::Z) => set_key(4),
                            Some(VirtualKeyCode::X) => set_key(5),
                            Some(VirtualKeyCode::Return) => set_key(6),
                            Some(VirtualKeyCode::S) => set_key(7),
                            Some(VirtualKeyCode::D) if input.state == ElementState::Pressed => {
                                sender.send(EmulatorEvent::Debug).unwrap();
                            }
                            Some(VirtualKeyCode::LShift) => sender
                                .send(EmulatorEvent::FrameLimit(
                                    input.state != ElementState::Pressed,
                                ))
                                .unwrap(),

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            Event::UserEvent(UserEvent::FrameUpdated) => {
                let img_data: &mut [u8] = &mut img_data.lock().unwrap();
                ui.frame_update(img_data);
                window.request_redraw();
            }
            Event::MainEventsCleared => {}
            Event::RedrawRequested(window_id) => {
                // render the gui
                ui.render(window_id);

                if ui.is_animating {
                    *control = ControlFlow::Poll;
                }

                sender.send(EmulatorEvent::SetKeys(joypad)).unwrap();
                sender.send(EmulatorEvent::Run).unwrap();
            }
            _ => {}
        }
    });
}

enum UserEvent {
    FrameUpdated,
}

enum EmulatorEvent {
    Run,
    FrameLimit(bool),
    Debug,
    SetKeys(u8),
}

fn emulator_thread(mut inter: Interpreter, recv: Receiver<EmulatorEvent>) {
    use EmulatorEvent::*;

    let mut debug = false;
    // When true, the program will sync the time that passed, and the time that is emulated.
    let mut frame_limit = true;
    // The instant in time that the gameboy supposedly was turned on.
    // Change when frame_limit is disabled.
    let mut start_time = Instant::now();
    // The number of clocks the gameboy runs per second.
    let clock_speed = 4_194_304;

    while let Ok(mut event) = recv.recv() {
        'handle_event: loop {
            match event {
                Run if frame_limit && !debug => {
                    let elapsed = start_time.elapsed();
                    let mut target_clock = clock_speed * elapsed.as_secs()
                        + (clock_speed as f64 * (elapsed.subsec_nanos() as f64 * 1e-9)) as u64;
                    // make sure that the target_clock don't increase exponentially if the program can't keep up.
                    if target_clock > inter.0.clock_count + clock_speed / 30 {
                        let expected_target = target_clock;
                        target_clock = inter.0.clock_count + clock_speed / 30;
                        start_time += Duration::from_secs_f64(
                            (expected_target - target_clock) as f64 / clock_speed as f64,
                        );
                    }
                    while inter.0.clock_count < target_clock {
                        inter.interpret_op();
                    }
                }
                Run => {}
                FrameLimit(value) => {
                    frame_limit = value;
                    if frame_limit {
                        let secs = inter.0.clock_count / clock_speed;
                        let nanos =
                            (inter.0.clock_count % clock_speed) * 1_000_000_000 / clock_speed;
                        start_time = Instant::now() - Duration::new(secs, nanos as u32);
                    }
                }
                SetKeys(keys) => inter.0.joypad = keys,
                Debug => debug = !debug,
            }

            if debug {
                inter.debug();
            } else if !frame_limit {
                // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                loop {
                    let target_clock = inter.0.clock_count + clock_speed / 600;
                    while inter.0.clock_count < target_clock {
                        inter.interpret_op();
                    }
                    match recv.try_recv() {
                        Ok(next_event) => {
                            event = next_event;
                            continue 'handle_event;
                        }
                        Err(TryRecvError::Disconnected) => return,
                        _ => {}
                    }
                }
            }

            break;
        }
    }
}
