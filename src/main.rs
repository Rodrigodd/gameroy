use std::fs::File;

use gameroy::{consts, cpu, dissasembler, gameboy, interpreter, ppu};
use pixel_canvas::canvas::CanvasInfo;
use pixel_canvas::input::glutin::event::{ElementState, VirtualKeyCode};
use pixel_canvas::input::{Event, WindowEvent};
use pixel_canvas::Color;

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

    let game_boy = gameboy::GameBoy::new(boot_rom_file, rom_file);

    let mut inter = interpreter::Interpreter(game_boy);

    if diss {
        let mut string = String::new();
        let mut trace = inter.0.trace.borrow_mut();
        trace.trace_starting_at(&inter.0, 0x100);
        trace.fmt(&inter.0, &mut string).unwrap();
        println!("{}", string);
        return;
    }

    let mut state = MyState::new();
    state.debug = debug;

    let canvas = pixel_canvas::Canvas::new(160, 144)
        .state(state)
        .input(MyState::handle_input);

    let mut target_clock = 0;
    canvas.render(move |state, image| {
        inter.0.joypad = state.joypad;
        if state.debug {
            inter.debug();
        } else {
            let clock_speed = 4_194_304 / 60;
            target_clock += clock_speed;
            while inter.0.clock_count < target_clock {
                inter.interpret_op();
            }
        }
        image.fill(Color::rgb(255, 0, 0));
        unsafe {
            let img_data: &mut [u8] =
                std::slice::from_raw_parts_mut(image.as_mut_ptr() as *mut u8, image.len() * 3);
            // ppu::draw_tiles(&inter.0.memory, img_data, image.width(), 0, 0);
            // ppu::draw_background(&inter.0.memory, img_data, image.width(), 130, 0);
            // ppu::draw_window(&inter.0.memory, img_data, image.width(), 388, 0);
            // ppu::draw_sprites(&inter.0.memory, img_data, image.width(), 130, 0);
            ppu::draw_screen(&inter.0.memory, &inter.0.ppu, img_data, image.width(), 0, 0);
        }
    })
}

pub struct MyState {
    debug: bool,
    joypad: u8,
}

impl MyState {
    pub fn new() -> Self {
        Self {
            debug: false,
            joypad: 0xFF,
        }
    }

    pub fn handle_input(_info: &CanvasInfo, this: &mut Self, event: &Event<()>) -> bool {
        match event {
            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                let mut set_key = |key: u8| {
                    this.joypad = (this.joypad & !(1 << key))
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
                        this.debug = !this.debug
                    }
                    _ => {}
                }
                // this.debug = true;
                true
            }
            _ => false,
        }
    }
}
