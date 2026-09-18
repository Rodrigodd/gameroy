// Prevent console window in addition to Slint window in Windows release builds when, e.g., starting the app via file manager. Ignored on other platforms.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::cell::Cell;
use std::error::Error;
use std::rc::Rc;
use std::sync::mpsc::Receiver;

use gameroy::gameboy::GameBoy;
use gameroy::gameboy::cartridge::Cartridge;
use gameroy::interpreter::Interpreter;
use slint::platform::Key;
use slint::{Rgb8Pixel, SharedPixelBuffer, SharedString, Weak};

slint::include_modules!();

enum GbEvent {
    Joypad(u8),
}

/// The bit of each joypad key in [`GameBoy::joypad`], which is a bitmask where 0 means pressed
/// and 1 released. Bits 0-3 are the direction keys, bits 4-7 the action buttons.
const JOYPAD_KEYS: [(Key, u8); 8] = [
    (Key::RightArrow, 0),
    (Key::LeftArrow, 1),
    (Key::UpArrow, 2),
    (Key::DownArrow, 3),
    (Key::X, 4),         // A
    (Key::Z, 5),         // B
    (Key::Backspace, 6), // Select
    (Key::Return, 7),    // Start
];

fn joypad_bit(text: &SharedString) -> Option<u8> {
    JOYPAD_KEYS
        .iter()
        .find(|(key, _)| *text == SharedString::from(*key))
        .map(|&(_, bit)| bit)
}

fn gameroy_thread(ui: Weak<AppWindow>, rx: Receiver<GbEvent>) {
    let rom = std::fs::read("roms/zelda.gb").expect("Failed to read ROM file");
    let cartridge = Cartridge::new(rom).expect("Failed to create cartridge");
    let mut gb = GameBoy::new(None, cartridge);

    gb.v_blank = Some(Box::new(move |gb| {
        let screen = gb.ppu.get_mut().screen;
        let ui = ui.clone();
        let _ = slint::invoke_from_event_loop(move || {
            let Some(ui) = ui.upgrade() else { return };
            let mut pixel_buffer = SharedPixelBuffer::<Rgb8Pixel>::new(
                gameroy::consts::SCREEN_WIDTH as u32,
                gameroy::consts::SCREEN_HEIGHT as u32,
            );
            for (p, c) in pixel_buffer
                .make_mut_slice()
                .iter_mut()
                .zip(screen.packed().iter())
            {
                let l = [255, 180, 100, 0][*c as usize];
                *p = Rgb8Pixel::new(l, l, l);
            }
            let image = slint::Image::from_rgb8(pixel_buffer);
            ui.set_screen(image);
        });
    }));

    let start_time = std::time::Instant::now();
    let start_clock = gb.clock_count;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(1));

        if let Ok(event) = rx.try_recv() {
            match event {
                GbEvent::Joypad(jp) => gb.joypad = jp,
            }
        }

        let target_clock = start_clock
            + start_time.elapsed().as_millis() as u64 * gameroy::consts::CLOCK_SPEED / 1000;

        while gb.clock_count < target_clock {
            Interpreter(&mut gb).interpret_op();
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let ui = AppWindow::new()?;

    let (tx, rx) = std::sync::mpsc::channel();

    let joypad = Rc::new(Cell::new(0xff));
    {
        let tx = tx.clone();
        let joypad = joypad.clone();
        ui.on_key_pressed(move |event| {
            if let Some(bit) = joypad_bit(&event.text) {
                joypad.update(|x| x & !(1 << bit));
                let _ = tx.send(GbEvent::Joypad(joypad.get()));
            }
        });
    }
    ui.on_key_release(move |event| {
        if let Some(bit) = joypad_bit(&event.text) {
            joypad.update(|x| x | (1 << bit));
            let _ = tx.send(GbEvent::Joypad(joypad.get()));
        }
    });

    let wui = ui.as_weak();
    std::thread::spawn(move || gameroy_thread(wui, rx));

    ui.run()?;

    Ok(())
}
