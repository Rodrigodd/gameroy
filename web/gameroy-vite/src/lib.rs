use std::sync::{Arc, Mutex};

use gameroy::consts::{SCREEN_HEIGHT, SCREEN_WIDTH};
use gameroy::gameboy::cartridge::Cartridge;
use gameroy::gameboy::GameBoy;
use gameroy::interpreter::Interpreter;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

struct GameroyContext {
    gameboy: Option<GameBoy>,
    screen_buffer: Option<Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>>>,
}

impl GameroyContext {
    const fn new() -> GameroyContext {
        Self {
            gameboy: None,
            screen_buffer: None,
        }
    }
}

static GAMEROY_CONTEXT: Mutex<GameroyContext> = Mutex::new(GameroyContext::new());

fn context_mut() -> std::sync::MutexGuard<'static, GameroyContext> {
    GAMEROY_CONTEXT.try_lock().unwrap()
}

#[wasm_bindgen]
pub fn load_rom(rom: Vec<u8>) -> Result<(), JsValue> {
    let mut context = context_mut();

    let cartridge = match Cartridge::new(rom) {
        Ok(rom) => Ok(rom),
        Err((warn, Some(rom))) => {
            println!("Warning: {}", warn.strip_suffix('\n').unwrap_or(&warn));
            // log::warn!("{}", warn);
            Ok(rom)
        }
        Err((err, None)) => Err(err),
    }?;

    let mut gameboy = GameBoy::new(None, cartridge);

    let screen = match context.screen_buffer.as_ref() {
        Some(screen) => screen.clone(),
        None => {
            let screen = Arc::new(Mutex::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]));
            context.screen_buffer = Some(screen.clone());
            screen
        }
    };

    gameboy.v_blank = Some(Box::new(move |gb| {
        *screen.try_lock().unwrap() = gb.ppu.get_mut().screen.packed();
    }));

    context.gameboy = Some(gameboy);

    Ok(())
}

#[wasm_bindgen]
pub fn run_frame(delta: f64) -> Result<Vec<u32>, JsValue> {
    let mut context = context_mut();

    let gameboy = context.gameboy.as_mut().ok_or("No ROM loaded")?;

    let target = gameboy.clock_count + (gameroy::consts::CLOCK_SPEED as f64 * delta) as u64;
    while gameboy.clock_count < target {
        Interpreter(gameboy).interpret_op();
    }

    let frame = context
        .screen_buffer
        .as_ref()
        .unwrap()
        .try_lock()
        .unwrap()
        .map(|c| {
            /// 0RGB1555 format
            /// FIXME: this format is deprecated
            #[allow(clippy::unusual_byte_groupings)]
            const COLOR: [u32; 4] = [
                0xffffffff, //
                0xffaaaaaa, //
                0xff555555, //
                0xff000000,
            ];
            COLOR[c as usize]
        });

    Ok(frame.to_vec())
}

#[wasm_bindgen]
pub fn take_audio_buffer() -> Result<Vec<f32>, JsValue> {
    let mut context = context_mut();

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();

    let clock_count = gb.clock_count;
    Ok(gb
        .sound
        .get_mut()
        .get_output(clock_count)
        .iter()
        .map(|&s| s as f32)
        .collect())
}
