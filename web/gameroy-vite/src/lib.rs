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
    gameboy.sound.get_mut().sample_frequency = 44100;

    context.gameboy = Some(gameboy);

    Ok(())
}

#[wasm_bindgen]
pub fn set_joypad(joypad: u8) {
    let mut context = context_mut();
    let gameboy = context.gameboy.as_mut().unwrap();
    gameboy.joypad = !joypad;
}

#[wasm_bindgen]
pub fn run_for(delta: f64) -> Result<(), JsValue> {
    let mut context = context_mut();

    let gameboy = context.gameboy.as_mut().ok_or("No ROM loaded")?;

    let target = gameboy.clock_count + (gameroy::consts::CLOCK_SPEED as f64 * delta) as u64;
    while gameboy.clock_count < target {
        Interpreter(gameboy).interpret_op();
    }

    Ok(())
}

#[wasm_bindgen]
pub fn get_frame() -> Result<Vec<u32>, JsValue> {
    let context = context_mut();

    let frame = context
        .screen_buffer
        .as_ref()
        .unwrap()
        .try_lock()
        .unwrap()
        .map(|c| {
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

#[wasm_bindgen(unchecked_return_type = "Float32Array[]")]
pub fn take_audio_buffer(gain: f32) -> Result<Vec<JsValue>, JsValue> {
    let mut context = context_mut();

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();

    let clock_count = gb.clock_count;
    let samples = gb.sound.get_mut().get_output(clock_count);
    // uninterlieve the samples into left and right channels
    let mut left = Vec::with_capacity(samples.len() / 2);
    let mut right = Vec::with_capacity(samples.len() / 2);
    for (i, sample) in samples.iter().enumerate() {
        if i % 2 == 0 {
            left.push(*sample as f32 * gain);
        } else {
            right.push(*sample as f32 * gain);
        }
    }

    Ok([JsValue::from(left), JsValue::from(right)].to_vec())
}

#[wasm_bindgen]
pub fn save_state() -> Result<Vec<u8>, JsValue> {
    let mut context = context_mut();

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();

    let mut save_state = Vec::new();
    gb.save_state(None, &mut save_state)
        .map_err(|e| format!("Failed to save state: {}", e))?;
    Ok(save_state)
}

#[wasm_bindgen]
pub fn load_state(state: Vec<u8>) -> Result<(), JsValue> {
    let mut context = context_mut();

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();

    gb.load_state(&mut state.as_slice())
        .map_err(|e| format!("Failed to load state: {:?}", e))?;

    Ok(())
}

#[wasm_bindgen]
pub fn reset() -> Result<(), JsValue> {
    let mut context = context_mut();
    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();
    gb.reset();
    Ok(())
}
