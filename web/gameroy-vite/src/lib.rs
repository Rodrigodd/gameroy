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

const COLOR: [u32; 4] = [
    0xffffffff, //
    0xffaaaaaa, //
    0xff555555, //
    0xff000000,
];

static GAMEROY_CONTEXT: Mutex<GameroyContext> = Mutex::new(GameroyContext::new());

static SETUP_PANIC_HOOK: std::sync::Once = std::sync::Once::new();

fn context_mut() -> Result<std::sync::MutexGuard<'static, GameroyContext>, JsValue> {
    SETUP_PANIC_HOOK.call_once(console_error_panic_hook::set_once);
    Ok(GAMEROY_CONTEXT
        .try_lock()
        .map_err(|_| "Failed to lock context")?)
}

#[wasm_bindgen]
pub fn load_rom(rom: Vec<u8>) -> Result<(), JsValue> {
    let mut context = context_mut()?;

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
pub fn set_joypad(joypad: u8) -> Result<(), JsValue> {
    let mut context = context_mut()?;
    let gameboy = context.gameboy.as_mut().ok_or("No ROM loaded")?;
    gameboy.joypad = !joypad;
    Ok(())
}

#[wasm_bindgen]
pub fn run_for(delta: f64) -> Result<(), JsValue> {
    let mut context = context_mut()?;

    let gameboy = context.gameboy.as_mut().ok_or("No ROM loaded")?;

    let target = gameboy.clock_count + (gameroy::consts::CLOCK_SPEED as f64 * delta) as u64;
    while gameboy.clock_count < target {
        Interpreter(gameboy).interpret_op();
    }

    Ok(())
}

#[wasm_bindgen]
pub fn get_last_screen() -> Result<Vec<u32>, JsValue> {
    let context = context_mut()?;

    let frame = context
        .screen_buffer
        .as_ref()
        .ok_or("No screen buffer")?
        .try_lock()
        .map_err(|_| "Failed to lock screen buffer")?
        .map(|c| COLOR[c as usize]);

    Ok(frame.to_vec())
}

#[wasm_bindgen]
pub fn get_ppu_background() -> Result<Vec<u32>, JsValue> {
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();
    let ppu = gb.ppu.get_mut();

    let mut background = vec![0; 256 * 256];
    gameroy::gameboy::ppu::draw_background(&*ppu, &mut |x, y, c| {
        let i = (x + y * 256) as usize;
        background[i] = COLOR[c as usize];
    });

    Ok(background)
}

#[wasm_bindgen]
pub fn get_ppu_window() -> Result<Vec<u32>, JsValue> {
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();
    let ppu = gb.ppu.get_mut();

    let mut window = vec![0; 256 * 256];
    gameroy::gameboy::ppu::draw_window(&*ppu, &mut |x, y, c| {
        let i = (x + y * 256) as usize;
        window[i] = COLOR[c as usize];
    });

    Ok(window)
}

#[wasm_bindgen]
pub fn get_ppu_tiles(pallete: u8) -> Result<Vec<u32>, JsValue> {
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded").unwrap();
    let ppu = gb.ppu.get_mut();

    let mut tiles = vec![0; 128 * 192];
    gameroy::gameboy::ppu::draw_tiles(
        &*ppu,
        &mut |x, y, c| {
            let i = (x + y * 128) as usize;
            tiles[i] = COLOR[c as usize];
        },
        pallete,
    );

    Ok(tiles)
}

#[wasm_bindgen(unchecked_return_type = "Float32Array[]")]
pub fn take_audio_buffer(gain: f32) -> Result<Vec<JsValue>, JsValue> {
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded")?;

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
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded")?;

    let mut save_state = Vec::new();
    gb.save_state(None, &mut save_state)
        .map_err(|e| format!("Failed to save state: {}", e))?;
    Ok(save_state)
}

#[wasm_bindgen]
pub fn load_state(state: Vec<u8>) -> Result<(), JsValue> {
    let mut context = context_mut()?;

    let gb = context.gameboy.as_mut().ok_or("No ROM loaded")?;

    gb.load_state(&mut state.as_slice())
        .map_err(|e| format!("Failed to load state: {:?}", e))?;

    Ok(())
}

#[wasm_bindgen]
pub fn reset() -> Result<(), JsValue> {
    let mut context = context_mut()?;
    let gb = context.gameboy.as_mut().ok_or("No ROM loaded")?;
    gb.reset();
    Ok(())
}
