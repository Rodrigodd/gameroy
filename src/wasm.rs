use std::panic;
use wasm_bindgen::prelude::*;

pub static RESIZE: parking_lot::Mutex<Option<(u32, u32)>> = parking_lot::const_mutex(None);

#[wasm_bindgen]
pub fn run() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    super::main();
}

#[wasm_bindgen]
pub fn resize_canvas(width: u32, height: u32) {
    *RESIZE.lock() = Some((width, height))
}
