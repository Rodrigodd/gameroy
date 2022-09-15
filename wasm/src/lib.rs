use std::panic;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    let _logger = wasm_logger::init(wasm_logger::Config::default().module_prefix("gameroy"));
    gameroy_lib::log_panic();
    gameroy_lib::main(None, None);
}

#[wasm_bindgen]
#[cfg(target_arch = "wasm32")]
pub fn resize_canvas(width: u32, height: u32) {
    *gameroy_lib::RESIZE.lock() = Some((width, height))
}
