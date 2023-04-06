#![no_main]

use libfuzzer_sys::fuzz_target;

use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

fuzz_target!(|data: &[u8]| {
    // Don't accept data bigger than the biggest possible ROM.
    if data.len() > 16 * 0x2000 {
        return;
    }

    let rom = data.to_vec();

    let cartridge = match Cartridge::new(rom) {
        Ok(x) => x,
        Err(_) => return,
    };

    let mut gb = GameBoy::new(None, cartridge);
    gb.predict_interrupt = true;
    gb.serial.borrow_mut().serial_transfer_callback = None;

    // emulate 2 seconds
    let target = gb.clock_count + 2 * CLOCK_SPEED;

    while gb.clock_count < target {
        // this should never panic
        Interpreter(&mut gb).interpret_op();
    }
});
