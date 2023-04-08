#![no_main]

use libfuzzer_sys::fuzz_target;

use gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

fuzz_target!(|data: &[u8]| {
    // Don't accept data bigger than the biggest possible ROM.
    if data.len() > 256 * 0x2000 {
        return;
    }

    // Too small.
    if data.len() < 0x150 {
        return;
    }

    let rom_sizes = [
        2 * 0x4000, // no ROM Banking
        4 * 0x4000,
        8 * 0x4000,
        16 * 0x4000,
        32 * 0x4000,
        64 * 0x4000,
        128 * 0x4000,
        256 * 0x4000,
        // 512 * 0x4000,
        // 72 * 0x2000,
        // 80 * 0x2000,
        // 96 * 0x2000,
    ];

    // find the next greater possible rom size
    let rom_size = rom_sizes
        .into_iter()
        .enumerate()
        .rev()
        .find(|(_, x)| data.len() <= *x)
        .map(|(i, _)| i)
        .unwrap();

    let mut rom = data.to_vec();
    // hardcode it in the data
    rom[0x0148] = rom_size as u8;

    // fill the remaning size with zeros
    rom.resize(rom_sizes[rom_size], 0);

    let cartridge = match Cartridge::new(rom) {
        Ok(x) => x,
        Err(_) => return,
    };

    let mut gb = GameBoy::new(None, cartridge);
    gb.predict_interrupt = true;
    gb.serial.borrow_mut().serial_transfer_callback = None;

    // emulate 100 ms
    let target = gb.clock_count + CLOCK_SPEED / 10;

    while gb.clock_count < target {
        // this should never panic
        Interpreter(&mut gb).interpret_op();
    }
});
