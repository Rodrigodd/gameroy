use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

const TEST_ROM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/gameboy-test-roms/");

#[test]
fn test_interrupt_prediction() {
    let rom = "mooneye-test-suite/acceptance/ppu/intr_2_0_timing.gb";
    let timeout = 120 * CLOCK_SPEED;
    let rom_path: PathBuf = (TEST_ROM_PATH.to_string() + rom).into();
    let rom = std::fs::read(rom_path).unwrap();

    let cartridge = Cartridge::new(rom).unwrap();

    let mut game_boy_a = GameBoy::new(None, cartridge.clone());
    game_boy_a.predict_interrupt = false;

    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = true;

    while game_boy_b.clock_count < timeout {
        print!("\u{001b}[37m");
        {
            let mut inter = Interpreter(&mut game_boy_a);
            inter.interpret_op();
        }
        print!("\u{001b}[0m");
        {
            let mut inter = Interpreter(&mut game_boy_b);
            inter.interpret_op();
        }
        assert_equal(&game_boy_a, &game_boy_b);

        // 0x40 = LD B, B
        if game_boy_b.read(game_boy_b.cpu.pc) == 0x40 {
            break;
        }
    }

    println!("final clock_count: {}", game_boy_b.clock_count);

    if game_boy_b.clock_count >= timeout {
        panic!("reach timeout!!");
    }
    let regs = game_boy_b.cpu;

    if regs.a != 0 {
        panic!("{} assertion failures in hardware test", regs.a);
    }
    if regs.b != 3 || regs.c != 5 || regs.d != 8 || regs.e != 13 || regs.h != 21 || regs.l != 34 {
        panic!("Hardware test failed");
    }
}

fn assert_equal(game_boy_a: &GameBoy, game_boy_b: &GameBoy) {
    if game_boy_a.cpu != game_boy_b.cpu || game_boy_a.clock_count != game_boy_b.clock_count {
        println!("{:>15}: {:?}", game_boy_a.clock_count, game_boy_a.cpu);
        println!("{:>15}: {:?}", game_boy_b.clock_count, game_boy_b.cpu);

        println!("{:?}", game_boy_a.ppu.borrow().oam);
        println!("{:?}", game_boy_b.ppu.borrow().oam);
        panic!("CPU desync!");
    }
}
