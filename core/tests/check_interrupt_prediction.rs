use std::path::PathBuf;

use gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rayon::prelude::*;

const TEST_ROM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/gameboy-test-roms/");

/// The last time I run this, this took 40 min. But there were many println's at ppu interrupts.
#[test]
#[ignore]
fn test_all_files() {
    let mut tree: Vec<_> = std::fs::read_dir(TEST_ROM_PATH).unwrap().collect();
    let mut roms = Vec::new();
    while let Some(entry) = tree.pop() {
        let entry = entry.unwrap();
        println!("{}", entry.path().display());
        if entry.file_type().unwrap().is_dir() {
            tree.extend(std::fs::read_dir(entry.path()).unwrap());
        } else if entry.path().extension().and_then(|x| x.to_str()) == Some("gb") {
            roms.push(entry.path().to_str().unwrap().to_string());
        }
    }

    println!("\ntesting {} roms:", roms.len());

    roms.into_par_iter().for_each(|rom| {
        println!("{}:", rom);
        test_interrupt_prediction(&rom, 20 * CLOCK_SPEED);
    });
}

#[test]
fn test_one() {
    let rom = "mooneye-test-suite/acceptance/ppu/intr_2_0_timing.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 120 * CLOCK_SPEED;
    test_interrupt_prediction(&rom, timeout);
}

fn test_interrupt_prediction(rom: &str, timeout: u64) {
    let rom_path: PathBuf = rom.into();
    let rom = std::fs::read(rom_path).unwrap();
    let cartridge = Cartridge::new(rom).unwrap();
    let mut game_boy_a = GameBoy::new(None, cartridge.clone());
    game_boy_a.predict_interrupt = false;
    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = true;
    while game_boy_b.clock_count < timeout {
        // print!("\u{001b}[37m");
        {
            let mut inter = Interpreter(&mut game_boy_a);
            inter.interpret_op();
        }
        // print!("\u{001b}[0m");
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
