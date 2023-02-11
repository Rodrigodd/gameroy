use std::path::PathBuf;

use gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rayon::prelude::*;

const TEST_ROM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/gameboy-test-roms/");

/// The last time I run this, this took 11min15s.
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

    let failed: Vec<_> = roms
        .into_par_iter()
        .filter_map({
            |rom| {
                println!("{}:", rom);
                let ok = test_interrupt_prediction(&rom, 20 * CLOCK_SPEED);
                (!ok).then_some(rom)
            }
        })
        .collect();

    if failed.is_empty() {
        return;
    }

    print!("\u{001b}[31m");
    println!("The following roms failed the test:");
    for rom in failed {
        println!("{rom}");
    }
    print!("\u{001b}[0m");
}

#[test]
fn test_one() {
    let rom = "gambatte/halt/lycint_dmgpalette_during_m3_4.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

fn test_interrupt_prediction(rom: &str, timeout: u64) -> bool {
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
        if !assert_equal(&game_boy_a, &game_boy_b) {
            return false;
        }

        // 0x40 = LD B, B
        if game_boy_b.read(game_boy_b.cpu.pc) == 0x40 {
            break;
        }
    }
    true
}

fn assert_equal(game_boy_a: &GameBoy, game_boy_b: &GameBoy) -> bool {
    use std::io::Write;
    #[allow(unused_must_use)]
    if game_boy_a.cpu != game_boy_b.cpu || game_boy_a.clock_count != game_boy_b.clock_count {
        print!("\u{001b}[31m");
        println!("{:>15}: {:?}", game_boy_a.clock_count, game_boy_a.cpu);
        println!("{:>15}: {:?}", game_boy_b.clock_count, game_boy_b.cpu);

        println!("{:?}", game_boy_a.ppu.borrow().oam);
        println!("{:?}", game_boy_b.ppu.borrow().oam);
        print!("\u{001b}[0m");
        return false;
    }
    true
}
