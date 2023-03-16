use std::{
    panic::catch_unwind,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rayon::prelude::*;

const TEST_ROM_PATH: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/gameboy-test-roms/"
);

/// The last time I run this, this took 11min15s.
#[test]
#[ignore]
fn test_all_files() -> Result<(), ()> {
    let search_paths = [
        TEST_ROM_PATH,
        // and any other folder with roms you migh want to test
    ];

    let mut tree: Vec<_> = search_paths
        .into_iter()
        .flat_map(|x| std::fs::read_dir(x).unwrap())
        .collect();
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

    // let r = std::time::SystemTime::now()
    //     .duration_since(std::time::UNIX_EPOCH)
    //     .unwrap()
    //     .as_millis() as usize
    //     % roms.len();
    // roms = vec![roms[r].clone()];
    roms = roms[10..50].to_vec();

    println!("\ntesting {} roms:", roms.len());

    let failed: Vec<_> = roms
        .into_par_iter()
        .filter_map({
            |rom| {
                let catch_unwind = catch_unwind(|| {
                    println!("{}:", rom);
                    test_interrupt_prediction(&rom, 20 * CLOCK_SPEED)
                });
                let ok = match catch_unwind {
                    Ok(ok) => ok,
                    Err(err) => {
                        println!(
                            "test_panicked!: {}",
                            err.downcast::<String>()
                                .map_or_else(|_| "Any".to_string(), |x| *x),
                        );
                        false
                    }
                };
                (!ok).then_some(rom)
            }
        })
        .collect();

    if failed.is_empty() {
        return Ok(());
    }

    print!("\u{001b}[31m");
    println!("The following roms failed the test:");
    for rom in failed {
        println!("{rom}");
    }
    print!("\u{001b}[0m");

    Err(())
}

#[test]
fn test_one() {
    let rom = r"blargg/cpu_instrs/cpu_instrs.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

#[test]
fn test_two() {
    let rom = r"blargg/instr_timing/instr_timing.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

#[derive(Default)]
struct VBlank {
    screen_a: Option<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    screen_b: Option<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    clock_count: Option<u64>,
}

fn test_interrupt_prediction(rom: &str, timeout: u64) -> bool {
    let rom_path: PathBuf = rom.into();
    let Ok(rom) = std::fs::read(rom_path) else {
        panic!("could not find rom: {}", rom);
    };
    let cartridge = match Cartridge::new(rom) {
        Ok(x) => x,
        Err(x) => {
            eprintln!("Error reading rom: {}", x);
            return true;
        }
    };

    let vblank = Arc::new(Mutex::new(VBlank::default()));

    let mut jit_compiler = gameroy_jit::JitCompiler::new();

    let mut game_boy_a = GameBoy::new(None, cartridge.clone());
    game_boy_a.predict_interrupt = true;
    game_boy_a.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_a = Some(gb.ppu.borrow().screen);
            vblank.clock_count = Some(gb.clock_count);
            if gb.clock_count == 81230904 {
                println!("{:?}", *gb.ppu.borrow());
            }
        }
    }));

    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = true;
    game_boy_b.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            let mut vblank = vblank.lock().unwrap();

            // The vblank shoud have been set by `gameboy_a`.
            match vblank.clock_count {
                Some(clock) if clock == gb.clock_count => {}
                _ => panic!(
                    "Clock count don't match?! {:?} != {}",
                    vblank.clock_count, gb.clock_count
                ),
            }

            vblank.screen_b = Some(gb.ppu.borrow().screen);
            if gb.clock_count == 81230904 {
                println!("{:?}", *gb.ppu.borrow());
            }
        }
    }));

    while game_boy_a.clock_count < timeout {
        // print!("\u{001b}[37m");
        {
            jit_compiler.interpret_block(&mut game_boy_a);
        }
        // print!("\u{001b}[0m");
        while game_boy_b.clock_count < game_boy_a.clock_count {
            Interpreter(&mut game_boy_b).interpret_op();
        }

        if !assert_equal(&game_boy_a, &game_boy_b, &vblank.lock().unwrap()) {
            return false;
        }

        // 0x40 = LD B, B
        if game_boy_b.read(game_boy_b.cpu.pc) == 0x40 {
            break;
        }
    }
    true
}

fn assert_equal(game_boy_a: &GameBoy, game_boy_b: &GameBoy, vblank: &VBlank) -> bool {
    let screen_unsync = vblank.screen_a != vblank.screen_b;
    #[allow(unused_must_use)]
    if game_boy_a.cpu != game_boy_b.cpu
        || game_boy_a.clock_count != game_boy_b.clock_count
        || screen_unsync
    {
        print!("\u{001b}[31m");
        println!("{:>15}: {:?}", game_boy_a.clock_count, game_boy_a.cpu);
        println!("{:>15}: {:?}", game_boy_b.clock_count, game_boy_b.cpu);

        println!("{:?}", game_boy_a.ppu.borrow().oam);
        println!("{:?}", game_boy_b.ppu.borrow().oam);

        if screen_unsync {
            let print_screen = |screen: &[u8; SCREEN_WIDTH * SCREEN_HEIGHT]| {
                println!("/{}\\", "-".repeat(SCREEN_WIDTH));
                for y in 0..SCREEN_HEIGHT {
                    print!("|");
                    for x in 0..SCREEN_WIDTH {
                        let c = screen[y * SCREEN_WIDTH + x];
                        if c < 4 {
                            print!("{}", [' ', '.', '+', '@', '~'][c as usize]);
                        } else {
                            print!("x{:x}", c);
                        }
                    }
                    println!("|");
                }
            };
            if let Some(x) = vblank.screen_a {
                print_screen(&x);
            } else {
                println!("None!!");
            }
            if let Some(x) = vblank.screen_b {
                print_screen(&x);
            } else {
                println!("None!!");
            }
        }

        print!("\u{001b}[0m");
        return false;
    }
    true
}
