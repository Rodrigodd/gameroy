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
        println!("{:>4} {}", roms.len(), entry.path().display());
        if entry.file_type().unwrap().is_dir() {
            tree.extend(std::fs::read_dir(entry.path()).unwrap());
        } else if entry.path().extension().and_then(|x| x.to_str()) == Some("gb") {
            roms.push(entry.path().to_str().unwrap().to_string());
        }
    }

    println!("\ntesting {} roms:", roms.len());

    let failed: Vec<_> = roms
        .into_iter()
        .enumerate()
        .par_bridge()
        .filter_map({
            |(i, rom)| {
                let catch_unwind = catch_unwind(|| {
                    println!("{i:>4} {}:", rom);
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

#[test]
fn test_sprite_late() {
    let rom = r"gambatte/sprites/sprite_late_late_disable_spx1B_2_dmg08_out3.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

#[test]
fn test_mbc3_rtc() {
    let rom = r"mealybug-tearoom-tests/mbc/mbc3_rtc.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

#[test]
fn test_three() {
    let rom = r"mooneye-test-suite/acceptance/ppu/intr_2_mode0_timing_sprites.gb";
    let rom = TEST_ROM_PATH.to_string() + rom;
    let timeout = 30 * CLOCK_SPEED;
    let ok = test_interrupt_prediction(&rom, timeout);
    if !ok {
        panic!("CPU desync!");
    }
}

#[test]
fn test_four() {
    let rom = r"mooneye-test-suite/acceptance/instr/daa.gb";
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
            // press the start button, repeatedly
            gb.joypad ^= 0x80;
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_a = Some(gb.ppu.borrow().screen.packed());
            vblank.clock_count = Some(gb.clock_count);
        }
    }));
    game_boy_a.serial.borrow_mut().serial_transfer_callback = None;

    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = true;
    game_boy_b.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            // press the start button, repeatedly
            gb.joypad ^= 0x80;
            let mut vblank = vblank.lock().unwrap();

            // The vblank shoud have been set by `gameboy_a`.
            match vblank.clock_count {
                Some(clock) if clock == gb.clock_count => {}
                _ => panic!(
                    "Clock count don't match?! {:?} != {}",
                    vblank.clock_count, gb.clock_count
                ),
            }

            vblank.screen_b = Some(gb.ppu.borrow().screen.packed());
        }
    }));
    game_boy_b.serial.borrow_mut().serial_transfer_callback = None;

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
            let mut dissasembly = String::new();
            game_boy_a
                .trace
                .borrow_mut()
                .fmt(&game_boy_b, &mut dissasembly)
                .unwrap();

            let h = &game_boy_b.cartridge.header;
            let _ = std::fs::create_dir("failed_test");
            let name = format!(
                "{}_{:02x}",
                h.title_as_string().split_whitespace().next().unwrap_or(""),
                h.global_checksum
            );
            println!("dumping ROM in {name}.s");
            std::fs::write("failed_test/".to_owned() + &name + ".s", dissasembly).unwrap();

            let bin_dir = format!("failed_test/{}", name);
            let _ = std::fs::remove_dir_all(&bin_dir);
            let _ = std::fs::create_dir(&bin_dir);

            // Dump compiled blocks.
            // Can be inspected using `objdump -D -b binary -Mintel,x86-64 -m i386 <file>`.
            for (address, block) in jit_compiler.blocks.iter() {
                std::fs::write(
                    format!(
                        "failed_test/{}/{:02x}_{:04x}.bin",
                        name, address.bank, address.address
                    ),
                    &*block._compiled_code,
                )
                .unwrap();
            }

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

    #[cfg(feature = "io_trace")]
    let io_unsync = !game_boy_a
        .io_trace
        .borrow()
        .iter()
        .filter(|(_, address, _)| *address >= 0x8000)
        .cmp(
            game_boy_b
                .io_trace
                .borrow()
                .iter()
                .filter(|(_, address, _)| *address >= 0x8000),
        )
        .is_eq();

    #[cfg(not(feature = "io_trace"))]
    let io_unsync = false;

    #[allow(unused_must_use)]
    if game_boy_a.cpu != game_boy_b.cpu
        || game_boy_a.clock_count != game_boy_b.clock_count
        || screen_unsync
        || io_unsync
    {
        print!("\u{001b}[31m");
        println!("{:>15}: {:x?}", game_boy_a.clock_count, game_boy_a.cpu);
        println!("{:>15}: {:x?}", game_boy_b.clock_count, game_boy_b.cpu);

        #[cfg(feature = "io_trace")]
        {
            let io_a = game_boy_a.io_trace.borrow();
            let io_b = game_boy_b.io_trace.borrow();
            let mut a = io_a.iter().copied();
            // .filter(|(_, address, _)| *address >= 0x8000);
            let mut b = io_b.iter().copied();
            // .filter(|(_, address, _)| *address >= 0x8000);

            println!("IO desync:");
            loop {
                let (a, b) = (a.next(), b.next());
                if let (None, None) = (a, b) {
                    break;
                }
                if let Some((kind, address, value)) = a {
                    print!(
                        "  {:1}{:02x} {value:02x} at {address:04x} | ",
                        if kind & 1 == 0 { 'R' } else { 'W' },
                        (kind & !1) << 1,
                    );
                } else {
                    print!("               | ");
                }

                if let Some((kind, address, value)) = b {
                    println!(
                        "{:1}{:02x} {value:02x} at {address:04x}",
                        if kind & 1 == 0 { 'R' } else { 'W' },
                        (kind & !1) << 1,
                    );
                } else {
                    println!("            ");
                }
            }
        }

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

    #[cfg(feature = "io_trace")]
    {
        game_boy_a.io_trace.borrow_mut().clear();
        game_boy_b.io_trace.borrow_mut().clear();
    }

    true
}
