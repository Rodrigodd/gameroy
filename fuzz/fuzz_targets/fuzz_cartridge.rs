#![no_main]

use std::sync::{Arc, Mutex};

use libfuzzer_sys::fuzz_target;

use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
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

    let start_clock = 23_440_324;
    // emulate 100 ms
    let target = start_clock + CLOCK_SPEED / 10;

    if !test_interrupt_prediction(cartridge.clone(), target) {
        panic!("CPU desync!");
    }

    if !test_jit(cartridge, target) {
        panic!("CPU desync!");
    }
});

#[derive(Default)]
struct VBlank {
    screen_a: Option<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    screen_b: Option<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    clock_count_a: Option<u64>,
    clock_count_b: Option<u64>,
}

fn test_interrupt_prediction(cartridge: Cartridge, target: u64) -> bool {
    let vblank = Arc::new(Mutex::new(VBlank::default()));

    let mut game_boy_a = GameBoy::new(None, cartridge.clone());
    game_boy_a.predict_interrupt = true;
    game_boy_a.serial.borrow_mut().serial_transfer_callback = None;
    game_boy_a.sound.get_mut().sample_frequency = 44100;
    game_boy_a.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_a = Some(gb.ppu.borrow().screen);
            vblank.clock_count_a = Some(gb.clock_count);
        }
    }));

    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = false;
    game_boy_b.serial.borrow_mut().serial_transfer_callback = None;
    game_boy_b.sound.get_mut().sample_frequency = 44100;
    game_boy_b.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_b = Some(gb.ppu.borrow().screen);
            vblank.clock_count_b = Some(gb.clock_count);
        }
    }));

    while game_boy_a.clock_count < target {
        // print!("\u{001b}[37m");
        {
            let mut inter = Interpreter(&mut game_boy_a);
            inter.interpret_op();
        }
        // print!("\u{001b}[0m");
        // the gameboy with predict interrupt may skip most part of a halt.
        while game_boy_b.clock_count < game_boy_a.clock_count {
            let mut inter = Interpreter(&mut game_boy_b);
            inter.interpret_op();
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

fn test_jit(cartridge: Cartridge, target: u64) -> bool {
    let vblank = Arc::new(Mutex::new(VBlank::default()));

    let mut jit_compiler = gameroy_jit::JitCompiler::new();

    let mut game_boy_a = GameBoy::new(None, cartridge.clone());
    game_boy_a.predict_interrupt = true;
    game_boy_a.serial.borrow_mut().serial_transfer_callback = None;
    game_boy_a.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            // press the start button, repeatedly
            gb.joypad ^= 0x80;
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_a = Some(gb.ppu.borrow().screen);
            vblank.clock_count_a = Some(gb.clock_count);
        }
    }));

    let mut game_boy_b = GameBoy::new(None, cartridge);
    game_boy_b.predict_interrupt = true;
    game_boy_b.serial.borrow_mut().serial_transfer_callback = None;
    game_boy_b.v_blank = Some(Box::new({
        let vblank = vblank.clone();
        move |gb| {
            // press the start button, repeatedly
            gb.joypad ^= 0x80;
            let mut vblank = vblank.lock().unwrap();
            vblank.screen_b = Some(gb.ppu.borrow().screen);
            vblank.clock_count_b = Some(gb.clock_count);
        }
    }));

    while game_boy_a.clock_count < target {
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
            game_boy_b
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
        || vblank.clock_count_a != vblank.clock_count_b
        || screen_unsync
        || io_unsync
    {
        print!("\u{001b}[31m");
        println!("{:>15}: {:x?}", game_boy_a.clock_count, game_boy_a.cpu);
        println!("{:>15}: {:x?}", game_boy_b.clock_count, game_boy_b.cpu);

        if vblank.clock_count_a != vblank.clock_count_b {
            println!("vblank a: {:?}", vblank.clock_count_a);
            println!("vblank b: {:?}", vblank.clock_count_b);
        }

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
