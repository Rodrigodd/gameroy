use gameroy::{
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

// const BOOT_ROM: Option<[u8; 256]> = Some(*include_bytes!("../../boot/dmg_boot.bin"));
const BOOT_ROM: Option<[u8; 256]> = None;

#[test]
#[ignore]
fn test_boot_state() {
    assert!(
        BOOT_ROM.is_some(),
        "Set the BOOT_ROM to a reference DMG boot rom"
    );

    let cartridge = Cartridge::halt_filled();

    let mut a = GameBoy::new(BOOT_ROM, cartridge.clone());
    let b = GameBoy::new(None, cartridge.clone());

    while a.cpu.pc < 0x100 {
        Interpreter(&mut a).interpret_op();
    }

    assert_eq!(a.cpu.pc, b.cpu.pc);

    let mut state_a = Vec::new();
    a.save_state(None, &mut state_a).unwrap();

    let mut state_b = Vec::new();
    b.save_state(None, &mut state_b).unwrap();

    if a != b {
        assert!(assert_gb_eq(&a, &b));

        print_diff_mem(&a, &b);

        print_diff_state(&state_a, &state_b);

        panic!("States at end of boot are different");
    }
}

fn print_diff_mem(a: &GameBoy, b: &GameBoy) {
    println!("Address space diff:");

    let mut is_different = false;
    for i in (0..0xFFFF).step_by(8) {
        let row_a = (0..8).map(|j| a.read(i + j)).collect::<Vec<_>>();
        let row_b = (0..8).map(|j| b.read(i + j)).collect::<Vec<_>>();

        if row_a != row_b {
            is_different = true;
            println!("  {:04x?}: {:02x?}", i, &row_a);
            println!("      | {:02x?}", &row_b);
        }
    }
    if !is_different {
        println!("   No differences found");
    }
}

fn print_diff_state(a: &[u8], b: &[u8]) {
    assert_eq!(a.len(), b.len());

    println!("State diff:");

    for i in (0..a.len()).step_by(8) {
        let j = if i + 8 < a.len() { i + 8 } else { a.len() };
        if a[i..j] != b[i..j] {
            println!(" {:04x?}: {:02x?}", i, &a[i..i + 8]);
            println!("     | {:02x?}", &b[i..i + 8]);
        }
    }

    if a == b {
        println!("   No differences found");
    }
}

fn assert_gb_eq(a: &GameBoy, b: &GameBoy) -> bool {
    if a != b {
        println!();
        println!();
        if a.cpu != b.cpu {
            println!("cpu don't match: {:?}", a.cpu);
            println!("                 {:?}", b.cpu);
        }
        if a.cartridge != b.cartridge {
            println!("cartridge don't match")
        }
        if a.wram != b.wram {
            println!("wram don't match")
        }
        if a.hram != b.hram {
            println!("hram don't match")
        }
        if a.boot_rom_active != b.boot_rom_active {
            println!("boot_rom_active don't match")
        }
        if a.clock_count != b.clock_count {
            println!("clock_count don't match")
        }
        if a.timer != b.timer {
            println!("timer don't match: {:?}", a.timer);
            println!("                   {:?}", b.timer);
        }
        if a.sound != b.sound {
            println!("sound don't match: {:?}", a.sound);
            println!("                   {:?}", b.sound);
        }
        if a.ppu != b.ppu {
            println!("ppu don't match: {:?}", a.ppu);
            println!("                 {:?}", b.ppu);
        }
        if a.joypad != b.joypad {
            println!("joypad don't match: {:02x}", a.joypad);
            println!("                    {:02x}", b.joypad);
        }
        if a.joypad_io != b.joypad_io {
            println!("joypad_io don't match: {:02x}", a.joypad_io);
            println!("                       {:02x}", b.joypad_io);
        }
        // let mut vec = Vec::new();
        // a.save_state(&mut vec);
        // std::fs::write("gameboy_a.dump.bin", &vec);
        // vec.clear();
        // b.save_state(&mut vec);
        // std::fs::write("gameboy_b.dump.bin", vec);
        return false;
    }
    true
}
