use gameroy::{interpreter::Interpreter, gameboy::GameBoy};
use std::fs::File;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};

macro_rules! blargg {
    ($test:ident, $path:expr, $timeout:expr) => {
        #[test]
        fn $test () {
            test_rom($path, $timeout).unwrap();
        }
    }
}

blargg!(blargg_01, "01-special.gb", 43000000);
blargg!(blargg_02, "02-interrupts.gb", 43000000);
blargg!(blargg_03, "03-op sp,hl.gb", 43000000);
blargg!(blargg_04, "04-op r,imm.gb", 43000000);
blargg!(blargg_05, "05-op rp.gb", 43000000);
blargg!(blargg_06, "06-ld r,r.gb", 43000000);
blargg!(blargg_07, "07-jr,jp,call,ret,rst.gb", 43000000);
blargg!(blargg_08, "08-misc instrs.gb", 43000000);
blargg!(blargg_09, "09-op r,r.gb", 43000000);
blargg!(blargg_10, "10-bit ops.gb", 43000000);
blargg!(blargg_11, "11-op a,(hl).gb", 43000000);

fn test_rom(path: &str, timeout: u64) -> Result<(), String> {
    let rom_path = "../roms/".to_string() + path;
    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();

    let mut game_boy = GameBoy::new(boot_rom_file, rom_file);
    // let mut game_boy = GameBoy::new(&vec![0; 256][..], rom_file);
    let string = Rc::new(RefCell::new(String::new()));
    let string_clone = string.clone();
    static STOP: AtomicBool = AtomicBool::new(false);
    game_boy.serial_transfer = Box::new(move |byte| {
        let mut string = string.borrow_mut();
        string.push(byte as char);
        if string.ends_with("Passed\n") {
            STOP.store(true, Ordering::Relaxed);
        }
    });

    let mut inter = Interpreter(game_boy);
    // while inter.0.clock_count < 33000000 {
    while inter.0.clock_count < timeout {
        inter.interpret_op();
        if STOP.load(Ordering::Relaxed) {
            break;
        }
    }
    // panic!("ahh");
    let string = string_clone.borrow();
    if STOP.load(Ordering::Relaxed) {
        Ok(())
    } else {
        Err(format!("test rom failed: \n{}", string))
    }
}
