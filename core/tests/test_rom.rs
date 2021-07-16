use gameroy::cartridge::Cartridge;
use gameroy::{gameboy::GameBoy, interpreter::Interpreter};
use std::fs::File;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

const SCREEN_HEIGHT: usize = 144;
const SCREEN_WIDTH: usize = 160;

macro_rules! blargg {
    ($test:ident, $path:expr, $timeout:expr) => {
        #[test]
        fn $test() {
            test_rom($path, $timeout).unwrap();
        }
    };
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
blargg!(blargg_cpu_instrs, "cpu_instrs.gb", 43000000);

fn test_rom(path: &str, timeout: u64) -> Result<(), String> {
    let rom_path = "../roms/".to_string() + path;
    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();

    let cartridge = Cartridge::new(rom_file).unwrap();

    let mut game_boy = GameBoy::new(boot_rom_file, cartridge).unwrap();
    // let mut game_boy = GameBoy::new(&vec![0; 256][..], rom_file);
    let string = Arc::new(Mutex::new(String::new()));
    let string_clone = string.clone();
    static STOP: AtomicBool = AtomicBool::new(false);
    game_boy.serial_transfer = Box::new(move |byte| {
        let mut string = string.lock().unwrap();
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
    if STOP.load(Ordering::Relaxed) {
        Ok(())
    } else {
        let string = string_clone.lock().unwrap();
        Err(format!("test rom failed: \n{}", string))
    }
}

fn lcd_to_rgba(screen: &[u8; 144 * 160], img_data: &mut [u8]) {
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let i = (x + y * SCREEN_WIDTH) as usize * 4;
            let c = screen[i / 4];
            const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
            img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
        }
    }
}

#[test]
fn dmg_acid2() {
    let rom_path = "../roms/dmg-acid2.gb";
    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();

    let cartridge = Cartridge::new(rom_file).unwrap();

    let mut game_boy = GameBoy::new(boot_rom_file, cartridge).unwrap();
    let img_data: Arc<Mutex<Vec<u8>>> =
        Arc::new(Mutex::new(vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4]));
    let img_data_clone = img_data.clone();
    game_boy.v_blank = Box::new(move |ppu| {
        let img_data: &mut [u8] = &mut img_data_clone.lock().unwrap();
        lcd_to_rgba(&ppu.screen, img_data);
    });

    let mut inter = Interpreter(game_boy);
    let timeout = 43000000;
    while inter.0.clock_count < timeout {
        inter.interpret_op();
        if inter.0.read(inter.0.cpu.pc) == 0x40 {
            break;
        }
    }

    let img_data: &[u8] = &mut img_data.lock().unwrap();
    let reference_img_data: &[u8] = &image::open("tests/reference-dmg.png").unwrap().to_rgba8();

    assert_eq!(img_data, reference_img_data);
}
