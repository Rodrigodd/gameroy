use gameroy::cartridge::Cartridge;
use gameroy::{gameboy::GameBoy, interpreter::Interpreter};
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

const SCREEN_HEIGHT: usize = 144;
const SCREEN_WIDTH: usize = 160;

macro_rules! console {
    ($test:ident, $path:expr, $timeout:expr) => {
        #[test]
        fn $test() {
            test_rom_serial($path, $timeout).unwrap();
        }
    };
}

macro_rules! memory {
    ($test:ident, $path:expr, $timeout:expr) => {
        #[test]
        fn $test() {
            test_rom_memory($path, $timeout).unwrap();
        }
    };
}

memory!(dmg_sound_01, "dmg_sound/01-registers.gb", 30_600_000);
memory!(dmg_sound_02, "dmg_sound/02-len ctr.gb", 65_000_000);
memory!(dmg_sound_03, "dmg_sound/03-trigger.gb", 95_000_000);
memory!(dmg_sound_04, "dmg_sound/04-sweep.gb", 29_500_000);
memory!(dmg_sound_05, "dmg_sound/05-sweep details.gb", 29_300_000);
memory!(
    dmg_sound_06,
    "dmg_sound/06-overflow on trigger.gb",
    29_800_000
);
memory!(
    dmg_sound_07,
    "dmg_sound/07-len sweep period sync.gb",
    27_400_000
);
memory!(
    dmg_sound_08,
    "dmg_sound/08-len ctr during power.gb",
    30_000_000
);
memory!(dmg_sound_09, "dmg_sound/09-wave read while on.gb", 43000000);
memory!(
    dmg_sound_10,
    "dmg_sound/10-wave trigger while on.gb",
    41_000_000
);
memory!(dmg_sound_11, "dmg_sound/11-regs after power.gb", 27_700_000);
memory!(
    dmg_sound_12,
    "dmg_sound/12-wave write while on.gb",
    41_000_000
);
memory!(dmg_sound, "dmg_sound.gb", 173_500_000);

console!(blargg_01, "01-special.gb", 34_500_000);
console!(blargg_02, "02-interrupts.gb", 43_000_000);
console!(blargg_03, "03-op sp,hl.gb", 34_000_000);
console!(blargg_04, "04-op r,imm.gb", 36_000_000);
console!(blargg_05, "05-op rp.gb", 40_000_000);
console!(blargg_06, "06-ld r,r.gb", 27_000_000);
console!(blargg_07, "07-jr,jp,call,ret,rst.gb", 28_000_000);
console!(blargg_08, "08-misc instrs.gb", 26_000_000);
console!(blargg_09, "09-op r,r.gb", 62_000_000);
console!(blargg_10, "10-bit ops.gb", 89_000_000);
console!(blargg_11, "11-op a,(hl).gb", 98_000_000);
console!(blargg_cpu_instrs, "cpu_instrs.gb", 250_400_000);

fn test_rom_serial(path: &str, timeout: u64) -> Result<(), String> {
    let rom_path = "../roms/".to_string() + path;
    let rom = std::fs::read(rom_path).unwrap();
    let mut boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();
    let mut boot_rom = [0; 0x100];
    boot_rom_file.read(&mut boot_rom).unwrap();

    let cartridge = Cartridge::new(rom).unwrap();

    let mut game_boy = GameBoy::new(boot_rom, cartridge);
    // let mut game_boy = GameBoy::new(&vec![0; 256][..], rom_file);
    let string = Arc::new(Mutex::new(String::new()));
    let string_clone = string.clone();
    let stop = Arc::new(AtomicBool::new(false));
    game_boy.serial_transfer = Box::new({
        let stop = stop.clone();
        move |byte| {
            let mut string = string.lock().unwrap();
            string.push(byte as char);
            if string.ends_with("Passed") {
                stop.store(true, Ordering::Relaxed);
            }
        }
    });

    let mut inter = Interpreter(game_boy);
    // while inter.0.clock_count < 33000000 {
    while inter.0.clock_count < timeout {
        inter.interpret_op();
        if stop.load(Ordering::Relaxed) {
            break;
        }
    }
    // panic!("ahh");
    if stop.load(Ordering::Relaxed) {
        Ok(())
        // let string = string_clone.lock().unwrap();
        // Err(format!("test rom failed: \n{}", string))
    } else {
        let string = string_clone.lock().unwrap();
        Err(format!("test rom failed: \n{}", string))
    }
}

fn test_rom_memory(path: &str, timeout: u64) -> Result<(), String> {
    let rom_path = "../roms/".to_string() + path;
    let rom = std::fs::read(rom_path).unwrap();
    let mut boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();
    let mut boot_rom = [0; 0x100];
    boot_rom_file.read(&mut boot_rom).unwrap();

    let cartridge = Cartridge::new(rom).unwrap();

    let game_boy = GameBoy::new(boot_rom, cartridge);

    let mut inter = Interpreter(game_boy);
    // while inter.0.clock_count < 33000000 {
    while inter.0.clock_count < timeout {
        inter.interpret_op();
    }

    let signature = &inter.0.memory[0xA001..=0xA003];
    if signature == [0xDE, 0xB0, 0x61] {
        return Err(format!(
            "invalid output to memory signature: {:?}",
            signature
        ));
    }

    let status_code = inter.0.read(0xA000);

    // panic!("ahh");
    if status_code == 0 {
        Ok(())
        // let string = string_clone.lock().unwrap();
        // Err(format!("test rom failed: \n{}", string))
    } else {
        let string = {
            let mut i = 0xA004;
            let mut string = Vec::new();
            loop {
                let value = inter.0.read(i);
                if value == 0 {
                    break;
                }
                string.push(value);
                i += 1;
            }
            String::from_utf8(string).unwrap()
        };
        Err(format!("test rom failed({}): \n{}", status_code, string))
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
    let rom = std::fs::read(rom_path).unwrap();
    let mut boot_rom_file = File::open("../bootrom/dmg_boot.bin").unwrap();
    let mut boot_rom = [0; 0x100];
    boot_rom_file.read(&mut boot_rom).unwrap();

    let cartridge = Cartridge::new(rom).unwrap();

    let mut game_boy = GameBoy::new(boot_rom, cartridge);
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
