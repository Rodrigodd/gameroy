use gameroy::{
    consts::{CLOCK_SPEED, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rayon::prelude::*;
use std::io::Write;
use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};

const TEST_ROM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../GBEmulatorShootout/");
const TSV_FILE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../GBEmulatorShootout/test.tsv"
));

// const BOOT_ROM: Option<[u8; 256]> = Some(*include_bytes!("../../boot/dmg_boot.bin"));
const BOOT_ROM: Option<[u8; 256]> = None;

macro_rules! log {
    ($rom:expr, $str:literal $($t:tt)*) => {
        println!(concat!("\"{}\" ", $str), $rom $($t)*);
    }
}

#[test]
#[ignore]
fn from_tsv() {
    let mut results: Vec<_> = TSV_FILE
        .lines()
        .enumerate()
        .par_bridge()
        .map(|(i, line)| {
            let [_name, rom_path, result_path, _runtime] =
                &line.split('\t').collect::<Vec<_>>()[..]
            else {
                panic!("could not parse line: {:?}", line);
            };

            // let timeout =
            //     (runtime.parse::<f64>().expect("invalid runtime") * CLOCK_SPEED as f64) as u64;

            let (ok, timeout) = test_screen(rom_path, result_path, 120 * CLOCK_SPEED);

            if ok {
                println!("Test sucess!!");
            } else {
                println!("Test failed!!");
            }

            (
                i,
                rom_path.to_string(),
                result_path.to_string(),
                ok,
                timeout,
            )
        })
        .collect();

    results.sort_by_key(|x| x.0);

    let mut output = std::fs::File::create("test_results.tsv").unwrap();
    for (_i, rom_path, result_path, ok, timeout) in results {
        println!("{} {}", if ok { "PASS  " } else { "FAILED" }, rom_path);
        writeln!(
            output,
            "{}\t{}\t{}\t{}",
            rom_path,
            result_path,
            timeout - 23_440_324,
            if ok { "ok" } else { "failed" },
        )
        .unwrap();
    }
}

fn test_screen(romstr: &str, reference: &str, timeout: u64) -> (bool, u64) {
    let rom_path: PathBuf = (TEST_ROM_PATH.to_string() + romstr).into();
    let reference_path = TEST_ROM_PATH.to_string() + reference;
    println!("{}", reference_path);
    let rom = std::fs::read(&rom_path).unwrap();

    let cartridge = Cartridge::new(rom).unwrap();

    let mut game_boy = GameBoy::new(BOOT_ROM, cartridge);

    let screen = Arc::new(Mutex::new([0u8; SCREEN_WIDTH * SCREEN_HEIGHT]));
    let matched = Arc::new(AtomicBool::new(false));

    let reference_img_data: &[u8] = &image::open(reference_path).unwrap().to_rgb8();
    let mut reference_screen = [0; SCREEN_WIDTH * SCREEN_HEIGHT];
    rgb_to_lcd(reference_img_data, &mut reference_screen);

    game_boy.v_blank = Some(Box::new({
        let matched = matched.clone();
        let screen = screen.clone();
        move |gb| {
            screen
                .lock()
                .unwrap()
                .copy_from_slice(&gb.ppu.borrow().screen.packed());
            if reference_screen == gb.ppu.borrow().screen.packed() {
                matched.store(true, Ordering::Relaxed);
            }
        }
    }));

    let mut inter = Interpreter(&mut game_boy);

    while inter.0.clock_count < timeout && !matched.load(Ordering::Relaxed) {
        inter.interpret_op();
    }
    log!(romstr, "final clock_count: {}", inter.0.clock_count);

    if inter.0.clock_count >= timeout {
        log!(romstr, "reach timeout!!");
    }

    if !matched.load(Ordering::Relaxed) {
        let mut img_data = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT * 3];
        lcd_to_rgb(&screen.lock().unwrap(), &mut img_data);

        let path: PathBuf = ("test_output/".to_string()
            + &rom_path.file_stem().unwrap().to_string_lossy()
            + "_output.png")
            .into();
        if let Some(x) = path.parent() {
            std::fs::create_dir_all(x).unwrap()
        }
        image::save_buffer(
            &path,
            &img_data,
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
            image::ColorType::Rgb8,
        )
        .unwrap();

        let mut img_data = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT * 3];
        lcd_to_rgb(&reference_screen, &mut img_data);

        return (false, inter.0.clock_count);
    }

    (true, inter.0.clock_count)
}

fn lcd_to_rgb(screen: &[u8; 144 * 160], img_data: &mut [u8]) {
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let i = (x + y * SCREEN_WIDTH) * 3;
            let c = screen[i / 3];
            const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
            img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
        }
    }
}

fn rgb_to_lcd(screen: &[u8], img_data: &mut [u8; 144 * 160]) {
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let i = (x + y * SCREEN_WIDTH) * 3;
            let r = screen[i];
            let g = screen[i + 1];
            let b = screen[i + 2];
            let intensity = r.max(g).max(b);
            // these intervals are arbitrary, it just need to work with the lcd_to_rgb above
            let c = match intensity {
                0..=39 => 3,    // 0
                40..=119 => 2,  // 85
                120..=219 => 1, // 170
                220..=255 => 0, // 255
            };
            img_data[i / 3] = c;
        }
    }
}
