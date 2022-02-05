use gameroy::cartridge::Cartridge;
use gameroy::save_state::SaveState;
use gameroy::{gameboy::GameBoy, interpreter::Interpreter};
use rand::{Rng, SeedableRng};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

const SCREEN_HEIGHT: usize = 144;
const SCREEN_WIDTH: usize = 160;

mod blargg {
    use super::*;

    macro_rules! console {
        { $( ( $test:ident, $path:expr, $timeout:expr ); )* } => {
            $(#[test]
            fn $test() {
                test_rom_serial($path, $timeout).unwrap();
            })*
        };
    }

    macro_rules! memory {
        { $( ( $test:ident, $path:expr, $timeout:expr ); )* } => {
            $(#[test]
            fn $test() {
                test_rom_memory($path, $timeout).unwrap();
            })*
        };
    }

    memory! {
        (dmg_sound_01, "dmg_sound/rom_singles/01-registers.gb", 30_600_000);
        (dmg_sound_02, "dmg_sound/rom_singles/02-len ctr.gb", 65_000_000);
        (dmg_sound_03, "dmg_sound/rom_singles/03-trigger.gb", 95_000_000);
        (dmg_sound_04, "dmg_sound/rom_singles/04-sweep.gb", 29_500_000);
        (dmg_sound_05, "dmg_sound/rom_singles/05-sweep details.gb", 29_300_000);
        (dmg_sound_06, "dmg_sound/rom_singles/06-overflow on trigger.gb", 29_800_000);
        (dmg_sound_07, "dmg_sound/rom_singles/07-len sweep period sync.gb", 27_400_000);
        (dmg_sound_08, "dmg_sound/rom_singles/08-len ctr during power.gb", 30_000_000);
        (dmg_sound_09, "dmg_sound/rom_singles/09-wave read while on.gb", 43000000);
        (dmg_sound_10, "dmg_sound/rom_singles/10-wave trigger while on.gb", 41_000_000);
        (dmg_sound_11, "dmg_sound/rom_singles/11-regs after power.gb", 27_700_000);
        (dmg_sound_12, "dmg_sound/rom_singles/12-wave write while on.gb", 41_000_000);
        (dmg_sound, "dmg_sound/dmg_sound.gb", 173_500_000);
    }

    console! {
        (instr_timing, "instr_timing/instr_timing.gb", 26_000_000);
    }

    console! {
        (mem_timing_01, "mem_timing/individual/01-read_timing.gb", 26_500_000);
        (mem_timing_02, "mem_timing/individual/02-write_timing.gb", 25_400_000);
        (mem_timing_03, "mem_timing/individual/03-modify_timing.gb", 30_600_000);
        (mem_timing, "mem_timing/mem_timing.gb", 30_600_000);
    }

    memory! {
        (mem_timing_2_01, "mem_timing-2/rom_singles/01-read_timing.gb", 25_200_000);
        (mem_timing_2_02, "mem_timing-2/rom_singles/02-write_timing.gb", 25_200_000);
        (mem_timing_2_03, "mem_timing-2/rom_singles/03-modify_timing.gb", 25_400_000);
        (mem_timing_2, "mem_timing-2/mem_timing.gb", 34_900_000);
    }

    console! {
        (cpu_instrs_01, "cpu_instrs/individual/01-special.gb", 34_500_000);
        (cpu_instrs_02, "cpu_instrs/individual/02-interrupts.gb", 43_000_000);
        (cpu_instrs_03, "cpu_instrs/individual/03-op sp,hl.gb", 34_000_000);
        (cpu_instrs_04, "cpu_instrs/individual/04-op r,imm.gb", 36_000_000);
        (cpu_instrs_05, "cpu_instrs/individual/05-op rp.gb", 40_000_000);
        (cpu_instrs_06, "cpu_instrs/individual/06-ld r,r.gb", 27_000_000);
        (cpu_instrs_07, "cpu_instrs/individual/07-jr,jp,call,ret,rst.gb", 28_000_000);
        (cpu_instrs_08, "cpu_instrs/individual/08-misc instrs.gb", 26_000_000);
        (cpu_instrs_09, "cpu_instrs/individual/09-op r,r.gb", 62_000_000);
        (cpu_instrs_10, "cpu_instrs/individual/10-bit ops.gb", 89_000_000);
        (cpu_instrs_11, "cpu_instrs/individual/11-op a,(hl).gb", 98_000_000);
        (cpu_instrs, "cpu_instrs/cpu_instrs.gb", 250_400_000);
    }

    fn test_rom_serial(path: &str, timeout: u64) -> Result<(), String> {
        let rom_path = "../roms/blargg/".to_string() + path;
        let rom = std::fs::read(rom_path).unwrap();

        let cartridge = Cartridge::new(rom).unwrap();

        let mut game_boy = GameBoy::new(None, cartridge);
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

        let mut inter = Interpreter(&mut game_boy);
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
        let rom_path = "../roms/blargg/".to_string() + path;
        let rom = std::fs::read(rom_path).unwrap();

        let cartridge = Cartridge::new(rom).unwrap();

        let mut game_boy = GameBoy::new(None, cartridge);

        let mut inter = Interpreter(&mut game_boy);
        while inter.0.clock_count < timeout {
            inter.interpret_op();
        }

        let signature = [
            inter.0.read(0xA001),
            inter.0.read(0xA002),
            inter.0.read(0xA003),
        ];
        if signature != [0xDE, 0xB0, 0x61] {
            return Err(format!(
                "invalid output to memory signature: {:0x?}",
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
}

#[test]
/// Run cpu_instrs for a random ammount of instructions, do a save state, and compare the load
/// state with the original. They should always be equal.
fn save_state() {
    let rom_path = "../roms/blargg/cpu_instrs/cpu_instrs.gb";
    let rom = std::fs::read(rom_path).unwrap();

    let cartridge = Cartridge::new(rom.clone()).unwrap();
    let mut game_boy = GameBoy::new(None, cartridge);

    let mut inter = Interpreter(&mut game_boy);
    let timeout = 250_400_000;
    let seed = rand::random();
    println!("test seed: {:08x}", seed);
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
    let mut count = 0;
    while inter.0.clock_count < timeout {
        let r = rng.gen_range(100_000..300_000);
        for _ in 0..r {
            inter.interpret_op();
        }
        use std::io::Cursor;
        let mut vec = Vec::new();
        inter.0.save_state(&mut vec).unwrap();
        let cartridge = Cartridge::new(rom.clone()).unwrap();
        let mut gb = GameBoy::new(None, cartridge);
        gb.load_state(&mut Cursor::new(&mut vec)).unwrap();
        assert_eq!(inter.0, &gb);
        count += 1;
    }
    println!("number of loads: {}", count);
}

mod mattcurrie {
    use std::path::PathBuf;

    use super::*;

    fn lcd_to_rgb(screen: &[u8; 144 * 160], img_data: &mut [u8]) {
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let i = (x + y * SCREEN_WIDTH) as usize * 3;
                let c = screen[i / 3];
                const COLOR: [[u8; 3]; 4] =
                    [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
            }
        }
    }

    fn test_screen(rom: &str, reference: &str, timeout: u64) {
        let rom_path = PathBuf::from("../roms/".to_string() + rom);
        let reference_path = "../roms/".to_string() + reference;
        let rom = std::fs::read(&rom_path).unwrap();

        let cartridge = Cartridge::new(rom).unwrap();

        let mut game_boy = GameBoy::new(None, cartridge);
        let screen: Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>> =
            Arc::new(Mutex::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]));
        let screen_clone = screen.clone();
        game_boy.v_blank = Some(Box::new(move |gb| {
            *screen_clone.lock().unwrap() = gb.ppu.screen;
        }));

        let mut inter = Interpreter(&mut game_boy);
        while inter.0.clock_count < timeout {
            inter.interpret_op();
            if inter.0.read(inter.0.cpu.pc) == 0x40 {
                break;
            }
        }

        if inter.0.clock_count >= timeout {
            println!("reach timeout!!");
        }

        let mut img_data = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT * 3];
        lcd_to_rgb(&*screen.lock().unwrap(), &mut img_data);
        let reference_img_data: &[u8] = &image::open(reference_path).unwrap().to_rgb8();

        if img_data != reference_img_data {
            panic!("screen don't match with expected image");
            let path = rom_path.file_stem().unwrap().to_string_lossy().to_string() + "_output.png";
            image::save_buffer(
                &path,
                &img_data,
                SCREEN_WIDTH as u32,
                SCREEN_HEIGHT as u32,
                image::ColorType::Rgb8,
            )
            .unwrap();
        }
    }

    macro_rules! screen {
        { $( $test:ident($rom:expr, $expec:expr, $timeout:expr, ); )* } => {
            $(#[test]
            fn $test() {
                test_screen($rom, $expec, $timeout);
            })*
        };
    }

    screen! {
        dmg_acid2(
            "dmg-acid2/dmg-acid2.gb",
            "dmg-acid2/dmg-acid2-dmg.png",
            24_554_332,
        );
        m2_win_en_toggle(
            "mealybug-tearoom-tests/ppu/m2_win_en_toggle.gb",
            "mealybug-tearoom-tests/ppu/m2_win_en_toggle_dmg_blob.png",
            24_065_976,
        );
        m3_bgp_change(
            "mealybug-tearoom-tests/ppu/m3_bgp_change.gb",
            "mealybug-tearoom-tests/ppu/m3_bgp_change_dmg_blob.png",
            24_065_976,
        );
        m3_bgp_change_sprites(
            "mealybug-tearoom-tests/ppu/m3_bgp_change_sprites.gb",
            "mealybug-tearoom-tests/ppu/m3_bgp_change_sprites_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_bg_en_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_bg_en_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_bg_en_change_dmg_blob.png",
            25_000_000,
        );
        // m3_lcdc_bg_en_change2(
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_bg_en_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_bg_en_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_lcdc_bg_map_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_bg_map_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_bg_map_change_dmg_blob.png",
            25_000_000,
        );
        // m3_lcdc_bg_map_change2(
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_bg_map_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_bg_map_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_lcdc_obj_en_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_obj_en_change_variant(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_variant.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_variant_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_obj_size_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_obj_size_change_scx(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change_scx.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change_scx_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_tile_sel_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_change_dmg_blob.png",
            25_000_000,
        );
        // m3_lcdc_tile_sel_change2(
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_lcdc_tile_sel_win_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_win_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_win_change_dmg_blob.png",
            25_000_000,
        );
        // m3_lcdc_tile_sel_win_change2(
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_win_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_tile_sel_win_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_lcdc_win_en_change_multiple(
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_win_en_change_multiple_wx(
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple_wx.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple_wx_dmg_blob.png",
            25_000_000,
        );
        m3_lcdc_win_map_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_map_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_map_change_dmg_blob.png",
            25_000_000,
        );
        // m3_lcdc_win_map_change2(
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_win_map_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_lcdc_win_map_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_obp0_change(
            "mealybug-tearoom-tests/ppu/m3_obp0_change.gb",
            "mealybug-tearoom-tests/ppu/m3_obp0_change_dmg_blob.png",
            25_000_000,
        );
        m3_scx_high_5_bits(
            "mealybug-tearoom-tests/ppu/m3_scx_high_5_bits.gb",
            "mealybug-tearoom-tests/ppu/m3_scx_high_5_bits_dmg_blob.png",
            25_000_000,
        );
        // m3_scx_high_5_bits_change2(
        //     "mealybug-tearoom-tests/ppu/m3_scx_high_5_bits_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_scx_high_5_bits_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_scx_low_3_bits(
            "mealybug-tearoom-tests/ppu/m3_scx_low_3_bits.gb",
            "mealybug-tearoom-tests/ppu/m3_scx_low_3_bits_dmg_blob.png",
            25_000_000,
        );
        m3_scy_change(
            "mealybug-tearoom-tests/ppu/m3_scy_change.gb",
            "mealybug-tearoom-tests/ppu/m3_scy_change_dmg_blob.png",
            25_000_000,
        );
        // m3_scy_change2(
        //     "mealybug-tearoom-tests/ppu/m3_scy_change2.gb",
        //     "mealybug-tearoom-tests/ppu/m3_scy_change2_dmg_blob.png",
        //     25_000_000,
        // );
        m3_window_timing(
            "mealybug-tearoom-tests/ppu/m3_window_timing.gb",
            "mealybug-tearoom-tests/ppu/m3_window_timing_dmg_blob.png",
            25_000_000,
        );
        m3_window_timing_wx_0(
            "mealybug-tearoom-tests/ppu/m3_window_timing_wx_0.gb",
            "mealybug-tearoom-tests/ppu/m3_window_timing_wx_0_dmg_blob.png",
            25_000_000,
        );
        m3_wx_4_change(
            "mealybug-tearoom-tests/ppu/m3_wx_4_change.gb",
            "mealybug-tearoom-tests/ppu/m3_wx_4_change_dmg_blob.png",
            25_000_000,
        );
        m3_wx_4_change_sprites(
            "mealybug-tearoom-tests/ppu/m3_wx_4_change_sprites.gb",
            "mealybug-tearoom-tests/ppu/m3_wx_4_change_sprites_dmg_blob.png",
            25_000_000,
        );
        m3_wx_5_change(
            "mealybug-tearoom-tests/ppu/m3_wx_5_change.gb",
            "mealybug-tearoom-tests/ppu/m3_wx_5_change_dmg_blob.png",
            25_000_000,
        );
        m3_wx_6_change(
            "mealybug-tearoom-tests/ppu/m3_wx_6_change.gb",
            "mealybug-tearoom-tests/ppu/m3_wx_6_change_dmg_blob.png",
            25_000_000,
        );
        // win_without_bg(
        //     "mealybug-tearoom-tests/ppu/win_without_bg.gb",
        //     "mealybug-tearoom-tests/ppu/win_without_bg_dmg_blob.png",
        //     25_000_000,
        // );
    }
}
