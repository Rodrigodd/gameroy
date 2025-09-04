use std::{
    fs::OpenOptions,
    io::Write,
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, OnceLock,
    },
};

use gameroy::{
    consts::{CLOCK_SPEED, FRAME_CYCLES, SCREEN_HEIGHT, SCREEN_WIDTH},
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rand::{Rng, SeedableRng};

const TEST_ROM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/gameboy-test-roms/");

/// The test run faster when not providing a boot rom (the emulator just skip to the state at the
/// end of the boot rom execution), but it may be useful to include the boot rom execution in the
/// test to check if a boot rom changes any behavior and it's a valid replacement for the original.
// const BOOT_ROM: Option<[u8; 256]> = Some(*include_bytes!("../../boot/dmg_boot.bin"));
const BOOT_ROM: Option<[u8; 256]> = None;

/// Defines when the test should terminate during emulation
#[derive(Debug, Clone)]
pub enum TerminationCondition {
    /// Stop when timeout is reached
    TimeoutOnly,
    /// Stop when a specific opcode is executed (e.g., 0x40 = LD B, B)
    OpcodeExecution(u8),
    /// Stop when serial transfer callback detects a specific pattern
    SerialPattern(String),
}

/// Defines how to check if the test passed
#[derive(Debug, Clone)]
pub enum SuccessCheck {
    /// Compare screen output with reference image
    ScreenComparison { reference_path: String },
    /// Check CPU register values
    FibonacciRegisters,
    /// Check for "Passed" message in serial output
    SerialPassed,
    /// Check memory signature and status
    BlarggMemorySignature,
}

macro_rules! log {
    ($rom:expr, $str:literal $($t:tt)*) => {
        println!(concat!("\"{}\" ", $str), $rom $($t)*);
    }
}

fn save_test_rom_as_json(
    rom_path: &str,
    termination_condition: TerminationCondition,
    success_check: SuccessCheck,
    timeout_cycles: u64,
    final_screen: Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>>,
    passed: bool,
) -> Result<(), String> {
    let termination_condition = match termination_condition {
        TerminationCondition::TimeoutOnly => r#"[]"#.to_string(),
        TerminationCondition::OpcodeExecution(op) => {
            format!(r#"[{{"type":"opcode","opcode":{op}}}]"#)
        }
        TerminationCondition::SerialPattern(pattern) => {
            format!(r#"[{{"type":"serial_pattern","pattern":"{}"}}]"#, pattern)
        }
    };

    let mut success_checks = match success_check {
        SuccessCheck::ScreenComparison { ref reference_path } => {
            format!(
                r#"{{"type":"screen_comparison","reference_path":"{}"}}"#,
                reference_path
            )
        }
        SuccessCheck::FibonacciRegisters => r#"{"type":"fibonacci_registers"}"#.to_string(),
        SuccessCheck::SerialPassed => r#"{"type":"serial_passed"}"#.to_string(),
        SuccessCheck::BlarggMemorySignature => {
            r#"[{"type":"blargg_memory_signature"}]"#.to_string()
        }
    };

    if passed && !matches!(success_check, SuccessCheck::ScreenComparison { .. }) {
        let path = format!(
            "{}test_output/{}.png",
            TEST_ROM_PATH,
            rom_path.replace(".gb", "")
        );
        println!("Saving screen output to {}", path);

        let screen = final_screen.lock().unwrap();

        let mut img_data = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT * 3];
        lcd_to_rgb(&screen, &mut img_data);

        // create directory hierarchy if it doesn't exist
        let path_buf: PathBuf = path.clone().into();
        if let Some(parent) = path_buf.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create directory: {}", e))?;
        }

        image::save_buffer(
            &path,
            &img_data,
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
            image::ColorType::Rgb8,
        )
        .unwrap();

        success_checks += &format!(r#",{{"type":"screen_output","output_path":"{}"}}"#, path);
    }

    let json_object = format!(
        r#"{{"rom_path":"{}","termination_condition":[{}],"success_check":[{}],"timeout_cycles":{}}}"#,
        rom_path, termination_condition, success_checks, timeout_cycles
    );

    let file_path = "test_database.json";

    static JSON_WRITE_MUTEX: OnceLock<Mutex<()>> = OnceLock::new();
    static FIRST: AtomicBool = AtomicBool::new(true);
    let mutex = JSON_WRITE_MUTEX.get_or_init(|| Mutex::new(()));
    let _lock = mutex.lock().unwrap();
    let first = FIRST.swap(false, Ordering::Relaxed);
    let write_mode = if !first {
        OpenOptions::new().append(true).open(file_path)
    } else {
        OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(file_path)
    };

    match write_mode {
        Ok(mut file) => {
            writeln!(file, "{}", json_object)
                .map_err(|e| format!("Failed to write to file: {}", e))?;
            Ok(())
        }
        Err(e) => Err(format!("Failed to open file: {}", e)),
    }
}

/// Main test runner function that executes test ROMs with configurable conditions
///
/// This unified function replaces all the individual test functions and provides a flexible
/// interface for running Game Boy test ROMs with different termination and success conditions.
///
/// # Arguments
///
/// * `rom_path` - Path to the ROM file relative to TEST_ROM_PATH
/// * `termination_condition` - Defines when the test should stop during emulation:
///   - `OpcodeExecution(opcode)` - Stop when a specific opcode is executed (e.g., 0x40 = LD B, B)
///   - `TimeoutOnly` - Only stop when timeout is reached
///   - `SerialPattern(pattern)` - Stop when serial output contains the pattern
///   - `VBlankCondition` - Stop when v_blank callback determines condition is met
/// * `success_check` - Defines how to verify if the test passed:
///   - `ScreenComparison { reference_path }` - Compare screen output with reference image
///   - `RegisterCheck { a, b, c, d, e, h, l }` - Check CPU register values (None = don't check)
///   - `SerialPassed` - Check for "Passed" message in serial output  
///   - `MemoryCheck { ... }` - Check memory signature and status at specific addresses
/// * `timeout_cycles` - Maximum number of CPU cycles to run before timing out
///
/// # Returns
///
/// Returns `Ok(())` if the test passes, or `Err(String)` with error description if it fails.
fn run_test_rom(
    rom_path: &str,
    termination_condition: TerminationCondition,
    success_check: SuccessCheck,
    timeout_cycles: u64,
) -> Result<(), String> {
    let full_rom_path: PathBuf = (TEST_ROM_PATH.to_string() + rom_path).into();
    let rom = std::fs::read(&full_rom_path)
        .map_err(|e| format!("Failed to read ROM {}: {}", rom_path, e))?;

    let cartridge =
        Cartridge::new(rom).map_err(|e| format!("Failed to create cartridge: {:?}", e))?;

    let mut game_boy = GameBoy::new(BOOT_ROM, cartridge);

    // Setup shared state for different test types
    let screen = Arc::new(Mutex::new([0u8; SCREEN_WIDTH * SCREEN_HEIGHT]));
    let stop_condition_met = Arc::new(AtomicBool::new(false));
    let serial_output = Arc::new(Mutex::new(String::new()));

    // Setup callbacks based on termination condition and success check
    setup_callbacks(
        &mut game_boy,
        &termination_condition,
        &success_check,
        &screen,
        &stop_condition_met,
        &serial_output,
    )?;

    let mut inter = Interpreter(&mut game_boy);

    // Main emulation loop
    while inter.0.clock_count < timeout_cycles && !stop_condition_met.load(Ordering::Relaxed) {
        inter.interpret_op();

        // Check termination condition
        if should_terminate(&inter, &termination_condition, &serial_output) {
            break;
        }
    }

    // Run one more frame (some tests report success before rendering the test passed screen)
    for _ in 0..FRAME_CYCLES {
        inter.interpret_op();
    }

    log!(rom_path, "final clock_count: {}", inter.0.clock_count);

    // Check success condition
    let res = check_success(inter.0, &success_check, &screen, &serial_output, rom_path);

    if option_env!("GAMEROY_TEST_OUTPUT_JSON").is_some() {
        save_test_rom_as_json(
            rom_path,
            termination_condition,
            success_check,
            inter.0.clock_count.min(timeout_cycles),
            screen,
            res.is_ok(),
        )
        .unwrap();
    }

    res
}

fn setup_callbacks(
    game_boy: &mut GameBoy,
    termination_condition: &TerminationCondition,
    success_check: &SuccessCheck,
    screen: &Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>>,
    stop_condition_met: &Arc<AtomicBool>,
    serial_output: &Arc<Mutex<String>>,
) -> Result<(), String> {
    // Setup v_blank callback for screen tests
    let screen_clone = screen.clone();

    // Load reference image for screen comparison
    if let SuccessCheck::ScreenComparison { reference_path } = success_check {
        let stop_clone = stop_condition_met.clone();

        let reference_path_full = TEST_ROM_PATH.to_string() + reference_path;
        let reference_img_data = image::open(&reference_path_full)
            .map_err(|e| format!("Failed to load reference image {}: {}", reference_path, e))?
            .to_rgb8();
        let mut reference_screen = [0; SCREEN_WIDTH * SCREEN_HEIGHT];
        rgb_to_lcd(reference_img_data.as_ref(), &mut reference_screen);

        game_boy.v_blank = Some(Box::new(move |gb| {
            screen_clone
                .lock()
                .unwrap()
                .copy_from_slice(&gb.ppu.borrow().screen.packed());
            if reference_screen == gb.ppu.borrow().screen.packed() {
                stop_clone.store(true, Ordering::Relaxed);
            }
        }));
    } else {
        game_boy.v_blank = Some(Box::new(move |gb| {
            screen_clone
                .lock()
                .unwrap()
                .copy_from_slice(&gb.ppu.borrow().screen.packed());
        }));
    }

    // Setup serial callback for serial tests
    if matches!(success_check, SuccessCheck::SerialPassed)
        || matches!(
            termination_condition,
            TerminationCondition::SerialPattern(_)
        )
    {
        let serial_clone = serial_output.clone();
        let stop_clone = stop_condition_met.clone();

        game_boy.serial.get_mut().serial_transfer_callback = Some(Box::new(move |byte| {
            let mut output = serial_clone.lock().unwrap();
            output.push(byte as char);
            if output.ends_with("Passed") {
                stop_clone.store(true, Ordering::Relaxed);
            }
        }));
    }

    Ok(())
}

fn should_terminate(
    inter: &Interpreter,
    termination_condition: &TerminationCondition,
    serial_output: &Arc<Mutex<String>>,
) -> bool {
    match termination_condition {
        TerminationCondition::OpcodeExecution(opcode) => inter.0.read(inter.0.cpu.pc) == *opcode,
        TerminationCondition::TimeoutOnly => false,
        TerminationCondition::SerialPattern(pattern) => {
            serial_output.lock().unwrap().contains(pattern)
        }
    }
}

fn check_success(
    game_boy: &GameBoy,
    success_check: &SuccessCheck,
    screen: &Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>>,
    serial_output: &Arc<Mutex<String>>,
    rom_path: &str,
) -> Result<(), String> {
    match success_check {
        SuccessCheck::ScreenComparison { reference_path } => {
            let reference_path_full = TEST_ROM_PATH.to_string() + reference_path;
            let reference_img_data = image::open(&reference_path_full)
                .map_err(|e| format!("Failed to load reference image: {}", e))?
                .to_rgb8();
            let mut reference_screen = [0; SCREEN_WIDTH * SCREEN_HEIGHT];
            rgb_to_lcd(reference_img_data.as_ref(), &mut reference_screen);

            let current_screen = screen.lock().unwrap();
            if *current_screen == reference_screen {
                Ok(())
            } else {
                save_comparison_images(&current_screen, &reference_screen, rom_path)?;
                Err("Screen doesn't match expected image".to_string())
            }
        }
        SuccessCheck::FibonacciRegisters => {
            let regs = &game_boy.cpu;

            if regs.b != 3
                || regs.c != 5
                || regs.d != 8
                || regs.e != 13
                || regs.h != 21
                || regs.l != 34
            {
                return Err("Hardware test failed".to_string());
            }

            Ok(())
        }
        SuccessCheck::SerialPassed => {
            let output = serial_output.lock().unwrap();
            if output.contains("Passed") {
                Ok(())
            } else {
                Err(format!("Test rom failed: \n{}", output))
            }
        }
        SuccessCheck::BlarggMemorySignature => {
            let signature = [
                game_boy.read(0xA001),
                game_boy.read(0xA002),
                game_boy.read(0xA003),
            ];
            if signature != [0xDE, 0xB0, 0x61] {
                return Err(format!(
                    "Invalid output to memory signature: {:0x?}",
                    signature
                ));
            }

            let status_code = game_boy.read(0xA000);
            if status_code == 0 {
                Ok(())
            } else {
                let mut message = Vec::new();
                let mut addr = 0xA004;
                loop {
                    let value = game_boy.read(addr);
                    if value == 0 {
                        break;
                    }
                    message.push(value);
                    addr += 1;
                }
                let message_str =
                    String::from_utf8(message).unwrap_or_else(|_| "Invalid UTF-8".to_string());
                Err(format!(
                    "Test rom failed({:02x}): \n{}",
                    status_code, message_str
                ))
            }
        }
    }
}

fn save_comparison_images(
    current_screen: &[u8; SCREEN_WIDTH * SCREEN_HEIGHT],
    reference_screen: &[u8; SCREEN_WIDTH * SCREEN_HEIGHT],
    rom_path: &str,
) -> Result<(), String> {
    let path_buf = PathBuf::from(rom_path);
    let rom_name = path_buf
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    // Save current output
    let mut img_data = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT * 3];
    lcd_to_rgb(current_screen, &mut img_data);

    let output_path: PathBuf = format!("test_output/{}_output.png", rom_name).into();
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("Failed to create directory: {}", e))?;
    }
    image::save_buffer(
        &output_path,
        &img_data,
        SCREEN_WIDTH as u32,
        SCREEN_HEIGHT as u32,
        image::ColorType::Rgb8,
    )
    .map_err(|e| format!("Failed to save output image: {}", e))?;

    // Save expected output
    lcd_to_rgb(reference_screen, &mut img_data);
    let expected_path: PathBuf = format!("test_output/{}_expected.png", rom_name).into();
    image::save_buffer(
        &expected_path,
        &img_data,
        SCREEN_WIDTH as u32,
        SCREEN_HEIGHT as u32,
        image::ColorType::Rgb8,
    )
    .map_err(|e| format!("Failed to save expected image: {}", e))?;

    Ok(())
}

macro_rules! screen {
    { $( $(#[$($attrib:meta)*])* $test:ident($rom:expr, $expec:expr, $timeout:expr, ); )* } => {
        $(#[test] $(#[$($attrib)*])*
        fn $test() {
            run_test_rom(
                $rom,
                TerminationCondition::OpcodeExecution(0x40),
                SuccessCheck::ScreenComparison { reference_path: $expec.to_string() },
                $timeout,
            ).unwrap();
        })*
    };
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

// Legacy function wrappers for backward compatibility and specific test types
fn test_registers(romstr: &str, timeout: u64) {
    run_test_rom(
        romstr,
        TerminationCondition::OpcodeExecution(0x40),
        SuccessCheck::FibonacciRegisters,
        timeout,
    )
    .unwrap();
}

fn test_rom_serial(romstr: &str, timeout: u64) -> Result<(), String> {
    let full_path = format!("blargg/{}", romstr);
    run_test_rom(
        &full_path,
        TerminationCondition::SerialPattern("Passed".to_string()),
        SuccessCheck::SerialPassed,
        timeout,
    )
}

fn test_rom_memory(romstr: &str, timeout: u64) -> Result<(), String> {
    let full_path = format!("blargg/{}", romstr);
    run_test_rom(
        &full_path,
        TerminationCondition::TimeoutOnly,
        SuccessCheck::BlarggMemorySignature,
        timeout,
    )
}

fn test_age(romstr: &str, timeout: u64) {
    run_test_rom(
        romstr,
        TerminationCondition::OpcodeExecution(0x40),
        SuccessCheck::FibonacciRegisters,
        timeout,
    )
    .unwrap();
}

mod blargg {
    use super::*;

    macro_rules! console {
        { $($(#[$($attrib:meta)*])* ( $test:ident, $path:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_rom_serial($path, $timeout).unwrap();
            })*
        };
    }

    macro_rules! memory {
        { $($(#[$($attrib:meta)*])* ( $test:ident, $path:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_rom_memory($path, $timeout).unwrap();
            })*
        };
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
        (instr_timing, "instr_timing/instr_timing.gb", 26_500_000);
    }

    console! {
        (mem_timing_01, "mem_timing/individual/01-read_timing.gb", 26_500_000);
        (mem_timing_02, "mem_timing/individual/02-write_timing.gb", 25_400_000);
        (mem_timing_03, "mem_timing/individual/03-modify_timing.gb", 30_600_000);
        (mem_timing, "mem_timing/mem_timing.gb", 30_600_000);
    }

    memory! {
        (mem_timing_2_01, "mem_timing-2/rom_singles/01-read_timing.gb", 26_500_000);
        (mem_timing_2_02, "mem_timing-2/rom_singles/02-write_timing.gb", 25_400_000);
        (mem_timing_2_03, "mem_timing-2/rom_singles/03-modify_timing.gb", 30_500_000);
        (mem_timing_2, "mem_timing-2/mem_timing.gb", 36_000_000);
    }

    memory! {
        #[ignore]
        (oam_bug, "oam_bug/oam_bug.gb", 83_000_000);
    }

    screen! {
        halt_bug(
            "blargg/halt_bug.gb",
            "blargg/halt_bug-dmg-cgb.png",
            803_000_000,
        );
    }
}

#[test]
/// Run cpu_instrs for a random ammount of instructions, do a save state, and compare the load
/// state with the original. They should always be equal.
fn save_state1() {
    let romstr = TEST_ROM_PATH.to_string() + "blargg/cpu_instrs/cpu_instrs.gb";
    let rom = std::fs::read(&romstr).unwrap();

    let cartridge = Cartridge::new(rom.clone()).unwrap();
    let mut game_boy = GameBoy::new(BOOT_ROM, cartridge);

    let mut inter = Interpreter(&mut game_boy);
    let timeout = 250_400_000;
    let seed = rand::random();
    log!(romstr, "test seed: {:08x}", seed);
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
    let mut count = 0;

    let mut vec = Vec::new();
    while inter.0.clock_count < timeout {
        // run for a random ammount
        let r = rng.gen_range(100_000..300_000);
        for _ in 0..r {
            inter.interpret_op();
        }

        // save state
        use std::io::Cursor;
        vec.clear();
        inter.0.save_state(None, &mut vec).unwrap();

        // load state
        let cartridge = Cartridge::new(rom.clone()).unwrap();
        let mut gb = GameBoy::new(BOOT_ROM, cartridge);
        gb.load_state(&mut Cursor::new(&mut vec)).unwrap();

        // compare
        if !assert_gb_eq(inter.0, &gb) {
            panic!("SaveState desync!!");
        }

        count += 1;
    }
    log!(romstr, "number of loads: {}", count);
}

#[test]
/// Do save a save state, run cpu_instrs for a random ammount of instructions, load the save state,
/// run the same ammount of instructions, and compare with the first one. They should always be
/// equal.
fn save_state2() {
    let romstr = TEST_ROM_PATH.to_string() + "blargg/cpu_instrs/cpu_instrs.gb";
    let rom = std::fs::read(&romstr).unwrap();

    let cartridge = Cartridge::new(rom.clone()).unwrap();
    let mut game_boy = GameBoy::new(BOOT_ROM, cartridge);

    let mut inter = Interpreter(&mut game_boy);
    let timeout = 250_400_000;
    let seed = rand::random();
    log!(romstr, "test seed: {:08x}", seed);
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
    let mut count = 0;

    while inter.0.clock_count < timeout {
        // save state
        let mut save_state = Vec::new();
        inter.0.save_state(None, &mut save_state).unwrap();

        // run random number of instructions
        let r = rng.gen_range(100_000..300_000);
        log!(romstr, "steps: {}", r);
        for _ in 0..r {
            inter.interpret_op();
        }

        // load state
        use std::io::Cursor;
        let cartridge = Cartridge::new(rom.clone()).unwrap();
        let mut gb = GameBoy::new(BOOT_ROM, cartridge);
        gb.load_state(&mut Cursor::new(&mut save_state)).unwrap();

        // run same number of instructions
        for _ in 0..r {
            Interpreter(&mut gb).interpret_op();
        }

        // compare them!!
        if !assert_gb_eq(inter.0, &gb) {
            panic!("SaveState desync!!");
        }
        count += 1;
    }
    log!(romstr, "number of loads: {}", count);
}
#[test]
/// Do a save state in the v_blank callback, run a random number of instructions, load that save
/// state, run the same number of instructions, and compare with the first one. They should always
/// be equal.
fn save_state3() {
    let romstr = TEST_ROM_PATH.to_string() + "blargg/cpu_instrs/cpu_instrs.gb";
    let rom = std::fs::read(&romstr).unwrap();

    let cartridge = Cartridge::new(rom.clone()).unwrap();
    let mut game_boy = GameBoy::new(BOOT_ROM, cartridge);

    let v_blank_state = Arc::new(Mutex::new(None));

    // also test in v_blank(I do this in the emulator), because if a v_blank call happens before a
    // state mutation, there will be a SaveState desync after loading that save.
    game_boy.v_blank = Some(Box::new({
        let v_blank_state = v_blank_state.clone();
        move |original| {
            // save state
            let mut state = Vec::new();
            original.save_state(None, &mut state).unwrap();
            *v_blank_state.lock().unwrap() = Some(state);
        }
    }));

    let mut inter = Interpreter(&mut game_boy);
    let timeout = 250_400_000;
    let seed = rand::random();
    log!(romstr, "test seed: {:08x}", seed);
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
    let mut count = 0;

    while inter.0.clock_count < timeout {
        // run random number of instructions
        let r = rng.gen_range(100_000..300_000);
        log!(romstr, "steps: {}", r);
        for _ in 0..r {
            inter.interpret_op();
        }

        // load state
        if let Some(save_state) = v_blank_state.lock().unwrap().take() {
            use std::io::Cursor;
            let cartridge = Cartridge::new(rom.clone()).unwrap();
            let mut gb = GameBoy::new(BOOT_ROM, cartridge);
            gb.load_state(&mut Cursor::new(save_state)).unwrap();

            // run to the current state
            while gb.clock_count < inter.0.clock_count {
                Interpreter(&mut gb).interpret_op();
            }

            // compare them!!
            if !assert_gb_eq(inter.0, &gb) {
                panic!("SaveState desync!!");
            }
            count += 1;
        }
    }
    log!(romstr, "number of loads: {}", count);
}

fn diff<T: std::fmt::Debug>(a: &T, b: &T) {
    text_diff::print_diff(&format!("{:#?}", a), &format!("{:#?}", b), "\n");
}

fn assert_gb_eq(a: &GameBoy, b: &GameBoy) -> bool {
    a.update_all();
    b.update_all();

    if a != b {
        println!();
        println!();
        if a.cpu != b.cpu {
            println!("cpu don't match:");
            diff(&a.cpu, &b.cpu);
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
            println!("timer don't match:");
            diff(&a.timer, &b.timer);
        }
        if a.sound != b.sound {
            println!("sound don't match")
        }
        if a.ppu != b.ppu {
            println!("ppu don't match:");
            diff(&a.ppu, &b.ppu);
        }
        if a.joypad != b.joypad {
            println!("joypad don't match: {:02x}", a.joypad);
            println!("                    {:02x}", b.joypad);
        }
        if a.joypad_io != b.joypad_io {
            println!("joypad_io don't match")
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

mod mattcurrie {
    use super::*;

    macro_rules! registers {
        { $( $(#[$($attrib:meta)*])* $test:ident($rom:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_registers(concat!("mealybug-tearoom-tests/", $rom, ".gb"), $timeout);
            })*
        };
    }

    registers! {
        #[ignore]
        mbc3_rtc("mbc/mbc3_rtc", 204_872_220);
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
            30_222_844,
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
        #[ignore]
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
        #[ignore]
        m3_lcdc_obj_en_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_dmg_blob.png",
            25_000_000,
        );
        #[ignore]
        m3_lcdc_obj_en_change_variant(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_variant.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_en_change_variant_dmg_blob.png",
            25_000_000,
        );
        #[ignore]
        m3_lcdc_obj_size_change(
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_obj_size_change_dmg_blob.png",
            25_000_000,
        );
        #[ignore]
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
        #[ignore]
        m3_lcdc_win_en_change_multiple(
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple.gb",
            "mealybug-tearoom-tests/ppu/m3_lcdc_win_en_change_multiple_dmg_blob.png",
            25_000_000,
        );
        #[ignore]
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
        #[ignore]
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
        #[ignore]
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

mod mooneye {
    use super::*;

    macro_rules! registers {
        { $( $(#[$($attrib:meta)*])* $test:ident($rom:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_registers(concat!("mooneye-test-suite/", $rom, ".gb"), $timeout);
            })*
        };
    }

    screen! {
        sprite_priority(
            "mooneye-test-suite/manual-only/sprite_priority.gb",
            "mooneye-test-suite/manual-only/sprite_priority-dmg.png",
            24_200_000,
        );
    }

    registers! {
      add_sp_e_timing("acceptance/add_sp_e_timing", 120*CLOCK_SPEED);
      boot_div_dmg_abc_mgb("acceptance/boot_div-dmgABCmgb", 120*CLOCK_SPEED);
      boot_hwio_dmg_abc_mgb("acceptance/boot_hwio-dmgABCmgb", 120*CLOCK_SPEED);
      boot_regs_dmg_abc("acceptance/boot_regs-dmgABC", 120*CLOCK_SPEED);
      call_cc_timing("acceptance/call_cc_timing", 120*CLOCK_SPEED);
      call_cc_timing2("acceptance/call_cc_timing2", 120*CLOCK_SPEED);
      call_timing("acceptance/call_timing", 120*CLOCK_SPEED);
      call_timing2("acceptance/call_timing2", 120*CLOCK_SPEED);
      di_timing_gs("acceptance/di_timing-GS", 120*CLOCK_SPEED);
      div_timing("acceptance/div_timing", 120*CLOCK_SPEED);
      ei_sequence("acceptance/ei_sequence", 120*CLOCK_SPEED);
      ei_timing("acceptance/ei_timing", 120*CLOCK_SPEED);
      halt_ime0_ei("acceptance/halt_ime0_ei", 120*CLOCK_SPEED);
      halt_ime0_nointr_timing("acceptance/halt_ime0_nointr_timing", 120*CLOCK_SPEED);
      halt_ime1_timing("acceptance/halt_ime1_timing", 120*CLOCK_SPEED);
      halt_ime1_timing2_gs("acceptance/halt_ime1_timing2-GS", 120*CLOCK_SPEED);
      if_ie_registers("acceptance/if_ie_registers", 120*CLOCK_SPEED);
      intr_timing("acceptance/intr_timing", 120*CLOCK_SPEED);
      jp_cc_timing("acceptance/jp_cc_timing", 120*CLOCK_SPEED);
      jp_timing("acceptance/jp_timing", 120*CLOCK_SPEED);
      ld_hl_sp_e_timing("acceptance/ld_hl_sp_e_timing", 120*CLOCK_SPEED);
      oam_dma_restart("acceptance/oam_dma_restart", 120*CLOCK_SPEED);
      oam_dma_start("acceptance/oam_dma_start", 120*CLOCK_SPEED);
      oam_dma_timing("acceptance/oam_dma_timing", 120*CLOCK_SPEED);
      pop_timing("acceptance/pop_timing", 120*CLOCK_SPEED);
      push_timing("acceptance/push_timing", 120*CLOCK_SPEED);
      rapid_di_ei("acceptance/rapid_di_ei", 120*CLOCK_SPEED);
      ret_timing("acceptance/ret_timing", 120*CLOCK_SPEED);
      reti_timing("acceptance/reti_timing", 120*CLOCK_SPEED);
      ret_cc_timing("acceptance/ret_cc_timing", 120*CLOCK_SPEED);
      reti_intr_timing("acceptance/reti_intr_timing", 120*CLOCK_SPEED);
      rst_timing("acceptance/rst_timing", 120*CLOCK_SPEED);
      bits_mem_oam("acceptance/bits/mem_oam", 120*CLOCK_SPEED);
      bits_reg_f("acceptance/bits/reg_f", 120*CLOCK_SPEED);
      bits_unused_hwio_gs("acceptance/bits/unused_hwio-GS", 120*CLOCK_SPEED);
      instr_daa("acceptance/instr/daa", 120*CLOCK_SPEED);
      interrupts_ie_push("acceptance/interrupts/ie_push", 120*CLOCK_SPEED);
      oam_dma_basic("acceptance/oam_dma/basic", 120*CLOCK_SPEED);
      oam_dma_reg_read("acceptance/oam_dma/reg_read", 120*CLOCK_SPEED);
      oam_dma_sources_gs("acceptance/oam_dma/sources-GS", 120*CLOCK_SPEED);
      #[ignore]
      ppu_hblank_ly_scx_timing_gs("acceptance/ppu/hblank_ly_scx_timing-GS", 120*CLOCK_SPEED);
      ppu_intr_1_2_timing_gs("acceptance/ppu/intr_1_2_timing-GS", 120*CLOCK_SPEED);
      ppu_intr_2_0_timing("acceptance/ppu/intr_2_0_timing", 120*CLOCK_SPEED);
      ppu_intr_2_mode0_timing("acceptance/ppu/intr_2_mode0_timing", 120*CLOCK_SPEED);
      ppu_stat_lyc_onoff("acceptance/ppu/stat_lyc_onoff", 120*CLOCK_SPEED);
      ppu_intr_2_mode0_timing_sprites("acceptance/ppu/intr_2_mode0_timing_sprites", 120*CLOCK_SPEED);
      ppu_intr_2_mode3_timing("acceptance/ppu/intr_2_mode3_timing", 120*CLOCK_SPEED);
      ppu_intr_2_oam_ok_timing("acceptance/ppu/intr_2_oam_ok_timing", 120*CLOCK_SPEED);
      ppu_lcdon_timing_gs("acceptance/ppu/lcdon_timing-GS", 120*CLOCK_SPEED);
      ppu_lcdon_write_timing_gs("acceptance/ppu/lcdon_write_timing-GS", 120*CLOCK_SPEED);
      ppu_stat_irq_blocking("acceptance/ppu/stat_irq_blocking", 120*CLOCK_SPEED);
      ppu_vblank_stat_intr_gs("acceptance/ppu/vblank_stat_intr-GS", 120*CLOCK_SPEED);
      serial_boot_sclk_align_dmg_abc_mgb("acceptance/serial/boot_sclk_align-dmgABCmgb", 120*CLOCK_SPEED);
      timer_div_write("acceptance/timer/div_write", 120*CLOCK_SPEED);
      timer_rapid_toggle("acceptance/timer/rapid_toggle", 120*CLOCK_SPEED);
      timer_tim00("acceptance/timer/tim00", 120*CLOCK_SPEED);
      timer_tim00_div_trigger("acceptance/timer/tim00_div_trigger", 120*CLOCK_SPEED);
      timer_tim01("acceptance/timer/tim01", 120*CLOCK_SPEED);
      timer_tim01_div_trigger("acceptance/timer/tim01_div_trigger", 120*CLOCK_SPEED);
      timer_tim10("acceptance/timer/tim10", 120*CLOCK_SPEED);
      timer_tim10_div_trigger("acceptance/timer/tim10_div_trigger", 120*CLOCK_SPEED);
      timer_tim11("acceptance/timer/tim11", 120*CLOCK_SPEED);
      timer_tim11_div_trigger("acceptance/timer/tim11_div_trigger", 120*CLOCK_SPEED);
      timer_tima_reload("acceptance/timer/tima_reload", 120*CLOCK_SPEED);
      timer_tima_write_reloading("acceptance/timer/tima_write_reloading", 120*CLOCK_SPEED);
      timer_tma_write_reloading("acceptance/timer/tma_write_reloading", 120*CLOCK_SPEED);
      mbc1_bits_ramg("emulator-only/mbc1/bits_ramg", 120*CLOCK_SPEED);
      mbc1_bits_bank1("emulator-only/mbc1/bits_bank1", 120*CLOCK_SPEED);
      mbc1_bits_bank2("emulator-only/mbc1/bits_bank2", 120*CLOCK_SPEED);
      mbc1_bits_mode("emulator-only/mbc1/bits_mode", 120*CLOCK_SPEED);
      mbc1_rom_512kb("emulator-only/mbc1/rom_512kb", 120*CLOCK_SPEED);
      mbc1_rom_1mb("emulator-only/mbc1/rom_1Mb", 120*CLOCK_SPEED);
      mbc1_rom_2mb("emulator-only/mbc1/rom_2Mb", 120*CLOCK_SPEED);
      mbc1_rom_4mb("emulator-only/mbc1/rom_4Mb", 120*CLOCK_SPEED);
      mbc1_rom_8mb("emulator-only/mbc1/rom_8Mb", 120*CLOCK_SPEED);
      mbc1_rom_16mb("emulator-only/mbc1/rom_16Mb", 120*CLOCK_SPEED);
      mbc1_ram_64kb("emulator-only/mbc1/ram_64kb", 120*CLOCK_SPEED);
      mbc1_ram_25kb("emulator-only/mbc1/ram_256kb", 120*CLOCK_SPEED);
      mbc1_multicart_rom_8mb("emulator-only/mbc1/multicart_rom_8Mb", 120*CLOCK_SPEED);
      mbc2_bits_ramg("emulator-only/mbc2/bits_ramg", 120*CLOCK_SPEED);
      mbc2_bits_romb("emulator-only/mbc2/bits_romb", 120*CLOCK_SPEED);
      mbc2_bits_unused("emulator-only/mbc2/bits_unused", 120*CLOCK_SPEED);
      mbc2_rom_512kb("emulator-only/mbc2/rom_512kb", 120*CLOCK_SPEED);
      mbc2_rom_1mb("emulator-only/mbc2/rom_1Mb", 120*CLOCK_SPEED);
      mbc2_rom_2mb("emulator-only/mbc2/rom_2Mb", 120*CLOCK_SPEED);
      mbc2_ram("emulator-only/mbc2/ram", 120*CLOCK_SPEED);
      mbc5_rom_512kb("emulator-only/mbc5/rom_512kb", 120*CLOCK_SPEED);
      mbc5_rom_1mb("emulator-only/mbc5/rom_1Mb", 120*CLOCK_SPEED);
      mbc5_rom_2mb("emulator-only/mbc5/rom_2Mb", 120*CLOCK_SPEED);
      mbc5_rom_4mb("emulator-only/mbc5/rom_4Mb", 120*CLOCK_SPEED);
      mbc5_rom_8mb("emulator-only/mbc5/rom_8Mb", 120*CLOCK_SPEED);
      mbc5_rom_16mb("emulator-only/mbc5/rom_16Mb", 120*CLOCK_SPEED);
      mbc5_rom_32mb("emulator-only/mbc5/rom_32Mb", 120*CLOCK_SPEED);
      mbc5_rom_64mb("emulator-only/mbc5/rom_64Mb", 120*CLOCK_SPEED);
    }
}

mod age {
    use super::*;

    macro_rules! registers {
        { $( $(#[$($attrib:meta)*])* $test:ident($rom:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_age(concat!("age-test-roms/", $rom, ".gb"), $timeout);
            })*
        };
    }

    screen! {
        #[ignore]
        bgp(
            "age-test-roms/m3-bg-bgp/m3-bg-bgp.gb",
            "age-test-roms/m3-bg-bgp/m3-bg-bgp-dmgC.png",
            24_554_332,
        );
        lcdc(
            "age-test-roms/m3-bg-lcdc/m3-bg-lcdc.gb",
            "age-test-roms/m3-bg-lcdc/m3-bg-lcdc-dmgC.png",
            24_554_332,
        );
        #[ignore]
        scx(
            "age-test-roms/m3-bg-scx/m3-bg-scx.gb",
            "age-test-roms/m3-bg-scx/m3-bg-scx-dmgC.png",
            24_554_332,
        );
    }

    registers! {
        #[ignore]
        ei_halt("halt/ei-halt-dmgC-cgbBCE", 120*CLOCK_SPEED);
        #[ignore]
        halt_m0("halt/halt-m0-interrupt-dmgC-cgbBCE", 120*CLOCK_SPEED);
        #[ignore]
        halt_prefetch("halt/halt-prefetch-dmgC-cgbBCE", 120*CLOCK_SPEED);
        ly("ly/ly-dmgC-cgbBC", 120*CLOCK_SPEED);
        oam_read("oam/oam-read-dmgC-cgbBC", 120*CLOCK_SPEED);
        #[ignore]
        oam_write("oam/oam-write-dmgC", 120*CLOCK_SPEED);
        #[ignore]
        stat_interrupt("stat-interrupt/stat-int-dmgC-cgbBCE", 120*CLOCK_SPEED);
        stat_mode("stat-mode/stat-mode-dmgC-cgbBC", 120*CLOCK_SPEED);
        stat_mode_sprites("stat-mode-sprites/stat-mode-sprites-dmgC-cgbBCE", 120*CLOCK_SPEED);
        stat_mode_window("stat-mode-window/stat-mode-window-dmgC", 120*CLOCK_SPEED);
        vram_read("vram/vram-read-dmgC", 120*CLOCK_SPEED);
    }
}

mod same_suite {
    use super::*;

    macro_rules! registers {
        { $( $(#[$($attrib:meta)*])* $test:ident($rom:expr, $timeout:expr ); )* } => {
            $(#[test] $(#[$($attrib)*])*
            fn $test() {
                test_registers(concat!("same-suite/", $rom, ".gb"), $timeout);
            })*
        };
    }
    registers! {
        #[ignore]
        div_write_trigger("apu/div_write_trigger", 12*CLOCK_SPEED);
        #[ignore]
        div_write_trigger_10("apu/div_write_trigger_10", 12*CLOCK_SPEED);
        #[ignore]
        ei_delay_halt("interrupt/ei_delay_halt", 12*CLOCK_SPEED);
    }
}
