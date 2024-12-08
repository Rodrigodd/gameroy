use gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

fn parse_timeout(timeout: &str) -> Option<u64> {
    Some(if timeout.ends_with("s") {
        timeout[..timeout.len() - 1].parse::<u64>().ok()? * CLOCK_SPEED
    } else if timeout.ends_with("ms") {
        timeout[..timeout.len() - 2].parse::<u64>().ok()? * CLOCK_SPEED / 1000
    } else {
        timeout.parse::<u64>().ok()?
    })
}

fn main() {
    let mut args = std::env::args();
    let mut boot_rom_path = None;
    let mut rom_path = None;
    let mut timeout = CLOCK_SPEED; // 1 second

    // Skip program name
    let _ = args.next();

    while let Some(arg) = args.next() {
        if arg == "--boot" {
            boot_rom_path = Some(args.next().expect("Missing arg value"));
        } else if arg == "--timeout" {
            timeout = parse_timeout(&args.next().expect("Missing arg value"))
                .expect("Invalid timeout value");
        } else if arg.starts_with("--") {
            panic!("Unknown argument: {}", arg);
        } else {
            rom_path = Some(arg);
        }
    }

    println!(
        "Running ROM: {}",
        rom_path.as_ref().expect("No rom path provided")
    );
    println!("Boot ROM: {}", boot_rom_path.as_deref().unwrap_or("None"));

    let rom = std::fs::read(rom_path.expect("No rom path provided")).unwrap();
    let boot_rom = boot_rom_path.map(|path| {
        std::fs::read(&path)
            .unwrap()
            .try_into()
            .expect("Boot ROM must be 256 bytes")
    });

    let cartridge = Cartridge::new(rom).expect("Invalid ROM");

    println!(
        "Cartridge: {:} ({})",
        cartridge.kind_name(),
        cartridge.header.cartridge_type
    );

    let mut gameboy = GameBoy::new(boot_rom, cartridge);

    let mut inter = Interpreter(&mut gameboy);

    while inter.0.clock_count < timeout {
        inter.interpret_op();
        if inter.0.read(inter.0.cpu.pc) == 0x40 {
            println!("LD B, B detected, stopping");
            break;
        }
    }
}
