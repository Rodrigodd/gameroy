use gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

fn f64_to_u64(x: f64) -> Option<u64> {
    let y = x as u64;
    if y as f64 != x {
        return None;
    }
    Some(y)
}

fn parse_timeout(timeout: &str) -> Option<u64> {
    let time = if let Some(value) = timeout.strip_suffix("s") {
        value.parse::<f64>().ok()? * CLOCK_SPEED as f64
    } else if let Some(value) = timeout.strip_suffix("ms") {
        value.parse::<f64>().ok()? * CLOCK_SPEED as f64 / 1000.0
    } else {
        timeout.parse::<f64>().ok()?
    };
    let time = f64_to_u64(time.ceil())?;
    Some(time)
}

const HELP: &str = "Usage: run [--boot <boot_rom_path>] [--timeout <timeout>] <rom_path>";
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
        } else if arg == "--help" {
            println!("{}", HELP);
            return;
        } else if arg.starts_with("--") {
            eprintln!("Unknown argument: {}", arg);
            eprintln!("{}", HELP);
            std::process::exit(1);
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
    let boot_rom = boot_rom_path.map(|path| match std::fs::read(&path).unwrap().try_into() {
        Ok(t) => t,
        Err(_) => panic!("Boot ROM must be 256 bytes"),
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
    #[cfg(feature = "wave_trace")]
    {
        inter.0.update_all();
        println!("VCD committed on end: {}", inter.0.clock_count);
        inter.0.vcd_writer.commit().unwrap();
    }
}
