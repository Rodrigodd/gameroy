#![cfg_attr(
    all(target_os = "windows", not(feature = "console")),
    windows_subsystem = "windows"
)]
#![cfg_attr(
    all(target_os = "windows", feature = "console"),
    windows_subsystem = "console"
)]

use std::path::PathBuf;

use clap::{ArgAction, Args, Parser, Subcommand};
use gameroy_lib::config::parse_screen_size;
use gameroy_lib::{config, gameroy, rom_loading::load_gameboy, RomFile};

mod bench;

// this struct is a mirror of gameroy_lib::Config.
#[derive(Parser)]
#[command(
    name = "GameRoy",
    version,
    author,
    about = "A high precision, high performance Game Boy emulator and debugger."
)]
pub struct Cli {
    /// Path to the game rom to be emulated
    rom_path: Option<String>,

    /// Start the emulator in debug mode
    #[arg(short, long)]
    debug: bool,

    /// Output to stdout the dissasembly of the rom
    //
    // The disassembly produced follows no particular synxtax, and don't show all instructions or
    // data. It only shows instructions that are statically reachable from the entry point.
    #[arg(long)]
    disassembly: bool,

    /// Play the given .vbm file
    #[arg(long)]
    movie: Option<String>,

    /// Specify the path of the folder for listing .gb roms
    #[arg(long = "rom_folder", value_name = "PATH")]
    rom_folder: Option<String>,

    /// Dump of the bootrom to be used
    #[arg(long = "boot_rom", value_name = "PATH")]
    boot_rom: Option<String>,

    /// Enables/disables rewinding
    #[arg(long, action = ArgAction::Set, value_name = "BOOL")]
    rewinding: Option<bool>,

    /// If the emulation will start running at max speed
    #[arg(long)]
    frame_skip: bool,

    /// Run the emulator with the Just-In-Time compiler
    #[arg(long)]
    jit: bool,

    /// Run the emulator with the interpreter
    #[arg(long)]
    interpreter: bool,

    /// The initial size of the window
    #[arg(long, value_name = "WIDTHxHEIGHT")]
    screen_size: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    // Emulate a given rom for some ammount of frames, and give back the time runned.
    Bench(Bench),
}

#[derive(Args)]
pub struct Bench {
    /// path to the game rom to be emulated
    rom_path: String,
    /// the number of frames to run for each run
    #[arg(short, long, default_value_t = 600)]
    frames: u64,
    /// the number of times run
    #[arg(short, long, default_value_t = 10)]
    times: usize,
    /// disables interrupt prediction optimization
    #[arg(long)]
    no_prediction: bool,
    /// run bench with the Just-In-Time compiler
    #[arg(long)]
    interpreter: bool,
    /// run bench with the interpreter
    #[arg(long)]
    jit: bool,
}

pub fn main() {
    let _logger = flexi_logger::Logger::try_with_env_or_str("gameroy=info")
        .unwrap()
        .log_to_file(
            flexi_logger::FileSpec::default()
                .directory(config::base_folder().unwrap_or_default())
                .basename("gameroy")
                .suppress_timestamp(),
        )
        .duplicate_to_stderr(if cfg!(debug_assertions) {
            flexi_logger::Duplicate::All
        } else {
            flexi_logger::Duplicate::None
        })
        .start()
        .unwrap();

    gameroy_lib::log_panic();

    let args: Cli = Cli::parse();

    if let Some(Commands::Bench(bench)) = args.command {
        return bench::benchmark(bench);
    }

    {
        let mut config = config::Config::load()
            .map_err(|e| log::error!("error loading config file 'gameroy.toml': {}", e))
            .unwrap_or_default();

        config.start_in_debug |= args.debug;

        config.rom_folder = args.rom_folder.or(config.rom_folder);

        config.boot_rom = args.boot_rom.or(config.boot_rom);

        config.rewinding = args.rewinding.unwrap_or(config.rewinding);

        config.frame_skip |= args.frame_skip;

        config.screen_size = args
            .screen_size
            .map(|x| {
                parse_screen_size(&x).unwrap_or_else(|err| {
                    eprintln!("failed to parse screen-size: {}", err);
                    std::process::exit(1)
                })
            })
            .or(config.screen_size);

        match (args.interpreter, args.jit) {
            (true, true) => {
                eprintln!("interpreter and jit are mutually exclusive");
                std::process::exit(1)
            }
            (true, false) => config.jit = false,
            (false, true) => config.jit = true,
            (false, false) => {}
        };

        gameroy_lib::config::init_config(config);
    }

    let diss = args.disassembly;
    let rom_path = args.rom_path;
    let movie = args.movie.map(|path| {
        let mut file = std::fs::File::open(path).unwrap();
        gameroy::parser::vbm(&mut file).unwrap()
    });

    // dissasembly and return early
    if diss {
        if let Some(rom_path) = &rom_path {
            let rom = std::fs::read(rom_path);

            let rom = match rom {
                Ok(x) => x,
                Err(e) => return eprintln!("failed to load '{}': {}", rom_path, e),
            };

            let gb = load_gameboy(rom, None);
            let mut gb = match gb {
                Ok(x) => x,
                Err(e) => return eprintln!("failed to load rom: {}", e),
            };
            gb.boot_rom_active = false;

            let mut string = String::new();
            gb.trace.borrow_mut().fmt(&gb, &mut string).unwrap();
            println!("{}", string);

            return;
        } else {
            unreachable!("the --disassembly flag already requires <ROM_PATH>");
        }
    }

    // load rom if necesary
    let gb = if let Some(rom_path) = &rom_path {
        let rom = std::fs::read(rom_path);

        let rom = match rom {
            Ok(x) => x,
            Err(e) => return eprintln!("failed to load '{}': {}", rom_path, e),
        };

        let file = RomFile::from_path(PathBuf::from(rom_path));

        let gb = load_gameboy(rom, None);
        match gb {
            Ok(x) => Some((file, x)),
            Err(e) => return eprintln!("failed to load rom: {}", e),
        }
    } else {
        None
    };

    gameroy_lib::main(gb, movie);
}
