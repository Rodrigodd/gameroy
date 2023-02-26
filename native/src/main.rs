#![cfg_attr(
    all(target_os = "windows", not(feature = "console")),
    windows_subsystem = "windows"
)]
#![cfg_attr(
    all(target_os = "windows", feature = "console"),
    windows_subsystem = "console"
)]

use std::path::PathBuf;

use clap::{arg, Command};
use gameroy_lib::{config, gameroy, rom_loading::load_gameboy, RomFile, VERSION};

mod bench;

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

    let matches = Command::new("GameRoy")
        .version(VERSION)
        .author("Rodrigo Moraes")
        .about("A Game Boy emulator and debugger (and disassembler?).")
        .arg(arg!(-d - -debug "start the emulation in debug mode").required(false))
        .arg(
            arg!(--disassembly "output to stdout the dissasembly of the rom (it isn't working nor complete assembly)")
                .required(false)
                .requires("ROM_PATH"),
        )
        .arg(arg!(--movie <PATH> "play the given .vbm file").required(false))
        .arg(arg!(--boot_rom <PATH> "dump of the bootrom to be used").required(false))
        .arg(arg!(--rom_folder <PATH> "specify the path of the folder for listing .gb roms").required(false))
        .arg(arg!(<ROM_PATH> "path to the game rom to be emulated").required(false))
        .subcommand(Command::new("bench")
            .about("Emulate a given rom for some ammount of frames, and give back the time runned.")
            .arg(arg!(-f --frames <NUMBER> "the number of frames to run for each run")
                 .required(false)
                 .default_value("600")
                 .validator(|x| x.parse::<u64>())
            )
            .arg(arg!(-t --times <NUMBER> "the number of times run")
                 .required(false)
                 .default_value("10")
                 .validator(|x| x.parse::<u64>())
            )
            .arg(arg!(--"no-prediction" "disables interrupt prediction optimization")
                 .required(false)
            )
            .arg(arg!(<ROM_PATH> "path to the game rom to be emulated").required(true)))
        .get_matches();

    if let Some(("bench", matches)) = matches.subcommand() {
        let rom_path = matches.value_of("ROM_PATH").unwrap();
        let frames: u64 = matches
            .value_of("frames")
            .and_then(|x| x.parse().ok())
            .unwrap();
        let number_of_times: usize = matches
            .value_of("times")
            .and_then(|x| x.parse().ok())
            .unwrap();
        let predict_interrupt = !matches.is_present("no-prediction");
        return bench::benchmark(
            rom_path,
            frames * gameroy::consts::FRAME_CYCLES,
            number_of_times,
            predict_interrupt,
        );
    }

    let debug = matches.is_present("debug");
    let diss = matches.is_present("disassembly");
    let boot_rom_path = matches.value_of("boot_rom");
    let rom_folder = matches.value_of("rom_folder");
    let rom_path = matches.value_of("ROM_PATH");
    let movie = matches.value_of("movie").map(|path| {
        let mut file = std::fs::File::open(path).unwrap();
        gameroy::parser::vbm(&mut file).unwrap()
    });

    gameroy_lib::config::init_config({
        let mut config = config::Config::load()
            .map_err(|e| log::error!("error loading config file 'gameroy.toml': {}", e))
            .unwrap_or_default();
        config.start_in_debug |= debug;
        config.rom_folder = config
            .rom_folder
            .or_else(|| rom_folder.map(|x| x.to_string()));
        config.boot_rom = boot_rom_path.map(|x| x.to_string());
        config
    });

    // dissasembly and return early
    if diss {
        if let Some(rom_path) = rom_path {
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
    let gb = if let Some(rom_path) = rom_path {
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
