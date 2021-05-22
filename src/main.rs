use std::fs::File;

mod consts;
mod cpu;
mod dissasembler;
mod gameboy;
mod interpreter;

fn main() {
    let mut diss = false;
    let mut debug = false;
    let mut rom_path = "roms/test.gb".to_string();

    for arg in std::env::args() {
        match arg.as_str() {
            "-d" | "--dissasembly" => diss = true,
            "-b" | "--debug" => debug = true,
            _ if arg.starts_with("-") => {
                eprintln!("unknown argument {}", arg);
                return;
            }
            _ => {
                rom_path = arg;
            }
        }
    }

    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("bootrom/dmg_boot.bin").unwrap();

    let game_boy = gameboy::GameBoy::new(boot_rom_file, rom_file);

    let mut inter = interpreter::Interpreter(game_boy);

    if diss {
        let mut string = String::new();
        inter
            .0
            .trace
            .borrow_mut()
            .fmt(&inter.0, &mut string)
            .unwrap();
        println!("{}", string);
    } else if debug {
        loop {
            inter.debug();
        }
    } else {
        loop {
            inter.interpret_op();
        }
    }
}
