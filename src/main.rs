use std::fs::File;
use std::io::Read;

mod cpu;
mod dissasembler;
mod interpreter;
mod memory;
mod opcode;

fn main() {
    let mut diss = false;
    let mut debug = false;
    let mut file_path = "roms/test.gb".to_string();

    for arg in std::env::args() {
        match arg.as_str() {
            "-d" | "--dissasembly" => diss = true,
            "-b" | "--debug" => debug = true,
            _ if arg.starts_with("-") => {
                eprintln!("unknown argument {}", arg);
                return;
            },
            _ => {
                file_path = arg;
            }
        }
    }

    let mut file = File::open(file_path).unwrap();
    let mut rom = Vec::new();
    file.read_to_end(&mut rom).unwrap();
    rom.resize(0x10000, 0);
    let mut memory = memory::Memory::from_rom(&rom);
    memory.write(0xff44, 0x90);
    let trace = dissasembler::Trace::new(&memory);

    let cpu = cpu::Cpu::default();
    let mut inter = interpreter::Interpreter {
        clock_count: 0,
        cpu,
        memory,
        trace,
    };

    if diss {
        let mut string = String::new();
        inter.trace.fmt(&inter.memory, &mut string).unwrap();
        println!("{}", string);
    }else if debug {
        loop {
            inter.debug();
        }
    } else {
        loop {
            inter.interpret_op();
        }
    }
}
