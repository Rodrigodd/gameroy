use std::fs::File;
use std::io::Read;

mod dissasembler;
mod opcode;

fn main() {
    let mut file = File::open("roms/Tetris (World) (Rev A).gb").unwrap();
    let mut rom = Vec::new();
    file.read_to_end(&mut rom).unwrap();
    let trace = dissasembler::Trace::from_rom(&rom);
    println!("{}", trace);
}
