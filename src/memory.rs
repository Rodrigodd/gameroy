pub struct Memory {
    pub memory: [u8; 0x10000],
}
impl Memory {
    pub fn from_rom(rom: &[u8]) -> Self {
        let mut memory = [0; 0x10000];
        memory[0..rom.len()].copy_from_slice(rom);
        Self { memory }
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    pub fn write(&mut self, address: u16, value: u8) {
        if address == 0xFF02 && value == 0x81 {
            eprint!("{}", self.read(0xFF01) as char);
        }
        if address == 0xFF44 {
            eprintln!("Write {:02x} to ff44", value);
        }
        self.memory[address as usize] = value;
    }

    pub fn read16(&mut self, address: u16) -> u16 {
        u16::from_le_bytes([
            self.read(address),
            self.read(address.wrapping_add(1))
        ])
    }


    pub fn write16(&mut self, address: u16, value: u16) {
        let [a, b] = value.to_le_bytes();
        self.write(address, a);
        self.write(address.wrapping_add(1), b);
    }
}
use std::{ops::Index, slice::SliceIndex};
impl<I: SliceIndex<[u8]>> Index<I> for Memory {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        Index::index(&self.memory, index)
    }
}
