use crate::{consts, cpu::Cpu, dissasembler::Trace};
use std::cell::RefCell;
use std::io::Read;

pub struct Timer {
    pub div: u16,
    pub tima: u8,
    tma: u8,
    tac: u8,
    last_counter_bit: bool,
}
// TODO: At some point, I want this timer to be lazy evaluated.
impl Timer {
    /// Advance the timer by one cycle
    /// Return true if there is a interrupt
    pub fn tick_one(&mut self) -> bool {
        self.div = self.div.wrapping_add(1);

        let f = [9, 3, 5, 7][(self.tac & 0b11) as usize];
        let counter_bit = ((self.div >> f) as u8 & (self.tac >> 2)) & 0b1 != 0;

        // faling edge
        if self.last_counter_bit && !counter_bit {
            let (v, o) = self.tima.overflowing_add(1);
            self.tima = v;
            // TODO: TIMA, on overflow, should keep the value 0 for 4 cycles
            // before the overflow be detected. A write in this interval would cancel it.
            if o {
                self.tima = self.tma;
                return true; // INTERRUPT
            }
        }

        self.last_counter_bit = counter_bit;
        false
    }

    fn read_div(&self) -> u8 {
        (self.div >> 8) as u8
    }
    fn write_div(&mut self, _div: u8) {
        self.div = 0;
    }

    fn read_tima(&self) -> u8 {
        self.tima
    }
    fn write_tima(&mut self, tima: u8) {
        self.tima = tima;
    }

    fn read_tma(&self) -> u8 {
        self.tma
    }
    fn write_tma(&mut self, tma: u8) {
        self.tma = tma;
    }

    fn read_tac(&self) -> u8 {
        self.tac
    }
    fn write_tac(&mut self, tac: u8) {
        self.tac = tac;
    }
}

pub struct GameBoy {
    pub trace: RefCell<Trace>,
    pub cpu: Cpu,
    pub memory: [u8; 0x10000],
    pub boot_rom: [u8; 0x100],
    pub boot_rom_active: bool,
    pub clock_count: u64,
    pub timer: Timer,
}
impl GameBoy {
    pub fn new(mut bootrom_file: impl Read, mut rom_file: impl Read) -> Self {
        let mut memory = [0; 0x10000];
        let mut boot_rom = [0; 0x100];

        bootrom_file.read(&mut boot_rom).unwrap();
        rom_file.read(&mut memory).unwrap();

        Self {
            trace: RefCell::new(Trace::new()),
            cpu: Cpu::default(),
            memory,
            boot_rom,
            boot_rom_active: true,
            clock_count: 0,
            timer: Timer {
                div: 0,
                tima: 0,
                tma: 0,
                tac: 0,
                last_counter_bit: false,
            },
        }
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn read(&mut self, mut address: u16) -> u8 {
        if address < 0x100 && self.boot_rom_active {
            return self.boot_rom[address as usize];
        }
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }
        match address {
            0x0000..=0xFEFF | 0xFF80..=0xFFFE => self.memory[address as usize],
            0xFF00..=0xFF7F | 0xFFFF => self.read_io(address as u8),
        }
    }

    pub fn write(&mut self, mut address: u16, value: u8) {
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }
        if address == 0xFF02 && value == 0x81 {
            eprint!("{}", self.memory[0xFF01] as char);
        }

        match address {
            0x0000..=0x7FFF => {}
            0xFF00..=0xFF7F | 0xFFFF => self.write_io(address as u8, value),
            _ => self.memory[address as usize] = value,
        }
    }

    /// Advante the clock by 'count' cycles
    pub fn tick(&mut self, count: u8) {
        self.clock_count += count as u64;
        for _ in 0..count {
            if self.timer.tick_one() {
                self.memory[consts::IF as usize] |= 1 << 2;
            }
        }
    }

    pub fn read16(&mut self, address: u16) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address.wrapping_add(1))])
    }

    pub fn write16(&mut self, address: u16, value: u16) {
        let [a, b] = value.to_le_bytes();
        self.write(address, a);
        self.write(address.wrapping_add(1), b);
    }

    fn write_io(&mut self, address: u8, value: u8) {
        match address {
            0x04 => self.timer.write_div(value),
            0x05 => self.timer.write_tima(value),
            0x06 => self.timer.write_tma(value),
            0x07 => self.timer.write_tac(value),
            // 0x44 => 0x90,
            0x4d => {},
            0x50 if value & 0b1 != 0 => self.boot_rom_active = false,
            _ => self.memory[0xFF00 | address as usize] = value,
        }
    }

    fn read_io(&mut self, address: u8) -> u8 {
        match address {
            0x04 => self.timer.read_div(),
            0x05 => self.timer.read_tima(),
            0x06 => self.timer.read_tma(),
            0x07 => self.timer.read_tac(),
            0x44 => 0x90,
            0x4d => 0xff,
            _ => self.memory[0xFF00 | address as usize],
        }
    }
}
impl std::ops::Index<usize> for GameBoy {
    type Output = u8;

    fn index(&self, mut index: usize) -> &Self::Output {
        index = index & 0xffff;
        if self.boot_rom_active && index <= 0xff {
            &self.boot_rom[index]
        } else {
            &self.memory[index]
        }
    }
}
