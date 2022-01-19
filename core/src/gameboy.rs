use crate::cartridge::Cartridge;
use crate::save_state::{LoadStateError, SaveState};
use crate::sound_controller::SoundController;
use crate::{consts, cpu::Cpu, disassembler::Trace, ppu::Ppu};
use std::cell::RefCell;
use std::io::{Read, Write};

#[derive(Default, PartialEq, Eq)]
pub struct Timer {
    pub div: u16,
    pub tima: u8,
    tma: u8,
    tac: u8,
    last_counter_bit: bool,
}
impl SaveState for Timer {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        self.div.save_state(data)?;
        self.tima.save_state(data)?;
        self.tma.save_state(data)?;
        self.tac.save_state(data)?;
        [&self.last_counter_bit].save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.div.load_state(data)?;
        self.tima.load_state(data)?;
        self.tma.load_state(data)?;
        self.tac.load_state(data)?;
        [&mut self.last_counter_bit].load_state(data)?;
        Ok(())
    }
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
    pub cartridge: Cartridge,
    pub memory: [u8; 0x10000],
    pub boot_rom: [u8; 0x100],
    pub boot_rom_active: bool,
    pub clock_count: u64,
    pub timer: Timer,
    pub sound: RefCell<SoundController>,
    pub ppu: Ppu,
    /// JoyPad state. 0 bit means pressed.
    /// From bit 7 to 0, the order is: Start, Select, B, A, Down, Up, Left, Right
    pub joypad: u8,
    pub serial_transfer: Box<dyn FnMut(u8) + Send>,
    pub v_blank: Option<Box<dyn FnMut(&mut GameBoy) + Send>>,
}

impl std::fmt::Debug for GameBoy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: derive Debug for fields when the time arrive.
        f.debug_struct("GameBoy")
            // .field("trace", &self.trace)
            // .field("cpu", &self.cpu)
            // .field("cartridge", &self.cartridge)
            .field("memory", &self.memory)
            .field("boot_rom", &self.boot_rom)
            .field("boot_rom_active", &self.boot_rom_active)
            .field("clock_count", &self.clock_count)
            // .field("timer", &self.timer)
            // .field("sound", &self.sound)
            // .field("ppu", &self.ppu)
            .field("joypad", &self.joypad)
            // .field("serial_transfer", &self.serial_transfer)
            // .field("v_blank", &self.v_blank)
            .finish()
    }
}

impl Eq for GameBoy {}
impl PartialEq for GameBoy {
    fn eq(&self, other: &Self) -> bool {
        // self.trace == other.trace &&
        self.cpu == other.cpu
            && self.cartridge == other.cartridge
            && self.memory == other.memory
            && self.boot_rom == other.boot_rom
            && self.boot_rom_active == other.boot_rom_active
            && self.clock_count == other.clock_count
            && self.timer == other.timer
            && self.sound == other.sound
            && self.ppu == other.ppu
            && self.joypad == other.joypad
        // && self.serial_transfer == other.serial_transfer
        // && self.v_blank == other.v_blank
    }
}
impl SaveState for GameBoy {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        // self.trace.save_state(output)?;
        self.cpu.save_state(data)?;
        self.cartridge.save_state(data)?;
        self.memory.save_state(data)?;
        // self.boot_rom.save_state(output)?;
        [&self.boot_rom_active].save_state(data)?;
        self.clock_count.save_state(data)?;
        self.timer.save_state(data)?;
        {
            let mut sound = self.sound.borrow_mut();
            sound.update(self.clock_count);
            sound.save_state(data)?;
        }
        self.ppu.save_state(data)?;
        self.joypad.save_state(data)?;
        // self.serial_transfer.save_state(data)?;
        // self.v_blank.save_state(data)
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        // self.trace.load_state(output)?;
        self.cpu.load_state(data)?;
        self.cartridge.load_state(data)?;
        self.memory.load_state(data)?;
        // self.boot_rom.load_state(output)?;
        [&mut self.boot_rom_active].load_state(data)?;
        self.clock_count.load_state(data)?;
        self.timer.load_state(data)?;
        {
            let mut sound = self.sound.borrow_mut();
            sound.load_state(data)?;
            if sound.last_clock != self.clock_count {
                return Err(LoadStateError::SoundControllerDesync(
                    sound.last_clock,
                    self.clock_count,
                ));
            }
        }
        self.ppu.load_state(data)?;
        self.joypad.load_state(data)?;
        // self.serial_transfer.load_state(data)?;
        // self.v_blank.load_state(data)
        Ok(())
    }
}
impl GameBoy {
    pub fn new(boot_rom: [u8; 0x100], cartridge: Cartridge) -> Self {
        Self {
            trace: RefCell::new(Trace::new()),
            cpu: Cpu::default(),
            cartridge,
            memory: [0; 0x10000],
            boot_rom,
            boot_rom_active: true,
            clock_count: 0,
            timer: Timer::default(),
            sound: RefCell::new(SoundController::default()),
            ppu: Ppu::default(),
            joypad: 0xFF,
            serial_transfer: Box::new(|c| {
                eprint!("{}", c as char);
            }),
            v_blank: None,
        }
    }

    /// Reset the gameboy to its stating state.
    pub fn reset(&mut self) {
        // TODO: Maybe I should reset the cartridge
        self.cpu = Cpu::default();
        self.memory = [0; 0x10000];
        self.boot_rom_active = true;
        self.clock_count = 0;
        self.timer = Timer::default();
        self.sound = RefCell::new(SoundController::default());
        self.ppu = Ppu::default();
        self.joypad = 0xFF;
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn read(&self, mut address: u16) -> u8 {
        if address < 0x100 && self.boot_rom_active {
            return self.boot_rom[address as usize];
        }
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }
        match address {
            // Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.read(address),
            // Cartridge RAM
            0xA000..=0xBFFF => self.cartridge.read(address),
            0x8000..=0xFEFF | 0xFF80..=0xFFFE => self.memory[address as usize],
            // IO Registers
            0xFF00..=0xFF7F | 0xFFFF => self.read_io(address as u8),
        }
    }

    pub fn write(&mut self, mut address: u16, value: u8) {
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }
        if address == 0xFF02 && value == 0x81 {
            (self.serial_transfer)(self.memory[0xFF01]);
        }

        match address {
            // Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.write(address, value),
            // Cartridge RAM
            0xA000..=0xBFFF => self.cartridge.write(address, value),
            // IO Registers
            0xFF00..=0xFF7F | 0xFFFF => self.write_io(address as u8, value),
            _ => self.memory[address as usize] = value,
        }
    }

    /// Advante the clock by 'count' cycles
    pub fn tick(&mut self, count: u8) {
        for _ in 0..count {
            self.clock_count += 1 as u64;
            if self.timer.tick_one() {
                self.memory[consts::IF as usize] |= 1 << 2;
            }
            Ppu::update(self);
        }
    }

    pub fn read16(&self, address: u16) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address.wrapping_add(1))])
    }

    pub fn write16(&mut self, address: u16, value: u16) {
        let [a, b] = value.to_le_bytes();
        self.write(address, a);
        self.write(address.wrapping_add(1), b);
    }

    fn write_io(&mut self, address: u8, value: u8) {
        match address {
            0x00 => self.memory[0xFF00] = 0b1100_1111 | (value & 0x30), // JOYPAD
            0x04 => self.timer.write_div(value),
            0x05 => self.timer.write_tima(value),
            0x06 => self.timer.write_tma(value),
            0x07 => self.timer.write_tac(value),
            0x10..=0x14 | 0x16..=0x1E | 0x20..=0x26 | 0x30..=0x3F => {
                self.sound
                    .borrow_mut()
                    .write(self.clock_count, address, value)
            }
            0x40 => self.ppu.set_lcdc(value),
            0x41 => self.ppu.set_stat(value),
            0x42 => self.ppu.set_scy(value),
            0x43 => self.ppu.set_scx(value),
            0x44 => self.ppu.set_ly(value),
            0x45 => self.ppu.set_lyc(value),
            0x46 => {
                // DMA Transfer
                // TODO: this is not the proper behavior, of course
                let start = (value as usize) << 8;
                for (i, j) in (0xFE00..=0xFE9F).zip(start..start + 0x9F) {
                    self.memory[i] = self.memory[j];
                }
            }
            0x47 => self.ppu.set_bgp(value),
            0x4a => self.ppu.set_wy(value),
            0x4b => self.ppu.set_wx(value),
            0x4d => {}
            0x50 if value & 0b1 != 0 => {
                self.boot_rom_active = false;
                self.cpu.pc = 0x100;
            }
            _ => self.memory[0xFF00 | address as usize] = value,
        }
    }

    fn read_io(&self, address: u8) -> u8 {
        match address {
            0x00 => {
                // JOYPAD
                let v = self.memory[0xFF00];
                let mut r = (v & 0x30) | 0b1100_0000;
                if v & 0x10 != 0 {
                    r |= (self.joypad & 0xF0) >> 4;
                }
                if v & 0x20 != 0 {
                    r |= self.joypad & 0x0F;
                }
                r
            }
            0x04 => self.timer.read_div(),
            0x05 => self.timer.read_tima(),
            0x06 => self.timer.read_tma(),
            0x07 => self.timer.read_tac(),
            0x10..=0x14 | 0x16..=0x1E | 0x20..=0x26 | 0x30..=0x3F => {
                self.sound.borrow_mut().read(self.clock_count, address)
            }
            0x40 => self.ppu.lcdc(),
            0x41 => self.ppu.stat(),
            0x42 => self.ppu.scy(),
            0x43 => self.ppu.scx(),
            0x44 => self.ppu.ly(),
            0x45 => self.ppu.lyc(),
            // 0x44 => ((self.clock_count / 456) % 153) as u8,
            0x47 => self.ppu.bgp(),
            0x4a => self.ppu.wy(),
            0x4b => self.ppu.wx(),
            0x4d => 0xff,
            0x80..=0xff | 0x0f => {
                // high RAM, IF flag and IE flag
                self.memory[0xFF00 | address as usize]
            }
            _ => 0xff,
        }
    }
}
impl std::ops::Index<usize> for GameBoy {
    type Output = u8;

    fn index(&self, mut index: usize) -> &Self::Output {
        index = index & 0xffff;
        if self.boot_rom_active && index <= 0xff {
            &self.boot_rom[index]
        } else if index < 0x7FFF {
            &self.cartridge.rom()[index]
        } else {
            &0x00
        }
    }
}
