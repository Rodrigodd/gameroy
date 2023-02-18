use std::cell::{Cell, RefCell};

use crate::{
    disassembler::Trace,
    save_state::{LoadStateError, SaveState, SaveStateContext, SaveStateHeader},
};

pub mod cartridge;
pub mod cpu;
pub mod ppu;
pub mod serial_transfer;
pub mod sound_controller;
pub mod timer;

use self::{
    cartridge::Cartridge, cpu::Cpu, ppu::Ppu, serial_transfer::Serial,
    sound_controller::SoundController, timer::Timer,
};

#[cfg(not(target_arch = "wasm32"))]
type VBlankCallback = Box<dyn FnMut(&mut GameBoy) + Send>;
#[cfg(target_arch = "wasm32")]
type VBlankCallback = Box<dyn FnMut(&mut GameBoy)>;

pub struct GameBoy {
    pub trace: RefCell<Trace>,
    pub cpu: Cpu,
    pub cartridge: Cartridge,
    /// C000-DFFF: Work RAM
    pub wram: [u8; 0x2000],
    /// FF80-FFFE: High RAM
    pub hram: [u8; 0x7F],
    pub boot_rom: Option<[u8; 0x100]>,
    pub boot_rom_active: bool,
    pub clock_count: u64,
    pub timer: RefCell<Timer>,
    pub sound: RefCell<SoundController>,
    pub ppu: RefCell<Ppu>,
    /// FF00: P1
    pub joypad_io: u8,
    /// JoyPad state. 0 bit means pressed.
    /// From bit 7 to 0, the order is: Start, Select, B, A, Down, Up, Left, Right
    pub joypad: u8,
    pub serial: RefCell<Serial>,
    /// FF0F: Interrupt Flag (IF)
    /// - bit 0: VBlank
    /// - bit 1: STAT
    /// - bit 2: Timer
    /// - bit 3: Serial
    /// - bit 4: Joypad
    pub interrupt_flag: Cell<u8>,
    /// FF46: DMA register
    pub dma: u8,
    /// FFFF: Interrupt Enabled (IE). Same scheme as `interrupt_flag`.
    pub interrupt_enabled: u8,

    /// This trigger control if in the next interpret the `v_blank` callback will be called.
    pub v_blank_trigger: Cell<bool>,
    /// A callback that is called after a VBlank. This is called when a vblank interrupt is
    /// triggered.
    pub v_blank: Option<VBlankCallback>,

    /// Used to toggle the next interrupt prediction, to be able to test its correctness.
    pub predict_interrupt: bool,
}

impl std::fmt::Debug for GameBoy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: derive Debug for fields when the time arrive.
        f.debug_struct("GameBoy")
            // .field("trace", &self.trace)
            .field("cpu", &self.cpu)
            // .field("cartridge", &self.cartridge)
            .field("wram", &self.wram)
            .field("hram", &self.hram)
            .field("boot_rom", &self.boot_rom)
            .field("boot_rom_active", &self.boot_rom_active)
            .field("clock_count", &self.clock_count)
            .field("timer", &self.timer)
            // .field("sound", &self.sound)
            // .field("ppu", &self.ppu)
            .field("joypad", &self.joypad)
            .field("joypad_io", &self.joypad_io)
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
            && self.wram == other.wram
            && self.hram == other.hram
            && self.boot_rom == other.boot_rom
            && self.boot_rom_active == other.boot_rom_active
            && self.clock_count == other.clock_count
            && self.timer == other.timer
            && self.sound == other.sound
            && self.ppu == other.ppu
            && self.joypad_io == other.joypad_io
            && self.joypad == other.joypad
            && self.serial == other.serial
            && self.interrupt_flag == other.interrupt_flag
            && self.interrupt_enabled == other.interrupt_enabled
        // && self.v_blank == other.v_blank
    }
}
crate::save_state!(GameBoy, self, ctx, data {
    SaveStateHeader;
    // self.trace;
    self.cpu;
    self.cartridge;
    self.wram;
    self.hram;
    // self.boot_rom;
    self.clock_count;
    on_load ctx.clock_count = Some(self.clock_count);
    self.timer.borrow_mut();

    self.sound.borrow_mut();
    self.ppu.borrow_mut();

    self.joypad_io;
    self.joypad;
    self.serial.borrow_mut();
    self.interrupt_flag;
    self.dma;
    self.interrupt_enabled;

    bitset [self.boot_rom_active, self.v_blank_trigger];
    // self.v_blank;
});
impl GameBoy {
    pub fn new(boot_rom: Option<[u8; 0x100]>, cartridge: Cartridge) -> Self {
        let mut this = Self {
            trace: RefCell::new(Trace::new()),
            cpu: Cpu::default(),
            cartridge,
            wram: [0; 0x2000],
            hram: [0; 0x7F],
            boot_rom,
            boot_rom_active: true,
            clock_count: 0,
            timer: Timer::new().into(),
            sound: RefCell::new(SoundController::default()),
            ppu: Ppu::default().into(),

            joypad: 0xFF,
            joypad_io: 0x00,
            serial: Serial::new().into(),
            interrupt_flag: 0.into(),
            dma: 0xff,
            interrupt_enabled: 0,
            v_blank_trigger: false.into(),
            v_blank: None,
            predict_interrupt: true,
        };

        if this.boot_rom.is_none() {
            this.reset_after_boot();
        }

        this
    }

    /// call the `v_blank` callback
    pub fn call_v_blank_callback(&mut self) {
        if let Some(mut v_blank) = self.v_blank.take() {
            v_blank(self);
            self.v_blank = Some(v_blank);
        }
    }

    /// Saves the current state of the GameBoy.
    ///
    /// `timestamp` is the instant that this file is being saved, in number of milliseconds since
    /// the UNIX_EPOCH. it may be None if the system could not provide one.
    pub fn save_state<W: std::io::Write>(
        &self,
        timestamp: Option<u64>,
        data: &mut W,
    ) -> Result<(), std::io::Error> {
        self.update_all();
        let ctx = &mut SaveStateContext::new(timestamp, self.clock_count);
        SaveState::save_state(self, ctx, data)
    }

    pub fn load_state<R: std::io::Read>(&mut self, data: &mut R) -> Result<(), LoadStateError> {
        let ctx = &mut SaveStateContext::default();
        self.update_all();
        SaveState::load_state(self, ctx, data)
    }

    /// Reset the gameboy to its stating state.
    pub fn reset(&mut self) {
        if self.boot_rom.is_none() {
            self.reset_after_boot();
            return;
        }
        // TODO: Maybe I should reset the cartridge
        self.cpu = Cpu::default();
        self.wram = [0; 0x2000];
        self.hram = [0; 0x7F];
        self.boot_rom_active = true;
        self.clock_count = 0;
        self.timer = Timer::new().into();
        self.sound = RefCell::new(SoundController::default());
        self.ppu = Ppu::default().into();
        self.joypad = 0xFF;
        self.joypad_io = 0x00;
    }

    /// Reset the gameboy to its state after disabling the boot.
    pub fn reset_after_boot(&mut self) {
        let ctx = &mut SaveStateContext::default();

        self.cpu = Cpu {
            a: 0x01,
            f: cpu::Flags(0xb0),
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xd8,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x0100,
            ime: cpu::ImeState::Disabled,
            state: cpu::CpuState::Running,
        };

        self.wram = [0; 0x2000];
        self.hram = [0; 0x7F];
        self.hram[0x7a..=0x7c].copy_from_slice(&[0x39, 0x01, 0x2e]);

        self.boot_rom_active = false;
        self.clock_count = 23_440_324;
        self.ppu.get_mut().reset_after_boot();

        self.joypad = 0xFF;

        self.joypad_io = 0xCF;
        self.serial.get_mut().reset();
        self.timer = Timer::after_boot(self.clock_count).into();
        self.interrupt_flag = 0xE1.into();
        self.sound
            .get_mut()
            .load_state(ctx, &mut &include_bytes!("../after_boot/sound.sav")[..])
            .unwrap();
    }

    pub fn read(&self, mut address: u16) -> u8 {
        if self.boot_rom_active && address < 0x100 {
            let boot_rom = self
                .boot_rom
                .expect("the boot rom is only actived when there is one");
            return boot_rom[address as usize];
        }
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }
        match address {
            // Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.read(address),
            // Video RAM
            0x8000..=0x9FFF => Ppu::read_vram(self, address),
            // Cartridge RAM
            0xA000..=0xBFFF => self.cartridge.read(address),
            // Work RAM
            0xC000..=0xDFFF => self.wram[address as usize - 0xC000],
            // ECHO RAM
            0xE000..=0xFDFF => unreachable!(),
            // Sprite Attribute table
            0xFE00..=0xFE9F => Ppu::read_oam(self, address),
            // Not Usable
            0xFEA0..=0xFEFF => 0xff,
            // I/O registers
            0xFF00..=0xFF7F => self.read_io(address as u8),
            // Hight RAM
            0xFF80..=0xFFFE => self.hram[address as usize - 0xFF80],
            // IE Register
            0xFFFF => self.read_io(address as u8),
        }
    }

    pub fn write(&mut self, mut address: u16, value: u8) {
        if (0xE000..=0xFDFF).contains(&address) {
            address -= 0x2000;
        }

        match address {
            // Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.write(address, value),
            // Video RAM
            0x8000..=0x9FFF => Ppu::write_vram(self, address, value),
            // Cartridge RAM
            0xA000..=0xBFFF => self.cartridge.write(address, value),
            // Work RAM
            0xC000..=0xDFFF => self.wram[address as usize - 0xC000] = value,
            // ECHO RAM
            0xE000..=0xFDFF => unreachable!(),
            // Sprite Attribute table
            0xFE00..=0xFE9F => Ppu::write_oam(self, address, value),
            // Not Usable
            0xFEA0..=0xFEFF => {}
            // I/O registers
            0xFF00..=0xFF7F => self.write_io(address as u8, value),
            // Hight RAM
            0xFF80..=0xFFFE => self.hram[address as usize - 0xFF80] = value,
            // IE Register
            0xFFFF => self.write_io(address as u8, value),
        }
    }

    /// Advance the clock by 'count' cycles
    pub fn tick(&mut self, count: u8) {
        self.clock_count += count as u64;
    }

    pub fn update_interrupt(&self) {
        if !self.predict_interrupt {
            self.update_all();
            return;
        }

        if self.ppu.borrow().next_interrupt < self.clock_count + 4 {
            self.update_ppu();
        }
        if self.timer.borrow().next_interrupt < self.clock_count + 4 {
            self.update_timer();
        }
        if self.serial.borrow().next_interrupt < self.clock_count + 4 {
            self.update_serial();
        }
    }

    pub fn update_all(&self) {
        self.update_ppu();
        self.update_timer();
        self.update_serial();
    }

    fn update_ppu(&self) {
        let (v_blank_interrupt, stat_interrupt) = Ppu::update(self);
        if stat_interrupt {
            self.interrupt_flag
                .set(self.interrupt_flag.get() | (1 << 1));
        }
        if v_blank_interrupt {
            self.interrupt_flag
                .set(self.interrupt_flag.get() | (1 << 0));
            self.v_blank_trigger.set(true);
        }
    }

    fn update_timer(&self) {
        if self.timer.borrow_mut().update(self.clock_count) {
            self.interrupt_flag
                .set(self.interrupt_flag.get() | (1 << 2));
        }
    }

    fn update_serial(&self) {
        if self.serial.borrow_mut().update(self.clock_count) {
            // interrupt
            self.interrupt_flag
                .set(self.interrupt_flag.get() | (1 << 3));
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
            0x00 => self.joypad_io = 0b1100_1111 | (value & 0x30), // JOYPAD
            0x01..=0x02 => Serial::write(self, address, value),
            0x03 => {}
            0x04..=0x07 => {
                self.update_timer();
                self.timer.get_mut().write(address, value);
            }
            0x08..=0x0e => {}
            0x0f => {
                self.update_interrupt();
                *self.interrupt_flag.get_mut() = value
            }
            0x10..=0x14 | 0x16..=0x1e | 0x20..=0x26 | 0x30..=0x3f => {
                self.sound.get_mut().write(self.clock_count, address, value)
            }
            0x15 => {}
            0x1f => {}
            0x27..=0x2f => {}
            0x40..=0x45 => Ppu::write(self, address, value),
            0x46 => {
                // DMA Transfer
                Ppu::start_dma(self, value);
            }
            0x47..=0x4b => Ppu::write(self, address, value),
            0x4c..=0x4f => {}
            0x50 => {
                if self.boot_rom_active && value & 0b1 != 0 {
                    self.boot_rom_active = false;
                    self.cpu.pc = 0x100;
                }
            }
            0x51..=0x7f => {}
            0x80..=0xfe => self.hram[address as usize - 0x80] = value,
            0xff => {
                self.update_interrupt();
                self.interrupt_enabled = value
            }
        }
    }

    fn read_io(&self, address: u8) -> u8 {
        match address {
            0x00 => {
                // JOYPAD
                let v = self.joypad_io & 0x30;
                let mut r = v | 0b1100_0000;
                if v & 0x10 != 0 {
                    r |= (self.joypad >> 4) & 0x0F;
                }
                if v & 0x20 != 0 {
                    r |= self.joypad & 0x0F;
                }
                if v == 0 {
                    r |= 0x0F;
                }
                r
            }
            0x01..=0x02 => Serial::read(self, address),
            0x03 => 0xff,
            0x04..=0x07 => {
                self.update_timer();
                self.timer.borrow().read(address)
            }
            0x08..=0x0e => 0xff,
            0x0f => {
                self.update_interrupt();
                self.interrupt_flag.get() | 0xE0
            }
            0x10..=0x14 | 0x16..=0x1e | 0x20..=0x26 | 0x30..=0x3f => {
                self.sound.borrow_mut().read(self.clock_count, address)
            }
            0x15 => 0xff,
            0x1f => 0xff,
            0x27..=0x2f => 0xff,
            0x40..=0x45 => Ppu::read(self, address),
            0x46 => self.dma,
            0x47..=0x4b => Ppu::read(self, address),
            0x4c => 0xff,
            0x4d => 0xff,
            0x4e..=0x4f => 0xff,
            0x50 => 0xff,
            0x51..=0x7F => 0xff,
            0x80..=0xfe => {
                // high RAM, IF flag and IE flag
                self.hram[address as usize - 0x80]
            }
            0xff => self.interrupt_enabled,
        }
    }
}
