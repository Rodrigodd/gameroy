use std::io::Read;

fn mbc_type_name(code: u8) -> &'static str {
    match code {
        0x00 => "ROM ONLY",
        0x01 => "MBC1",
        0x02 => "MBC1+RAM",
        0x03 => "MBC1+RAM+BATTERY",
        0x05 => "MBC2",
        0x06 => "MBC2+BATTERY",
        0x08 => "ROM+RAM",
        0x09 => "ROM+RAM+BATTERY",
        0x0B => "MMM01",
        0x0C => "MMM01+RAM",
        0x0D => "MMM01+RAM+BATTERY",
        0x0F => "MBC3+TIMER+BATTERY",
        0x10 => "MBC3+TIMER+RAM+BATTERY",
        0x11 => "MBC3",
        0x12 => "MBC3+RAM",
        0x13 => "MBC3+RAM+BATTERY",
        0x19 => "MBC5",
        0x1A => "MBC5+RAM",
        0x1B => "MBC5+RAM+BATTERY",
        0x1C => "MBC5+RUMBLE",
        0x1D => "MBC5+RUMBLE+RAM",
        0x1E => "MBC5+RUMBLE+RAM+BATTERY",
        0x20 => "MBC6",
        0x22 => "MBC7+SENSOR+RUMBLE+RAM+BATTERY",
        0xFC => "POCKET CAMERA",
        0xFD => "BANDAI TAMA5",
        0xFE => "HuC3",
        0xFF => "HuC1+RAM+BATTERY",
        _ => "Unknown",
    }
}

enum MBC {
    None(MBC0),
    MBC1(MBC1),
    MBC2(MBC2),
    MBC3(MBC3),
}

pub struct Cartridge {
    mbc: MBC,
}
impl Cartridge {
    pub fn new(mut rom: Vec<u8>) -> Result<Self, String> {

        if rom.len() < 0x8000 {
            rom.resize(0x8000, 0);
        }

        // Cartridge Type
        let mbc_kind = rom[0x0147];

        Ok(Self {
            mbc: match mbc_kind {
                0 => MBC::None(MBC0 {
                    rom,
                    ram: vec![0; 0x8000],
                }),
                1 | 2 | 3 => MBC::MBC1(MBC1 {
                    rom,
                    selected_bank: 0,
                    mode: false,
                    ram: vec![0; 0x8000],
                    ram_enabled: false,
                }),
                5 | 6 => MBC::MBC2(MBC2 {
                    rom,
                    selected_bank: 0,
                    ram: vec![0; 0x8000],
                    ram_enabled: false,
                }),
                0x0F | 0x10 | 0x11 | 0x12 | 0x13 => MBC::MBC3(MBC3 {
                    rom,
                    selected_bank: 0,
                    ram: vec![0; 0x8000],
                    ram_enabled: false,
                    ram_bank: 0,
                    rtc: [0; 5],
                    latch_clock_data: 0,
                }),
                _ => {
                    return Err(format!(
                        "MBC type '{}' ({:02x}) is not supported",
                        mbc_type_name(mbc_kind),
                        mbc_kind
                    ))
                }
            },
        })
    }

    /// The number of banks in this cartridge. A cartridge without bank switching have 2 banks.
    pub fn num_banks(&self) -> u8 {
        match &self.mbc {
            MBC::None(x) => (x.rom.len() / 0x4000) as u8,
            MBC::MBC1(x) => (x.rom.len() / 0x4000) as u8,
            MBC::MBC2(x) => (x.rom.len() / 0x4000) as u8,
            MBC::MBC3(x) => (x.rom.len() / 0x4000) as u8,
        }
    }

    /// The current selected rom bank
    pub fn curr_bank(&self) -> u8 {
        match &self.mbc {
            MBC::None(_) => 1,
            MBC::MBC1(x) => x.curr_bank(),
            MBC::MBC2(x) => x.curr_bank(),
            MBC::MBC3(x) => x.curr_bank(),
        }
    }

    pub fn rom(&self) -> &[u8] {
        match &self.mbc {
            MBC::None(x) => &x.rom,
            MBC::MBC1(x) => &x.rom,
            MBC::MBC2(x) => &x.rom,
            MBC::MBC3(x) => &x.rom,
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        match &self.mbc {
            MBC::None(x) => x.read(address),
            MBC::MBC1(x) => x.read(address),
            MBC::MBC2(x) => x.read(address),
            MBC::MBC3(x) => x.read(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match &mut self.mbc {
            MBC::None(x) => x.write(address, value),
            MBC::MBC1(x) => x.write(address, value),
            MBC::MBC2(x) => x.write(address, value),
            MBC::MBC3(x) => x.write(address, value),
        }
    }
}

/// Cartridge without a MBC chip
struct MBC0 {
    pub rom: Vec<u8>,
    ram: Vec<u8>,
}
impl MBC0 {
    pub fn read(&self, address: u16) -> u8 {
        match address {
            // ROM
            0x0000..=0x7FFF => self.rom[address as usize],
            // RAM
            0xA000..=0xBFFF => self.ram[address as usize - 0xA000],
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            // ROM
            0x0000..=0x7FFF => {}
            // RAM
            0xA000..=0xBFFF => {
                self.ram[address as usize - 0xA000] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC1 chip
struct MBC1 {
    pub rom: Vec<u8>,
    // the banking register, including second 2-bit
    selected_bank: u8,
    // false is mode 0, true is mode 1
    mode: bool,
    ram: Vec<u8>,
    ram_enabled: bool,
}
impl MBC1 {
    fn curr_bank(&self) -> u8 {
        let mut bank = if self.mode {
            // Mode 1
            self.selected_bank & 0x1F
        } else {
            // Mode 0
            self.selected_bank & 0x7F
        };

        // cannot adress a bank where the 5-bit bank register is 0
        if bank & 0x1F == 0 {
            bank += 1;
        }

        // mask upper bits if the bank is out of bounds
        bank %= (self.rom.len() / 0x4000) as u8;
        bank
    }

    pub fn read(&self, address: u16) -> u8 {
        match address {
            // ROM Bank X0
            0x0000..=0x3FFF => self.rom[address as usize],
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank();

                let address_start = 0x4000 * bank as usize;
                self.rom[address as usize - 0x4000 + address_start]
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled {
                    return 0xff;
                }
                let start_address = if self.mode {
                    // Large ROM have >= 1MiB
                    let large_rom = self.rom.len() >= 0x10_0000;
                    if large_rom {
                        0
                    } else {
                        0x2000 * ((self.selected_bank >> 5) & 0x3) as usize
                    }
                } else {
                    0
                };
                self.ram[address as usize - 0xA000 + start_address]
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            // RAM Enable
            0x0000..=0x1FFF => {
                // Enable ram if 4-bit value 0xA is write here.
                // Disable otherwise.
                self.ram_enabled = value & 0x0F == 0x0A;
            }
            // ROM Bank Number
            0x2000..=0x3FFF => {
                // only lower 5 bits are written
                self.selected_bank = (self.selected_bank & 0xE0) | value & 0x1F;
            }
            // RAM Bank Number - or - Upper Bits of ROM Bank Number
            0x4000..=0x5FFF => {
                // only higher 2 bits are written
                self.selected_bank = (self.selected_bank & 0x1F) | value & 0x3 << 5;
            }
            // Banking Mode Select
            0x6000..=0x7FFF => {
                self.mode = value & 0x01 != 0;
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled {
                    return;
                }
                let start_address = if self.mode {
                    // Large ROM have >= 1MiB
                    let large_rom = self.rom.len() >= 0x10_0000;
                    if large_rom {
                        0
                    } else {
                        0x2000 * ((self.selected_bank >> 5) & 0x3) as usize
                    }
                } else {
                    0
                };
                self.ram[address as usize - 0xA000 + start_address] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC2 chip
struct MBC2 {
    pub rom: Vec<u8>,
    // the banking register
    selected_bank: u8,
    ram: Vec<u8>,
    ram_enabled: bool,
}
impl MBC2 {
    fn curr_bank(&self) -> u8 {
        self.selected_bank
    }

    pub fn read(&self, address: u16) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => self.rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank();

                // PERF: I could already store the start_address, instead of computing it every
                // time. The same for write, and others MBC's.
                let address_start = 0x4000 * bank as usize;
                self.rom[address as usize - 0x4000 + address_start]
            }
            // 512x4bits RAM
            0xA000..=0xBFFF => {
                let address = address & 0x1FF; // only the bottom 9 bits are used
                if !self.ram_enabled {
                    return 0xff;
                }
                // upper 4bits are undefined
                self.ram[address as usize] | 0xF0
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            // RAM Enable and ROM Bank Number
            0x0000..=0x3FFF => {
                if (address >> 8) & 0x1 == 0 {
                    // RAM Enable
                    self.ram_enabled = value == 0x0A;
                } else {
                    // ROM Bank Number
                    // The lower 4 bits control the selected bank.
                    self.selected_bank = value & 0x0F;
                    if self.selected_bank == 0 {
                        self.selected_bank = 1;
                    }
                }
            }
            0x4000..=0x7FFF => {}
            // 512x4bits RAM
            0xA000..=0xBFFF => {
                let address = address & 0x1FF; // only the bottom 9 bits are used
                if !self.ram_enabled {
                    return;
                }
                // upper 4bits are undefined
                self.ram[address as usize] = value | 0xF0;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC3 chip
struct MBC3 {
    pub rom: Vec<u8>,
    // the banking register, including second 2-bit
    selected_bank: u8,
    // false is mode 0, true is mode 1
    ram: Vec<u8>,
    ram_enabled: bool,
    ram_bank: u8,
    rtc: [u8; 5],
    // the state in the latch clock data operation.
    // 0 is the intial state
    // 1 means that 0 was written
    latch_clock_data: u8,
}
impl MBC3 {
    fn curr_bank(&self) -> u8 {
        self.selected_bank
    }

    pub fn read(&self, address: u16) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => self.rom[address as usize],
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank();

                let address_start = 0x4000 * bank as usize;
                self.rom[address as usize - 0x4000 + address_start]
            }
            // RAM Bank 00-03, or RTC registers 08-0C
            0xA000..=0xBFFF => {
                match self.ram_bank {
                    // RAM bank
                    0x0..=0x03 => {
                        if !self.ram_enabled {
                            return 0xff;
                        }
                        let start_address = 0x2000 * ((self.selected_bank >> 5) & 0x3) as usize;
                        self.ram[address as usize - 0xA000 + start_address]
                    }
                    // RTC registers
                    0x8..=0xC => {
                        // TODO: implement the rtc correctly
                        self.rtc[self.ram_bank as usize - 0x8]
                    }
                    _ => {
                        // I don't know what happen here
                        0xff
                    }
                }
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            // RAM Enable
            0x0000..=0x1FFF => {
                // Enable ram if 4-bit value 0xA is write here.
                // Disable otherwise.
                self.ram_enabled = value & 0x0F == 0x0A;
            }
            // ROM Bank Number
            0x2000..=0x3FFF => {
                // all 7 bits are written
                self.selected_bank = value & 0x7F;
            }
            // RAM Bank Number - or - Upper Bits of ROM Bank Number
            0x4000..=0x5FFF => {
                self.ram_bank = value;
            }
            // Latch Clock Data
            0x6000..=0x7FFF => {
                // state transition for
                // 0 =(write 0)=> 1 =(write 1)=> 2
                // x =(write x)=> 0
                if value == 0 {
                    self.latch_clock_data = 1;
                } else if value == 1 && self.latch_clock_data == 1 {
                    self.latch_clock_data = 0;
                    // TODO: complete the rtc implementation

                    let now = std::time::UNIX_EPOCH
                        .elapsed()
                        .unwrap_or(std::time::Duration::ZERO);
                    let seconds = now.as_secs();
                    let secs = (seconds % 60) as u8;
                    let mins = ((seconds / 60) % 60) as u8;
                    let hous = (((seconds / 60) / 60) % 24) as u8;
                    let days = (((seconds / 60) / 60) / 24) as u16;
                    let dayl = (days & 0xFF) as u8;
                    let dayu = ((days >> 8) & 0x1) as u8;
                    // latch the current time into rtc registers
                    self.rtc = [
                        secs, // RTC S   Seconds   0-59 (0-3Bh)
                        mins, // RTC M   Minutes   0-59 (0-3Bh)
                        hous, // RTC H   Hours     0-23 (0-17h)
                        dayl, // RTC DL  Lower 8 bits of Day Counter (0-FFh)
                        dayu, // RTC DH  Upper 1 bit of Day Counter, Carry Bit, Halt Flag
                              //        Bit 0  Most significant bit of Day Counter (Bit 8)
                              //        Bit 6  Halt (0=Active, 1=Stop Timer)
                              //        Bit 7  Day Counter Carry Bit (1=Counter Overflow)
                    ];
                } else {
                    self.latch_clock_data = 0;
                }
            }
            // RAM Bank 00-03, or RTC registers 08-0C
            0xA000..=0xBFFF => {
                match self.ram_bank {
                    // RAM bank
                    0x0..=0x03 => {
                        if !self.ram_enabled {
                            return;
                        }
                        let start_address = 0x2000 * ((self.selected_bank >> 5) & 0x3) as usize;
                        self.ram[address as usize - 0xA000 + start_address] = value;
                    }
                    // RTC registers
                    0x8..=0xC => {
                        // TODO: implement the rtc correctly
                        self.rtc[self.ram_bank as usize - 0x8] = value;
                    }
                    _ => {
                        // I don't know what happen here
                    }
                }
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}
