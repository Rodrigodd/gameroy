use std::convert::TryInto;
use std::io::Read;

use crate::save_state::{LoadStateError, SaveState};

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

#[derive(PartialEq, Eq)]
pub struct CartridgeHeader {
    /// 0104-0133: Logo
    pub logo: [u8; 48],
    /// 0134-0143: Title
    pub title: [u8; 16],
    ///0143: CGB Flag
    pub cgb_flag: u8,
    /// 0146: SGB Flag
    pub sgb_flag: u8,
    /// 0147: Cartridge Type
    pub cartridge_type: u8,
    /// 0148: ROM Size
    pub rom_size: u8,
    /// 0149: RAM Size
    pub ram_size: u8,
    /// 014C: Mask ROM Version number
    pub version: u8,
    /// 014D: Header Checksum
    pub header_checksum: u8,
    /// 014E-014F: Global Checksum
    pub global_checksum: u16,
}
impl CartridgeHeader {
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, (Option<Self>, String)> {
        if bytes.len() < 0x150 {
            return Err((None, "file has less than 0x150 bytes".to_string()));
        }
        let this = Self {
            logo: bytes[0x0104..=0x0133].try_into().unwrap(),
            title: bytes[0x0134..=0x0143].try_into().unwrap(),
            cgb_flag: bytes[0x143],
            sgb_flag: bytes[0x0146],
            cartridge_type: bytes[0x0147],
            rom_size: bytes[0x0148],
            ram_size: bytes[0x0149],
            version: bytes[0x014C],
            header_checksum: bytes[0x014D],
            global_checksum: u16::from_le_bytes([bytes[0x014E], bytes[0x014F]]),
        };

        {
            let check_sum = bytes[0x134..=0x014C]
                .iter()
                .fold(0u8, |x, &b| x.wrapping_add(!b));
            if check_sum != this.header_checksum {
                return Err((Some(this), "checksum don't match".to_string()));
            }
        }

        Ok(this)
    }

    pub fn from_reader(reader: &mut impl Read) -> Result<Self, (Option<Self>, String)> {
        let mut bytes = [0; 0x150];
        let len = match reader.read(&mut bytes) {
            Ok(x) => x,
            Err(err) => return Err((None, format!("io error: {}", err))),
        };
        Self::from_bytes(&bytes[0..len])
    }
}

#[derive(PartialEq, Eq)]
enum MBC {
    None(MBC0),
    MBC1(MBC1),
    MBC2(MBC2),
    MBC3(MBC3),
    MBC5(MBC5),
}

#[derive(PartialEq, Eq)]
pub struct Cartridge {
    pub header: CartridgeHeader,
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,
    mbc: MBC,
}
impl SaveState for Cartridge {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        // self.rom.save_state(data)?;
        self.ram.save_state(data)?;
        match &self.mbc {
            MBC::None(x) => x.save_state(data),
            MBC::MBC1(x) => x.save_state(data),
            MBC::MBC2(x) => x.save_state(data),
            MBC::MBC3(x) => x.save_state(data),
            MBC::MBC5(x) => x.save_state(data),
        }
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        // self.rom.load_state(data)?;
        self.ram.load_state(data)?;
        match &mut self.mbc {
            MBC::None(x) => x.load_state(data),
            MBC::MBC1(x) => x.load_state(data),
            MBC::MBC2(x) => x.load_state(data),
            MBC::MBC3(x) => x.load_state(data),
            MBC::MBC5(x) => x.load_state(data),
        }
    }
}
impl Cartridge {
    pub fn new(rom: Vec<u8>) -> Result<Self, String> {
        let header = match CartridgeHeader::from_bytes(&rom) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((_, err)) => return Err(err),
        };
        // Cartridge Type
        let mbc_kind = header.cartridge_type;
        let mbc = match mbc_kind {
            0 => MBC::None(MBC0 {}),
            1 | 2 | 3 => MBC::MBC1(MBC1::new()),
            5 | 6 => MBC::MBC2(MBC2::new()),
            0x0F | 0x10 | 0x11 | 0x12 | 0x13 => MBC::MBC3(MBC3 {
                selected_bank: 0,
                ram_enabled: false,
                ram_bank: 0,
                rtc: [0; 5],
                latch_clock_data: 0,
            }),
            0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E => MBC::MBC5(MBC5::new()),
            _ => {
                return Err(format!(
                    "MBC type '{}' ({:02x}) is not supported",
                    mbc_type_name(mbc_kind),
                    mbc_kind
                ))
            }
        };

        let rom_sizes = [
            2 * 0x4000, // no ROM Banking
            4 * 0x4000,
            8 * 0x4000,
            16 * 0x4000,
            32 * 0x4000,
            64 * 0x4000,
            128 * 0x4000,
            256 * 0x4000,
            512 * 0x4000,
            // 72 * 0x2000,
            // 80 * 0x2000,
            // 96 * 0x2000,
        ];
        let rom_size_type = header.rom_size;
        let rom_size = rom_sizes
            .get(rom_size_type as usize)
            .copied()
            .ok_or_else(|| format!("Rom size '{:02x}' is no supported", rom_size_type))?;

        if rom_size != rom.len() {
            return Err(format!(
                "In the rom header the expected size is '{}' bytes, but the given rom has '{}' bytes",
                rom_size,
                rom.len()
            ));
        }

        let ram_sizes = [
            0,
            0x800,
            1 * 0x2000, // Single Bank
            4 * 0x2000,
            16 * 0x2000,
            8 * 0x2000,
        ];
        let ram_size_type = header.ram_size;
        let ram_size = if let MBC::MBC2(_) = mbc {
            if ram_size_type != 0 {
                return Err(format!("Cartridge use MBC2, with a integrated ram (type '00'), but report the ram type '{:02x}'", ram_size_type));
            }
            0x200
        } else {
            ram_sizes
                .get(ram_size_type as usize)
                .copied()
                .ok_or_else(|| format!("Ram size '{:02x}' is no supported", ram_size_type))?
        };

        Ok(Self {
            header,
            rom,
            ram: vec![0; ram_size],
            mbc,
        })
    }

    /// The number of banks in this cartridge. A cartridge without bank switching have 2 banks.
    pub fn num_banks(&self) -> u8 {
        (self.rom.len() / 0x4000) as u8
    }

    /// The current selected rom bank
    pub fn curr_bank(&self) -> u16 {
        match &self.mbc {
            MBC::None(_) => 1,
            MBC::MBC1(x) => x.curr_bank(&self.rom),
            MBC::MBC2(x) => x.curr_bank(&self.rom),
            MBC::MBC3(x) => x.curr_bank(&self.rom),
            MBC::MBC5(x) => x.curr_bank(&self.rom),
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        match &self.mbc {
            MBC::None(x) => x.read(address, &self.rom, &self.ram),
            MBC::MBC1(x) => x.read(address, &self.rom, &self.ram),
            MBC::MBC2(x) => x.read(address, &self.rom, &self.ram),
            MBC::MBC3(x) => x.read(address, &self.rom, &self.ram),
            MBC::MBC5(x) => x.read(address, &self.rom, &self.ram),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match &mut self.mbc {
            MBC::None(x) => x.write(address, value, &self.rom, &mut self.ram),
            MBC::MBC1(x) => x.write(address, value, &self.rom, &mut self.ram),
            MBC::MBC2(x) => x.write(address, value, &self.rom, &mut self.ram),
            MBC::MBC3(x) => x.write(address, value, &self.rom, &mut self.ram),
            MBC::MBC5(x) => x.write(address, value, &self.rom, &mut self.ram),
        }
    }
}

/// Cartridge without a MBC chip
#[derive(PartialEq, Eq)]
struct MBC0 {}
impl SaveState for MBC0 {
    fn save_state(&self, _data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        Ok(())
    }

    fn load_state(&mut self, _data: &mut impl Read) -> Result<(), LoadStateError> {
        Ok(())
    }
}
impl MBC0 {
    pub fn read(&self, address: u16, rom: &[u8], ram: &Vec<u8>) -> u8 {
        match address {
            // ROM
            0x0000..=0x7FFF => rom[address as usize],
            // RAM
            0xA000..=0xBFFF => ram[address as usize - 0xA000],
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut Vec<u8>) {
        match address {
            // ROM
            0x0000..=0x7FFF => {}
            // RAM
            0xA000..=0xBFFF => {
                ram[address as usize - 0xA000] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC1 chip
#[derive(PartialEq, Eq)]
struct MBC1 {
    // the banking register. Includes the 5-bit register 1, and the 2-bit register 2.
    selected_bank: u8,
    // false is mode 0, true is mode 1
    mode: bool,
    ram_enabled: bool,
}
impl SaveState for MBC1 {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.selected_bank.save_state(data)?;
        [&self.mode, &self.ram_enabled].save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.selected_bank.load_state(data)?;
        [&mut self.mode, &mut self.ram_enabled].load_state(data)?;
        Ok(())
    }
}
impl MBC1 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            mode: false,
            ram_enabled: false,
        }
    }

    fn curr_bank(&self, rom: &[u8]) -> u16 {
        let mut bank = self.selected_bank;

        // cannot adress a bank where the 5-bit bank register is 0
        debug_assert!(bank & 0x1F != 0);

        // mask upper bits if the bank is out of bounds
        bank %= (rom.len() / 0x4000) as u8;
        bank as u16
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &Vec<u8>) -> u8 {
        match address {
            // ROM Bank X0
            0x0000..=0x3FFF => {
                if self.mode {
                    let bank = self.selected_bank & 0x60;

                    let address_start = (0x4000 * bank as usize) % rom.len();
                    rom[address as usize + address_start]
                } else {
                    rom[address as usize]
                }
            }
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom);

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled {
                    return 0xff;
                }
                let start_address = if self.mode {
                    // Mode 1

                    // Large ROM have >= 1MiB
                    let large_rom = rom.len() >= 0x10_0000;
                    if large_rom {
                        0
                    } else {
                        0x2000 * ((self.selected_bank >> 5) & 0x03) as usize
                    }
                } else {
                    // Mode 0
                    0
                };
                let ram_address = (address as usize - 0xA000 + start_address) % ram.len();
                ram[ram_address]
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, rom: &[u8], ram: &mut Vec<u8>) {
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
                let value = value & 0x1F;
                let value = if value == 0 {
                    // Write 0 became 1 (5-bit register cannot be 0)
                    1
                } else {
                    value
                };
                self.selected_bank = (self.selected_bank & 0x60) | (value & 0x1F);
            }
            // RAM Bank Number - or - Upper Bits of ROM Bank Number
            0x4000..=0x5FFF => {
                // only higher 2 bits are written
                self.selected_bank = (self.selected_bank & 0x1F) | ((value & 0x3) << 5);
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
                    // Mode 1

                    // Large ROM have >= 1MiB
                    let large_rom = rom.len() >= 0x10_0000;
                    if large_rom {
                        0
                    } else {
                        0x2000 * ((self.selected_bank >> 5) & 0x03) as usize
                    }
                } else {
                    // Mode 0
                    0
                };
                let ram_address = (address as usize - 0xA000 + start_address) % ram.len();
                ram[ram_address] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC2 chip
#[derive(PartialEq, Eq)]
struct MBC2 {
    // the banking register
    selected_bank: u8,
    ram_enabled: bool,
}
impl SaveState for MBC2 {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.selected_bank.save_state(data)?;
        [&self.ram_enabled].save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.selected_bank.load_state(data)?;
        [&mut self.ram_enabled].load_state(data)?;
        Ok(())
    }
}
impl MBC2 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            ram_enabled: false,
        }
    }
    fn curr_bank(&self, rom: &[u8]) -> u16 {
        debug_assert!(self.selected_bank != 0);
        (self.selected_bank % (rom.len() / 0x4000) as u8) as u16
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &Vec<u8>) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom);

                // PERF: I could already store the start_address, instead of computing it every
                // time. The same for write, and others MBC's.
                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // 512x4bits RAM
            0xA000..=0xBFFF => {
                let address = address & 0x1FF; // only the bottom 9 bits are used
                if !self.ram_enabled {
                    return 0xff;
                }
                // upper 4bits are undefined
                ram[address as usize] | 0xF0
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut Vec<u8>) {
        match address {
            // RAM Enable and ROM Bank Number
            0x0000..=0x3FFF => {
                if (address >> 8) & 0x1 == 0 {
                    // RAM Enable
                    self.ram_enabled = (value & 0x0F) == 0x0A;
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
                if !self.ram_enabled {
                    return;
                }
                let address = address & 0x1FF; // only the bottom 9 bits are used
                                               // upper 4bits are undefined
                ram[address as usize] = value | 0xF0;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}

/// Cartridge with a MBC3 chip
#[derive(PartialEq, Eq)]
struct MBC3 {
    // the banking register, including second 2-bit
    selected_bank: u8,
    // false is mode 0, true is mode 1
    ram_enabled: bool,
    ram_bank: u8,
    rtc: [u8; 5],
    // the state in the latch clock data operation.
    // 0 is the intial state
    // 1 means that 0 was written
    latch_clock_data: u8,
}
impl SaveState for MBC3 {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.selected_bank.save_state(data)?;
        self.ram_bank.save_state(data)?;
        self.rtc.save_state(data)?;
        [&self.ram_enabled].save_state(data)?;
        self.latch_clock_data.save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.selected_bank.load_state(data)?;
        self.ram_bank.load_state(data)?;
        self.rtc.load_state(data)?;
        [&mut self.ram_enabled].load_state(data)?;
        self.latch_clock_data.load_state(data)?;
        Ok(())
    }
}
impl MBC3 {
    fn curr_bank(&self, _rom: &[u8]) -> u16 {
        self.selected_bank as u16
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &Vec<u8>) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom);

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
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
                        ram[address as usize - 0xA000 + start_address]
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

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut Vec<u8>) {
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
                        ram[address as usize - 0xA000 + start_address] = value;
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

/// Cartridge with a MBC5 chip
#[derive(PartialEq, Eq)]
struct MBC5 {
    selected_bank: u16,
    selected_ram_bank: u8,
    ram_enabled: bool,
}
impl SaveState for MBC5 {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.selected_bank.save_state(data)?;
        [&self.ram_enabled].save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.selected_bank.load_state(data)?;
        [&mut self.ram_enabled].load_state(data)?;
        Ok(())
    }
}
impl MBC5 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            selected_ram_bank: 0,
            ram_enabled: false,
        }
    }
    fn curr_bank(&self, rom: &[u8]) -> u16 {
        self.selected_bank % (rom.len() / 0x4000) as u16
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &Vec<u8>) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom);

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // RAM banks
            0xA000..=0xBFFF => {
                if !self.ram_enabled {
                    return 0xff;
                }
                let start_address = (self.selected_ram_bank as usize * 0x2000) % ram.len();
                ram[address as usize - 0xA000 + start_address]
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut Vec<u8>) {
        match address {
            // RAM Enable
            0x0000..=0x1FFF => {
                self.ram_enabled = value == 0x0A;
            }
            // lower ROM Bank
            0x2000..=0x2FFF => {
                self.selected_bank = (self.selected_bank & 0xFF00) | value as u16;
            }
            // upper ROM Bank
            0x3000..=0x3FFF => {
                // write the to bit-8 of the bank register
                self.selected_bank = (self.selected_bank & 0x00FF) | ((value as u16 & 0b1) << 8)
            }
            0x4000..=0x5FFF => {
                self.selected_ram_bank = value & 0x0F;
            }
            // RAM banks
            0xA000..=0xBFFF => {
                if !self.ram_enabled {
                    return;
                }
                let start_address = (self.selected_ram_bank as usize * 0x2000) % ram.len();
                ram[address as usize - 0xA000 + start_address] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }
}
