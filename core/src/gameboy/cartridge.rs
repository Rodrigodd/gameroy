use std::{convert::TryInto, io::Read};

use crate::save_state::{LoadStateError, SaveState, SaveStateContext};

const NINTENDOO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

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

#[derive(PartialEq, Eq, Clone)]
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
    /// Return  Err(Some(Self)) if the load was sucessful but the checksum don't match.
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
            if Self::compute_check_sum(bytes) != this.header_checksum {
                return Err((Some(this), "checksum don't match".to_string()));
            }
        }

        Ok(this)
    }

    pub fn compute_check_sum(bytes: &[u8]) -> u8 {
        bytes[0x134..=0x014C]
            .iter()
            .fold(0u8, |x, &b| x.wrapping_add(!b))
    }

    /// Return  Err(Some(Self)) if the load was sucessful but the checksum don't match.
    pub fn from_reader(reader: &mut impl Read) -> Result<Self, (Option<Self>, String)> {
        let mut bytes = [0; 0x150];
        let len = match reader.read(&mut bytes) {
            Ok(x) => x,
            Err(err) => return Err((None, format!("io error: {}", err))),
        };
        Self::from_bytes(&bytes[0..len])
    }

    /// Return true if it has the correct values for the first  0x18  bytes of the Nintendo logo.
    pub fn check_logo(&self) -> bool {
        self.logo[..0x18] == NINTENDOO_LOGO[..0x18]
    }

    pub fn rom_size_in_bytes(&self) -> Result<usize, String> {
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
        let rom_size_type = self.rom_size;
        let rom_size = rom_sizes
            .get(rom_size_type as usize)
            .copied()
            .ok_or_else(|| format!("Rom size '{:02x}' is no supported", rom_size_type))?;
        Ok(rom_size)
    }

    pub fn title_as_string(&self) -> String {
        let l = self
            .title
            .as_slice()
            .iter()
            .position(|&x| x == 0)
            .unwrap_or(self.title.len());
        String::from_utf8_lossy(&self.title[0..l]).into_owned()
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(PartialEq, Eq, Clone)]
enum Mbc {
    None(Mbc0),
    Mbc1(Mbc1),
    Mbc1M(Mbc1M),
    Mbc2(Mbc2),
    Mbc3(Mbc3),
    Mbc5(Mbc5),
}

#[derive(PartialEq, Eq, Clone)]
pub struct Cartridge {
    pub header: CartridgeHeader,
    /// The current bank of ROM mapped in 0000..=3FFF.
    pub lower_bank: u16,
    /// The current bank of ROM mapped in 4000..=7FFF.
    pub upper_bank: u16,
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,
    mbc: Mbc,
}
impl SaveState for Cartridge {
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl std::io::Write,
    ) -> Result<(), std::io::Error> {
        // self.rom.save_state(data)?;
        self.ram.save_state(ctx, data)?;
        match &self.mbc {
            Mbc::None(x) => x.save_state(ctx, data),
            Mbc::Mbc1(x) => x.save_state(ctx, data),
            Mbc::Mbc1M(x) => x.save_state(ctx, data),
            Mbc::Mbc2(x) => x.save_state(ctx, data),
            Mbc::Mbc3(x) => x.save_state(ctx, data),
            Mbc::Mbc5(x) => x.save_state(ctx, data),
        }
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        // self.rom.load_state(data)?;
        self.ram.load_state(ctx, data)?;
        match &mut self.mbc {
            Mbc::None(x) => x.load_state(ctx, data)?,
            Mbc::Mbc1(x) => x.load_state(ctx, data)?,
            Mbc::Mbc1M(x) => x.load_state(ctx, data)?,
            Mbc::Mbc2(x) => x.load_state(ctx, data)?,
            Mbc::Mbc3(x) => x.load_state(ctx, data)?,
            Mbc::Mbc5(x) => x.load_state(ctx, data)?,
        }
        self.update_banks();
        Ok(())
    }
}
impl Cartridge {
    pub fn new(rom: Vec<u8>) -> Result<Self, String> {
        let header = match CartridgeHeader::from_bytes(&rom) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((None, err)) => return Err(err),
        };

        let rom_size = header.rom_size_in_bytes()?;

        if rom_size != rom.len() {
            return Err(format!(
                "In the rom header the expected size is '{}' bytes, but the given rom has '{}' bytes",
                rom_size,
                rom.len()
            ));
        }

        // Cartridge Type
        let mbc_kind = header.cartridge_type;
        let mbc = match mbc_kind {
            0 | 8 | 9 => Mbc::None(Mbc0 {}),
            1..=3 => 'mbc1: {
                // Detect if it is a MBC1M card
                if header.rom_size == 5 {
                    let mut number_of_games = 0;
                    for i in 0..4 {
                        let header = match CartridgeHeader::from_bytes(&rom[i * 0x40000..]) {
                            Ok(x) | Err((Some(x), _)) => x,
                            Err((None, _)) => continue,
                        };
                        if header.check_logo() {
                            number_of_games += 1;
                        }
                    }
                    // multicarts will have, at least, a game selecion screen, and two other games.
                    if number_of_games >= 3 {
                        break 'mbc1 Mbc::Mbc1M(Mbc1M::new());
                    }
                }
                Mbc::Mbc1(Mbc1::new())
            }
            5 | 6 => Mbc::Mbc2(Mbc2::new()),
            0x0F..=0x13 => Mbc::Mbc3(Mbc3::new()),
            0x19..=0x1E => Mbc::Mbc5(Mbc5::new()),
            _ => {
                return Err(format!(
                    "MBC type '{}' ({:02x}) is not supported",
                    mbc_type_name(mbc_kind),
                    mbc_kind
                ))
            }
        };

        let ram_sizes = [
            0,
            0x800,
            0x2000, // Single Bank
            4 * 0x2000,
            16 * 0x2000,
            8 * 0x2000,
        ];
        let ram_size_type = header.ram_size;

        let ram_size = if let Mbc::Mbc2(_) = mbc {
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
            lower_bank: 0,
            upper_bank: 1,
            rom,
            ram: vec![0; ram_size],
            mbc,
        })
    }

    /// A Cartridge filled with HALT instructions. Used as a test cartridge, when the CPU does not
    /// need to be tested.
    pub fn halt_filled() -> Self {
        let mut rom = vec![0x76; 2 * 0x4000];

        // fill header with zeros
        rom[0x100..0x150].iter_mut().for_each(|x| *x = 0);

        // fill with nintendo logo and the correct check sum, to make it look like a valid rom
        rom[0x104..=0x133].copy_from_slice(&NINTENDOO_LOGO);
        rom[0x14D] = CartridgeHeader::compute_check_sum(&rom);

        let this = Self::new(rom).unwrap();

        assert!(this.ram.is_empty());

        this
    }

    /// The number of banks in this cartridge. A cartridge without bank switching have 2 banks.
    pub fn num_banks(&self) -> u8 {
        (self.rom.len() / 0x4000) as u8
    }

    /// Return a string with the kind of the cartridge.
    pub fn kind_name(&self) -> &str {
        match &self.mbc {
            Mbc::None(_) => "None",
            Mbc::Mbc1(_) => "MBC1",
            Mbc::Mbc1M(_) => "MBC1M",
            Mbc::Mbc2(_) => "MBC2",
            Mbc::Mbc3(_) => "MBC3",
            Mbc::Mbc5(_) => "MBC5",
        }
    }

    /// The current pair of ROM banks beign mapped to 0..=3FFF and 4000..=7FFF, respectvely.
    pub fn curr_bank(&self) -> (u16, u16) {
        (self.lower_bank, self.upper_bank)
    }

    pub fn read(&self, address: u16) -> u8 {
        if address <= 0x3FFF {
            return self.rom[self.lower_bank as usize * 0x4000..][address as usize];
        }
        if address <= 0x7FFF {
            return self.rom[self.upper_bank as usize * 0x4000..][(address - 0x4000) as usize];
        }
        match &self.mbc {
            Mbc::None(x) => x.read(address, &self.rom, &self.ram),
            Mbc::Mbc1(x) => x.read(address, &self.rom, &self.ram),
            Mbc::Mbc1M(x) => x.read(address, &self.rom, &self.ram),
            Mbc::Mbc2(x) => x.read(address, &self.rom, &self.ram),
            Mbc::Mbc3(x) => x.read(address, &self.rom, &self.ram),
            Mbc::Mbc5(x) => x.read(address, &self.rom, &self.ram),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match &mut self.mbc {
            Mbc::None(x) => x.write(address, value, &self.rom, &mut self.ram),
            Mbc::Mbc1(x) => x.write(address, value, &self.rom, &mut self.ram),
            Mbc::Mbc1M(x) => x.write(address, value, &self.rom, &mut self.ram),
            Mbc::Mbc2(x) => x.write(address, value, &self.rom, &mut self.ram),
            Mbc::Mbc3(x) => x.write(address, value, &self.rom, &mut self.ram),
            Mbc::Mbc5(x) => x.write(address, value, &self.rom, &mut self.ram),
        }
        self.update_banks();
    }

    fn update_banks(&mut self) {
        (self.lower_bank, self.upper_bank) = match &self.mbc {
            Mbc::None(_) => (0, 1),
            Mbc::Mbc1(x) => x.curr_bank(&self.rom),
            Mbc::Mbc1M(x) => x.curr_bank(&self.rom),
            Mbc::Mbc2(x) => x.curr_bank(&self.rom),
            Mbc::Mbc3(x) => x.curr_bank(&self.rom),
            Mbc::Mbc5(x) => x.curr_bank(&self.rom),
        }
    }

    /// Read at the given address, as if the given bank is active
    pub fn read_at_bank(&self, bank: u16, address: u16) -> u8 {
        match &self.mbc {
            Mbc::None(x) => x.read_at_bank(bank, address, &self.rom),
            Mbc::Mbc1(x) => x.read_at_bank(bank, address, &self.rom),
            Mbc::Mbc1M(x) => x.read_at_bank(bank, address, &self.rom),
            Mbc::Mbc2(x) => x.read_at_bank(bank, address, &self.rom),
            Mbc::Mbc3(x) => x.read_at_bank(bank, address, &self.rom),
            Mbc::Mbc5(x) => x.read_at_bank(bank, address, &self.rom),
        }
    }

    pub fn bank0_from_bank(&self, bank: u16) -> u16 {
        match &self.mbc {
            Mbc::None(_) => 0,
            Mbc::Mbc1(_) => bank & 0x60,
            Mbc::Mbc1M(_) => bank & 0x30,
            Mbc::Mbc2(_) => 0,
            Mbc::Mbc3(_) => 0,
            Mbc::Mbc5(_) => 0,
        }
    }

    pub fn bank_from_write(&self, previous_bank: u16, address: u16, value: u8) -> u16 {
        match &self.mbc {
            Mbc::None(_) => previous_bank,
            Mbc::Mbc1(_) => match address {
                // RAM Enable
                0x0000..=0x1FFF => previous_bank,
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
                    (previous_bank & 0x60) | (value & 0x1F) as u16
                }
                // RAM Bank Number - or - Upper Bits of ROM Bank Number
                0x4000..=0x5FFF => {
                    // only higher 2 bits are written
                    (previous_bank & 0x1F) | ((value as u16 & 0x3) << 5)
                }
                _ => previous_bank,
            },
            Mbc::Mbc1M(_) => match address {
                // RAM Enable
                0x0000..=0x1FFF => previous_bank,
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
                    (previous_bank & 0x30) | (value & 0x0F) as u16
                }
                // RAM Bank Number - or - Upper Bits of ROM Bank Number
                0x4000..=0x5FFF => {
                    // only higher 2 bits are written
                    (previous_bank & 0x0F) | ((value & 0x3) << 4) as u16
                }
                // Banking Mode Select
                0x6000..=0x7FFF => previous_bank,
                _ => previous_bank,
            },
            Mbc::Mbc2(_) => match address {
                // RAM Enable and ROM Bank Number
                0x0000..=0x3FFF => {
                    if (address >> 8) & 0x1 == 0 {
                        previous_bank
                    } else {
                        // ROM Bank Number
                        // The lower 4 bits control the selected bank.
                        let value = value & 0x0F;
                        if value == 0 {
                            1
                        } else {
                            value as u16
                        }
                    }
                }
                0x4000..=0x7FFF => previous_bank,
                _ => previous_bank,
            },
            Mbc::Mbc3(_) => match address {
                // RAM Enable
                0x0000..=0x1FFF => previous_bank,

                // ROM Bank Number
                0x2000..=0x3FFF => {
                    // all 7 bits are written
                    let value = value & 0x7F;
                    if value == 0 {
                        // Write 0 became 1 (register cannot be 0)
                        1
                    } else {
                        value as u16
                    }
                }
                // RAM Bank Number - or - Upper Bits of ROM Bank Number
                0x4000..=0x5FFF => previous_bank,
                // Latch Clock Data
                0x6000..=0x7FFF => previous_bank,
                _ => previous_bank,
            },
            Mbc::Mbc5(_) => match address {
                // RAM Enable
                0x0000..=0x1FFF => previous_bank,
                // lower ROM Bank
                0x2000..=0x2FFF => (previous_bank & 0xFF00) | value as u16,
                // upper ROM Bank
                0x3000..=0x3FFF => {
                    // write the to bit-8 of the bank register
                    (previous_bank & 0x00FF) | ((value as u16 & 0b1) << 8)
                }
                // RAM bank number
                0x4000..=0x5FFF => (value & 0x0F) as u16,
                0x6000..=0x7FFF => previous_bank,
                _ => previous_bank,
            },
        }
    }
}

/// Cartridge without a MBC chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc0 {}
crate::save_state!(Mbc0, self, data {});
impl Mbc0 {
    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
        match address {
            // ROM
            0x0000..=0x7FFF => rom[address as usize],
            // RAM
            0xA000..=0xBFFF => {
                if address as usize > ram.len() {
                    return 0xff;
                }
                ram[address as usize - 0xA000]
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut [u8]) {
        match address {
            // ROM
            0x0000..=0x7FFF => {}
            // RAM
            0xA000..=0xBFFF => {
                if address as usize > ram.len() {
                    return;
                }
                ram[address as usize - 0xA000] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }

    pub fn read_at_bank(&self, _bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM
            0x0000..=0x7FFF => rom[address as usize],
            _ => unreachable!("read rom out of bounds"),
        }
    }
}

/// Cartridge with a MBC1 chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc1 {
    // the banking register. Includes the 5-bit register 1, and the 2-bit register 2.
    selected_bank: u8,
    // false is mode 0, true is mode 1
    mode: bool,
    ram_enabled: bool,
}
crate::save_state!(Mbc1, self, data {
    self.selected_bank;
    bitset [self.mode, self.ram_enabled];
});
impl Mbc1 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            mode: false,
            ram_enabled: false,
        }
    }

    fn curr_bank(&self, rom: &[u8]) -> (u16, u16) {
        let bank = self.selected_bank;

        // cannot adress a bank where the 5-bit bank register is 0
        debug_assert!(bank & 0x1F != 0);

        // mask upper bits if the bank is out of bounds
        let bank = (bank as usize % (rom.len() / 0x4000)) as u16;

        let lower_bank = if self.mode { bank & 0x60 } else { 0 };

        let upper_bank = bank;

        (lower_bank, upper_bank)
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
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
                let bank = self.curr_bank(rom).1;

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled || ram.is_empty() {
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

    pub fn write(&mut self, address: u16, value: u8, rom: &[u8], ram: &mut [u8]) {
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
                if !self.ram_enabled || ram.is_empty() {
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

    pub fn read_at_bank(&self, bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM Bank X0
            0x0000..=0x3FFF => {
                if self.mode {
                    let bank = bank & 0x60;

                    let address_start = (0x4000 * bank as usize) % rom.len();
                    rom[address as usize + address_start]
                } else {
                    rom[address as usize]
                }
            }
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let address_start = (0x4000 * bank as usize) % rom.len();
                rom[address as usize - 0x4000 + address_start]
            }
            _ => unreachable!("read rom out of bounds"),
        }
    }
}

/// Cartridge with a MBC1 multicart chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc1M {
    // the banking register. Includes the 4-bit register 1 (the bit 4 of the 5-bit register is
    // skipped in the multicard), and the 2-bit register 2. A total of 6-bit.
    selected_bank: u8,
    // false is mode 0, true is mode 1
    mode: bool,
    ram_enabled: bool,
}
crate::save_state!(Mbc1M, self, data {
    self.selected_bank;
    bitset [self.mode, self.ram_enabled];
});
impl Mbc1M {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            mode: false,
            ram_enabled: false,
        }
    }

    fn curr_bank(&self, rom: &[u8]) -> (u16, u16) {
        let bank = self.selected_bank;

        // mask upper bits if the bank is out of bounds
        let bank = (bank as usize % (rom.len() / 0x4000)) as u16;

        let lower_bank = if self.mode { bank & 0x30 } else { 0 };

        let upper_bank = bank;

        (lower_bank, upper_bank)
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
        match address {
            // ROM Bank X0
            0x0000..=0x3FFF => {
                if self.mode {
                    let bank = self.selected_bank & 0x30;

                    let address_start = (0x4000 * bank as usize) % rom.len();
                    rom[address as usize + address_start]
                } else {
                    rom[address as usize]
                }
            }
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom).1;

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled || ram.is_empty() {
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

    pub fn write(&mut self, address: u16, value: u8, rom: &[u8], ram: &mut [u8]) {
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
                self.selected_bank = (self.selected_bank & 0x30) | (value & 0x0F);
            }
            // RAM Bank Number - or - Upper Bits of ROM Bank Number
            0x4000..=0x5FFF => {
                // only higher 2 bits are written
                self.selected_bank = (self.selected_bank & 0x0F) | ((value & 0x3) << 4);
            }
            // Banking Mode Select
            0x6000..=0x7FFF => {
                self.mode = value & 0x01 != 0;
            }
            // RAM Bank 00-03, if any
            0xA000..=0xBFFF => {
                if !self.ram_enabled || ram.is_empty() {
                    return;
                }
                let start_address = if self.mode {
                    // Mode 1

                    // Large ROM have >= 1MiB
                    let large_rom = rom.len() >= 0x10_0000;
                    if large_rom {
                        0
                    } else {
                        0x2000 * ((self.selected_bank >> 4) & 0x03) as usize
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

    pub fn read_at_bank(&self, bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM Bank X0
            0x0000..=0x3FFF => {
                if self.mode {
                    let bank = bank & 0x30;

                    let address_start = (0x4000 * bank as usize) % rom.len();
                    rom[address as usize + address_start]
                } else {
                    rom[address as usize]
                }
            }
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            _ => unreachable!("read rom out of bounds"),
        }
    }
}

/// Cartridge with a MBC2 chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc2 {
    // the banking register
    selected_bank: u8,
    ram_enabled: bool,
}
crate::save_state!(Mbc2, self, data {
    self.selected_bank;
    bitset [self.ram_enabled];
});
impl Mbc2 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            ram_enabled: false,
        }
    }
    fn curr_bank(&self, rom: &[u8]) -> (u16, u16) {
        debug_assert!(self.selected_bank != 0);
        let lower_bank = 0;
        let upper_bank = (self.selected_bank as usize % (rom.len() / 0x4000)) as u16;

        (lower_bank, upper_bank)
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom).1;

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

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut [u8]) {
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

    pub fn read_at_bank(&self, bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                // PERF: I could already store the start_address, instead of computing it every
                // time. The same for write, and others MBC's.
                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            _ => unreachable!("read rom out of bounds"),
        }
    }
}

/// Cartridge with a MBC3 chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc3 {
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
crate::save_state!(Mbc3, self, data {
    self.selected_bank;
    self.ram_bank;
    self.rtc;
    bitset [self.ram_enabled];
    self.latch_clock_data;
});
impl Mbc3 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            ram_enabled: false,
            ram_bank: 0,
            rtc: [0; 5],
            latch_clock_data: 0,
        }
    }

    fn curr_bank(&self, rom: &[u8]) -> (u16, u16) {
        let bank = self.selected_bank;

        // cannot adress a bank where the 7-bit bank register is 0
        debug_assert!(bank != 0);

        let lower_bank = 0;

        let upper_bank = {
            // mask upper bits if the bank is out of bounds
            (bank as usize % (rom.len() / 0x4000)) as u16
        };

        (lower_bank, upper_bank)
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom).1;

                let address_start = 0x4000 * bank as usize;
                let i = address as usize - 0x4000 + address_start;
                let len = rom.len();
                if i > len {
                    panic!(
                        "reading from bank {:02x}, address {:04x}, but rom length is {:04x}",
                        bank, i, len
                    );
                }
                rom[i]
            }
            // RAM Bank 00-03, or RTC registers 08-0C
            0xA000..=0xBFFF => {
                match self.ram_bank {
                    // RAM bank
                    0x0..=0x03 => {
                        if !self.ram_enabled || ram.is_empty() {
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

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut [u8]) {
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
                let value = value & 0x7F;
                let value = if value == 0 {
                    // Write 0 became 1 (register cannot be 0)
                    1
                } else {
                    value
                };
                self.selected_bank = value;
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

                    let now = std::time::Duration::ZERO;
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
                        if !self.ram_enabled || ram.is_empty() {
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

    pub fn read_at_bank(&self, bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-7F
            0x4000..=0x7FFF => {
                let address_start = 0x4000 * bank as usize;
                let i = address as usize - 0x4000 + address_start;
                let len = rom.len();
                if i > len {
                    panic!(
                        "reading from bank {:02x}, address {:04x}, but rom length is {:04x}",
                        bank, i, len
                    );
                }
                rom[i]
            }
            _ => unreachable!("read rom out of bounds"),
        }
    }
}

/// Cartridge with a MBC5 chip
#[derive(PartialEq, Eq, Clone)]
struct Mbc5 {
    selected_bank: u16,
    selected_ram_bank: u8,
    ram_enabled: bool,
}
crate::save_state!(Mbc5, self, data {
    self.selected_bank;
    self.selected_ram_bank;
    bitset [self.ram_enabled];
});
impl Mbc5 {
    fn new() -> Self {
        Self {
            selected_bank: 1,
            selected_ram_bank: 0,
            ram_enabled: false,
        }
    }
    fn curr_bank(&self, rom: &[u8]) -> (u16, u16) {
        let lower_bank = 0;
        let upper_bank = (self.selected_bank as usize % (rom.len() / 0x4000)) as u16;
        (lower_bank, upper_bank)
    }

    pub fn read(&self, address: u16, rom: &[u8], ram: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 00-1FF
            0x4000..=0x7FFF => {
                let bank = self.curr_bank(rom).1;

                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            // RAM banks
            0xA000..=0xBFFF => {
                if !self.ram_enabled || ram.is_empty() {
                    return 0xff;
                }
                let start_address = (self.selected_ram_bank as usize * 0x2000) % ram.len();
                ram[address as usize - 0xA000 + start_address]
            }
            _ => unreachable!("read cartridge out of bounds"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8, _rom: &[u8], ram: &mut [u8]) {
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
            // RAM bank number
            0x4000..=0x5FFF => {
                self.selected_ram_bank = value & 0x0F;
            }
            0x6000..=0x7FFF => {}
            // RAM banks
            0xA000..=0xBFFF => {
                if !self.ram_enabled || ram.is_empty() {
                    return;
                }
                let start_address = (self.selected_ram_bank as usize * 0x2000) % ram.len();
                ram[address as usize - 0xA000 + start_address] = value;
            }
            _ => unreachable!("write cartridge out of bounds"),
        }
    }

    pub fn read_at_bank(&self, bank: u16, address: u16, rom: &[u8]) -> u8 {
        match address {
            // ROM Bank 00
            0x0000..=0x3FFF => rom[address as usize],
            // ROM Bank 01-0F
            0x4000..=0x7FFF => {
                let address_start = 0x4000 * bank as usize;
                rom[address as usize - 0x4000 + address_start]
            }
            _ => unreachable!("read rom out of bounds"),
        }
    }
}
