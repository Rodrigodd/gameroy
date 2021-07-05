use std::io::Read;

const MBC_TYPE: &[&str] = &[
    "ROM ONLY",
    "MBC1",
    "MBC1+RAM",
    "MBC1+RAM+BATTERY",
    "MBC2",
    "MBC2+BATTERY",
    "ROM+RAM 1",
    "ROM+RAM+BATTERY 1",
    "MMM01",
    "MMM01+RAM",
    "MMM01+RAM+BATTERY",
    "MBC3+TIMER+BATTERY",
    "MBC3+TIMER+RAM+BATTERY 2",
    "MBC3",
    "MBC3+RAM 2",
    "MBC3+RAM+BATTERY 2",
    "MBC5",
    "MBC5+RAM",
    "MBC5+RAM+BATTERY",
    "MBC5+RUMBLE",
    "MBC5+RUMBLE+RAM",
    "MBC5+RUMBLE+RAM+BATTERY",
    "MBC6",
    "MBC7+SENSOR+RUMBLE+RAM+BATTERY",
    "POCKET CAMERA",
    "BANDAI TAMA5",
    "HuC3",
    "HuC1+RAM+BATTERY",
];

enum MBC {
    None,
    MBC1,
}
impl MBC {
    fn from_kind(kind: u8) -> Result<Self, String> {
        Ok(match kind {
            0 => MBC::None,
            1 | 2 | 3 => MBC::MBC1,
            _ => {
                return Err(format!(
                    "MBC type '{}' ({}) is not supported",
                    MBC_TYPE[kind as usize], kind
                ))
            }
        })
    }
}

pub struct Cartridge {
    mbc: MBC,
    pub rom: Vec<u8>,
    // the banking register, including second 2-bit
    selected_bank: u8,
    // false is mode 0, true is mode 1
    mode: bool,
    ram: Vec<u8>,
    ram_enabled: bool,
}
impl Cartridge {
    pub fn new(mut file: impl Read) -> Result<Self, String> {
        let mut buffer = Vec::with_capacity(0x10000);
        file.read_to_end(&mut buffer)
            .map_err(|err| err.to_string())?;

        if buffer.len() < 0x10000 {
            buffer.resize(0x10000, 0);
        }

        // Cartridge Type
        let mbc_kind = buffer[0x0147];

        Ok(Self {
            mbc: MBC::from_kind(mbc_kind)?,
            rom: buffer,
            selected_bank: 0,
            mode: false,
            ram: vec![0; 0x8000],
            ram_enabled: false,
        })
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match self.mbc {
            MBC::None => {
                match address {
                    // ROM
                    0x0000..=0x7FFF => self.rom[address as usize],
                    // RAM
                    0xA000..=0xBFFF => self.ram[address as usize - 0xA000],
                    _ => unreachable!("read cartridge out of bounds"),
                }
            }
            MBC::MBC1 => {
                match address {
                    // ROM Bank X0
                    0x0000..=0x3FFF => self.rom[address as usize],
                    // ROM Bank 01-7F
                    0x4000..=0x7FFF => {
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
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match self.mbc {
            MBC::None => {
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
            MBC::MBC1 => {
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
    }
}
