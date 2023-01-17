use std::io::{Read, Seek, SeekFrom};

fn read_u32(file: &mut impl Read) -> Result<u32, std::io::Error> {
    let mut value = [0; 4];
    file.read_exact(&mut value)?;
    Ok(u32::from_le_bytes(value))
}

fn read_u16(file: &mut impl Read) -> Result<u16, std::io::Error> {
    let mut value = [0; 2];
    file.read_exact(&mut value)?;
    Ok(u16::from_le_bytes(value))
}

fn read_u8(file: &mut impl Read) -> Result<u8, std::io::Error> {
    let mut value = 0;
    file.read_exact(std::slice::from_mut(&mut value))?;
    Ok(value)
}

/// VBM is the movie capture format of Visual Boy Advance
pub struct Vbm {
    pub magic: u32,
    pub version: u32,
    pub uid: u32,
    pub length_frames: u32,
    pub rerecord_count: u32,
    pub start_flags: u8,
    pub controller_flags: u8,
    pub type_flags: u8,
    pub options_flags: u8,
    pub save_type: u32,
    pub flash_size: u32,
    pub gb_emulator_type: u32,
    pub rom_title: [u8; 12],
    pub vbm_version: u8,
    pub rom_crc: u8,
    pub rom_or_bios_checksum: u16,
    pub rom_game_code: u32,
    pub name: String,
    pub description: String,
    pub start_data: Vec<u8>,
    pub controller_data: Vec<u16>,
}

impl std::fmt::Debug for Vbm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vbm")
            .field("signature", &self.magic)
            .field("verson", &self.version)
            .field("uid", &self.uid)
            .field("num_frames", &self.length_frames)
            .field("rerecord_count", &self.rerecord_count)
            .field("movie_start_flags", &self.start_flags)
            .field("controller_flags", &self.controller_flags)
            .field("system_flags", &self.type_flags)
            .field("emulator_flags", &self.options_flags)
            .field("win_save_type", &self.save_type)
            .field("win_flash_size", &self.flash_size)
            .field("gb_emulator_type", &self.gb_emulator_type)
            .field("game_title", &self.rom_title)
            .field("vbm_version", &self.vbm_version)
            .field("rom_crc", &self.rom_crc)
            .field("rom_checksum", &self.rom_or_bios_checksum)
            .field("game_code", &self.rom_game_code)
            .field("name", &self.name)
            .field("description", &self.description)
            // .field("start_data", &self.start_data)
            // .field("controller_data", &self.controller_data)
            .finish()
    }
}

#[derive(Debug)]
pub enum VbmParseError {
    InvalidSignature(u32),
    InvalidVersion(u32),
    InvalidControllerFlags(u8),
    InvalidSystemFlags(u8),
    InvalidStartFlags(u8),
    InvalidName,
    InvalidDescription,
    IoError(std::io::Error),
}
impl From<std::io::Error> for VbmParseError {
    fn from(v: std::io::Error) -> Self {
        Self::IoError(v)
    }
}

#[test]
#[ignore]
fn parse_vbm() {
    let mut file = std::fs::File::open("../roms/mugg-kirbysdreamland.vbm").unwrap();
    let vbm = vbm(&mut file).unwrap();
    println!("{:#x?}", vbm);
    assert_eq!(vbm.name, "mugg");
}

pub fn vbm(file: &mut (impl Read + Seek)) -> Result<Vbm, VbmParseError> {
    // from https://tasvideos.org/EmulatorResources/VBA/VBM and VBA-rerecord source code.
    // 000	4-byte signature	56 42 4D 1A "VBM\x1A"
    let magic = read_u32(file)?;
    if magic != 0x1A4D4256 {
        return Err(VbmParseError::InvalidSignature(magic));
    }
    // 004	4-byte little-endian unsigned int	major version number, must be "1"
    let version = read_u32(file)?;
    if version != 1 {
        return Err(VbmParseError::InvalidVersion(version));
    }
    // 008	4-byte little-endian integer	movie "uid" - identifies the movie-savestate relationship, also used as the recording time in Unix epoch format
    let uid = read_u32(file)?;
    // 00C	4-byte little-endian unsigned int	number of frames
    let length_frames = read_u32(file)?;
    // 010	4-byte little-endian unsigned int	rerecord count
    let rerecord_count = read_u32(file)?;
    // 014	1-byte flags	(movie start flags)
    // bit 0	if "1", movie starts from an embedded "quicksave" snapshot
    // bit 1	if "1", movie starts from reset with an embedded SRAM
    // If both bits 0 and 1 are "1", the movie file is invalid
    // other	reserved, set to 0
    let start_flags = read_u8(file)?;
    // 015	1-byte flags	controller flags
    // bit 0	controller 1 in use
    // bit 1	controller 2 in use (SGB games can be 2-player multiplayer)
    // bit 2	controller 3 in use (SGB games can be 3- or 4-player multiplayer with multitap)
    // bit 3	controller 4 in use (SGB games can be 3- or 4-player multiplayer with multitap)
    // other	reserved
    let controller_flags = read_u8(file)?;
    match controller_flags {
        0x01 => {}
        x if x & 0x0E != 0 => unimplemented!("only single controller is implemented"),
        x => return Err(VbmParseError::InvalidControllerFlags(x)),
    }
    // 016	1-byte flags	system flags (game always runs at 60 frames/sec)
    // bit 0	if "1", movie is for the GBA system
    // bit 1	if "1", movie is for the GBC system
    // bit 2	if "1", movie is for the SGB system
    // If all 3 of these bits are "0", it is for regular GB.
    // At most one of bits 0, 1, 2 can be "1"
    // other	reserved, set to 0
    let type_flags = read_u8(file)?;
    match type_flags {
        0x00 => {}
        0x01 | 0x02 | 0x04 => unimplemented!("only DMG is implemented"),
        x => return Err(VbmParseError::InvalidSystemFlags(x)),
    }
    // 017	1-byte flags	(values of some boolean emulator options)
    // bit 0: useBiosFile	if "1" and the movie is of a GBA game, the movie was made using a GBA BIOS file.
    // bit 1: skipBiosFile	if "0" and the movie was made with a GBA BIOS file, the BIOS intro is included in the movie.
    // bit 2: rtcEnable	if "1", the emulator "real time clock" feature was enabled.
    // bit 3: gbInputHack	if "1" and the movie is of a GB, GBC, or SGB game, the movie was made with the Null Input Kludge on, otherwise it was not.
    // bit 4: lagReduction	if "0" and the movie is of a GBA game, the movie was made using the old excessively laggy GBA timing.
    // bit 5: gbcHdma5Fix	if "0" and the movie is of a GBC game, the movie was made using the old buggy HDMA5 timing.
    // bit 6: echoRAMFix	if "1" and the movie is of a GB, GBC, or SGB game, the movie was made with Echo RAM Fix on, otherwise it was made with Echo RAM Fix off.
    // bit 7: sramInitFix	if "1" and the movie is of a GBA game, the movie was made with SRAM Init Fix on, otherwise it was not.
    let options_flags = read_u8(file)?;
    // 018	4-byte little-endian unsigned int	theApp.winSaveType (value of that emulator option)
    let save_type = read_u32(file)?;
    // 01C	4-byte little-endian unsigned int	theApp.winFlashSize (value of that emulator option)
    let flash_size = read_u32(file)?;
    // 020	4-byte little-endian unsigned int	gbEmulatorType (value of that emulator option)
    let gb_emulator_type = read_u32(file)?;
    // 024	12-byte character array	the internal game title of the ROM used while recording, not necessarily null-terminated (ASCII?)
    let mut rom_title = [0; 12];
    file.read_exact(&mut rom_title)?;
    // 030	1-byte unsigned char	minor version/revision number of current VBM version, the latest is "1"
    let vbm_version = read_u8(file)?;
    // 031	1-byte unsigned char	the internal CRC of the ROM used while recording
    let rom_crc = read_u8(file)?;
    // 032	2-byte little-endian unsigned short	the internal Checksum of the ROM used while recording, or a calculated CRC16 of the BIOS if GBA
    let rom_or_bios_checksum = read_u16(file)?;
    // 034	4-byte little-endian unsigned int	the Game Code of the ROM used while recording, or the Unit Code if not GBA
    let rom_game_code = read_u32(file)?;
    // 038	4-byte little-endian unsigned int	offset to the savestate or SRAM inside file, set to 0 if unused
    let offset_to_savestate = read_u32(file)?;
    // 03C	4-byte little-endian unsigned int	offset to the controller data inside file
    let offset_to_controller_data = read_u32(file)?;

    let name = {
        let mut buffer = [0; 64];
        file.read_exact(&mut buffer)?;
        let null_terminate = buffer
            .iter()
            .position(|&x| x == 0)
            .ok_or(VbmParseError::InvalidName)?;

        String::from_utf8_lossy(&buffer[0..null_terminate]).into_owned()
    };
    let description = {
        let mut buffer = [0; 128];
        file.read_exact(&mut buffer)?;
        let null_terminate = buffer
            .iter()
            .position(|&x| x == 0)
            .ok_or(VbmParseError::InvalidDescription)?;

        String::from_utf8_lossy(&buffer[0..null_terminate]).into_owned()
    };

    let start_data = if offset_to_savestate == 0 {
        Vec::new()
    } else {
        match start_flags {
            0x01 => unimplemented!("start from \"quicksave\" snapshot is unimplemented"),
            0x02 => unimplemented!("start from SRAM snapshot is unimplemented"),
            x => return Err(VbmParseError::InvalidStartFlags(x)),
        }
    };
    let controller_data: Vec<u16> = {
        file.seek(SeekFrom::Start(offset_to_controller_data as u64))?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;
        data.chunks_exact(2)
            .map(|x| u16::from_le_bytes([x[0], x[1]]))
            .collect()
    };
    assert_eq!(controller_data.len(), length_frames as usize + 1);

    // TODO: implement the rest
    Ok(Vbm {
        magic,
        version,
        uid,
        length_frames,
        rerecord_count,
        start_flags,
        controller_flags,
        type_flags,
        options_flags,
        save_type,
        flash_size,
        gb_emulator_type,
        rom_title,
        vbm_version,
        rom_crc,
        rom_or_bios_checksum,
        rom_game_code,
        name,
        description,
        start_data,
        controller_data,
    })
}
