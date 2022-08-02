use std::{borrow::Cow, path::PathBuf};

use gameroy::gameboy::cartridge::CartridgeHeader;

use crate::config;

fn open_and_read(
    rom_path: &std::path::Path,
    writer: &mut impl std::io::Write,
) -> Result<usize, String> {
    let file = &mut std::fs::File::open(&rom_path)
        .map_err(|x| format!("error loading '{}': {}", rom_path.display(), x))?;

    Ok(std::io::copy(file, writer)
        .map_err(|x| format!("error reading '{}': {}", rom_path.display(), x))? as usize)
}

pub fn load_boot_rom() -> Option<[u8; 256]> {
    let boot_rom_path = if let Some(x) = &config().boot_rom {
        PathBuf::from(x)
    } else {
        return None;
    };

    let mut boot_rom = [0; 0x100];
    match open_and_read(&boot_rom_path, &mut &mut boot_rom[..]) {
        Err(e) => {
            eprintln!("{}", e);
            return None;
        }
        Ok(_) => Some(boot_rom),
    }
}

#[derive(Clone, Debug)]
pub struct RomFile {
    path: PathBuf,
}
impl RomFile {
    pub fn from_path(path: PathBuf) -> Self {
        Self { path }
    }

    pub async fn get_header(&self) -> Result<CartridgeHeader, String> {
        let mut file =
            std::fs::File::open(self.path.clone()).map_err(|e| format!("io error: {}", e))?;
        let header = match CartridgeHeader::from_reader(&mut file) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((_, e)) => return Err(e),
        };
        Ok(header)
    }

    pub fn file_name(&self) -> Cow<str> {
        self.path
            .file_name()
            .map_or("".into(), |x| x.to_string_lossy())
    }

    pub async fn read(&self) -> Result<Vec<u8>, String> {
        let mut rom = Vec::new();
        let rom_path = &self.path;
        let file = &mut std::fs::File::open(&rom_path)
            .map_err(|x| format!("error loading '{}': {}", rom_path.display(), x))?;

        std::io::copy(file, &mut rom)
            .map_err(|x| format!("error reading '{}': {}", rom_path.display(), x))?;

        Ok(rom)
    }

    pub async fn load_ram_data(&self) -> Result<Vec<u8>, String> {
        let save_path = self.save_path();
        log::info!("loading save at {}", save_path.display());
        std::fs::read(&save_path).map_err(|x| format!("load save failed: {}", x))
    }

    fn save_path(&self) -> PathBuf {
        self.path.with_extension("sav")
    }

    fn save_state_path(&self) -> PathBuf {
        self.path.with_extension("save_state")
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), String> {
        let save_path = self.save_path();
        std::fs::write(save_path, data).map_err(|x| x.to_string())
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), String> {
        let save_path = self.save_state_path();
        std::fs::write(save_path, state).map_err(|x| x.to_string())
    }

    pub fn load_state(&self) -> Result<Vec<u8>, String> {
        let save_path = self.save_state_path();
        std::fs::read(save_path).map_err(|x| x.to_string())
    }
}
#[cfg(feature = "rfd")]
impl From<rfd::FileHandle> for RomFile {
    fn from(handle: rfd::FileHandle) -> Self {
        Self {
            path: handle.inner().to_path_buf(),
        }
    }
}
