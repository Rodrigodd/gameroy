//! Everything that touches the filesystem.
//!
//! The server deals only in bytes and has no `std::fs` dependency at all, so every
//! path, every read and every write lives here. That is what lets the same server
//! run in a web worker later, and it removes the whole class of "which side
//! resolved this path" bugs.

use std::path::{Path, PathBuf};

pub struct RomFile {
    path: PathBuf,
}

impl RomFile {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Battery-backed cartridge RAM, kept beside the ROM as `<rom>.sav`.
    pub fn save_ram_path(&self) -> PathBuf {
        self.path.with_extension("sav")
    }

    pub fn read_rom(&self) -> std::io::Result<Vec<u8>> {
        std::fs::read(&self.path)
    }

    pub fn read_save_ram(&self) -> Option<Vec<u8>> {
        std::fs::read(self.save_ram_path()).ok()
    }

    /// Writes cartridge RAM via a temporary file and a rename, so an interrupted
    /// write cannot leave a truncated save where a good one used to be.
    pub fn write_save_ram(&self, data: &[u8]) -> std::io::Result<()> {
        let final_path = self.save_ram_path();
        let tmp_path = final_path.with_extension("sav.tmp");
        std::fs::write(&tmp_path, data)?;
        std::fs::rename(&tmp_path, &final_path)
    }
}

impl RomFile {
    /// Single-slot save state, kept beside the ROM as `<rom>.save_state`.
    pub fn save_state_path(&self) -> PathBuf {
        self.path.with_extension("save_state")
    }

    pub fn read_save_state(&self) -> std::io::Result<Vec<u8>> {
        std::fs::read(self.save_state_path())
    }

    pub fn write_save_state(&self, data: &[u8]) -> std::io::Result<()> {
        let final_path = self.save_state_path();
        let tmp_path = final_path.with_extension("save_state.tmp");
        std::fs::write(&tmp_path, data)?;
        std::fs::rename(&tmp_path, &final_path)
    }
}

/// True for a path the emulator will accept as a ROM.
///
/// The extension is all there is to go on: a drop hands over a path, and reading
/// the file to sniff a cartridge header before deciding whether to react would
/// stall the event loop on whatever the user happened to drag.
pub fn is_rom_path(path: &Path) -> bool {
    path.extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("gb"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognises_rom_paths_whatever_the_case() {
        assert!(is_rom_path(Path::new("/roms/tetris.gb")));
        assert!(is_rom_path(Path::new("/roms/Tetris.GB")));
        assert!(!is_rom_path(Path::new("/roms/tetris.gbc")));
        assert!(!is_rom_path(Path::new("/roms/tetris.sav")));
        assert!(!is_rom_path(Path::new("/roms/tetris")));
        assert!(!is_rom_path(Path::new("/roms")));
    }
}
