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
