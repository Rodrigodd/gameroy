//! Front-end configuration, in `gameroy-slint.toml`.
//!
//! Deliberately a separate file from the old front-end's `gameroy.toml`: the
//! keymap has to move from winit key codes to Slint key names, and sharing one
//! file would mean each frontend writing a format the other cannot read. The two
//! can diverge safely this way.
//!
//! Note the old `Config::save()` was `Err("unimplemented")` on desktop, so its
//! sort-order and rom-folder persistence silently did nothing. This actually
//! writes.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::library::Sort;

const FILE_NAME: &str = "gameroy-slint.toml";

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Config {
    /// Folder scanned for `.gb` files.
    pub rom_folder: Option<String>,
    /// Sort order, in the old `"+File"` / `"-Size"` notation.
    pub sort_list: Option<String>,
    /// Scale the screen by whole pixels only.
    pub only_integer_scaling: bool,
    /// Allocate the rewind ring. Costs 32 MiB when on.
    pub rewinding: bool,
    /// Open the debugger overlay at startup.
    pub start_in_debug: bool,
    /// Use the x86_64 recompiler. No effect on other architectures.
    pub jit: bool,
}

impl Config {
    pub fn sort(&self) -> Sort {
        self.sort_list
            .as_deref()
            .and_then(Sort::from_config)
            .unwrap_or_default()
    }

    pub fn set_sort(&mut self, sort: Sort) {
        self.sort_list = Some(sort.to_config());
    }

    /// Reads the config, falling back to defaults for anything missing or broken.
    /// A malformed config should not stop the emulator from starting.
    pub fn load() -> Self {
        let path = config_path();
        let text = match std::fs::read_to_string(&path) {
            Ok(t) => t,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Self::with_defaults(),
            Err(e) => {
                log::warn!("could not read {}: {e}", path.display());
                return Self::with_defaults();
            }
        };
        match toml::from_str(&text) {
            Ok(c) => c,
            Err(e) => {
                log::warn!("ignoring malformed {}: {e}", path.display());
                Self::with_defaults()
            }
        }
    }

    fn with_defaults() -> Self {
        Self {
            rewinding: true,
            jit: true,
            ..Self::default()
        }
    }

    /// Writes through a temp file and a rename, so an interrupted write cannot
    /// leave a half-written config behind.
    pub fn save(&self) -> std::io::Result<()> {
        let path = config_path();
        let text = toml::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        let tmp = path.with_extension("toml.tmp");
        std::fs::write(&tmp, text)?;
        std::fs::rename(&tmp, &path)
    }
}

fn config_path() -> PathBuf {
    base_folder().join(FILE_NAME)
}

/// Where config lives.
///
/// `CARGO_WORKSPACE_DIR` is set by this workspace's `.cargo/config.toml`, so under
/// `cargo run` the config resolves at the workspace root rather than somewhere in
/// `target/`. That matches the old front-end; changing it would silently move
/// everyone's config.
fn base_folder() -> PathBuf {
    if let Some(dir) = option_env!("CARGO_WORKSPACE_DIR") {
        if !dir.is_empty() {
            return PathBuf::from(dir);
        }
    }
    std::env::current_exe()
        .ok()
        .as_deref()
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::library::SortColumn;

    #[test]
    fn round_trips_through_toml() {
        let mut c = Config {
            rom_folder: Some("/roms".into()),
            only_integer_scaling: true,
            rewinding: true,
            start_in_debug: false,
            jit: true,
            sort_list: None,
        };
        c.set_sort(Sort {
            column: SortColumn::Size,
            ascending: false,
        });

        let text = toml::to_string_pretty(&c).unwrap();
        let back: Config = toml::from_str(&text).unwrap();
        assert_eq!(back.rom_folder.as_deref(), Some("/roms"));
        assert!(back.only_integer_scaling);
        assert_eq!(back.sort().column, SortColumn::Size);
        assert!(!back.sort().ascending);
    }

    #[test]
    fn missing_and_broken_values_fall_back() {
        // An empty table must parse, thanks to serde(default).
        let c: Config = toml::from_str("").unwrap();
        assert_eq!(c.sort(), Sort::default());
        // A sort string that means nothing must not panic or persist.
        let c: Config = toml::from_str(r#"sort_list = "garbage""#).unwrap();
        assert_eq!(c.sort(), Sort::default());
    }
}
