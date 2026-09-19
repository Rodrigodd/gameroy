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

use crate::input::KeyMap;
use crate::library::Sort;

const FILE_NAME: &str = "gameroy-slint.toml";

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    /// Let the emulator predict interrupts. An accuracy/speed trade-off the old
    /// front-end exposed and this one did not.
    pub interrupt_prediction: bool,
    /// Start with the frame limiter off, i.e. as fast as the host can manage.
    pub frame_skip: bool,
    /// A 256-byte boot ROM to run before the cartridge. Without one the machine
    /// starts in its post-boot state.
    pub boot_rom: Option<String>,
    /// Initial window size, as `"WIDTHxHEIGHT"`.
    pub screen_size: Option<String>,
    /// Keyboard bindings.
    pub keymap: KeyMap,
}

/// Parses the `"WIDTHxHEIGHT"` form the old config used.
pub fn parse_screen_size(text: &str) -> Option<(u32, u32)> {
    let (w, h) = text.split_once(['x', 'X'])?;
    let (w, h) = (w.trim().parse().ok()?, h.trim().parse().ok()?);
    // A zero-sized window is not a size; treat it as a malformed value rather
    // than handing it to the windowing system.
    (w > 0 && h > 0).then_some((w, h))
}

impl Default for Config {
    /// Matches `with_defaults`, so a config file that omits a field means the
    /// same thing as no config file at all. Deriving this instead would default
    /// every bool to false and quietly turn off rewinding and the recompiler.
    fn default() -> Self {
        Self {
            rom_folder: None,
            sort_list: None,
            only_integer_scaling: false,
            rewinding: true,
            start_in_debug: false,
            jit: true,
            interrupt_prediction: true,
            frame_skip: false,
            boot_rom: None,
            screen_size: None,
            keymap: KeyMap::default(),
        }
    }
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
        Self::default()
    }

    /// The boot ROM's bytes, if one is configured and readable.
    ///
    /// Reading it is this side's job; the server only ever sees the bytes.
    pub fn read_boot_rom(&self) -> Option<Vec<u8>> {
        let path = self.boot_rom.as_deref()?;
        match std::fs::read(path) {
            Ok(bytes) if bytes.len() == 256 => Some(bytes),
            Ok(bytes) => {
                log::warn!(
                    "ignoring boot ROM {path}: {} bytes, expected 256",
                    bytes.len()
                );
                None
            }
            Err(e) => {
                log::warn!("ignoring boot ROM {path}: {e}");
                None
            }
        }
    }

    pub fn window_size(&self) -> Option<(u32, u32)> {
        let text = self.screen_size.as_deref()?;
        match parse_screen_size(text) {
            Some(size) => Some(size),
            None => {
                log::warn!("ignoring malformed screen_size {text:?}");
                None
            }
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
pub fn base_folder() -> PathBuf {
    if let Some(dir) = option_env!("CARGO_WORKSPACE_DIR")
        && !dir.is_empty()
    {
        return PathBuf::from(dir);
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
            ..Config::default()
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
    fn an_omitted_field_means_what_no_file_means() {
        // Deriving Default would make every bool false here, silently disabling
        // rewinding and the recompiler for anyone with an older config file.
        let c: Config = toml::from_str(r#"rom_folder = "/roms""#).unwrap();
        assert!(c.rewinding);
        assert!(c.jit);
        assert!(c.interrupt_prediction);
        assert_eq!(c.keymap, KeyMap::default());
    }

    #[test]
    fn parses_a_screen_size_and_rejects_nonsense() {
        assert_eq!(parse_screen_size("768x400"), Some((768, 400)));
        assert_eq!(parse_screen_size(" 640 X 480 "), Some((640, 480)));
        assert_eq!(parse_screen_size("768"), None);
        assert_eq!(parse_screen_size("0x400"), None);
        assert_eq!(parse_screen_size("widexhigh"), None);
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
