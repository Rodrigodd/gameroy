//! The ROM library: scanning a folder, reading headers, sorting.
//!
//! This is all front-end work. Scanning is filesystem access, which the server
//! does not do, and a cartridge header is 16 bytes at a fixed offset — shipping a
//! whole ROM across the boundary to have it parsed would be absurd. So the few
//! header fields the list needs are read here.

use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Title, as 15 bytes at 0x134. (0x143 is the CGB flag on later carts, so it is
/// excluded rather than risk rendering it as a character.)
const TITLE_OFFSET: usize = 0x134;
const TITLE_LEN: usize = 15;
/// ROM size code at 0x148: the size is 32 KiB << code.
const ROM_SIZE_OFFSET: usize = 0x148;
/// The smallest cartridge; anything shorter is not a ROM.
const MIN_ROM_LEN: usize = 0x8000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RomEntry {
    pub path: PathBuf,
    pub file_name: String,
    /// `None` when the header could not be read.
    pub title: Option<String>,
    pub size: Option<u64>,
    /// Modification time of the `.sav` file, i.e. when it was last played.
    pub last_played: Option<SystemTime>,
}

impl RomEntry {
    pub fn read(path: &Path) -> Self {
        let file_name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();

        let header = read_header(path);
        let last_played = path
            .with_extension("sav")
            .metadata()
            .and_then(|m| m.modified())
            .ok();

        Self {
            path: path.to_path_buf(),
            file_name,
            title: header.as_ref().map(|h| h.title.clone()),
            size: header.as_ref().map(|h| h.rom_size),
            last_played,
        }
    }

    pub fn size_text(&self) -> String {
        match self.size {
            None => "-".into(),
            Some(bytes) => format_size(bytes),
        }
    }

    pub fn last_played_text(&self) -> String {
        match self.last_played {
            None => "-".into(),
            Some(t) => format_age(t.elapsed().unwrap_or_default().as_secs()),
        }
    }

    pub fn title_text(&self) -> String {
        match &self.title {
            Some(t) if !t.is_empty() => t.clone(),
            Some(_) => "-".into(),
            None => "unreadable".into(),
        }
    }
}

struct Header {
    title: String,
    rom_size: u64,
}

fn read_header(path: &Path) -> Option<Header> {
    let bytes = std::fs::read(path).ok()?;
    if bytes.len() < MIN_ROM_LEN {
        return None;
    }
    let raw = &bytes[TITLE_OFFSET..TITLE_OFFSET + TITLE_LEN];
    let title = raw
        .iter()
        .take_while(|&&b| b != 0)
        .filter(|&&b| b.is_ascii_graphic() || b == b' ')
        .map(|&b| b as char)
        .collect::<String>()
        .trim()
        .to_string();
    let rom_size = 32 * 1024u64 << bytes[ROM_SIZE_OFFSET];
    Some(Header { title, rom_size })
}

fn format_size(bytes: u64) -> String {
    const KIB: u64 = 1024;
    const MIB: u64 = 1024 * KIB;
    if bytes >= MIB {
        let mib = bytes as f64 / MIB as f64;
        // 1.5 MiB reads better than 2 MiB when it is really 1.5.
        if (mib.fract()).abs() < 0.05 {
            format!("{mib:.0} MiB")
        } else {
            format!("{mib:.1} MiB")
        }
    } else {
        format!("{} KiB", bytes / KIB)
    }
}

/// Relative time, as the old front-end rendered it.
///
/// The old implementation had two bugs this does not: a `"{}s agi"` typo, and
/// minutes and months both rendering as `"{}m ago"`.
fn format_age(secs: u64) -> String {
    const MINUTE: u64 = 60;
    const HOUR: u64 = 60 * MINUTE;
    const DAY: u64 = 24 * HOUR;
    const MONTH: u64 = 30 * DAY;
    const YEAR: u64 = 365 * DAY;
    match secs {
        0..=4 => "Just Now".into(),
        s if s < MINUTE => format!("{s}s ago"),
        s if s < HOUR => format!("{}min ago", s / MINUTE),
        s if s < DAY => format!("{}h ago", s / HOUR),
        s if s < MONTH => format!("{}d ago", s / DAY),
        s if s < YEAR => format!("{}mo ago", s / MONTH),
        s => format!("{}y ago", s / YEAR),
    }
}

/// Which column the list is ordered by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortColumn {
    File,
    Title,
    Size,
    LastPlayed,
}

impl SortColumn {
    pub fn from_index(i: i32) -> Option<Self> {
        Some(match i {
            0 => Self::File,
            1 => Self::Title,
            2 => Self::Size,
            3 => Self::LastPlayed,
            _ => return None,
        })
    }

    pub fn index(self) -> i32 {
        match self {
            Self::File => 0,
            Self::Title => 1,
            Self::Size => 2,
            Self::LastPlayed => 3,
        }
    }

    fn tag(self) -> &'static str {
        match self {
            Self::File => "File",
            Self::Title => "Header Name",
            Self::Size => "Size",
            Self::LastPlayed => "Last played",
        }
    }

    fn from_tag(tag: &str) -> Option<Self> {
        Some(match tag {
            "File" => Self::File,
            "Header Name" => Self::Title,
            "Size" => Self::Size,
            "Last played" => Self::LastPlayed,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Sort {
    pub column: SortColumn,
    pub ascending: bool,
}

impl Default for Sort {
    fn default() -> Self {
        Self {
            column: SortColumn::File,
            ascending: true,
        }
    }
}

impl Sort {
    /// The old config format, e.g. `"+File"` / `"-Size"`. Kept so an existing
    /// gameroy.toml's sort choice still means something to a reader.
    pub fn to_config(self) -> String {
        format!(
            "{}{}",
            if self.ascending { '+' } else { '-' },
            self.column.tag()
        )
    }

    pub fn from_config(s: &str) -> Option<Self> {
        let (sign, tag) = s.split_at_checked(1)?;
        let ascending = match sign {
            "+" => true,
            "-" => false,
            _ => return None,
        };
        Some(Self {
            column: SortColumn::from_tag(tag)?,
            ascending,
        })
    }
}

/// Lists `.gb` files in a folder. Not recursive, matching the old behaviour.
pub fn scan(folder: &Path) -> std::io::Result<Vec<RomEntry>> {
    let mut entries = Vec::new();
    for dir_entry in std::fs::read_dir(folder)? {
        let path = match dir_entry {
            Ok(e) => e.path(),
            Err(e) => {
                log::warn!("skipping unreadable directory entry: {e}");
                continue;
            }
        };
        if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("gb"))
        {
            entries.push(RomEntry::read(&path));
        }
    }
    Ok(entries)
}

pub fn sort_entries(entries: &mut [RomEntry], sort: Sort) {
    use std::cmp::Ordering;
    entries.sort_by(|a, b| {
        // Never-played entries sort last in *both* directions: that is absent
        // data, not "oldest", so it must not be swept to the top by reversing.
        if sort.column == SortColumn::LastPlayed {
            match (a.last_played.is_some(), b.last_played.is_some()) {
                (true, false) => return Ordering::Less,
                (false, true) => return Ordering::Greater,
                _ => {}
            }
        }

        let ord = match sort.column {
            SortColumn::File => a.file_name.to_lowercase().cmp(&b.file_name.to_lowercase()),
            SortColumn::Title => a.title.cmp(&b.title),
            SortColumn::Size => a.size.cmp(&b.size),
            // Most recently played first when ascending.
            SortColumn::LastPlayed => b.last_played.cmp(&a.last_played),
        };
        let ord = if sort.ascending { ord } else { ord.reverse() };
        // Filename breaks ties, always ascending, so the order is stable.
        ord.then_with(|| a.file_name.cmp(&b.file_name))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_sizes() {
        assert_eq!(format_size(32 * 1024), "32 KiB");
        assert_eq!(format_size(512 * 1024), "512 KiB");
        assert_eq!(format_size(1024 * 1024), "1 MiB");
        assert_eq!(format_size(1536 * 1024), "1.5 MiB");
    }

    #[test]
    fn formats_ages_without_the_old_minute_month_collision() {
        assert_eq!(format_age(0), "Just Now");
        assert_eq!(format_age(30), "30s ago");
        assert_eq!(format_age(5 * 60), "5min ago");
        assert_eq!(format_age(3 * 3600), "3h ago");
        assert_eq!(format_age(2 * 86400), "2d ago");
        // Distinct suffixes: the old code rendered both of these as "{}m ago".
        assert_ne!(format_age(5 * 60), format_age(60 * 86400));
        assert_eq!(format_age(60 * 86400), "2mo ago");
        assert_eq!(format_age(800 * 86400), "2y ago");
    }

    #[test]
    fn sort_config_round_trips() {
        for column in [
            SortColumn::File,
            SortColumn::Title,
            SortColumn::Size,
            SortColumn::LastPlayed,
        ] {
            for ascending in [true, false] {
                let sort = Sort { column, ascending };
                let text = sort.to_config();
                assert_eq!(Sort::from_config(&text), Some(sort), "{text}");
            }
        }
        assert_eq!(Sort::from_config("nonsense"), None);
        assert_eq!(Sort::from_config(""), None);
    }

    fn entry(name: &str, size: Option<u64>) -> RomEntry {
        RomEntry {
            path: PathBuf::from(name),
            file_name: name.into(),
            title: Some(name.to_uppercase()),
            size,
            last_played: None,
        }
    }

    #[test]
    fn sorts_and_reverses() {
        let mut v = vec![
            entry("c.gb", Some(3)),
            entry("a.gb", Some(1)),
            entry("b.gb", Some(2)),
        ];
        sort_entries(
            &mut v,
            Sort {
                column: SortColumn::File,
                ascending: true,
            },
        );
        assert_eq!(
            v.iter().map(|e| &*e.file_name).collect::<Vec<_>>(),
            ["a.gb", "b.gb", "c.gb"]
        );

        sort_entries(
            &mut v,
            Sort {
                column: SortColumn::Size,
                ascending: false,
            },
        );
        assert_eq!(
            v.iter().map(|e| e.size).collect::<Vec<_>>(),
            [Some(3), Some(2), Some(1)]
        );
    }

    #[test]
    fn never_played_entries_sort_last_in_both_directions() {
        let now = SystemTime::now();
        let mut v = vec![
            entry("never.gb", Some(1)),
            RomEntry {
                last_played: Some(now),
                ..entry("played.gb", Some(1))
            },
        ];
        for ascending in [true, false] {
            sort_entries(
                &mut v,
                Sort {
                    column: SortColumn::LastPlayed,
                    ascending,
                },
            );
            let last = &v.last().unwrap().file_name;
            assert_eq!(last, "never.gb", "ascending={ascending}");
        }
    }
}
