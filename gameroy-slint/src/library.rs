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
const MIN_ROM_LEN: u64 = 0x8000;
/// Everything the header needs sits below this, so this is all that is read.
const HEADER_LEN: usize = 0x150;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RomEntry {
    pub path: PathBuf,
    pub file_name: String,
    /// `None` when the header could not be read.
    pub title: Option<String>,
    pub size: Option<u64>,
    /// Modification time of the `.sav` file, i.e. when it was last played.
    pub last_played: Option<SystemTime>,
    /// No worker has reported on this entry yet, so `title` and `size` being
    /// `None` means "not looked at" rather than "unreadable". The two render
    /// differently, and conflating them is how the old list showed "Loading..."
    /// forever on a corrupt file.
    pub loading: bool,
}

impl RomEntry {
    /// An entry with nothing but its path, as it appears the instant the folder
    /// is listed. The worker fills the rest in.
    pub fn pending(path: PathBuf) -> Self {
        let file_name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        Self {
            path,
            file_name,
            title: None,
            size: None,
            last_played: None,
            loading: true,
        }
    }

    /// Folds in what a worker read off the UI thread.
    pub fn apply(&mut self, header: Option<Header>, last_played: Option<SystemTime>) {
        self.title = header.as_ref().map(|h| h.title.clone());
        self.size = header.as_ref().map(|h| h.rom_size);
        self.last_played = last_played;
        self.loading = false;
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
        if self.loading {
            return "Loading...".into();
        }
        match &self.title {
            Some(t) if !t.is_empty() => t.clone(),
            Some(_) => "-".into(),
            None => "unreadable".into(),
        }
    }
}

pub struct Header {
    pub title: String,
    pub rom_size: u64,
}

/// Reads the cartridge header, and only the cartridge header.
///
/// This used to `std::fs::read` the whole file -- up to 8 MiB per entry -- to
/// look at 16 bytes of it. The length check moved to the file's metadata for the
/// same reason.
pub fn read_header(path: &Path) -> Option<Header> {
    use std::io::Read;

    let mut file = std::fs::File::open(path).ok()?;
    if file.metadata().ok()?.len() < MIN_ROM_LEN {
        return None;
    }
    let mut bytes = [0u8; HEADER_LEN];
    file.read_exact(&mut bytes).ok()?;

    let raw = &bytes[TITLE_OFFSET..TITLE_OFFSET + TITLE_LEN];
    let title = raw
        .iter()
        .take_while(|&&b| b != 0)
        .filter(|&&b| b.is_ascii_graphic() || b == b' ')
        .map(|&b| b as char)
        .collect::<String>()
        .trim()
        .to_string();
    let rom_size = (32 * 1024u64) << bytes[ROM_SIZE_OFFSET];
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
///
/// Opens nothing, so it is cheap enough to stay on the UI thread and let the
/// table appear at once; the headers and thumbnails follow from a worker.
pub fn list_roms(folder: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    for dir_entry in std::fs::read_dir(folder)? {
        let path = match dir_entry {
            Ok(e) => e.path(),
            Err(e) => {
                log::warn!("skipping unreadable directory entry: {e}");
                continue;
            }
        };
        if crate::storage::is_rom_path(&path) {
            paths.push(path);
        }
    }
    Ok(paths)
}

/// When the ROM was last played, from its save file's mtime.
pub fn last_played_of(path: &Path) -> Option<SystemTime> {
    path.with_extension("sav").metadata().ok()?.modified().ok()
}

/// The display order of `entries`, as indices into it.
///
/// A permutation rather than an in-place sort because the loader addresses
/// entries by their scan index: re-sorting mid-load must not make an in-flight
/// header land on somebody else's row.
pub fn sort_order(entries: &[RomEntry], sort: Sort) -> Vec<usize> {
    use std::cmp::Ordering;
    let mut order: Vec<usize> = (0..entries.len()).collect();
    order.sort_by(|&ia, &ib| {
        let (a, b) = (&entries[ia], &entries[ib]);
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
    order
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
            title: Some(name.to_uppercase()),
            size,
            loading: false,
            ..RomEntry::pending(PathBuf::from(name))
        }
    }

    /// The names in display order, which is what the permutation is for.
    fn ordered(entries: &[RomEntry], sort: Sort) -> Vec<&str> {
        sort_order(entries, sort)
            .into_iter()
            .map(|i| &*entries[i].file_name)
            .collect()
    }

    #[test]
    fn sorts_and_reverses() {
        let v = vec![
            entry("c.gb", Some(3)),
            entry("a.gb", Some(1)),
            entry("b.gb", Some(2)),
        ];
        assert_eq!(
            ordered(
                &v,
                Sort {
                    column: SortColumn::File,
                    ascending: true
                }
            ),
            ["a.gb", "b.gb", "c.gb"]
        );
        assert_eq!(
            ordered(
                &v,
                Sort {
                    column: SortColumn::Size,
                    ascending: false
                }
            ),
            ["c.gb", "b.gb", "a.gb"]
        );
    }

    /// The whole point of returning a permutation: `entries` keeps its scan
    /// order, so an index handed to the loader stays valid across a re-sort.
    #[test]
    fn sorting_leaves_the_entries_where_they_were() {
        let v = vec![entry("c.gb", Some(3)), entry("a.gb", Some(1))];
        let before: Vec<&str> = v.iter().map(|e| &*e.file_name).collect();
        let _ = sort_order(
            &v,
            Sort {
                column: SortColumn::File,
                ascending: true,
            },
        );
        let after: Vec<&str> = v.iter().map(|e| &*e.file_name).collect();
        assert_eq!(before, after);
    }

    #[test]
    fn never_played_entries_sort_last_in_both_directions() {
        let v = vec![
            entry("never.gb", Some(1)),
            RomEntry {
                last_played: Some(SystemTime::now()),
                ..entry("played.gb", Some(1))
            },
        ];
        for ascending in [true, false] {
            let names = ordered(
                &v,
                Sort {
                    column: SortColumn::LastPlayed,
                    ascending,
                },
            );
            assert_eq!(names.last(), Some(&"never.gb"), "ascending={ascending}");
        }
    }

    /// A pending row must not read as a broken one; the old list conflated them.
    #[test]
    fn a_pending_entry_reads_as_loading() {
        let pending = RomEntry::pending(PathBuf::from("/roms/x.gb"));
        assert_eq!(pending.title_text(), "Loading...");
        assert_eq!(pending.size_text(), "-");

        let mut unreadable = pending.clone();
        unreadable.apply(None, None);
        assert_eq!(unreadable.title_text(), "unreadable");

        let mut good = pending;
        good.apply(
            Some(Header {
                title: "TETRIS".into(),
                rom_size: 32 * 1024,
            }),
            None,
        );
        assert_eq!(good.title_text(), "TETRIS");
        assert_eq!(good.size_text(), "32 KiB");
    }

    #[test]
    fn read_header_needs_a_whole_cartridge() {
        let dir = std::env::temp_dir().join("gameroy-slint-header-test");
        let _ = std::fs::create_dir_all(&dir);

        // Shorter than the smallest cartridge: rejected without reading on.
        let short = dir.join("short.gb");
        std::fs::write(&short, vec![0u8; 0x100]).unwrap();
        assert!(read_header(&short).is_none());

        let mut rom = vec![0u8; MIN_ROM_LEN as usize];
        rom[TITLE_OFFSET..TITLE_OFFSET + 6].copy_from_slice(b"TETRIS");
        rom[ROM_SIZE_OFFSET] = 1;
        let full = dir.join("full.gb");
        std::fs::write(&full, &rom).unwrap();
        let header = read_header(&full).expect("a full-size ROM has a header");
        assert_eq!(header.title, "TETRIS");
        assert_eq!(header.rom_size, 64 * 1024);

        let _ = std::fs::remove_dir_all(&dir);
    }
}
