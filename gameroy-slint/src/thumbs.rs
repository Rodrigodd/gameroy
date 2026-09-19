//! Box art for the ROM list.
//!
//! Filesystem and network only, no Slint: a `slint::Image` can only be built on
//! the UI thread, so the worker gets as far as decoded pixels and hands those
//! over.
//!
//! The source is the libretro thumbnail server, as in the old front-end, but over
//! https — its `FIXME` about not knowing whether to use TLS is settled here.

use std::io::Read;
use std::path::{Path, PathBuf};

use image::ImageEncoder;
use image::codecs::png::PngEncoder;

const LIBRETRO_THUMBNAILS: &str =
    "https://thumbnails.libretro.com/Nintendo%20-%20Game%20Boy/Named_Boxarts/";

/// Cached thumbnails are square and small; the list draws them at 48 logical px,
/// so 96 covers a 2x display without storing full box art.
const THUMB_SIZE: u32 = 96;

/// A download that hangs must not keep a worker thread alive forever.
const TIMEOUT: std::time::Duration = std::time::Duration::from_secs(10);

/// Refuses a response too large to be box art, so a wrong URL cannot exhaust
/// memory. Real thumbnails are tens of kilobytes.
const MAX_DOWNLOAD: u64 = 4 * 1024 * 1024;

/// PNG bytes for `stem`: the local cache first, then the network.
pub fn fetch(stem: &str) -> Result<Vec<u8>, String> {
    match load_cached(stem) {
        Ok(Some(bytes)) => return Ok(bytes),
        Ok(None) => {}
        Err(e) => log::warn!("ignoring unreadable thumbnail cache for {stem:?}: {e}"),
    }

    let png = normalize(&download(stem)?)?;
    if let Err(e) = store(stem, &png) {
        // A cache we cannot write is a slow list, not a broken one.
        log::warn!("could not cache thumbnail for {stem:?}: {e}");
    }
    Ok(png)
}

/// Where `stem`'s cached thumbnail lives.
///
/// The stem goes in verbatim, spaces and all, because that is what the old
/// front-end wrote and an existing `thumbnails/` folder should keep working.
pub fn cache_path(stem: &str) -> PathBuf {
    crate::config::base_folder()
        .join("thumbnails")
        .join(stem)
        .with_extension("png")
}

fn load_cached(stem: &str) -> Result<Option<Vec<u8>>, std::io::Error> {
    match std::fs::read(cache_path(stem)) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

fn store(stem: &str, png: &[u8]) -> std::io::Result<()> {
    let path = cache_path(stem);
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)?;
    }
    // Temp file and rename, so an interrupted write cannot leave a truncated PNG
    // that every later run then treats as a valid cache hit.
    let tmp = path.with_extension("png.tmp");
    std::fs::write(&tmp, png)?;
    std::fs::rename(&tmp, &path)
}

fn download(stem: &str) -> Result<Vec<u8>, String> {
    let url = format!("{LIBRETRO_THUMBNAILS}{}.png", urlencode(stem));
    log::debug!("GET {url}");

    let agent = ureq::AgentBuilder::new()
        .timeout_connect(TIMEOUT)
        .timeout_read(TIMEOUT)
        .build();
    let response = match agent.get(&url).call() {
        Ok(r) => r,
        Err(ureq::Error::Status(code, _)) => return Err(format!("server returned {code}")),
        Err(ureq::Error::Transport(e)) => return Err(format!("network error: {e}")),
    };

    let mut bytes = Vec::new();
    response
        .into_reader()
        .take(MAX_DOWNLOAD)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("could not read the response: {e}"))?;
    Ok(bytes)
}

/// Percent-encodes the characters a ROM file name can legally contain that a URL
/// path cannot carry as-is. Names like `Zelda (USA, Europe)` are the norm here.
fn urlencode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for byte in s.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(byte as char)
            }
            other => out.push_str(&format!("%{other:02X}")),
        }
    }
    out
}

/// Decodes whatever the server sent, squares it off at [`THUMB_SIZE`] and
/// re-encodes it as the PNG that goes in the cache.
pub fn normalize(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let image = image::load_from_memory(bytes).map_err(|e| format!("not an image: {e}"))?;
    let resized = image::imageops::resize(
        &image,
        THUMB_SIZE,
        THUMB_SIZE,
        image::imageops::FilterType::Lanczos3,
    );

    let mut out = Vec::new();
    PngEncoder::new(&mut out)
        .write_image(
            resized.as_raw(),
            THUMB_SIZE,
            THUMB_SIZE,
            image::ExtendedColorType::Rgba8,
        )
        .map_err(|e| format!("could not encode the thumbnail: {e}"))?;
    Ok(out)
}

/// PNG bytes to `(width, height, rgba)`, ready for a `SharedPixelBuffer`.
///
/// Decoding happens on the worker rather than the UI thread; only the wrapping
/// has to happen where Slint lives.
pub fn decode_rgba(png: &[u8]) -> Result<(u32, u32, Vec<u8>), String> {
    let image = image::load_from_memory(png)
        .map_err(|e| format!("could not decode the thumbnail: {e}"))?
        .to_rgba8();
    Ok((image.width(), image.height(), image.into_raw()))
}

/// The name a thumbnail is filed under: the ROM's file name without `.gb`.
pub fn stem_of(path: &Path) -> Option<String> {
    Some(path.file_stem()?.to_string_lossy().into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalizes_to_a_square_png() {
        let source = image::RgbaImage::from_pixel(200, 120, image::Rgba([10, 20, 30, 255]));
        let mut png = Vec::new();
        PngEncoder::new(&mut png)
            .write_image(source.as_raw(), 200, 120, image::ExtendedColorType::Rgba8)
            .unwrap();

        let (w, h, _) = decode_rgba(&normalize(&png).unwrap()).unwrap();
        assert_eq!((w, h), (THUMB_SIZE, THUMB_SIZE));
    }

    #[test]
    fn rejects_bytes_that_are_not_an_image() {
        assert!(normalize(b"this is not a png").is_err());
        assert!(decode_rgba(b"nor is this").is_err());
    }

    #[test]
    fn cache_path_keeps_the_name_verbatim() {
        // Spaces, commas and parentheses are ordinary in a ROM file name, and the
        // old front-end's cache files have them, so they must not be mangled.
        let name = "Legend of Zelda, The - Link's Awakening (USA, Europe)";
        let path = cache_path(name);
        assert_eq!(path.extension().unwrap(), "png");
        assert_eq!(path.file_stem().unwrap(), name);
        assert_eq!(path.parent().unwrap().file_name().unwrap(), "thumbnails");
    }

    #[test]
    fn url_encodes_what_a_path_cannot_carry() {
        assert_eq!(urlencode("Tetris"), "Tetris");
        assert_eq!(
            urlencode("Zelda (USA, Europe)"),
            "Zelda%20%28USA%2C%20Europe%29"
        );
        assert_eq!(urlencode("Link's"), "Link%27s");
    }

    #[test]
    fn stem_strips_the_extension() {
        assert_eq!(
            stem_of(Path::new("/roms/Tetris (World).gb")).as_deref(),
            Some("Tetris (World)")
        );
    }
}
