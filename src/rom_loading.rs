use std::io::Read;

use gameroy::gameboy::{cartridge::Cartridge, GameBoy};
use image::codecs::png::PngEncoder;
use image::ImageEncoder;

use crate::config::{config, normalize_config_path};

cfg_if::cfg_if! {
    if #[cfg(target_os = "android")] {
        mod android;
        pub use android::*;
    } else if #[cfg(target_arch = "wasm32")] {
        mod wasm;
        pub use wasm::*;
    } else {
        mod native;
        pub use native::*;
    }
}

pub fn load_gameboy(rom: Vec<u8>, ram: Option<Vec<u8>>) -> Result<Box<GameBoy>, String> {
    let boot_rom = load_boot_rom();

    let mut cartridge = Cartridge::new(rom)?;
    log::info!("Cartridge type: {}", cartridge.kind_name());

    if let Some(ram) = ram {
        cartridge.ram = ram;
    }

    let mut game_boy = GameBoy::new(boot_rom, cartridge);
    game_boy.predict_interrupt = config().interrupt_prediction;
    {
        let mut trace = game_boy.trace.borrow_mut();

        let banks = game_boy.cartridge.curr_bank();
        trace.trace_starting_at(&game_boy, banks, 0x100, Some("entry point".into()));
        trace.trace_starting_at(&game_boy, banks, 0x40, Some("RST_0x40".into()));
        trace.trace_starting_at(&game_boy, banks, 0x48, Some("RST_0x48".into()));
        trace.trace_starting_at(&game_boy, banks, 0x50, Some("RST_0x50".into()));
        trace.trace_starting_at(&game_boy, banks, 0x58, Some("RST_0x58".into()));
        trace.trace_starting_at(&game_boy, banks, 0x60, Some("RST_0x60".into()));
    }
    // GameBoy is too big to live on the stack.
    Ok(Box::new(game_boy))
}

/// Returns a PNG encoded image.
pub fn get_thumb(file_name: &str) -> Result<Vec<u8>, String> {
    match load_thumb(file_name) {
        Ok(image) => {
            log::info!("loading thumb from file");
            return Ok(image);
        }
        Err(err) => {
            if !err.is_empty() {
                log::error!("error loading thumbnail for '{file_name}' from file: {err}");
            }
        }
    }

    // FIXME: check if I should use https, and enable reqwest features for that.
    const LIBRETRO_THUMBNAILS: &str =
        "http://thumbnails.libretro.com/Nintendo%20-%20Game%20Boy/Named_Boxarts/";

    let url = LIBRETRO_THUMBNAILS.to_string() + file_name + ".png";

    log::info!("GET {url}");
    let res = ureq::get(&url).call();
    let res = match res {
        Ok(res) => res,
        Err(ureq::Error::Status(code, _)) => {
            return Err(format!("server return error code: {code}"))
        }
        Err(ureq::Error::Transport(x)) => return Err(format!("io error: {x}")),
    };

    let mut bytes = Vec::new();
    res.into_reader().read_to_end(&mut bytes).unwrap();

    let image = image::load_from_memory(&bytes).unwrap();
    let image = image::imageops::resize(&image, 96, 96, image::imageops::FilterType::Lanczos3);

    let mut buffer = Vec::new();
    PngEncoder::new(&mut buffer)
        .write_image(
            image.as_raw().as_slice(),
            image.width(),
            image.height(),
            image::ExtendedColorType::Rgba8,
        )
        .map_err(|err| format!("failed to png encode thumbnail: {}", err))?;

    if let Err(err) = crate::rom_loading::save_thumb(&buffer, file_name) {
        log::error!("failed to save thumbnail for '{file_name}': {err}");
    }

    Ok(buffer)
}

/// Returns a PNG encoded image.
pub fn load_thumb(file_name: &str) -> Result<Vec<u8>, String> {
    let thumbs_folder = normalize_config_path("thumbnails");
    let save_path = thumbs_folder.join(file_name).with_extension("png");

    let mut file = match std::fs::File::open(save_path) {
        Ok(file) => file,
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotFound => return Err(String::new()),
            _ => return Err(format!("error opening thumpnail file: {err}")),
        },
    };

    let mut buf = Vec::new();
    file.read_to_end(&mut buf)
        .map_err(|err| format!("failed reading file: {err}"))?;
    Ok(buf)
}

/// Receives a PNG encoded image
pub fn save_thumb(thumb: &Vec<u8>, file_name: &str) -> Result<(), String> {
    let thumbs_folder = normalize_config_path("thumbnails");
    let save_path = thumbs_folder.join(file_name).with_extension("png");

    log::debug!("save thumbnail path: {}", save_path.display());
    if let Err(err) = std::fs::create_dir(&thumbs_folder) {
        match err.kind() {
            std::io::ErrorKind::AlreadyExists => {}
            _ => {
                return Err(format!("failed to create thumbnails folder: {err}"));
            }
        }
    }
    std::fs::write(save_path, thumb).map_err(|x| x.to_string())
}
