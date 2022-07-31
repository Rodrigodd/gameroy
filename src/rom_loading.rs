use gameroy::gameboy::cartridge::Cartridge;
use gameroy::gameboy::GameBoy;

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

pub fn load_gameboy(rom: Vec<u8>, ram: Option<Vec<u8>>) -> Result<GameBoy, String> {
    let boot_rom = load_boot_rom();

    let mut cartridge = Cartridge::new(rom)?;
    log::info!("Cartridge type: {}", cartridge.kind_name());

    if let Some(ram) = ram {
        cartridge.ram = ram;
    }

    let game_boy = GameBoy::new(boot_rom, cartridge);
    {
        let mut trace = game_boy.trace.borrow_mut();

        trace.trace_starting_at(&game_boy, 0, 0x100, Some("entry point".into()));
        trace.trace_starting_at(&game_boy, 0, 0x40, Some("RST_0x40".into()));
        trace.trace_starting_at(&game_boy, 0, 0x48, Some("RST_0x48".into()));
        trace.trace_starting_at(&game_boy, 0, 0x50, Some("RST_0x50".into()));
        trace.trace_starting_at(&game_boy, 0, 0x58, Some("RST_0x58".into()));
        trace.trace_starting_at(&game_boy, 0, 0x60, Some("RST_0x60".into()));
    }
    Ok(game_boy)
}
