use crui::text::Text;

use crate::style::Style;

pub fn create_rom_loading_ui(gui: &mut crui::Gui, style: &Style) {
    gui.create_control()
        .graphic(Text::new(
            "Drop the game rom file here to load it".to_string(),
            (0, 0),
            style.text_style.clone(),
        ))
        .build(gui);
}

