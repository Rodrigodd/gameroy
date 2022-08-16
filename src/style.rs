use std::{collections::HashMap, rc::Rc};

use giui::{
    font::Fonts,
    graphics::{Graphic, TextStyle},
    style::{ButtonStyle, TabStyle, TextFieldStyle},
    style_loader::load_style,
};
use sprite_render::SpriteRender;

use crate::widget::fold_view::FoldIcon;

pub struct Loader<'a> {
    pub fonts: &'a mut Fonts,
    pub render: &'a mut (dyn SpriteRender + 'a),
    pub textures: HashMap<String, (u32, u32, u32)>,
}

#[cfg(not(feature = "static"))]
mod loaded_files {
    use giui::{font::Font, style_loader::StyleLoaderCallback};

    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, name: String) -> (u32, u32, u32) {
            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let path = format!("assets/{}", name);
            let data = match image::open(&path) {
                Ok(x) => x,
                Err(_) => {
                    log::error!("not found texture in '{}'", path);
                    return (0, 0, 0);
                }
            };
            let data = data.to_rgba8();

            let texture = (
                self.render
                    .new_texture(data.width(), data.height(), data.as_ref(), true),
                data.width(),
                data.height(),
            );
            self.textures.insert(name, texture);
            texture
        }

        fn load_font(&mut self, name: String) -> giui::font::FontId {
            // load a font
            let path = "assets/".to_string() + &name;
            log::info!("load font: '{}'", path);
            let font_data = std::fs::read(path).unwrap();
            self.fonts.add(Font::new(&font_data))
        }
    }
}

#[cfg(feature = "static")]
mod static_files {
    use giui::{font::Font, style_loader::StyleLoaderCallback};

    pub struct StaticFiles {
        pub font: &'static [u8],
        pub style: &'static str,
        pub white_texture: &'static [u8],
        pub icons_texture: &'static [u8],
        pub gamepad: &'static [u8],
    }
    pub static FILES: StaticFiles = StaticFiles {
        font: include_bytes!("../assets/NotoSansMono.ttf"),
        style: include_str!("../assets/style.ron"),
        white_texture: include_bytes!("../assets/white.png"),
        icons_texture: include_bytes!("../assets/icons.png"),
        gamepad: include_bytes!("../assets/gamepad.png"),
    };
    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, name: String) -> (u32, u32, u32) {
            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let data = match name.as_str() {
                "white.png" => FILES.white_texture,
                "icons.png" => FILES.icons_texture,
                "gamepad.png" => FILES.gamepad,
                _ => panic!("unkown texture '{}'", name),
            };

            let data = match image::load_from_memory(data) {
                Ok(x) => x,
                Err(e) => {
                    log::error!("cannot load texture in '{}': {}", name, e);
                    return (0, 0, 0);
                }
            };

            let data = data.to_rgba8();

            let texture = (
                self.render
                    .new_texture(data.width(), data.height(), data.as_ref(), true),
                data.width(),
                data.height(),
            );
            self.textures.insert(name, texture);
            texture
        }

        fn load_font(&mut self, name: String) -> giui::font::FontId {
            // load a font
            log::info!("load font: '{}'", name);
            let font_data = match name.as_str() {
                "NotoSansMono.ttf" => FILES.font,
                _ => panic!("unknown font '{}'", name),
            };
            self.fonts.add(Font::new(font_data))
        }
    }
}

#[derive(LoadStyle, Clone)]
pub struct GamePad {
    pub cross: Graphic,
    pub start: Graphic,
    pub select: Graphic,
    pub a: Graphic,
    pub b: Graphic,
    pub ab: Graphic,
}

#[derive(LoadStyle, Clone)]
pub struct Style {
    pub text_style: TextStyle,
    pub text_menu: TextStyle,
    pub blocker: Graphic,
    pub split_background: Graphic,
    pub terminal_background: Graphic,
    pub terminal_text_style: TextStyle,
    pub background: Graphic,
    pub header_background: Graphic,
    pub text_field: Rc<TextFieldStyle>,
    pub scrollbar: Rc<ButtonStyle>,
    pub delete_button: Rc<ButtonStyle>,
    pub tab_style: Rc<TabStyle>,
    pub fold_icon: FoldIcon,
    pub delete_icon: Graphic,
    pub open_icon: Graphic,
    pub gamepad: GamePad,
}
impl Style {
    pub fn load(fonts: &mut Fonts, render: &mut dyn SpriteRender) -> Option<Self> {
        let loader = Loader {
            fonts,
            render,
            textures: HashMap::default(),
        };

        #[cfg(not(feature = "static"))]
        let file = std::fs::read_to_string("assets/style.ron").unwrap();
        #[cfg(feature = "static")]
        let file = static_files::FILES.style;

        let mut deser = ron::Deserializer::from_str(&file).unwrap();
        let style: Result<Self, _> = load_style(&mut deser, loader);

        Some(style.unwrap())
    }
}
