use std::{collections::HashMap, rc::Rc};

use giui::{
    font::Fonts,
    graphics::{Graphic, TextStyle},
    style::{ButtonStyle, TabStyle, TextFieldStyle},
    style_loader::load_style,
};
use sprite_render::SpriteRender;

#[cfg(not(feature = "static"))]
use crate::config;
use crate::widget::fold_view::FoldIcon;

/// Avaliable scales for bitmaps.
///
/// From Android's documentation: https://developer.android.com/training/multiscreen/screendensities#TaskProvideAltBmp
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum ScaleFactor {
    /// 0.75x scale
    Mdpi,
    /// 1x scale
    Hdpi,
    /// 1.5x scale
    Xhdpi,
    /// 2x scale
    Xxhdpi,
    /// 3x scale
    Xxxhdpi,
    /// 4x scale
    Xxxxhdpi,
}
impl ScaleFactor {
    fn to_float(self) -> f32 {
        match self {
            ScaleFactor::Mdpi => 0.75,
            ScaleFactor::Hdpi => 1.0,
            ScaleFactor::Xhdpi => 1.5,
            ScaleFactor::Xxhdpi => 2.0,
            ScaleFactor::Xxxhdpi => 3.0,
            ScaleFactor::Xxxxhdpi => 4.0,
        }
    }

    fn from_float(scale_factor: f64) -> Self {
        if scale_factor <= 0.75 {
            Self::Mdpi
        } else if scale_factor <= 1.0 {
            Self::Hdpi
        } else if scale_factor <= 1.5 {
            Self::Xhdpi
        } else if scale_factor <= 2.0 {
            Self::Xxhdpi
        } else if scale_factor <= 3.0 {
            Self::Xxxhdpi
        } else {
            Self::Xxxxhdpi
        }
    }
}

struct Loader<'a> {
    pub fonts: &'a mut Fonts,
    pub render: &'a mut (dyn SpriteRender + 'a),
    pub textures: HashMap<String, (u32, u32, u32)>,
    pub scale_factor: ScaleFactor,
}

#[cfg(not(feature = "static"))]
mod loaded_files {
    use giui::{font::Font, graphics::Graphic, style_loader::StyleLoaderCallback};
    use image::{ImageBuffer, Rgba};
    use sprite_render::{Texture, TextureId};

    use super::config;

    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, mut name: String) -> (u32, u32, u32) {
            if name == "icons.png" {
                name = format!("icons{}x.png", self.scale_factor.to_float()).to_string();
            }

            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let data = 'data: {
                if name == "white.png" {
                    let mut image_buffer = ImageBuffer::new(1, 1);
                    image_buffer
                        .pixels_mut()
                        .for_each(|x| *x = Rgba::<u8>::from([255, 255, 255, 255]));
                    break 'data image_buffer;
                }

                let mut path = config::base_folder().unwrap();
                path.push("assets");
                path.push(&name);

                let data = match image::open(&path) {
                    Ok(x) => x,
                    Err(_) => {
                        log::error!("not found texture in '{}'", path.display());
                        return (0, 0, 0);
                    }
                };

                data.to_rgba8()
            };

            // FIXME: this is vunerable to hash collisions, and relies on how TextureId is used
            // internally
            let id = {
                let hash = super::hash(name.as_bytes());
                (hash & 0x7fff_ffff) as u32
            };

            let mut texture = (
                Texture::new(data.width(), data.height())
                    .id(TextureId(id))
                    .data(data.as_ref())
                    .create(self.render)
                    .unwrap()
                    .0,
                data.width(),
                data.height(),
            );

            // unscale texture width and height
            let scale_factor = self.scale_factor.to_float();
            texture.1 = (texture.1 as f32 / scale_factor).ceil() as u32;
            texture.2 = (texture.2 as f32 / scale_factor).ceil() as u32;

            self.textures.insert(name, texture);
            texture
        }

        fn modify_graphic(&mut self, graphic: &mut Graphic) {
            let scale_factor = self.scale_factor.to_float();
            match graphic {
                Graphic::Icon(icon) => {
                    icon.size = icon.size.map(|x| scale_factor * x);
                }
                Graphic::Panel(panel) => {
                    panel.border = panel.border.map(|x| scale_factor * x);
                }
                _ => (),
            }
        }

        fn load_font(&mut self, name: String) -> giui::font::FontId {
            // load a font
            let mut path = config::base_folder().unwrap();
            path.push("assets");
            path.push(&name);

            log::info!("load font: '{}'", path.display());
            let font_data = std::fs::read(path).unwrap();
            self.fonts.add(Font::new(&font_data))
        }
    }
}

#[cfg(feature = "static")]
mod static_files {
    use giui::{font::Font, graphics::Graphic, style_loader::StyleLoaderCallback};
    use image::{ImageBuffer, Rgba};
    use sprite_render::{Texture, TextureId};

    pub struct StaticFiles {
        pub font: &'static [u8],
        pub style: &'static str,
        pub icons_texture: &'static [&'static [u8]],
    }
    pub static FILES: StaticFiles = StaticFiles {
        font: include_bytes!("../assets/NotoSansMono.ttf"),
        style: include_str!("../assets/style.ron"),
        icons_texture: &[
            include_bytes!("../assets/icons0.75x.png"),
            include_bytes!("../assets/icons1x.png"),
            include_bytes!("../assets/icons1.5x.png"),
            include_bytes!("../assets/icons2x.png"),
            include_bytes!("../assets/icons3x.png"),
            include_bytes!("../assets/icons4x.png"),
        ],
    };
    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, name: String) -> (u32, u32, u32) {
            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let data = 'data: {
                let data = match name.as_str() {
                    "white.png" => {
                        let mut image_buffer = ImageBuffer::new(1, 1);
                        image_buffer
                            .pixels_mut()
                            .for_each(|x| *x = Rgba::<u8>::from([255, 255, 255, 255]));
                        break 'data image_buffer;
                    }
                    "icons.png" => FILES.icons_texture[self.scale_factor as usize],
                    _ => panic!("unkown texture '{}'", name),
                };

                let data = match image::load_from_memory(data) {
                    Ok(x) => x,
                    Err(e) => {
                        log::error!("cannot load texture in '{}': {}", name, e);
                        return (0, 0, 0);
                    }
                };

                data.to_rgba8()
            };

            // FIXME: this is vunerable to hash collisions, and relies on how TextureId is used
            // internally
            let id = {
                let hash = super::hash(name.as_bytes());
                (hash & 0x7fff_ffff) as u32
            };

            let mut texture = (
                Texture::new(data.width(), data.height())
                    .id(TextureId(id))
                    .data(data.as_ref())
                    .create(self.render)
                    .unwrap()
                    .0,
                data.width(),
                data.height(),
            );

            // unscale texture width and height
            let scale_factor = self.scale_factor.to_float();
            texture.1 = (texture.1 as f32 / scale_factor).ceil() as u32;
            texture.2 = (texture.2 as f32 / scale_factor).ceil() as u32;

            self.textures.insert(name, texture);
            texture
        }

        fn modify_graphic(&mut self, graphic: &mut Graphic) {
            let scale_factor = self.scale_factor.to_float();
            match graphic {
                Graphic::Icon(icon) => {
                    icon.size = icon.size.map(|x| scale_factor * x);
                }
                Graphic::Panel(panel) => {
                    panel.border = panel.border.map(|x| scale_factor * x);
                }
                _ => (),
            }
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
    pub entry_selected: Graphic,
    pub header_style: Rc<ButtonStyle>,
    pub text_field: Rc<TextFieldStyle>,
    pub scrollbar: Rc<ButtonStyle>,
    pub delete_button: Rc<ButtonStyle>,
    pub tab_style: Rc<TabStyle>,
    pub fold_icon: FoldIcon,
    pub button_panel: Graphic,
    pub delete_icon: Graphic,
    #[cfg_attr(
        not(all(feature = "rfd", not(target_arch = "wasm32"))),
        allow(dead_code)
    )]
    pub open_icon: Graphic,
    #[cfg_attr(not(feature = "rfd"), allow(dead_code))]
    pub file_icon: Graphic,
    pub menu_icon: Graphic,
    pub forward_icon: Graphic,
    pub rewind_icon: Graphic,
    pub gamepad: GamePad,
}
impl Style {
    pub fn load(
        fonts: &mut Fonts,
        render: &mut dyn SpriteRender,
        scale_factor: f64,
    ) -> Option<Self> {
        let loader = Loader {
            fonts,
            render,
            textures: HashMap::default(),
            scale_factor: ScaleFactor::from_float(scale_factor),
        };

        #[cfg(not(feature = "static"))]
        let file = &{
            let mut path = config::base_folder().unwrap();
            path.push("assets/style.ron");
            std::fs::read_to_string(path)
                .unwrap_or_else(|err| panic!("failed reading 'assets/style.ron': {}", err))
        };
        #[cfg(feature = "static")]
        let file = static_files::FILES.style;

        let mut deser = ron::Deserializer::from_str(file).unwrap();
        let style: Result<Self, _> = load_style(&mut deser, loader);

        Some(style.unwrap())
    }
}

// From https://stackoverflow.com/a/7666577 or http://www.cse.yorku.ca/~oz/hash.html
pub fn hash(s: &[u8]) -> u64 {
    use std::num::Wrapping as w;
    let mut hash = w(5381u64);
    for &c in s {
        let c = w(c as u64);
        hash = ((hash << 5) + hash) + c;
    }
    hash.0
}
