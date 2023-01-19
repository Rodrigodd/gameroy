use std::{collections::HashMap, rc::Rc};

use giui::{
    font::Fonts,
    graphics::{Graphic, TextStyle},
    style::{ButtonStyle, TabStyle, TextFieldStyle},
    style_loader::load_style,
};
use sprite_render::SpriteRender;

use crate::widget::fold_view::FoldIcon;

#[cfg(not(feature = "static"))]
use crate::config;

pub struct Loader<'a> {
    pub fonts: &'a mut Fonts,
    pub render: &'a mut (dyn SpriteRender + 'a),
    pub textures: HashMap<String, (u32, u32, u32)>,
    pub scale_factor: f64,
}

#[cfg(not(feature = "static"))]
mod loaded_files {
    use super::config;
    use giui::{font::Font, graphics::Graphic, style_loader::StyleLoaderCallback};
    use image::{ImageBuffer, Rgba};
    use sprite_render::{Texture, TextureId};

    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, mut name: String) -> (u32, u32, u32) {
            let scale2x = self.scale_factor >= 1.5;

            if scale2x {
                if name == "icons.png" {
                    name = "icons2x.png".to_string();
                }
            }

            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let data = loop {
                if name == "white.png" {
                    let mut image_buffer = ImageBuffer::new(1, 1);
                    image_buffer
                        .pixels_mut()
                        .for_each(|x| *x = Rgba::<u8>::from([255, 255, 255, 255]));
                    break image_buffer;
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
                let data = data.to_rgba8();

                break data;
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
            if scale2x {
                // unscale texture width and height
                texture.1 /= 2;
                texture.2 /= 2;
            }
            self.textures.insert(name, texture);
            texture
        }

        fn modify_graphic(&mut self, graphic: &mut Graphic) {
            match graphic {
                Graphic::Icon(icon) => {
                    if self.scale_factor >= 1.5 {
                        icon.size = icon.size.map(|x| 2.0 * x);
                    }
                }
                Graphic::Panel(panel) => {
                    if self.scale_factor >= 1.5 {
                        panel.border = panel.border.map(|x| 2.0 * x);
                    }
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
        pub icons_texture: &'static [u8],
        pub icons2x_texture: &'static [u8],
    }
    pub static FILES: StaticFiles = StaticFiles {
        font: include_bytes!("../assets/NotoSansMono.ttf"),
        style: include_str!("../assets/style.ron"),
        icons_texture: include_bytes!("../assets/icons.png"),
        icons2x_texture: include_bytes!("../assets/icons2x.png"),
    };
    impl<'a> StyleLoaderCallback for super::Loader<'a> {
        fn load_texture(&mut self, name: String) -> (u32, u32, u32) {
            if let Some(texture) = self.textures.get(&name) {
                return *texture;
            }

            let scale2x = self.scale_factor >= 1.5;

            let data = 'data: {
                let data = match name.as_str() {
                    "white.png" => {
                        let mut image_buffer = ImageBuffer::new(1, 1);
                        image_buffer
                            .pixels_mut()
                            .for_each(|x| *x = Rgba::<u8>::from([255, 255, 255, 255]));
                        break 'data image_buffer;
                    }
                    "icons.png" => {
                        if scale2x {
                            FILES.icons2x_texture
                        } else {
                            FILES.icons_texture
                        }
                    }
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
            if scale2x {
                // unscale texture width and height
                texture.1 /= 2;
                texture.2 /= 2;
            }
            self.textures.insert(name, texture);
            texture
        }

        fn modify_graphic(&mut self, graphic: &mut Graphic) {
            match graphic {
                Graphic::Icon(icon) => {
                    if self.scale_factor >= 1.5 {
                        icon.size = icon.size.map(|x| 2.0 * x);
                    }
                }
                Graphic::Panel(panel) => {
                    if self.scale_factor >= 1.5 {
                        panel.border = panel.border.map(|x| 2.0 * x);
                    }
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
    pub open_icon: Graphic,
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
            scale_factor,
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
