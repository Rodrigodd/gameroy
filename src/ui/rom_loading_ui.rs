use std::path::PathBuf;

use crui::layouts::{FitGraphic, HBoxLayout, MarginLayout};
use crui::text::Text;
use crui::widgets::{Button, ListBuilder};
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

#[derive(Clone, Debug)]
struct RomEntry {
    name: String,
    path: PathBuf,
}

struct RomList {
    roms: Vec<RomEntry>,
}
impl ListBuilder for RomList {
    fn update_item(
        &mut self,
        _index: usize,
        _item_id: crui::Id,
        _ctx: &mut dyn crui::BuilderContext,
    ) -> bool {
        true
    }

    fn item_count(&mut self, _ctx: &mut dyn crui::BuilderContext) -> usize {
        self.roms.len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: crui::Id,
        cb: crui::ControlBuilder,
        ctx: &mut dyn crui::BuilderContext,
    ) -> crui::ControlBuilder {
        let style = &ctx.get::<Style>().clone();
        let RomEntry { name, path } = self.roms[index].clone();
        cb.layout(HBoxLayout::new(4.0, [0.0; 4], -1))
            .child(ctx, |cb, ctx| {
                let path = path.clone();
                cb.behaviour(Button::new(
                    style.delete_button.clone(),
                    true,
                    move |_, ctx| {
                        ctx.get::<EventLoopProxy<UserEvent>>()
                            .send_event(UserEvent::LoadRom(path.clone().into()))
                            .unwrap();
                    },
                ))
                .child(ctx, move |cb, _| {
                    cb.graphic(Text::new(
                        name,
                        (0, 0),
                        style.text_style.clone().with_font_size(20.0),
                    ))
                    .layout(FitGraphic)
                })
                .layout(MarginLayout::new([2.0; 4]))
                .min_width(180.0)
            })
            .child(ctx, |cb, _| {
                cb.graphic(Text::new(
                    path.file_name().unwrap().to_string_lossy().into_owned(),
                    (-1, 0),
                    style.text_style.clone(),
                ))
                .expand_x(true)
            })
    }
}

pub fn create_rom_loading_ui(gui: &mut crui::Gui, style: &Style) {
    // let folders = ["roms/", "core/tests/gameboy-test-roms/**"];
    let roms = crate::config()
        .rom_folder
        .as_ref()
        .and_then(|x| {
            load_roms(&x)
                .map_err(|e| log::error!("error reading roms folder: {}", e))
                .ok()
        })
        .unwrap_or_default();

    crate::ui::list(gui.create_control(), gui, style, [0.0; 4], RomList { roms })
        .graphic(style.background.clone())
        .build(gui);
}

fn load_roms(roms_path: &str) -> Result<Vec<RomEntry>, std::io::Error> {
    let roms_path = crate::normalize_config_path(roms_path);

    let roms = std::fs::read_dir(&roms_path)?
        .flat_map(|x| x.map_err(|e| log::error!("error: {}", e)).ok())
        .filter_map(|x| {
            if x.path().extension()? != "gb" {
                return None;
            }
            Some(x)
        })
        .filter_map(|x| {
            let header = match gameroy::gameboy::cartridge::CartridgeHeader::from_reader(
                &mut std::fs::File::open(x.path().clone()).ok()?,
            ) {
                Ok(x) | Err((Some(x), _)) => x,
                _ => return None,
            };
            let name = {
                let l = header
                    .title
                    .as_slice()
                    .iter()
                    .position(|&x| x == 0)
                    .unwrap_or(header.title.len());
                String::from_utf8_lossy(&header.title[0..l]).into_owned()
            };
            Some(RomEntry {
                name,
                path: x.path(),
            })
        })
        .collect::<Vec<_>>();
    Ok(roms)
}
