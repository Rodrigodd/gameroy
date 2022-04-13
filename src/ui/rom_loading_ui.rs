use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use giui::layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout};
use giui::text::Text;
use giui::widgets::{Button, ListBuilder};
use winit::event_loop::EventLoopProxy;
use winit::window::Window;

use crate::style::Style;
use crate::widget::table_item::{TableGroup, TableItem};
use crate::UserEvent;

#[derive(Clone, Debug)]
struct RomEntry {
    /// The name of the game as write in the rom header.
    name: String,
    /// The size of the rom file in bytes
    size: u64,
    /// The path to the rom
    path: PathBuf,
}

struct RomList {
    roms: Vec<RomEntry>,
    table_group: Rc<RefCell<TableGroup>>,
}
impl ListBuilder for RomList {
    fn update_item(
        &mut self,
        _index: usize,
        _item_id: giui::Id,
        _ctx: &mut dyn giui::BuilderContext,
    ) -> bool {
        true
    }

    fn item_count(&mut self, _ctx: &mut dyn giui::BuilderContext) -> usize {
        self.roms.len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: giui::Id,
        cb: giui::ControlBuilder,
        ctx: &mut dyn giui::BuilderContext,
    ) -> giui::ControlBuilder {
        let style = &ctx.get::<Style>().clone();
        let RomEntry { name, size, path } = self.roms[index].clone();
        cb.behaviour_and_layout(TableItem::new(self.table_group.clone()))
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
                .layout(FitGraphic)
            })
            .child(ctx, |cb, _| {
                let size = if size < (1 << 20) {
                    format!("{} KiB", size >> 10)
                } else {
                    format!("{}.{} MiB", size >> 20, ((size * 10) >> 20) % 10)
                };
                cb.graphic(Text::new(size, (-1, 0), style.text_style.clone()))
            })
    }
}

pub fn create_rom_loading_ui(ctx: &mut giui::Gui, style: &Style) {
    let v_box = ctx
        .create_control()
        .layout(VBoxLayout::new(2.0, [0.0; 4], -1))
        .build(ctx);

    let h_box = ctx
        .create_control()
        .layout(HBoxLayout::new(0.0, [0.0; 4], -1))
        .parent(v_box)
        .build(ctx);

    #[cfg(feature = "rfd")]
    let _open_button = ctx
        .create_control()
        .parent(h_box)
        .layout(HBoxLayout::new(0.0, [0.0; 4], -1))
        .behaviour(Button::new(
            style.delete_button.clone(),
            true,
            move |_, ctx| {
                let handle = ctx.get::<std::rc::Rc<Window>>().clone();
                let path = rfd::FileDialog::new()
                    .set_title("Open GameBoy Rom file")
                    .add_filter("GameBoy roms", &["gb"])
                    .set_parent(&*handle)
                    .pick_file();
                if let Some(path) = path {
                    ctx.get::<EventLoopProxy<UserEvent>>()
                        .send_event(UserEvent::LoadRom(path.clone().into()))
                        .unwrap();
                }
            },
        ))
        .child(ctx, |cb, _| {
            cb.graphic(style.open_icon.clone()).layout(FitGraphic)
        })
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "open rom".to_string(),
                (-1, 0),
                style.text_style.clone(),
            ))
            .layout(FitGraphic)
        })
        .build(ctx);

    let _remain = ctx
        .create_control()
        .graphic(style.background.clone())
        .parent(h_box)
        .expand_x(true)
        .build(ctx);

    let roms = crate::config()
        .rom_folder
        .as_ref()
        .and_then(|x| {
            load_roms(&x)
                .map_err(|e| log::error!("error reading roms folder: {}", e))
                .ok()
        })
        .unwrap_or_default();

    let table = TableGroup::new(4.0, 2.0, [1.0, 1.0])
        .collumn(150.0, false)
        .collumn(200.0, true)
        .collumn(60.0, false);

    crate::ui::list(
        ctx.create_control(),
        ctx,
        style,
        [0.0; 4],
        RomList {
            roms,
            table_group: Rc::new(RefCell::new(table)),
        },
    )
    .graphic(style.background.clone())
    .parent(v_box)
    .expand_y(true)
    .build(ctx);
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
            let mut file = std::fs::File::open(x.path().clone()).ok()?;
            let size = file.metadata().ok()?.len();
            let header = match gameroy::gameboy::cartridge::CartridgeHeader::from_reader(&mut file)
            {
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
                size,
                path: x.path(),
            })
        })
        .collect::<Vec<_>>();
    Ok(roms)
}
