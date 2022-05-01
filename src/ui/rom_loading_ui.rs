use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use giui::graphics::Graphic;
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

struct SetSelected(usize);

struct RomList {
    roms: Vec<RomEntry>,
    table_group: Rc<RefCell<TableGroup>>,
    last_selected: Option<usize>,
    selected: Option<usize>,
}
impl RomList {
    fn new(roms: Vec<RomEntry>, table_group: Rc<RefCell<TableGroup>>) -> Self {
        Self {
            roms,
            table_group,
            last_selected: None,
            selected: None,
        }
    }
}
impl ListBuilder for RomList {
    fn update_item(
        &mut self,
        index: usize,
        item_id: giui::Id,
        ctx: &mut dyn giui::BuilderContext,
    ) -> bool {
        if self.last_selected.is_some() {
            if Some(index) == self.last_selected || Some(index) == self.selected {
                *ctx.get_graphic_mut(item_id) = if self.selected == Some(index) {
                    ctx.get::<Style>().header_background.clone()
                } else {
                    Graphic::None
                };
            }
        }
        true
    }

    fn finished_layout(&mut self) {
        self.last_selected = None;
    }

    fn item_count(&mut self, _ctx: &mut dyn giui::BuilderContext) -> usize {
        self.roms.len() + 1
    }

    fn on_event(&mut self, event: Box<dyn std::any::Any>, this: giui::Id, ctx: &mut giui::Context) {
        if let Some(&SetSelected(index)) = event.downcast_ref() {
            if self.selected == Some(index) {
                return;
            }
            self.last_selected = self.selected.or(Some(index));
            self.selected = Some(index);
            ctx.dirty_layout(this);
        }
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        list_id: giui::Id,
        cb: giui::ControlBuilder,
        ctx: &mut dyn giui::BuilderContext,
    ) -> giui::ControlBuilder {
        let style = &ctx.get::<Style>().clone();
        let header = index == 0;
        let (name, size, file, path) = if !header {
            let RomEntry { name, size, path } = self.roms[index - 1].clone();
            let size = if size < (1 << 20) {
                format!("{} KiB", size >> 10)
            } else {
                format!("{}.{} MiB", size >> 20, ((size * 10) >> 20) % 10)
            };
            (
                name,
                size,
                path.file_name().unwrap().to_string_lossy().into_owned(),
                Some(path),
            )
        } else {
            (
                "Header Name".to_string(),
                "Size".to_string(),
                "File".to_string(),
                None,
            )
        };
        let cell_backgroud = if header {
            style.header_background.clone()
        } else {
            Graphic::None
        };
        let parent = cb.id();
        for text in [name, file, size] {
            let cb = ctx
                .create_control()
                .parent(parent)
                .child(ctx, move |cb, _| {
                    let text_style = style.text_style.clone();
                    // I could use `.layout(FitGraphic)` but I want to the text to be cut off.
                    cb.min_size([0.0, text_style.font_size])
                        .graphic(Text::new(text, (-1, 0), text_style).with_wrap(false))
                        .expand_x(true)
                })
                .graphic(cell_backgroud.clone());

            if header {
                cb.layout(HBoxLayout::new(0.0, [2.0; 4], -1))
                    .child(ctx, move |cb, _| {
                        cb.graphic(style.fold_icon.close.clone()).layout(FitGraphic)
                    })
            } else {
                cb.layout(MarginLayout::new([2.0; 4]))
            }
            .build(ctx);
        }
        cb.behaviour_and_layout({
            let mut item = TableItem::new(self.table_group.clone()).with_resizable(header);
            if !header {
                let path = path.unwrap();
                item.set_on_click(move |click_count, ctx| {
                    if click_count == 1 {
                        ctx.send_event_to(list_id, SetSelected(index))
                    } else if click_count == 2 {
                        ctx.get::<EventLoopProxy<UserEvent>>()
                            .send_event(UserEvent::LoadRom(path.clone().into()))
                            .unwrap();
                    }
                });
            }
            item
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
                let proxy = ctx.get::<EventLoopProxy<UserEvent>>().clone();
                let p = proxy.clone();
                let task = async move {
                    let handle = &*handle;
                    let file = rfd::AsyncFileDialog::new()
                        .set_title("Open GameBoy Rom file")
                        .add_filter("GameBoy roms", &["gb"])
                        .set_parent(handle)
                        .pick_file()
                        .await;

                    if let Some(file) = file {
                        #[cfg(not(target_arch = "wasm32"))]
                        proxy
                            .send_event(UserEvent::LoadRom(file.path().into()))
                            .unwrap();
                        #[cfg(target_arch = "wasm32")]
                        unimplemented!()
                    }
                };
                use std::future::Future;
                use std::pin::Pin;
                ctx.set::<Pin<Box<dyn Future<Output = ()>>>>(Box::pin(task));
                p.send_event(UserEvent::SpawnTask(0)).unwrap();
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

    #[cfg(not(target_arch = "wasm32"))]
    let roms = crate::config()
        .rom_folder
        .as_ref()
        .and_then(|x| {
            load_roms(&x)
                .map_err(|e| log::error!("error reading roms folder: {}", e))
                .ok()
        })
        .unwrap_or_default();
    #[cfg(target_arch = "wasm32")]
    let roms = vec![RomEntry {
        name: "Name".to_string(),
        size: 100,
        path: PathBuf::from("name.gb".to_string()),
    }];

    let table = TableGroup::new(4.0, 2.0, [1.0, 1.0])
        .column(120.0, false)
        .column(490.0, false)
        .column(60.0, false);

    crate::ui::list(
        ctx.create_control(),
        ctx,
        style,
        [0.0; 4],
        RomList::new(roms, Rc::new(RefCell::new(table))),
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
