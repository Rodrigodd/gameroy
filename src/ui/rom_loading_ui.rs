use std::{cell::RefCell, path::PathBuf, rc::Rc};

use giui::{
    graphics::Graphic,
    layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout},
    text::Text,
    widgets::{Button, ListBuilder},
};
use winit::{event_loop::EventLoopProxy, window::Window};

use crate::{
    config::config,
    event_table::{self, EventTable, Handle},
    executor,
    rom_loading::{load_gameboy, RomFile},
    style::Style,
    widget::table_item::{TableGroup, TableItem},
    UserEvent,
};

#[derive(Clone, Debug)]
pub struct RomEntry {
    /// The name of the game as write in the rom header.
    name: String,
    /// The size of the rom file in bytes
    size: u64,
    /// The path to the rom
    pub file: RomFile,
}
impl RomEntry {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    pub fn from_path(path: PathBuf) -> Result<RomEntry, String> {
        let file = RomFile::from_path(path);
        // let header = file.get_header()?;
        Ok(RomEntry {
            name: "name".to_string(), //header.title_as_string(),
            size: 0,                  // header.rom_size_in_bytes().unwrap_or(0) as u64,
            file,
        })
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    fn size(&self) -> u64 {
        self.size
    }
}
impl From<RomFile> for RomEntry {
    fn from(file: RomFile) -> Self {
        Self {
            name: file.file_name().to_string(),
            size: 0,
            file,
        }
    }
}

struct SetSelected(usize);

struct RomList {
    roms: Vec<RomEntry>,
    table_group: Rc<RefCell<TableGroup>>,
    last_selected: Option<usize>,
    selected: Option<usize>,
    rebuild_everthing: bool,
    _handle: Handle<event_table::UpdateRomList>,
}
impl RomList {
    fn new(
        roms: Vec<RomEntry>,
        table_group: Rc<RefCell<TableGroup>>,
        handle: Handle<event_table::UpdateRomList>,
    ) -> Self {
        Self {
            roms,
            table_group,
            last_selected: None,
            rebuild_everthing: false,
            selected: None,
            _handle: handle,
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
        if self.rebuild_everthing {
            return false;
        }

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
        self.rebuild_everthing = false;
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
        } else if event.is::<event_table::UpdateRomList>() {
            self.roms = list_roms();
            self.rebuild_everthing = true;
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
        let (name, size, file, entry) = if !header {
            let entry = self.roms[index - 1].clone();
            let size = entry.size();
            let size = if size < (1 << 20) {
                format!("{} KiB", size >> 10)
            } else {
                format!("{}.{} MiB", size >> 20, ((size * 10) >> 20) % 10)
            };
            (
                entry.name(),
                size,
                entry.file.file_name().into_owned(),
                Some(entry),
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
            if let Some(entry) = entry {
                item.set_on_click(move |click_count, ctx| {
                    if click_count == 1 {
                        ctx.send_event_to(list_id, SetSelected(index))
                    } else if click_count == 2 {
                        let proxy = ctx.get::<EventLoopProxy<UserEvent>>().clone();
                        let p = proxy.clone();
                        let file = entry.file.clone();
                        let task = async move {
                            let rom = file.read().await.unwrap();
                            let ram = match file.load_ram_data().await {
                                Ok(x) => Some(x),
                                Err(err) => {
                                    log::error!("{}", err);
                                    None
                                }
                            };
                            log::debug!("sending LoadRom");
                            p.send_event(UserEvent::LoadRom {
                                file,
                                game_boy: load_gameboy(rom, ram).unwrap(),
                            })
                            .unwrap();
                        };
                        executor::Executor::spawn_task(task, ctx);
                    }
                });
            }
            item
        })
    }
}

pub fn create_rom_loading_ui(
    ctx: &mut giui::Gui,
    style: &Style,
    event_table: Rc<RefCell<EventTable>>,
) {
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
                let task = async move {
                    let handle = &*handle;
                    let file = rfd::AsyncFileDialog::new()
                        .set_title("Open GameBoy Rom file")
                        .add_filter("GameBoy roms", &["gb"])
                        .set_parent(handle)
                        .pick_file()
                        .await;

                    if let Some(file) = file {
                        let file: RomFile = file.into();
                        let rom = file.read().await.unwrap();
                        let ram = match file.load_ram_data().await {
                            Ok(x) => Some(x),
                            Err(err) => {
                                log::error!("{}", err);
                                None
                            }
                        };
                        proxy
                            .send_event(UserEvent::LoadRom {
                                file,
                                game_boy: load_gameboy(rom, ram).unwrap(),
                            })
                            .unwrap();
                    }
                };
                executor::Executor::spawn_task(task, ctx);
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

    #[cfg(all(feature = "rfd", not(target_arch = "wasm32")))]
    let _folder_button = ctx
        .create_control()
        .parent(h_box)
        .layout(HBoxLayout::new(0.0, [0.0; 4], -1))
        .behaviour(Button::new(
            style.delete_button.clone(),
            true,
            move |_, ctx| {
                let handle = ctx.get::<std::rc::Rc<Window>>().clone();
                let proxy = ctx.get::<EventLoopProxy<UserEvent>>().clone();
                let task = async move {
                    let handle = &*handle;
                    let folder = rfd::AsyncFileDialog::new()
                        .set_title("Open GameBoy Rom file")
                        .add_filter("GameBoy roms", &["gb"])
                        .set_parent(handle)
                        .pick_folder()
                        .await;

                    if let Some(folder) = folder {
                        let path = folder.path().to_string_lossy().to_string();
                        config().rom_folder = Some(path);

                        proxy.send_event(UserEvent::UpdateRomList).unwrap();
                    }
                };
                executor::Executor::spawn_task(task, ctx);
            },
        ))
        .child(ctx, |cb, _| {
            cb.graphic(style.open_icon.clone()).layout(FitGraphic)
        })
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "choose folder".to_string(),
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

    let roms = list_roms();

    let table = TableGroup::new(4.0, 2.0, [1.0, 1.0])
        .column(120.0, false)
        .column(490.0, false)
        .column(60.0, false);

    let rom_list_id = ctx.reserve_id();

    crate::ui::list(
        ctx.create_control_reserved(rom_list_id),
        ctx,
        style,
        [0.0; 4],
        RomList::new(
            roms,
            Rc::new(RefCell::new(table)),
            event_table.borrow_mut().register(rom_list_id),
        ),
    )
    .graphic(style.background.clone())
    .parent(v_box)
    .expand_y(true)
    .build(ctx);
}

fn list_roms() -> Vec<RomEntry> {
    let roms = crate::config()
        .rom_folder
        .as_ref()
        .and_then(|x| {
            crate::rom_loading::load_roms(&x)
                .map_err(|e| log::error!("error reading roms folder: {}", e))
                .ok()
        })
        .unwrap_or_default();
    let roms = roms.into_iter().map(|x| RomEntry::from(x)).collect();
    roms
}
