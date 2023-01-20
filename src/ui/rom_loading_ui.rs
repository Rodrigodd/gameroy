use std::{
    cell::RefCell,
    ops::Add,
    rc::Rc,
    sync::{Arc, RwLock, RwLockReadGuard},
};

use giui::{
    graphics::{Graphic, Texture},
    layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout},
    text::Text,
    widgets::{Button, ListBuilder},
    Id,
};
use winit::{event_loop::EventLoopProxy, window::Window};

use crate::{
    config::config,
    event_table::{self, EventTable},
    executor,
    rom_loading::{load_gameboy, RomFile},
    style::Style,
    widget::table_item::{TableGroup, TableItem},
    UserEvent,
};

const COLLUMNS: &[(&str, f32)] = &[
    ("File", 490.0),
    ("Header Name", 129.0),
    ("Size", 60.0),
    ("Last played", 100.0),
];

struct SortBy(usize);
#[derive(Clone, Copy, Debug)]
enum SortDirection {
    Ascending,
    Descending,
}

pub struct RomEntries {
    /// In `start_loading`, a background thread get a write lock to the Vec to fill it. After that,
    /// it holds a read lock for a long time while it runs. Also get a write lock when updating each
    /// RomEntry.
    ///
    /// `sort_by` and `RomList` only get read locks.
    roms: Arc<RwLock<Vec<RwLock<RomEntry>>>>,
    order: Vec<usize>,
    sort_collumn: usize,
    sort_direction: SortDirection,
    pub observers: Vec<giui::Id>,
}
impl RomEntries {
    pub fn new(proxy: EventLoopProxy<UserEvent>) -> Self {
        let (sort_collumn, sort_direction) = match &config().sort_list {
            Some(sort_config) => {
                let sort_config = sort_config.clone().to_lowercase();
                let mut sort_config = sort_config.as_str();

                let dir = if sort_config.starts_with('-') {
                    sort_config = &sort_config[1..];
                    SortDirection::Descending
                } else if sort_config.starts_with('+') {
                    sort_config = &sort_config[1..];
                    SortDirection::Ascending
                } else {
                    // Don't specifing a direction defaults to ascending.
                    SortDirection::Ascending
                };

                let col = COLLUMNS
                    .iter()
                    .position(|&(title, _)| sort_config.contains(&title.to_lowercase()))
                    .unwrap_or_else(|| {
                        log::error!("Unkown collumn name '{}'", sort_config);
                        0
                    });

                (col, dir)
            }
            None => (0, SortDirection::Ascending),
        };

        let this = Self {
            roms: Default::default(),
            order: Default::default(),
            observers: Vec::new(),
            sort_collumn,
            sort_direction,
        };
        this.start_loading(proxy);
        this
    }

    pub fn sort_by(&mut self, collumn_index: usize) {
        if self.sort_collumn == collumn_index {
            self.sort_direction = match self.sort_direction {
                SortDirection::Ascending => SortDirection::Descending,
                SortDirection::Descending => SortDirection::Ascending,
            };
        } else {
            self.sort_direction = SortDirection::Ascending;
        }
        self.sort_collumn = collumn_index;

        {
            if let Some((title, _)) = COLLUMNS.get(collumn_index) {
                let dir = match self.sort_direction {
                    SortDirection::Ascending => "+".to_string(),
                    SortDirection::Descending => "-".to_string(),
                };
                config().sort_list = Some(dir + title);
                let _ = config()
                    .save()
                    .map_err(|x| log::error!("error saving config: {}", x));
            }
        }

        self.update_sort();
    }

    pub fn update_sort(&mut self) {
        use std::cmp::Ordering;

        let sort_direction = self.sort_direction;
        let sort_collumn = self.sort_collumn;

        let entries = self.roms.read().unwrap();
        if self.order.len() != entries.len() {
            self.order = (0..entries.len()).collect();
        }

        self.order.sort_by(|a, b| {
            let a = entries[*a].read().unwrap();
            let b = entries[*b].read().unwrap();

            fn some_first<T, U: Ord>(a: &T, b: &T, map: impl Fn(&T) -> &Option<U>) -> Ordering {
                let a = map(a);
                let b = map(b);
                a.is_some()
                    .cmp(&b.is_some())
                    .reverse()
                    .then_with(|| a.cmp(b))
            }

            let ord = match sort_collumn {
                0 => a.file.file_name().cmp(&b.file.file_name()),
                1 => some_first(&*a, &*b, |x| &x.header_name),
                2 => some_first(&*a, &*b, |x| &x.size),
                3 => a.save_time.cmp(&b.save_time).reverse(),
                _ => {
                    log::error!("Unknown collumn index: {}", sort_collumn);
                    Ordering::Equal
                }
            };

            fn default_key<'a>(
                a: &'a RwLockReadGuard<'a, RomEntry>,
            ) -> (
                std::borrow::Cow<'a, str>,
                Option<&'a String>,
                Option<&'a u64>,
            ) {
                (a.file.file_name(), a.header_name.as_ref(), a.size.as_ref())
            }

            let ord = match ord {
                Ordering::Equal => default_key(&a).cmp(&default_key(&b)),
                _ => ord,
            };

            if let SortDirection::Ascending = sort_direction {
                ord
            } else {
                ord.reverse()
            }
        })
    }

    #[cfg(target_arch = "wasm32")]
    pub fn start_loading(&self, _: EventLoopProxy<UserEvent>) {}

    #[cfg(not(target_arch = "wasm32"))]
    #[allow(clippy::await_holding_lock)]
    pub fn start_loading(&self, proxy: EventLoopProxy<UserEvent>) {
        let roms_path = &crate::config::config().rom_folder;

        let roms_path = match roms_path {
            Some(x) => x.clone(),
            None => {
                proxy.send_event(UserEvent::UpdatedRomList).unwrap();
                return;
            }
        };
        let entries = self.roms.clone();

        let f = move || {
            let start = instant::Instant::now();

            let roms = crate::rom_loading::load_roms(&roms_path)
                .map_err(|e: String| log::error!("error reading roms: {}", e))
                .ok()
                .unwrap_or_default();

            *entries.write().unwrap() = roms
                .into_iter()
                .map(|x| {
                    let save_time = x.get_save_time();
                    log::debug!("{}", x.file_name());
                    RwLock::new(RomEntry {
                        file: x,
                        header_name: None,
                        size: None,
                        save_time: save_time.ok(),
                        thumbnail: None,
                    })
                })
                .collect();

            proxy.send_event(UserEvent::UpdatedRomList).unwrap();

            let index = std::sync::atomic::AtomicUsize::new(0);

            std::thread::scope(|s| {
                /// The threads are mainly IO bound so it makes sense to use a lot of threads. Not
                /// sure about the right ammount through.
                let max_threads = 100.min(entries.read().unwrap().len());
                for _i in 0..max_threads {
                    let proxy = proxy.clone();
                    let entries = entries.clone();
                    let index = &index;
                    s.spawn(move || loop {
                        let entries = entries.read().unwrap();
                        let index = index.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(entry) = entries.get(index) else {
                            break;
                        };

                        let (header, file_name) = {
                            let rom_file = entry.read().unwrap().file.clone();
                            let file_name =
                                rom_file.file_name().trim_end_matches(".gb").to_string();
                            let header = rom_file.get_header();
                            (header, file_name)
                        };

                        let mut thumbnail = None;
                        match crate::rom_loading::get_thumb(&file_name) {
                            Ok(image) => {
                                let texture_id =
                                    (crate::style::hash(file_name.as_bytes()) & 0x7fff_ffff) as u32;
                                proxy
                                    .send_event(UserEvent::NewTexture(
                                        texture_id,
                                        image.width(),
                                        image.height(),
                                        image.into_raw().into_boxed_slice(),
                                    ))
                                    .unwrap();

                                thumbnail = Some(texture_id);
                            }
                            Err(err) => {
                                log::info!("get thumbnail failed: {err}");
                            }
                        }

                        {
                            let mut entry = entry.write().unwrap();
                            entry.thumbnail = thumbnail;
                            match header {
                                Ok(header) => {
                                    entry.header_name = Some(header.title_as_string());
                                    entry.size =
                                        Some(header.rom_size_in_bytes().unwrap_or(0) as u64);
                                }
                                Err(err) => {
                                    entry.header_name = Some("Error reading header...".to_string());
                                    entry.size = None;
                                    log::error!(
                                        "error reading '{}' header: {}",
                                        entry.file.file_name(),
                                        err
                                    );
                                }
                            }
                            proxy.send_event(UserEvent::UpdatedRomList).unwrap();
                        }
                    });
                }
            });

            log::info!("loading roms took: {:?}", start.elapsed());
        };
        std::thread::Builder::new()
            .name("loading roms".to_string())
            .spawn(f)
            .unwrap();
    }

    fn get_rom(&self, pos: usize) -> Option<RomEntry> {
        let index = self.order[pos];
        self.roms
            .read()
            .unwrap()
            .get(index)
            .map(|x| x.read().unwrap().clone())
    }

    /// The total number of roms
    fn len(&self) -> usize {
        self.order.len()
    }

    fn register(&mut self, id: Id) {
        self.observers.push(id);
    }
}

#[derive(Clone, Debug)]
pub struct RomEntry {
    /// The name of the game as write in the rom header.
    header_name: Option<String>,
    /// The size of the rom file in bytes
    size: Option<u64>,
    /// The instant in millisenconds since epoch of this rom's ram save file
    save_time: Option<u64>,
    /// The path to the rom
    pub file: RomFile,
    /// The index of the texture that contains this Rom thumbnail
    thumbnail: Option<u32>,
}
impl RomEntry {
    pub fn name(&self) -> String {
        self.header_name
            .clone()
            .unwrap_or_else(|| "Loading...".to_string())
    }

    fn size(&self) -> String {
        if let Some(size) = self.size {
            if size < (1 << 20) {
                format!("{} KiB", size >> 10)
            } else {
                format!("{}.{} MiB", size >> 20, ((size * 10) >> 20) % 10)
            }
        } else {
            "-".to_string()
        }
    }

    fn save_age(&self) -> String {
        use instant::{Duration, SystemTime};

        let last_played = match self.save_time {
            Some(x) => x,
            None => return " - ".to_string(),
        };
        let then = SystemTime::UNIX_EPOCH.add(Duration::from_millis(last_played));
        let now = SystemTime::now();

        let delta = now.duration_since(then);

        let delta = match delta {
            Ok(x) => x.as_secs(),
            Err(_) => return " - ".to_string(),
        };

        const SECOND: u64 = 1;
        const MINUTE: u64 = 60 * SECOND;
        const HOUR: u64 = 60 * MINUTE;
        const DAY: u64 = 24 * HOUR;
        // const WEEK: u64 = 7 * DAY;
        const MONTH: u64 = 30 * DAY;
        const YEAR: u64 = 365 * DAY;

        match delta {
            x if x < SECOND => "Just Now".to_string(),
            x if x < MINUTE => format!("{}s agi", x / SECOND),
            x if x < HOUR => format!("{}min ago", x / MINUTE),
            x if x < DAY => format!("{}h ago", x / HOUR),
            x if x < MONTH => format!("{}d ago", x / DAY),
            x if x < YEAR => format!("{}m ago", x / MONTH),
            x => format!("{} years", x / YEAR),
        }
    }
}

struct SetSelected(usize);

struct RomList {
    table_group: Rc<RefCell<TableGroup>>,
    last_selected: Option<usize>,
    selected: Option<usize>,
    rebuild_everthing: bool,
}
impl RomList {
    fn new(table_group: Rc<RefCell<TableGroup>>) -> Self {
        Self {
            table_group,
            last_selected: None,
            rebuild_everthing: false,
            selected: None,
        }
    }
}
impl ListBuilder for RomList {
    fn content_width(&mut self) -> f32 {
        self.table_group.borrow_mut().total_width()
    }

    fn update_item(
        &mut self,
        index: usize,
        item_id: giui::Id,
        ctx: &mut dyn giui::BuilderContext,
    ) -> bool {
        if self.rebuild_everthing {
            return false;
        }

        if Some(index) == self.last_selected || Some(index) == self.selected {
            *ctx.get_graphic_mut(item_id) = if self.selected == Some(index) {
                ctx.get::<Style>().entry_selected.clone()
            } else {
                Graphic::None
            };
        }
        true
    }

    fn finished_layout(&mut self) {
        self.last_selected = None;
        self.rebuild_everthing = false;
    }

    fn item_count(&mut self, ctx: &mut dyn giui::BuilderContext) -> usize {
        ctx.get::<RomEntries>().len()
    }

    fn on_event(&mut self, event: Box<dyn std::any::Any>, this: giui::Id, ctx: &mut giui::Context) {
        if let Some(&SetSelected(index)) = event.downcast_ref() {
            if self.selected == Some(index) {
                return;
            }
            self.last_selected = self.selected.or(Some(index));
            self.selected = Some(index);
            ctx.dirty_layout(this);
        } else if event.is::<event_table::UpdatedRomList>() {
            log::trace!("rebuilding rom list ui");
            self.rebuild_everthing = true;
            ctx.dirty_layout(this);
        } else if let Some(&SortBy(index)) = event.downcast_ref() {
            ctx.get_mut::<RomEntries>().sort_by(index);
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
        let (file, name, size, age, entry) = if !header {
            let entry = ctx.get::<RomEntries>().get_rom(index - 1).unwrap();
            let size = entry.size();
            let age = entry.save_age();
            (
                entry.file.file_name().into_owned(),
                entry.name(),
                size,
                age,
                Some(entry),
            )
        } else {
            (
                COLLUMNS[0].0.to_string(),
                COLLUMNS[1].0.to_string(),
                COLLUMNS[2].0.to_string(),
                COLLUMNS[3].0.to_string(),
                None,
            )
        };
        let parent = cb.id();

        // Thumbnail
        if header {
            ctx.create_control().parent(parent).build(ctx);
        } else {
            let graphic = if let Some(texture) = entry.as_ref().and_then(|x| x.thumbnail) {
                Texture::new(texture, [0.0, 0.0, 1.0, 1.0])
            } else {
                Texture::new(0, [0.0, 0.0, 1.0, 1.0]).with_color([120, 120, 120, 255].into())
            };
            ctx.create_control()
                .child(ctx, |cb, _| {
                    cb.graphic(graphic)
                        .min_size([48.0, 48.0])
                        .fill_x(giui::RectFill::ShrinkCenter)
                })
                .parent(parent)
                .layout(MarginLayout::new([2.0; 4]))
                .build(ctx);
        }

        for (collumn_index, text) in [file, name, size, age].into_iter().enumerate() {
            let cb = ctx
                .create_control()
                .parent(parent)
                .child(ctx, move |cb, _| {
                    let text_style = style.text_style.clone();
                    // I could use `.layout(FitGraphic)` but I want to the text to be cut off.
                    cb.min_size([0.0, text_style.font_size])
                        .graphic(Text::new(text, (-1, 0), text_style).with_wrap(false))
                        .expand_x(true)
                });

            if header {
                let (sort_collumn, sort_direction) = {
                    let rom_entries = ctx.get::<RomEntries>();
                    (rom_entries.sort_collumn, rom_entries.sort_direction)
                };

                let header_style = style.header_style.clone();
                cb.layout(HBoxLayout::new(0.0, [2.0; 4], -1))
                    .child(ctx, move |cb, _| {
                        if collumn_index == sort_collumn {
                            let graphic = match sort_direction {
                                SortDirection::Ascending => style.fold_icon.close.clone(),
                                SortDirection::Descending => {
                                    style.fold_icon.close.clone().with_flip_y()
                                }
                            };
                            cb.graphic(graphic).layout(FitGraphic)
                        } else {
                            cb
                        }
                    })
                    .behaviour(Button::new(header_style, false, move |_, ctx| {
                        log::info!("sort by {collumn_index}");
                        ctx.send_event_to(list_id, SortBy(collumn_index))
                    }))
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
                        let p = proxy;
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
                            let game_boy = match load_gameboy(rom, ram) {
                                Ok(x) => x,
                                Err(err) => {
                                    log::error!("failed to load rom: {}", err);
                                    return;
                                }
                            };
                            log::debug!("sending LoadRom");
                            p.send_event(UserEvent::LoadRom { file, game_boy }).unwrap();
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
    _event_table: Rc<RefCell<EventTable>>,
) {
    let rom_list_id = ctx.reserve_id();

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
            cb.graphic(style.file_icon.clone()).layout(FitGraphic)
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
                        log::info!("setting rom folder to '{}'", path);

                        let mut conf = crate::config::config();
                        conf.rom_folder = Some(path);
                        let _ = conf
                            .save()
                            .map_err(|x| log::error!("error saving config: {}", x));

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

    #[cfg(target_os = "android")]
    let _licenses_button = ctx
        .create_control()
        .parent(h_box)
        .layout(HBoxLayout::new(0.0, [0.0; 4], -1))
        .behaviour(Button::new(
            style.delete_button.clone(),
            true,
            move |_, _| crate::rom_loading::show_licenses(),
        ))
        .child(ctx, |cb, _| {
            cb.graphic(style.file_icon.clone()).layout(FitGraphic)
        })
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "licenses".to_string(),
                (-1, 0),
                style.text_style.clone(),
            ))
            .layout(FitGraphic)
        })
        .build(ctx);

    let table = {
        let mut tg = TableGroup::new(4.0, 2.0, [1.0, 1.0]);

        // Thumbnail
        tg.add_column(60.0, false);
        for &(_, width) in COLLUMNS.iter() {
            tg.add_column(width, false)
        }
        tg
    };

    ctx.get_mut::<RomEntries>().register(rom_list_id);
    crate::ui::list(
        ctx.create_control_reserved(rom_list_id),
        ctx,
        style,
        [0.0; 4],
        RomList::new(Rc::new(RefCell::new(table))),
    )
    .graphic(style.background.clone())
    .parent(v_box)
    .expand_y(true)
    .build(ctx);
}
