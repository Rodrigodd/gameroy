use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use gameroy::gameboy::cartridge::Cartridge;
use gameroy::gameboy::GameBoy;
use giui::graphics::Graphic;
use giui::layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout};
use giui::text::Text;
use giui::widgets::{Button, ListBuilder};
use winit::event_loop::EventLoopProxy;
use winit::window::Window;

use crate::config::config;
use crate::style::Style;
use crate::widget::table_item::{TableGroup, TableItem};
use crate::UserEvent;

#[derive(Clone, Debug)]
pub struct RomEntry {
    /// The name of the game as write in the rom header.
    name: String,
    /// The size of the rom file in bytes
    size: u64,
    /// The path to the rom
    #[cfg(not(target_arch = "wasm32"))]
    path: PathBuf,
    /// The rom data
    #[cfg(target_arch = "wasm32")]
    rom: std::sync::Arc<[u8]>,
    /// The file name
    #[cfg(target_arch = "wasm32")]
    file_name: String,
}
#[cfg(target_arch = "wasm32")]
impl RomEntry {
    pub fn file_name(&self) -> std::borrow::Cow<str> {
        self.file_name.clone().into()
    }

    pub fn from_bytes(rom: Box<[u8]>) -> Result<RomEntry, String> {
        let size = rom.len() as u64;
        let header = match gameroy::gameboy::cartridge::CartridgeHeader::from_bytes(rom.as_ref()) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((_, e)) => return Err(e),
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
        Ok(RomEntry {
            name,
            size,
            rom: rom.into(),
            file_name: String::new(),
        })
    }

    fn read(&self) -> Result<Vec<u8>, String> {
        let rom = self.rom.to_vec();
        Ok(rom)
    }

    fn load_boot_rom() -> Option<[u8; 256]> {
        None
    }

    fn load_ram_data(&self, ram: &mut Vec<u8>) {
        log::error!("load save failed: unimplemented");
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), std::io::Error> {
        Err(std::io::Error::last_os_error())
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), std::io::Error> {
        Err(std::io::Error::last_os_error())
    }

    pub fn load_state(&self) -> Result<Vec<u8>, std::io::Error> {
        Err(std::io::Error::last_os_error())
    }
}
#[cfg(not(target_arch = "wasm32"))]
impl RomEntry {
    pub fn file_name(&self) -> std::borrow::Cow<str> {
        self.path
            .file_name()
            .map_or("".into(), |x| x.to_string_lossy())
    }

    pub fn from_path(path: PathBuf) -> Result<RomEntry, String> {
        let mut file = std::fs::File::open(path.clone()).map_err(|e| format!("io error: {}", e))?;
        let size = file
            .metadata()
            .map_err(|e| format!("io error: {}", e))?
            .len();
        let header = match gameroy::gameboy::cartridge::CartridgeHeader::from_reader(&mut file) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((_, e)) => return Err(e),
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
        Ok(RomEntry { name, size, path })
    }

    fn read(&self) -> Result<Vec<u8>, String> {
        let mut rom = Vec::new();
        let rom_path = &self.path;
        let file = &mut std::fs::File::open(&rom_path)
            .map_err(|x| format!("error loading '{}': {}", rom_path.display(), x))?;

        std::io::copy(file, &mut rom)
            .map_err(|x| format!("error reading '{}': {}", rom_path.display(), x))?;

        Ok(rom)
    }

    fn load_boot_rom() -> Option<[u8; 256]> {
        let boot_rom_path = if let Some(x) = &config().boot_rom {
            PathBuf::from(x)
        } else {
            return None;
        };

        let mut boot_rom = [0; 0x100];
        match Self::open_and_read(&boot_rom_path, &mut &mut boot_rom[..]) {
            Err(e) => {
                eprintln!("{}", e);
                return None;
            }
            Ok(_) => Some(boot_rom),
        }
    }

    fn load_ram_data(&self, ram: &mut Vec<u8>) {
        let save_path = self.save_path();
        log::info!("loading save at {}", save_path.display());
        let saved_ram = std::fs::read(&save_path);
        match saved_ram {
            Ok(save) => *ram = save,
            Err(err) => {
                log::error!("load save failed: {}", err);
            }
        }
    }

    fn save_path(&self) -> PathBuf {
        self.path.with_extension("sav")
    }

    fn save_state_path(&self) -> PathBuf {
        self.path.with_extension("save_state")
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), std::io::Error> {
        let save_path = self.save_path();
        std::fs::write(save_path, data)
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), std::io::Error> {
        let save_path = self.save_state_path();
        std::fs::write(save_path, state)
    }

    pub fn load_state(&self) -> Result<Vec<u8>, std::io::Error> {
        let save_path = self.save_state_path();
        std::fs::read(save_path)
    }
}

impl RomEntry {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn size(&self) -> u64 {
        self.size
    }

    fn open_and_read(
        rom_path: &std::path::Path,
        writer: &mut impl std::io::Write,
    ) -> Result<usize, String> {
        let file = &mut std::fs::File::open(&rom_path)
            .map_err(|x| format!("error loading '{}': {}", rom_path.display(), x))?;

        Ok(std::io::copy(file, writer)
            .map_err(|x| format!("error reading '{}': {}", rom_path.display(), x))?
            as usize)
    }

    pub fn load_gameboy(&self) -> Result<GameBoy, String> {
        let rom = self.read()?;
        let boot_rom = Self::load_boot_rom();

        let mut cartridge = Cartridge::new(rom).unwrap();
        log::info!("Cartridge type: {}", cartridge.kind_name());

        self.load_ram_data(&mut cartridge.ram);

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
                entry.file_name().into_owned(),
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
                        ctx.get::<EventLoopProxy<UserEvent>>()
                            .send_event(UserEvent::LoadRom(entry.clone()))
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
                        #[cfg(target_arch = "wasm32")]
                        let entry = match RomEntry::from_bytes(file.read().await.into_boxed_slice())
                        {
                            Ok(x) => x,
                            Err(e) => {
                                log::error!("failed to load rom: {}", e);
                                return;
                            }
                        };
                        #[cfg(not(target_arch = "wasm32"))]
                        let entry = match RomEntry::from_path(file.path().into()) {
                            Ok(x) => x,
                            Err(e) => {
                                log::error!(
                                    "failed to load rom from {}: {}",
                                    file.path().display(),
                                    e
                                );
                                return;
                            }
                        };
                        proxy.send_event(UserEvent::LoadRom(entry)).unwrap();
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
        name: "OH DEMO".to_string(),
        size: 32 << 10,
        rom: include_bytes!("../../roms/oh.gb")
            .to_vec()
            .into_boxed_slice()
            .into(),
        file_name: "oh.gb".to_string(),
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

#[cfg(not(target_arch = "wasm32"))]
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
        .filter_map(|x| RomEntry::from_path(x.path()).ok())
        .collect::<Vec<_>>();
    Ok(roms)
}
