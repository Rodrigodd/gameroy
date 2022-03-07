use std::str::FromStr;

use crui::layouts::{FitText, HBoxLayout, MarginLayout};
use crui::text::Text;
use crui::widgets::{Button, ListBuilder};
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

struct RomList {
    roms: Vec<String>,
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
        let name = self.roms[index].clone();
        let path = self.roms[index].clone();
        cb.layout(HBoxLayout::default())
            .graphic(style.background.clone())
            .child(ctx, |cb, ctx| {
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
                        style.text_style.clone().with_font_size(24.0),
                    ))
                    .layout(FitText)
                })
                .layout(MarginLayout::new([2.0; 4]))
            })
            .child(ctx, |cb, _| {
                cb.graphic(Text::new(
                    "Yeh".to_string(),
                    (0, 0),
                    style.text_style.clone(),
                ))
                .expand_x(true)
            })
    }
}

pub fn create_rom_loading_ui(gui: &mut crui::Gui, style: &Style) {
    let roms = std::fs::read_dir("roms/")
        .unwrap()
        .filter_map(|x| {
            let x = x.ok()?;
            if x.path().extension()? != "gb" {
                return None;
            }
            let mut path = std::path::PathBuf::from_str("roms/").ok()?;
            path.push(x.file_name());
            Some(path.to_string_lossy().into_owned())
        })
        .collect();
    crate::ui::list(gui.create_control(), gui, style, RomList { roms }).build(gui);
}
