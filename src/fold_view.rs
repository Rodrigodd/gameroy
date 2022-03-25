use crui::graphics::Graphic;
use crui::layouts::{FitGraphic, HBoxLayout, VBoxLayout};
use crui::text::Text;
use crui::{Behaviour, BuilderContext, Id};

use crate::style::Style;

#[derive(LoadStyle, Clone)]
pub struct FoldIcon {
    open: Graphic,
    close: Graphic,
}

pub struct FoldView {
    pub fold: bool,
    style: FoldIcon,
    icon: Id,
}
impl FoldView {
    fn update_fold(&mut self, ctx: &mut crui::Context, this: crui::Id) {
        ctx.set_graphic(
            self.icon,
            [&self.style.close, &self.style.open][self.fold as usize].clone(),
        );
        for c in ctx
            .get_all_children(this)
            .iter()
            .skip(1)
            .cloned()
            .collect::<Vec<_>>()
        {
            if self.fold {
                ctx.deactive(c);
            } else {
                ctx.active(c);
            }
        }
    }
}
impl Behaviour for FoldView {
    fn on_active(&mut self, this: crui::Id, ctx: &mut crui::Context) {
        self.update_fold(ctx, this);
    }

    fn input_flags(&self) -> crui::InputFlags {
        crui::InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: crui::MouseInfo, this: crui::Id, ctx: &mut crui::Context) {
        match mouse.event {
            crui::MouseEvent::Enter => (),
            crui::MouseEvent::Exit => (),
            crui::MouseEvent::Down(_) => (),
            crui::MouseEvent::Up(_) => {
                self.fold = !self.fold;
                self.update_fold(ctx, this);
            }
            crui::MouseEvent::Moved => (),
            crui::MouseEvent::None => (),
        }
    }
}

pub fn folder(ctx: &mut dyn BuilderContext, title: String, style: &Style) -> crui::ControlBuilder {
    let icon = ctx.reserve();
    let this = ctx
        .create_control()
        .behaviour(FoldView {
            fold: false,
            style: style.fold_icon.clone(),
            icon,
        })
        .layout(VBoxLayout::new(1.0, [0.0; 4], -1));

    let _header = ctx
        .create_control()
        .parent(this.id())
        .graphic(style.header_background.clone())
        .layout(HBoxLayout::new(0.0, [0.0, 0.0, 0.0, 1.0], -1))
        .child_reserved(icon, ctx, |cb, _| {
            cb.graphic(style.fold_icon.open.clone()).layout(FitGraphic)
        })
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(title, (-1, 0), style.text_style.clone()))
                .layout(FitGraphic)
        })
        .build(ctx);

    this
}
