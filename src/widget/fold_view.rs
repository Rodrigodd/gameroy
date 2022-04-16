use giui::graphics::Graphic;
use giui::layouts::{FitGraphic, HBoxLayout, VBoxLayout};
use giui::text::Text;
use giui::{Behaviour, BuilderContext, Id};

use crate::style::Style;

#[derive(LoadStyle, Clone)]
pub struct FoldIcon {
    pub open: Graphic,
    pub close: Graphic,
}

pub struct FoldView {
    pub fold: bool,
    style: FoldIcon,
    icon: Id,
}
impl FoldView {
    fn update_fold(&mut self, ctx: &mut giui::Context, this: giui::Id) {
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
    fn on_active(&mut self, this: giui::Id, ctx: &mut giui::Context) {
        self.update_fold(ctx, this);
    }

    fn input_flags(&self) -> giui::InputFlags {
        giui::InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: giui::MouseInfo, this: giui::Id, ctx: &mut giui::Context) {
        match mouse.event {
            giui::MouseEvent::Enter => (),
            giui::MouseEvent::Exit => (),
            giui::MouseEvent::Down(_) => (),
            giui::MouseEvent::Up(_) => {
                self.fold = !self.fold;
                self.update_fold(ctx, this);
            }
            giui::MouseEvent::Moved => (),
            giui::MouseEvent::None => (),
        }
    }
}

pub fn folder(ctx: &mut dyn BuilderContext, title: String, style: &Style) -> giui::ControlBuilder {
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
