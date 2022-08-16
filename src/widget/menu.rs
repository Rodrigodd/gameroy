use std::cell::RefCell;
use std::rc::Rc;

use giui::layouts::{FitGraphic, MarginLayout, VBoxLayout};
use giui::text::Text;
use giui::widgets::{Blocker, Button};
use giui::{BuilderContext, Context, Id, RectFill};

use crate::style::Style;

pub type MenuOption<'a> = (&'a str, Box<dyn FnMut(&mut Context)>);

pub fn create_menu(
    options: Vec<MenuOption>,
    on_close: impl Fn(&mut Context) + 'static,
    ctx: &mut impl BuilderContext,
    style: &Style,
) -> Id {
    let on_close = Rc::new(RefCell::new(Some(on_close)));
    let [menu, blocker] = [(); 2].map(|_| ctx.reserve());
    let close = move |ctx: &mut Context| {
        ctx.remove(menu);
        ctx.remove(blocker);
        on_close.borrow_mut().take().map(|x| x(ctx));
    };
    let close_ = close.clone();
    let _blocker = ctx
        .create_control_reserved(blocker)
        .parent(Id::ROOT_ID)
        .behaviour(Blocker::new(move |_, ctx| {
            (close_)(ctx);
        }))
        .graphic(style.blocker.clone())
        .build(ctx);

    let _menu = ctx
        .create_control_reserved(menu)
        .parent(Id::ROOT_ID)
        .layout(VBoxLayout::new(0.0, [0.0; 4], -1))
        .fill_y(RectFill::ShrinkEnd)
        .fill_x(RectFill::ShrinkCenter)
        .build(ctx);

    for (text, mut function) in options {
        let close = close.clone();
        ctx.create_control()
            .parent(menu)
            .layout(MarginLayout::new([0.0, 10.0, 0.0, 10.0]))
            .behaviour(Button::new(
                style.delete_button.clone(),
                false,
                move |_, ctx| {
                    function(ctx);
                    (close)(ctx);
                },
            ))
            .child(ctx, |cb, _| {
                cb.graphic(Text::new(
                    text.to_string(),
                    (-1, 0),
                    style.text_menu.clone(),
                ))
                .layout(FitGraphic)
            })
            .build(ctx);
    }

    menu
}
