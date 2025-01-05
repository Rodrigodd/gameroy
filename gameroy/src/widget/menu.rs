use std::{cell::RefCell, rc::Rc};

use giui::{
    layouts::{FitGraphic, MarginLayout, VBoxLayout},
    text::Text,
    widgets::{Blocker, Button},
    BuilderContext, Context, Id, RectFill,
};

use crate::style::Style;

pub type MenuOption<'a> = (&'a str, Box<dyn FnMut(&mut Context)>);

pub fn create_menu(
    options: Vec<MenuOption>,
    on_close: impl Fn(&mut Context) + 'static,
    ctx: &mut Context,
    style: &Style,
) -> Id {
    let [menu, blocker] = [(); 2].map(|_| ctx.reserve());

    let alpha = style.blocker.get_color().a;
    let anim_id = ctx.add_animation(
        0.200,
        move |t: f32, _dt: f32, _length: f32, ctx: &mut Context| {
            ctx.get_graphic_mut(blocker)
                .set_alpha((t * alpha as f32) as u8);
            let t = 1.0 - (1.0 - t) * (1.0 - t); // quadrac easy-in
            ctx.set_anchor_bottom(menu, 1.0 + (1.0 - t) * 0.5);
        },
    );

    // This could be replaced by `let close: impl Fn(...)` when stabilized (issue #63065).
    fn fn_once<F: FnOnce(&mut Context)>(x: F) -> F {
        x
    }

    let mut on_close = Some(on_close);
    let close = fn_once(move |ctx: &mut Context| {
        let on_close = match on_close.take() {
            Some(x) => x,
            _ => return,
        };
        let mut on_close = Some(on_close);
        ctx.add_animation(
            0.150,
            move |t: f32, _dt: f32, _length: f32, ctx: &mut Context| {
                if t == 1.0 {
                    ctx.remove(menu);
                    ctx.remove(blocker);
                    ctx.remove_animation(anim_id);
                    if let Some(x) = on_close.take() {
                        x(ctx)
                    }
                    return;
                }

                if !ctx.is_active(blocker) {
                    return;
                }

                let t = 1.0 - t;
                ctx.get_graphic_mut(blocker)
                    .set_alpha((t * alpha as f32) as u8);
                let t = 1.0 - (1.0 - t) * (1.0 - t); // quadrac easy-in
                ctx.set_anchor_bottom(menu, 1.0 + (1.0 - t));
            },
        );
    });
    let close = Rc::new(RefCell::new(Some(close)));

    let close_ = close.clone();
    let _blocker = ctx
        .create_control_reserved(blocker)
        .parent(Id::ROOT_ID)
        .behaviour(Blocker::new(move |_, ctx| {
            if let Some(x) = close_.take() {
                x(ctx)
            }
        }))
        .graphic(style.blocker.clone())
        .build(ctx);

    let _menu = ctx
        .create_control_reserved(menu)
        .parent(Id::ROOT_ID)
        .layout(VBoxLayout::new(1.0, [0.0, 10.0, 0.0, 10.0], -1))
        .graphic(style.split_background.clone())
        .fill_y(RectFill::ShrinkEnd)
        .fill_x(RectFill::ShrinkCenter)
        .build(ctx);

    for (text, mut function) in options {
        let close = close.clone();
        ctx.create_control()
            .parent(menu)
            .layout(MarginLayout::new([10.0, 10.0, 10.0, 10.0]))
            .behaviour(Button::new(
                style.delete_button.clone(),
                false,
                move |_, ctx| {
                    // make sure to only call `function` if `close` is taken, to avoid clicking on
                    // the button while the menu is closing.
                    if let Some(close) = close.take() {
                        function(ctx);
                        (close)(ctx);
                    }
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
