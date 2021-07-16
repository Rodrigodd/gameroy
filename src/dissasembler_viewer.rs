use std::{borrow::Borrow, cell::RefCell, rc::Rc, sync::Arc};

use parking_lot::Mutex;

// use std::{rc::Rc, cell::RefCell};
use crate::{
    event_table::{Event, EventTable, FrameUpdated},
    style::Style,
};
use crui::{
    graphics::{Text, TextStyle},
    ControlBuilder, Gui, Id,
};
use gameroy::interpreter::Interpreter;

struct DissasemblerView {
    text: Id,
}

pub fn build(mut cb: ControlBuilder, event_table: &mut EventTable, style: &Style) {
    let diss_view = Rc::new(RefCell::new(DissasemblerView { text: cb.reserve() }));
    let _text = cb
        .graphic(style.background.clone())
        .expand_y(true)
        .expand_x(true)
        .child_reserved(diss_view.borrow_mut().text, |cb| {
            list(cb, style, diss_view.clone(), event_table)
        })
        .build();
}

fn list<'a>(
    cb: ControlBuilder<'a>,
    style: &Style,
    diss_view: Rc<RefCell<DissasemblerView>>,
    event_table: &mut EventTable,
) -> ControlBuilder<'a> {
    cb.graphic(
        Text::new(
            "This is a dissasembler viewer!!".to_string(),
            (-1, 0),
            style.text_style.clone(),
        )
        .into(),
    )
    .behaviour(event_table.register::<FrameUpdated, _>(move |_, ctx| {
        let mut text = String::new();
        {
            let inter = ctx.get::<Arc<Mutex<Interpreter>>>().lock();
            text += &format!("{}", inter.0.cpu);
            // let mut around = String::new();
            inter
                .0
                .trace
                .borrow_mut()
                .print_around(inter.0.cpu.pc, &inter.0, &mut text)
                .unwrap();
        }
        ctx.get_graphic_mut(diss_view.borrow_mut().text)
            .set_text(&text);
    }))
}
