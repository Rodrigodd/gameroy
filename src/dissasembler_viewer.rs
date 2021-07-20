use std::sync::{Arc, mpsc::SyncSender};

use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

// use std::{rc::Rc, cell::RefCell};
use crate::{EmulatorEvent, UserEvent, event_table::{EmulatorUpdated, EventTable}, style::Style};
use crui::{
    graphics::Text,
    layouts::VBoxLayout,
    widgets::{TextField, TextFieldCallback},
    Context, Gui, Id,
};
use gameroy::interpreter::Interpreter;

#[derive(Clone, Copy)]
struct DissasemblerView {
    text: Id,
}

struct Callback;
#[allow(unused_variables)]
impl TextFieldCallback for Callback {
    fn on_submit(&mut self, this: Id, ctx: &mut Context, text: &mut String) -> bool {
        let sender = ctx.get::<SyncSender<EmulatorEvent>>();
        let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
        let mut args = text.split_ascii_whitespace();
        match args.next() {
            Some("step" | "") | None => sender.send(EmulatorEvent::Step).unwrap(),
            Some("runto") => {
                if let Some(arg) = args.next() {
                    if !args.next().map(str::is_empty).unwrap_or(true) {
                        // report a error!!
                        return true;
                    }
                    let address = match u16::from_str_radix(arg, 16) {
                        Ok(x) => x,
                        Err(_) => return true,
                    };
                    sender.send(EmulatorEvent::RunTo(address)).unwrap();
                } else {
                    return true;
                }
            }
            Some("run") => proxy.send_event(UserEvent::Debug(false)).unwrap(),
            _ => return true,
        }
        text.clear();
        true
    }

    fn on_change(&mut self, this: Id, ctx: &mut Context, text: &str) {}

    fn on_unfocus(&mut self, this: Id, ctx: &mut Context, text: &mut String) -> bool {
        true
    }
}

pub fn build(parent: Id, gui: &mut Gui, event_table: &mut EventTable, style: &Style) {
    let diss_view = DissasemblerView {
        text: gui.reserve_id(),
    };

    let vbox = gui
        .create_control()
        .graphic(style.background.clone())
        .parent(parent)
        .expand_y(true)
        .expand_x(true)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
        .build();

    list(vbox, gui, style, diss_view, event_table);

    let caret = gui.reserve_id();
    let label = gui.reserve_id();

    let text_field = gui
        .create_control()
        .parent(vbox)
        .behaviour(TextField::new(
            "test".to_string(),
            caret,
            label,
            style.text_field.clone(),
            Callback,
        ))
        .min_size([20.0; 2])
        .build();

    gui.create_control_reserved(caret)
        .parent(text_field)
        .graphic(style.background.clone().with_color([0,0,0,255]))
        .anchors([0.0; 4])
        .build();

    gui.create_control_reserved(label)
        .parent(text_field)
        .graphic(Text::new("Testando!!".into(), (-1, -1), style.text_style.clone()).into())
        .build();
}

fn list<'a>(
    parent: Id,
    gui: &mut Gui,
    style: &Style,
    diss_view: DissasemblerView,
    event_table: &mut EventTable,
) {
    gui.create_control_reserved(diss_view.text)
        .parent(parent)
        .expand_y(true)
        .graphic(
            Text::new(
                "This is a dissasembler viewer!!".to_string(),
                (-1, 0),
                style.text_style.clone(),
            )
            .into(),
        )
        .behaviour(
            event_table.register::<EmulatorUpdated, _>(move |_: (), ctx: &mut Context| {
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
                ctx.get_graphic_mut(diss_view.text)
                    .set_text(&text);
            }),
        )
        .build();
}
