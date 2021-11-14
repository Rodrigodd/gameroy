use std::sync::{mpsc::SyncSender, Arc};

use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

// use std::{rc::Rc, cell::RefCell};
use crate::{
    event_table::{EmulatorUpdated, EventTable},
    style::Style,
    EmulatorEvent, UserEvent,
};
use crui::{
    graphics::Text,
    layouts::VBoxLayout,
    widgets::{TextField, TextFieldCallback},
    Behaviour, Context, Gui, Id,
};
use gameroy::interpreter::Interpreter;

struct DisassemblerView {
    text: Id,
    _emulator_updated_event: crate::event_table::Handle<EmulatorUpdated>,
}
impl Behaviour for DisassemblerView {
    fn on_event(&mut self, event: Box<dyn std::any::Any>, _this: Id, ctx: &mut Context) {
        if event.is::<EmulatorUpdated>() {
            eprintln!(">> EMULATOR IPGAED");
            let mut text = String::new();
            {
                let inter = ctx.get::<Arc<Mutex<Interpreter>>>().lock();
                text += &format!("{}", inter.0.cpu);
                // let mut around = String::new();
                inter
                    .0
                    .trace
                    .borrow_mut()
                    .print_around(
                        inter.0.cartridge.curr_bank(),
                        inter.0.cpu.pc,
                        &inter.0,
                        &mut text,
                    )
                    .unwrap();
            }
            ctx.get_graphic_mut(self.text).set_text(&text);
        }
    }
}

struct Callback;
#[allow(unused_variables)]
impl TextFieldCallback for Callback {
    fn on_submit(&mut self, this: Id, ctx: &mut Context, text: &mut String) {
        let sender = ctx.get::<SyncSender<EmulatorEvent>>();
        let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
        let mut args = text.split_ascii_whitespace();
        match args.next() {
            Some("step" | "") | None => sender.send(EmulatorEvent::Step).unwrap(),
            Some("runto") => {
                if let Some(arg) = args.next() {
                    if !args.next().map(str::is_empty).unwrap_or(true) {
                        // report a error!!
                        return;
                    }
                    let address = match u16::from_str_radix(arg, 16) {
                        Ok(x) => x,
                        Err(_) => return,
                    };
                    sender.send(EmulatorEvent::RunTo(address)).unwrap();
                } else {
                    return;
                }
            }
            Some("run") => proxy.send_event(UserEvent::Debug(false)).unwrap(),
            _ => return,
        }
        text.clear();
    }

    fn on_change(&mut self, this: Id, ctx: &mut Context, text: &str) {}

    fn on_unfocus(&mut self, this: Id, ctx: &mut Context, text: &mut String) {}
}

pub fn build(parent: Id, gui: &mut Gui, event_table: &mut EventTable, style: &Style) {
    let diss_view_id = gui.reserve_id();
    let text_id = gui.reserve_id();
    let diss_view = DisassemblerView {
        text: text_id,
        _emulator_updated_event: event_table.register(diss_view_id),
    };

    let vbox = gui
        .create_control_reserved(diss_view_id)
        .graphic(style.background.clone())
        .parent(parent)
        .expand_y(true)
        .expand_x(true)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
        .behaviour(diss_view)
        .build(gui);

    list(vbox, gui, style, text_id);

    let caret = gui.reserve_id();
    let label = gui.reserve_id();

    let text_field = gui
        .create_control()
        .parent(vbox)
        .behaviour(TextField::new(
            caret,
            label,
            style.text_field.clone(),
            Callback,
        ))
        .min_size([20.0; 2])
        .build(gui);

    gui.create_control_reserved(caret)
        .parent(text_field)
        .graphic(style.background.clone().with_color([0, 0, 0, 255].into()))
        .anchors([0.0; 4])
        .build(gui);

    gui.create_control_reserved(label)
        .parent(text_field)
        .graphic(Text::new("test".into(), (-1, -1), style.text_style.clone()).into())
        .build(gui);
}

fn list<'a>(parent: Id, gui: &mut Gui, style: &Style, text_id: Id) {
    gui.create_control_reserved(text_id)
        .parent(parent)
        .expand_y(true)
        .graphic(
            Text::new(
                "This is a disassembler viewer!!".to_string(),
                (-1, 0),
                style.text_style.clone(),
            )
            .into(),
        )
        .build(gui);
}
