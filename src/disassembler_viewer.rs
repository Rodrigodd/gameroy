use std::{
    any::Any,
    sync::{mpsc::SyncSender, Arc},
};

use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

// use std::{rc::Rc, cell::RefCell};
use crate::{
    event_table::{EmulatorUpdated, EventTable},
    style::Style,
    EmulatorEvent, UserEvent,
};
use crui::{
    graphics::{Graphic, Text},
    layouts::{FitText, MarginLayout, VBoxLayout},
    text::TextStyle,
    widgets::{ListBuilder, SetScrollPosition, TextField, TextFieldCallback},
    BuilderContext, Context, ControlBuilder, Gui, Id, RectFill,
};
use gameroy::{
    disassembler::{Address, Directive},
    interpreter::Interpreter,
};

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

struct DissasemblerList {
    text_style: TextStyle,
    list: Id,
    reg: Id,
    pc: Option<Address>,
    directives: Vec<Directive>,
    _emulator_updated_event: crate::event_table::Handle<EmulatorUpdated>,
}
impl DissasemblerList {
    fn graphic(
        &mut self,
        direc: Directive,
        trace: std::cell::Ref<gameroy::disassembler::Trace>,
        pc: Option<Address>,
    ) -> Graphic {
        let curr = direc.address;
        let mut text = format!("{:16} ", trace.labels.get(&curr).map_or("", |x| &x.name));
        let label = |pc, x| {
            if let Some(address) = trace.jumps.get(&pc) {
                let name = trace.labels.get(&address).unwrap().name.clone();
                return name;
            }
            format!("${:04x}", x)
        };
        gameroy::disassembler::disassembly_opcode(
            direc.address.address,
            &direc.op[0..direc.len as usize],
            |x| label(curr, x),
            &mut text,
        )
        .unwrap();
        let mut style = self.text_style.clone();
        if Some(curr) == pc {
            style.color = [255, 0, 0, 255].into();
        }
        let graphic = Text::new(text, (-1, 0), style).into();
        graphic
    }
}
impl ListBuilder for DissasemblerList {
    fn on_event(&mut self, event: Box<dyn Any>, _this: Id, ctx: &mut Context) {
        if event.is::<EmulatorUpdated>() {
            let (scroll_value, reg_text) = {
                let inter = ctx.get::<Arc<Mutex<Interpreter>>>().lock();

                let cpu = &inter.0.cpu;
                let reg_text = format!(
                    "AF: {:02x} {:02x}\nBC: {:02x} {:02x}\nDE: {:02x} {:02x}\nHL: {:02x} {:02x}\nSP: {:04x} \nPC: {:04x}",
                    cpu.a, cpu.f.0, cpu.b, cpu.c, cpu.d, cpu.e, cpu.h, cpu.l, cpu.sp, cpu.pc
                );

                let pc = cpu.pc;
                let bank = inter.0.cartridge.curr_bank();
                self.pc = Address::from_pc(Some(bank), pc);
                let pc = match self.pc {
                    Some(x) => x,
                    _ => return,
                };

                let trace = inter.0.trace.borrow();

                self.directives.clear();
                self.directives.extend(trace.directives.iter().cloned());

                let pos = trace.directives.iter().position(|x| x.address == pc);
                let pos = match pos {
                    Some(x) => x,
                    _ => return,
                };

                let len = trace.directives.len();
                let scroll_value = pos as f32 / len as f32;

                (scroll_value, reg_text)
            };

            if let Graphic::Text(text) = ctx.get_graphic_mut(self.reg) {
                text.set_text(&reg_text);
            }

            // scroll disassembly list to the current program counter
            ctx.send_event_to(
                self.list,
                SetScrollPosition {
                    vertical: true,
                    value: scroll_value,
                },
            );
        }
    }

    fn item_count(&mut self, _ctx: &mut dyn crui::BuilderContext) -> usize {
        self.directives.len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: Id,
        cb: crui::ControlBuilder,
        ctx: &mut dyn crui::BuilderContext,
    ) -> crui::ControlBuilder {
        let inter = ctx.get::<Arc<Mutex<Interpreter>>>().lock();

        let trace = inter.0.trace.borrow();
        let directive = self.directives[index].clone();

        let pc = inter.0.cpu.pc;
        let bank = inter.0.cartridge.curr_bank();
        let pc = Address::from_pc(Some(bank), pc);
        let graphic = self.graphic(directive, trace, pc);
        cb.graphic(graphic).layout(FitText)
    }

    fn update_item(&mut self, index: usize, item_id: Id, ctx: &mut dyn BuilderContext) {
        let inter = ctx.get::<Arc<Mutex<Interpreter>>>().lock();

        let trace = inter.0.trace.borrow();
        let directive = self.directives[index].clone();

        let graphic = self.graphic(directive, trace, self.pc);
        drop(inter);
        *ctx.get_graphic_mut(item_id) = graphic;
    }
}

pub fn build(parent: Id, gui: &mut Gui, event_table: &mut EventTable, style: &Style) {
    let diss_view_id = gui.reserve_id();
    let list_id = gui.reserve_id();
    let reg_id = gui.reserve_id();

    let vbox = gui
        .create_control_reserved(diss_view_id)
        .graphic(style.background.clone())
        .parent(parent)
        .expand_y(true)
        .expand_x(true)
        .min_width(100.0)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
        .build(gui);

    let stack = gui.create_control().parent(vbox).expand_y(true).build(gui);

    list(
        gui.create_control_reserved(list_id),
        gui,
        style,
        DissasemblerList {
            text_style: style.text_style.clone(),
            list: list_id,
            reg: reg_id,
            pc: None,
            directives: Vec::new(),
            _emulator_updated_event: event_table.register(list_id),
        },
    )
    .parent(stack)
    .build(gui);

    let caret = gui.reserve_id();
    let label = gui.reserve_id();

    let _reg_view = gui
        .create_control()
        .child_reserved(reg_id, gui, |cb, _| {
            cb.graphic(
                Text::new(
                    format!(
                        "AF: {:02x} {:02x}\nBC: {:02x} {:02x}\nDE: {:02x} {:02x}\nHL: {:02x} {:02x}\nSP: {:04x} \nPC: {:04x}",
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                    ),
                    (-1, 0),
                    style.text_style.clone(),
                )
                .into(),
            )
            .layout(FitText)
        })
        .parent(stack)
        .layout(MarginLayout::default())
        .graphic(style.background.clone())
        .fill_x(RectFill::ShrinkEnd)
        .fill_y(RectFill::ShrinkEnd)
        .build(gui);

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

fn list(
    cb: ControlBuilder,
    ctx: &mut impl BuilderContext,
    style: &Style,
    list_builder: impl ListBuilder + 'static,
) -> ControlBuilder {
    use crui::widgets::{List, ScrollBar, ViewLayout};
    let scroll_view = cb.id();
    let view = ctx
        .create_control()
        .parent(scroll_view)
        .layout(ViewLayout::new(false, true))
        .build(ctx);
    let h_scroll_bar_handle = ctx.reserve();
    let h_scroll_bar = ctx
        .create_control()
        .min_size([10.0, 10.0])
        .parent(scroll_view)
        .behaviour(ScrollBar::new(
            h_scroll_bar_handle,
            scroll_view,
            false,
            style.scrollbar.clone(),
        ))
        .build(ctx);
    let h_scroll_bar_handle = ctx
        .create_control_reserved(h_scroll_bar_handle)
        .min_size([10.0, 10.0])
        .parent(h_scroll_bar)
        .build(ctx);
    let v_scroll_bar_handle = ctx.reserve();
    let v_scroll_bar = ctx
        .create_control()
        .min_size([10.0, 10.0])
        // .graphic(style.scroll_background.clone())
        .parent(scroll_view)
        .behaviour(ScrollBar::new(
            v_scroll_bar_handle,
            scroll_view,
            true,
            style.scrollbar.clone(),
        ))
        .build(ctx);
    let v_scroll_bar_handle = ctx
        .create_control_reserved(v_scroll_bar_handle)
        .min_size([10.0, 10.0])
        .parent(v_scroll_bar)
        .build(ctx);

    cb.behaviour_and_layout(List::new(
        10.0,
        0.0,
        [10.0; 4],
        view,
        v_scroll_bar,
        v_scroll_bar_handle,
        h_scroll_bar,
        h_scroll_bar_handle,
        list_builder,
    ))
}
