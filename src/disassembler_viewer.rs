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
        let mut args: Vec<_> = text.split_ascii_whitespace().collect();
        if args.len() == 0 {
            args.push("");
        }
        match args[0] {
            "step" | "" => sender.send(EmulatorEvent::Step).unwrap(),
            "runto" => {
                if args.len() != 2 {
                    // report a error!!
                    return;
                }
                let address = match u16::from_str_radix(args[1], 16) {
                    Ok(x) => x,
                    Err(_) => return,
                };
                sender.send(EmulatorEvent::RunTo(address)).unwrap();
            }
            "run" => sender.send(EmulatorEvent::Run).unwrap(),
            "break" => {
                if args.len() != 3 {
                    // report a error!!
                    return;
                }

                let address = match u16::from_str_radix(args[2], 16) {
                    Ok(x) => x,
                    Err(_) => return,
                };

                let write = args[1].contains('w') as u8;
                let execute = args[1].contains('x') as u8;
                let jump = args[1].contains('j') as u8;

                use crate::emulator::break_flags::*;

                let flags = (write * WRITE) | (execute * EXECUTE) | (jump * JUMP);

                sender
                    .send(EmulatorEvent::AddBreakpoint { flags, address })
                    .unwrap();
            }
            // write the currently dissasembly to a file
            "dump" => {
                if args.len() != 2 {
                    // report a error!!
                    return;
                }
                let file = args[1];
                let game_boy = &ctx.get::<Arc<Mutex<Interpreter>>>().lock().0;
                let trace = game_boy.trace.borrow();
                let mut string = String::new();
                trace.fmt(game_boy, &mut string).unwrap();
                std::fs::write(file, string).unwrap();
            }
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

                let trace = inter.0.trace.borrow();

                self.directives.clear();
                self.directives.extend(trace.directives.iter().cloned());

                let pc = cpu.pc;
                let bank = inter.0.cartridge.curr_bank();
                self.pc = Address::from_pc(Some(bank), pc);
                let pc = match self.pc {
                    Some(x) => x,
                    _ => Address { address: pc, bank },
                };

                let mut scroll_value = None;
                let pos = self.directives.binary_search_by(|x| x.address.cmp(&pc));
                match pos {
                    Ok(pos) => {
                        let len = trace.directives.len();
                        scroll_value = Some(pos as f32 / len as f32);
                    }
                    _ => {}
                };

                (scroll_value, reg_text)
            };

            if let Graphic::Text(text) = ctx.get_graphic_mut(self.reg) {
                text.set_text(&reg_text);
            }

            // scroll disassembly list to the current program counter
            if let Some(value) = scroll_value {
                ctx.send_event_to(
                    self.list,
                    SetScrollPosition {
                        vertical: true,
                        value,
                    },
                );
            }
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

pub fn build(
    parent: Id,
    ctx: &mut dyn BuilderContext,
    event_table: &mut EventTable,
    style: &Style,
) {
    let diss_view_id = ctx.reserve();
    let list_id = ctx.reserve();
    let reg_id = ctx.reserve();

    let vbox = ctx
        .create_control_reserved(diss_view_id)
        .graphic(style.background.clone())
        .parent(parent)
        .expand_y(true)
        .expand_x(true)
        .min_width(100.0)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
        .build(ctx);

    let stack = ctx.create_control().parent(vbox).expand_y(true).build(ctx);

    list(
        ctx.create_control_reserved(list_id),
        ctx,
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
    .build(ctx);

    let caret = ctx.reserve();
    let label = ctx.reserve();

    let _reg_view = ctx
        .create_control()
        .child_reserved(reg_id, ctx, |cb, _| {
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
        .build(ctx);

    let text_field = ctx
        .create_control()
        .parent(vbox)
        .behaviour(TextField::new(
            caret,
            label,
            style.text_field.clone(),
            Callback,
        ))
        .min_size([20.0; 2])
        .build(ctx);

    ctx.create_control_reserved(caret)
        .parent(text_field)
        .graphic(style.background.clone().with_color([0, 0, 0, 255].into()))
        .anchors([0.0; 4])
        .build(ctx);

    ctx.create_control_reserved(label)
        .parent(text_field)
        .graphic(Text::new("test".into(), (-1, -1), style.text_style.clone()).into())
        .build(ctx);
}

fn list(
    cb: ControlBuilder,
    ctx: &mut (impl BuilderContext + ?Sized),
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
