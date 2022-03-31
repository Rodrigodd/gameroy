use std::{any::Any, ops::Range, sync::Arc, usize};

use gameroy::{
    debugger::{break_flags, Debugger},
    disassembler::{Address, Directive},
    gameboy::GameBoy,
};
use giui::{
    event::SetValue,
    graphics::{Graphic, Text},
    layouts::{FitGraphic, HBoxLayout},
    text::{Span, TextStyle},
    widgets::{
        Button, FocusItem, InteractiveText, ListBuilder, TextField, TextFieldCallback, UpdateItems,
    },
    BuilderContext, Color, Context, ControlBuilder, Id, MouseEvent, MouseInfo,
};
use parking_lot::Mutex;
use winit::event::VirtualKeyCode;

use crate::{
    event_table::{self, BreakpointsUpdated, EmulatorUpdated, EventTable, Handle, WatchsUpdated},
    fold_view,
    style::Style,
    ui,
};

struct Callback {
    /// A list of past submitted texts, that allow to be reused by pressing `UpArrow`.
    history: Vec<String>,
    curr: usize,
}
impl Callback {
    fn new() -> Self {
        Self {
            history: Vec::new(),
            curr: 0,
        }
    }
}
impl TextFieldCallback for Callback {
    fn on_submit(&mut self, _this: Id, ctx: &mut Context, text: &mut String) {
        let mut debugger = ctx.get::<Arc<Mutex<Debugger>>>().lock();
        let gb = ctx.get::<Arc<Mutex<GameBoy>>>().lock();
        let mut args: Vec<&str> = text.split_ascii_whitespace().collect();
        if args.len() == 0 {
            args.push("");
        }
        match debugger.execute_command(&*gb, &args) {
            Ok(_) => {}
            Err(x) => eprintln!("{}", x),
        }

        if !text.trim().is_empty() {
            // don't add to history if it is the same text again and again
            if self.history.last() != Some(text) {
                self.history.push(text.clone());
            }
        }
        self.curr = self.history.len();

        text.clear();
    }

    fn on_change(&mut self, _this: Id, _ctx: &mut Context, _text: &str) {}

    fn on_unfocus(&mut self, _this: Id, _ctx: &mut Context, _text: &mut String) {}

    fn on_keyboard_event(
        &mut self,
        event: giui::KeyboardEvent,
        this: Id,
        ctx: &mut Context,
    ) -> bool {
        use giui::KeyboardEvent::*;
        match event {
            Pressed(VirtualKeyCode::Up) if self.curr > 0 => {
                self.curr -= 1;
                let text = match self.history.get(self.curr) {
                    Some(x) => x.clone(),
                    None => String::new(),
                };
                ctx.send_event_to(this, SetValue(text));
                true
            }
            Pressed(VirtualKeyCode::Down) if self.curr < self.history.len() => {
                self.curr += 1;
                let text = match self.history.get(self.curr) {
                    Some(x) => x.clone(),
                    None => String::new(),
                };
                ctx.send_event_to(this, SetValue(text));
                true
            }
            Pressed(VirtualKeyCode::Escape) => {
                self.curr = self.history.len();
                ctx.send_event_to(this, SetValue(String::new()));
                true
            }
            _ => false,
        }
    }
}

struct JumpToAddress {
    from_address: Address,
}

struct DissasemblerList {
    list: Id,
    cpu: Id,
    ppu: Id,
    pc: Option<Address>,
    directives: Vec<Directive>,
    items_are_dirty: bool,
    _emulator_updated_event: Handle<EmulatorUpdated>,
}
impl DissasemblerList {
    fn graphic(
        &mut self,
        style: TextStyle,
        direc: Directive,
        trace: std::cell::Ref<gameroy::disassembler::Trace>,
        pc: Option<Address>,
    ) -> (Graphic, Option<Range<usize>>) {
        let curr = direc.address;
        let mut text = format!(
            "{:04x} {:16} ",
            {
                let mut address = curr.address;
                if address < 0x4000 && curr.bank != 0 {
                    address += 0x4000;
                }
                address
            },
            trace
                .labels
                .get(&curr)
                .map(|x| x.name.as_str())
                .or_else(|| trace.ram_labels.get(&curr.address).map(|x| x.as_str()))
                .unwrap_or("")
        );
        let label = |pc, x| {
            if let Some(address) = trace.jumps.get(&pc) {
                let mut name = trace.labels.get(&address).unwrap().name.clone();
                name.insert_str(0, "<l>");
                name += "</l>";
                return name;
            }
            if let Some(name) = trace.ram_labels.get(&x) {
                let mut name = name.clone();
                name.insert_str(0, "<l>");
                name += "</l>";
                return name;
            }
            format!("<a>${:04x}</a>", x)
        };
        gameroy::disassembler::disassembly_opcode(
            direc.address.address,
            &direc.op[0..direc.len as usize],
            |x| label(curr, x),
            &mut text,
        )
        .unwrap();
        let label_range = if let Some(start) = text.find("<l>") {
            let end = text.find("</l>").unwrap() - 3;
            text.replace_range(start..start + 3, "");
            text.replace_range(end..end + 4, "");
            Some(start..end)
        } else {
            None
        };
        let address_range = if let Some(start) = text.find("<a>") {
            let end = text.find("</a>").unwrap() - 3;
            text.replace_range(start..start + 3, "");
            text.replace_range(end..end + 4, "");
            Some(start..end)
        } else {
            None
        };
        let op_len = text[22..].find(" ").unwrap();

        let mut text = Text::new(text, (-1, 0), style);

        let label = 0x2e8bb2ff.into();
        let op = 0xff1a1aff.into();
        let number = 0xd79314ff.into();
        let address = 0x6f7e67ff.into();

        text.add_span(0..4, Span::Color(address));
        text.add_span(5..21, Span::Color(label));
        label_range
            .as_ref()
            .map(|r| text.add_span(r.clone(), Span::Color(label)));
        text.add_span(22..22 + op_len, Span::Color(op));
        address_range.map(|r| text.add_span(r, Span::Color(number)));
        if Some(curr) == pc {
            text.add_span(
                0..text.len(),
                Span::Selection {
                    bg: Color::BLACK,
                    fg: None,
                },
            );
        }
        (text.into(), label_range)
    }
}
impl ListBuilder for DissasemblerList {
    fn on_event(&mut self, event: Box<dyn Any>, _this: Id, ctx: &mut Context) {
        if event.is::<EmulatorUpdated>() {
            let gb = ctx.get::<Arc<Mutex<GameBoy>>>().clone();
            let gb = gb.lock();

            fn decimal_mark(n: u64) -> String {
                let s = n.to_string();
                let mut result = String::with_capacity(s.len() + ((s.len() - 1) / 3));
                let mut i = s.len();
                for c in s.chars() {
                    result.push(c);
                    i -= 1;
                    if i > 0 && i % 3 == 0 {
                        result.push(' ');
                    }
                }
                result
            }

            let cpu = &gb.cpu;
            let cpu_text = format!(
                " clock: {}
 AF: {:02x} {:02x}
 BC: {:02x} {:02x}
 DE: {:02x} {:02x}
 HL: {:02x} {:02x}
 SP: {:04x}
 PC: {:04x}
 DIV:{:04x}",
                decimal_mark(gb.clock_count),
                cpu.a,
                cpu.f.0,
                cpu.b,
                cpu.c,
                cpu.d,
                cpu.e,
                cpu.h,
                cpu.l,
                cpu.sp,
                cpu.pc,
                gb.timer.div,
            );

            if let Graphic::Text(text) = ctx.get_graphic_mut(self.cpu) {
                text.set_string(&cpu_text);
            }

            let ppu = gb.ppu.borrow();
            let ppu_text = format!(
                " LCDC:{:02x}
 STAT:{:02x}
 SCY: {:02x}
 SCX: {:02x}
 LYC: {:02x}
 LY:  {:02x}
 LX:  {:02x}
 BGP: {:02x}
 OBP0:{:02x}
 OBP1:{:02x}
 WYC: {:02x}
 WY:  {:02x}
 WX:  {:02x}
 state: {}
 next: {}",
                ppu.lcdc,
                ppu.stat,
                ppu.scy,
                ppu.scx,
                ppu.lyc,
                ppu.ly,
                (gb.clock_count - ppu.line_start_clock_count) % 456,
                ppu.bgp,
                ppu.obp0,
                ppu.obp1,
                ppu.wyc,
                ppu.wy,
                ppu.wx,
                ppu.state,
                decimal_mark(ppu.next_clock_count),
            );

            if let Graphic::Text(text) = ctx.get_graphic_mut(self.ppu) {
                text.set_string(&ppu_text);
            }

            let trace = gb.trace.borrow();

            self.items_are_dirty = true;
            self.directives.clear();
            self.directives.extend(trace.directives.iter().cloned());
            self.directives
                .extend(trace.ram_directives.iter().map(|&(address, op, len)| {
                    // TODO: I am violating my own rule about Address being only rom addresses.
                    // Maybe I should not have this rule, or have multiple address Types.
                    Directive {
                        address: Address {
                            bank: 0xFF,
                            address,
                        },
                        len: len as u16,
                        op,
                    }
                }));
            debug_assert!(self.directives.windows(2).all(|x| x[0] <= x[1]));

            let pc = cpu.pc;
            let bank = gb.cartridge.curr_bank();
            self.pc = Some(Address::from_pc(Some(bank), pc).unwrap_or(Address {
                address: pc,
                bank: 0xFF,
            }));
            let pc = self.pc.unwrap();

            let pos = self.directives.binary_search_by(|x| x.address.cmp(&pc));
            if let Ok(pos) = pos {
                ctx.send_event_to(
                    self.list,
                    FocusItem {
                        index: pos,
                        margin: 30.0,
                    },
                );
            };
        } else if let Some(JumpToAddress { from_address }) = event.downcast_ref::<JumpToAddress>() {
            let gb = ctx.get::<Arc<Mutex<GameBoy>>>().lock();
            let trace = gb.trace.borrow_mut();
            let jump_to = trace.jumps.get(&from_address).unwrap();
            let pos = self
                .directives
                .binary_search_by(|x| x.address.cmp(&jump_to));
            drop(trace);
            drop(gb);
            if let Ok(pos) = pos {
                ctx.send_event_to(
                    self.list,
                    FocusItem {
                        index: pos,
                        margin: 30.0,
                    },
                );
            };
        }
    }

    fn item_count(&mut self, _ctx: &mut dyn giui::BuilderContext) -> usize {
        self.directives.len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: Id,
        cb: giui::ControlBuilder,
        ctx: &mut dyn giui::BuilderContext,
    ) -> giui::ControlBuilder {
        cb.min_size([0.0, 15.0]).child(ctx, |cb, ctx| {
            let inter = ctx.get::<Arc<Mutex<GameBoy>>>().lock();

            let trace = inter.trace.borrow();
            let directive = self.directives[index].clone();
            let style = ctx.get::<Style>().text_style.clone();
            let (graphic, label_range) = self.graphic(style, directive.clone(), trace, self.pc);
            let cb = cb.graphic(graphic).layout(FitGraphic);
            let mut span = 0;
            if let Some(label_range) = label_range {
                cb.behaviour(InteractiveText::new(vec![(
                    label_range.clone(),
                    Box::new(move |mouse: MouseInfo, this: Id, ctx: &mut Context| {
                        let text = match ctx.get_graphic_mut(this) {
                            Graphic::Text(x) => x,
                            _ => return,
                        };
                        match mouse.event {
                            MouseEvent::Enter => {
                                let label = 0x2e8bb2ff.into();
                                span = text
                                    .add_span(label_range.clone(), Span::Underline(Some(label)));
                            }
                            MouseEvent::Exit => {
                                text.remove_span(span);
                            }
                            _ if mouse.click() => ctx.send_event_to(
                                _list_id,
                                JumpToAddress {
                                    from_address: dbg!(directive.address),
                                },
                            ),
                            _ => {}
                        }
                    }),
                )]))
            } else {
                cb
            }
        })
    }

    fn update_item(&mut self, _index: usize, _item_id: Id, _ctx: &mut dyn BuilderContext) -> bool {
        if self.items_are_dirty {
            false
        } else {
            true
        }
    }

    fn finished_layout(&mut self) {
        self.items_are_dirty = false;
    }
}

struct BreakpointList {
    _breakpoints_updated_event: Handle<BreakpointsUpdated>,
}
impl BreakpointList {
    fn get_text(ctx: &mut dyn BuilderContext, index: usize) -> String {
        let debugger = ctx.get::<Arc<Mutex<Debugger>>>().lock();
        let (address, flags) = debugger.breakpoints().iter().nth(index).unwrap();
        let flags = {
            let mut flags_str = String::new();
            let check = |c, flag| if flags & flag != 0 { c } else { '-' };

            use break_flags::*;
            flags_str.push(check('w', WRITE));
            flags_str.push(check('r', READ));
            flags_str.push(check('j', JUMP));
            flags_str.push(check('x', EXECUTE));

            flags_str
        };
        let text = format!("{} {:04x}", flags, address);
        text
    }
}
impl ListBuilder for BreakpointList {
    fn on_event(&mut self, event: Box<dyn Any>, this: Id, ctx: &mut Context) {
        if event.is::<event_table::BreakpointsUpdated>() {
            ctx.send_event_to(this, UpdateItems);
        }
    }

    fn item_count(&mut self, ctx: &mut dyn BuilderContext) -> usize {
        ctx.get::<Arc<Mutex<Debugger>>>().lock().breakpoints().len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: Id,
        cb: ControlBuilder,
        ctx: &mut dyn BuilderContext,
    ) -> ControlBuilder {
        let text = Self::get_text(ctx, index);
        list_item(ctx, cb, text, move |_, ctx| {
            let mut debugger = ctx.get::<Arc<Mutex<Debugger>>>().lock();
            let &address = debugger.breakpoints().keys().nth(index).unwrap();
            debugger.remove_break(address);
        })
    }

    fn update_item(&mut self, index: usize, item_id: Id, ctx: &mut dyn BuilderContext) -> bool {
        let text = Self::get_text(ctx, index);
        let text_id = ctx.get_active_children(item_id)[0];
        if let Graphic::Text(x) = ctx.get_graphic_mut(text_id) {
            x.set_string(&text);
        }
        true
    }
}

struct WatchsList {
    _watchs_updated_event: Handle<WatchsUpdated>,
    _emulator_updated_event: Handle<EmulatorUpdated>,
}
impl WatchsList {
    fn get_text(ctx: &mut dyn BuilderContext, index: usize) -> (u16, String) {
        let &address = ctx
            .get::<Arc<Mutex<Debugger>>>()
            .lock()
            .watchs()
            .iter()
            .nth(index)
            .unwrap();
        let value = ctx.get::<Arc<Mutex<GameBoy>>>().lock().read(address);
        let text = format!("{:04x} = {:02x}", address, value);
        (address, text)
    }
}
impl ListBuilder for WatchsList {
    fn on_event(&mut self, event: Box<dyn Any>, this: Id, ctx: &mut Context) {
        if event.is::<event_table::WatchsUpdated>() || event.is::<event_table::EmulatorUpdated>() {
            ctx.send_event_to(this, UpdateItems);
        }
    }

    fn item_count(&mut self, ctx: &mut dyn BuilderContext) -> usize {
        ctx.get::<Arc<Mutex<Debugger>>>().lock().watchs().len()
    }

    fn create_item<'a>(
        &mut self,
        index: usize,
        _list_id: Id,
        cb: ControlBuilder,
        ctx: &mut dyn BuilderContext,
    ) -> ControlBuilder {
        let (address, text) = Self::get_text(ctx, index);
        list_item(ctx, cb, text, move |_: Id, ctx| {
            let mut debugger = ctx.get::<Arc<Mutex<Debugger>>>().lock();
            debugger.remove_watch(address);
        })
    }

    fn update_item(&mut self, index: usize, item_id: Id, ctx: &mut dyn BuilderContext) -> bool {
        let (_, text) = Self::get_text(ctx, index);
        let text_id = ctx.get_active_children(item_id)[0];
        if let Graphic::Text(x) = ctx.get_graphic_mut(text_id) {
            x.set_string(&text);
        }
        true
    }
}

fn list_item(
    ctx: &mut dyn BuilderContext,
    cb: ControlBuilder,
    text: String,
    on_click: impl FnMut(Id, &mut Context) + 'static,
) -> ControlBuilder {
    let Style {
        text_style,
        delete_button,
        delete_icon,
        ..
    } = ctx.get::<Style>().clone();
    cb.layout(HBoxLayout::new(0.0, [0.0; 4], 1))
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(text, (-1, 0), text_style))
                .layout(FitGraphic)
                .expand_x(true)
        })
        .child(ctx, |cb, ctx| {
            cb.behaviour(Button::new(delete_button, true, on_click))
                .min_size([16.0, 16.0])
                .child(ctx, |cb, _| cb.graphic(delete_icon))
                .fill_y(giui::RectFill::ShrinkCenter)
        })
}

pub fn build(
    parent: Id,
    ctx: &mut dyn BuilderContext,
    event_table: &mut EventTable,
    style: &Style,
    cpu_id: Id,
    ppu_id: Id,
) {
    let list_id = ctx.reserve();
    ui::list(
        ctx.create_control_reserved(list_id),
        ctx,
        style,
        [0.0; 4],
        DissasemblerList {
            list: list_id,
            cpu: cpu_id,
            ppu: ppu_id,
            pc: None,
            directives: Vec::new(),
            items_are_dirty: true,
            _emulator_updated_event: event_table.register(list_id),
        },
    )
    .parent(parent)
    .build(ctx);
}

pub fn side_panel(
    ctx: &mut dyn BuilderContext,
    style: &Style,
    parent: Id,
    cpu_id: Id,
    ppu_id: Id,
    event_table: &mut EventTable,
) {
    let scroll_view = ctx.reserve();
    let right_panel = ctx.reserve();
    ui::scroll_viewer(ctx, scroll_view, right_panel, style)
        .parent(parent)
        .min_size([100.0, 0.0])
        .graphic(style.split_background.clone())
        .build(ctx);
    let cpu = fold_view::folder(ctx, "cpu".to_string(), style)
        .parent(right_panel)
        .build(ctx);
    let _reg_view = ctx
        .create_control_reserved(cpu_id)
        .parent(cpu)
        .graphic(Text::new(String::new(), (-1, 0), style.text_style.clone()))
        .layout(FitGraphic)
        .build(ctx);
    let ppu = fold_view::folder(ctx, "ppu".to_string(), style)
        .parent(right_panel)
        .build(ctx);
    let _ppu_view = ctx
        .create_control_reserved(ppu_id)
        .parent(ppu)
        .graphic(Text::new(String::new(), (-1, 0), style.text_style.clone()))
        .layout(FitGraphic)
        .build(ctx);
    let breaks = fold_view::folder(ctx, "breaks".to_string(), style)
        .parent(right_panel)
        .build(ctx);
    let break_list = ctx.reserve();
    ui::list(
        ctx.create_control_reserved(break_list)
            .parent(breaks)
            .min_size([50.0, 100.0]),
        ctx,
        style,
        [10.0, 0.0, 0.0, 0.0],
        BreakpointList {
            _breakpoints_updated_event: event_table.register(break_list),
        },
    )
    .build(ctx);
    let watchs = fold_view::folder(ctx, "watchs".to_string(), style)
        .parent(right_panel)
        .build(ctx);
    let watchs_list = ctx.reserve();
    ui::list(
        ctx.create_control_reserved(watchs_list)
            .parent(watchs)
            .min_size([50.0, 100.0]),
        ctx,
        style,
        [10.0, 0.0, 0.0, 0.0],
        WatchsList {
            _watchs_updated_event: event_table.register(watchs_list),
            _emulator_updated_event: event_table.register(watchs_list),
        },
    )
    .build(ctx);
    ctx.create_control()
        .expand_y(true)
        .parent(right_panel)
        .graphic(style.header_background.clone())
        .build(ctx);
}

pub fn text_field(ctx: &mut dyn BuilderContext, parent: Id, style: &Style) {
    let caret = ctx.reserve();
    let label = ctx.reserve();
    let text_field = ctx
        .create_control()
        .parent(parent)
        .behaviour(TextField::new(
            caret,
            label,
            false,
            style.text_field.clone(),
            Callback::new(),
        ))
        .min_size([20.0; 2])
        .focus(true)
        .build(ctx);
    ctx.create_control_reserved(caret)
        .parent(text_field)
        .graphic(style.background.clone().with_color([0, 0, 0, 255].into()))
        .anchors([0.0; 4])
        .build(ctx);
    ctx.create_control_reserved(label)
        .parent(text_field)
        .graphic(Text::new(String::new(), (-1, -1), style.text_style.clone()))
        .build(ctx);
}
