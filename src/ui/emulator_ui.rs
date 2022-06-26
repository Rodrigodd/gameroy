use crate::{
    event_table::EventTable,
    style::Style,
    ui::{Textures, Ui},
    widget::{PixelPerfectLayout, SplitView},
    EmulatorEvent, UserEvent,
};
use std::{cell::RefCell, rc::Rc};

use giui::{
    graphics::{Graphic, Texture},
    layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout},
    text::Text,
    widgets::{ButtonGroup, OnKeyboardEvent, TabButton},
    BuilderContext, Context, Gui, Id,
};

use winit::event_loop::EventLoopProxy;
mod disassembler_viewer;
mod ppu_viewer;

pub fn create_emulator_ui(ui: &mut Ui, debug: bool) {
    let style = &ui.gui.get::<Style>().clone();
    create_gui(
        &mut ui.gui,
        &ui.textures,
        ui.event_table.clone(),
        style,
        debug,
    );
}

pub fn create_gui(
    gui: &mut Gui,
    textures: &Textures,
    event_table: Rc<RefCell<EventTable>>,
    style: &Style,
    debug: bool,
) {
    let root = gui.reserve_id();
    let mut screen_id = gui.reserve_id();
    let mut split_view = gui.reserve_id();

    let sty = style.clone();
    let event_table_clone = event_table.clone();
    gui.create_control_reserved(root)
        .behaviour(OnKeyboardEvent::new(move |event, _, ctx| {
            use giui::KeyboardEvent::*;
            let sender = ctx.get::<flume::Sender<EmulatorEvent>>().clone();
            let debug = ctx.get::<crate::AppState>().debug;
            let app_state = ctx.get_mut::<crate::AppState>();
            let mut set_key = |key: u8, value: bool| {
                app_state.joypad = (app_state.joypad & !(1 << key)) | ((!value as u8) << key)
            };
            let km = &crate::config().keymap;
            match event {
                Pressed(x) if x == km.right => set_key(0, true), // Left
                Release(x) if x == km.right => set_key(0, false),
                Pressed(x) if x == km.left => set_key(1, true), // Right
                Release(x) if x == km.left => set_key(1, false),
                Pressed(x) if x == km.up => set_key(2, true), // Up
                Release(x) if x == km.up => set_key(2, false),
                Pressed(x) if x == km.down => set_key(3, true), // Down
                Release(x) if x == km.down => set_key(3, false),
                Pressed(x) if x == km.a => set_key(4, true), // A
                Release(x) if x == km.a => set_key(4, false),
                Pressed(x) if x == km.b => set_key(5, true), // B
                Release(x) if x == km.b => set_key(5, false),
                Pressed(x) if x == km.select => set_key(6, true), // Select
                Release(x) if x == km.select => set_key(6, false),
                Pressed(x) if x == km.start => set_key(7, true), // Start
                Release(x) if x == km.start => set_key(7, false),
                event => {
                    if debug {
                        match event {
                            Pressed(x) if x == km.save_state => {
                                sender.send(EmulatorEvent::SaveState).unwrap();
                            }
                            Pressed(x) if x == km.load_state => {
                                sender.send(EmulatorEvent::LoadState).unwrap();
                            }
                            Pressed(x) if x == km.debug_stepback => {
                                sender.send(EmulatorEvent::StepBack).unwrap();
                            }
                            Pressed(x) if x == km.debug_step => {
                                sender.send(EmulatorEvent::Step).unwrap();
                            }
                            Pressed(x) if x == km.debug_run => {
                                sender.send(EmulatorEvent::Run).unwrap();
                            }
                            Pressed(x) if x == km.open_debugger => {
                                let textures = ctx.get::<Textures>().clone();
                                close_debug_panel(
                                    ctx,
                                    &textures,
                                    &mut split_view,
                                    &mut screen_id,
                                    root,
                                    &sty,
                                );
                            }
                            _ => {}
                        }
                    } else {
                        match event {
                            Pressed(x) if x == km.save_state => {
                                sender.send(EmulatorEvent::SaveState).unwrap();
                            }
                            Pressed(x) if x == km.load_state => {
                                sender.send(EmulatorEvent::LoadState).unwrap();
                            }
                            Pressed(x) if x == km.open_debugger => {
                                let textures = ctx.get::<Textures>().clone();
                                // Debug
                                open_debug_panel(
                                    ctx,
                                    &textures,
                                    split_view,
                                    root,
                                    &sty,
                                    &mut screen_id,
                                    event_table.clone(),
                                );
                            }
                            Pressed(x) | Release(x) if x == km.speed => sender
                                .send(EmulatorEvent::FrameLimit(!matches!(event, Pressed(_))))
                                .unwrap(),
                            Pressed(x) | Release(x) if x == km.rewind => sender
                                .send(EmulatorEvent::Rewind(matches!(event, Pressed(_))))
                                .unwrap(),

                            _ => {}
                        }
                    }
                }
            }
            true
        }))
        .build(gui);

    if debug {
        open_debug_panel(
            &mut gui.get_context(),
            textures,
            split_view,
            root,
            style,
            &mut screen_id,
            event_table_clone,
        );
    } else {
        create_screen(
            &mut gui.get_context(),
            textures,
            &mut screen_id,
            root,
            style,
        );
        gui.set_focus(Some(screen_id));
    }
}

fn close_debug_panel(
    ctx: &mut Context,
    textures: &Textures,
    split_view: &mut Id,
    screen_id: &mut Id,
    root: Id,
    style: &Style,
) {
    ctx.remove(*split_view);
    *split_view = ctx.reserve();

    create_screen(ctx, textures, screen_id, root, style);
    ctx.set_focus(*screen_id);
    let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
    proxy.send_event(UserEvent::Debug(false)).unwrap();
}

fn open_debug_panel(
    ctx: &mut Context,
    textures: &Textures,
    split_view: Id,
    root: Id,
    style: &Style,
    screen_id: &mut Id,
    event_table: Rc<RefCell<EventTable>>,
) {
    let event_table = &mut *event_table.borrow_mut();
    ctx.create_control_reserved(split_view)
        .parent(root)
        .graphic(style.split_background.clone())
        .behaviour_and_layout(SplitView::new(0.333, 4.0, [2.0; 4], false))
        .build(ctx);
    ctx.remove(*screen_id);

    create_screen(ctx, textures, screen_id, split_view, style);

    // create debug panel
    let debug_panel = ctx
        .create_control()
        .layout(VBoxLayout::default())
        .parent(split_view)
        .build(ctx);

    let h_box = ctx
        .create_control()
        .parent(debug_panel)
        .behaviour_and_layout(SplitView::new(1.0, 2.0, [2.0; 4], false))
        .expand_y(true)
        .build(ctx);

    let vbox = ctx
        .create_control()
        .graphic(style.background.clone())
        .parent(h_box)
        .expand_y(true)
        .expand_x(true)
        .min_width(100.0)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
        .build(ctx);

    let tab_header = ctx
        .create_control()
        .parent(vbox)
        .layout(HBoxLayout::default())
        .min_size([16.0, 16.0])
        .build(ctx);

    let v_split = ctx
        .create_control()
        .parent(vbox)
        // .graphic(style.split_background.clone())
        .expand_y(true)
        .behaviour_and_layout(SplitView::new(0.9, 2.0, [0.0; 4], true))
        .build(ctx);

    let tab_page = ctx
        .create_control()
        .parent(v_split)
        .expand_y(true)
        .build(ctx);

    let cpu_id = ctx.reserve();
    let ppu_id = ctx.reserve();
    disassembler_viewer::side_panel(ctx, style, h_box, cpu_id, ppu_id, event_table);

    let scroll_log = ctx.reserve();
    let content = ctx.reserve();
    super::scroll_viewer(ctx, scroll_log, content, style, (true, true))
        .parent(v_split)
        .graphic(style.terminal_background.clone())
        .build(ctx);
    let log = ctx
        .create_control()
        .graphic(Text::new(
            String::new(),
            (-1, -1),
            style.terminal_text_style.clone(),
        ))
        .layout(FitGraphic)
        .parent(content)
        .build(ctx);
    disassembler_viewer::command_field(ctx, vbox, style, scroll_log, log);

    let tab_group = ButtonGroup::new(|_, _| ());

    let disas_page = ctx.create_control().parent(tab_page).build(ctx);
    disassembler_viewer::build(disas_page, ctx, event_table, &style, cpu_id, ppu_id);
    let _disas_tab = ctx
        .create_control()
        .parent(tab_header)
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "disassembly".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitGraphic)
        })
        .layout(MarginLayout::default())
        .behaviour(TabButton::new(
            tab_group.clone(),
            disas_page,
            true,
            style.tab_style.clone(),
        ))
        .build(ctx);

    let ppu_page = ctx.create_control().parent(tab_page).build(ctx);
    ppu_viewer::build(ppu_page, ctx, event_table, &style, textures);
    let _ppu_tab = ctx
        .create_control()
        .parent(tab_header)
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "ppu".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitGraphic)
        })
        .layout(MarginLayout::default())
        .behaviour(TabButton::new(
            tab_group.clone(),
            ppu_page,
            false,
            style.tab_style.clone(),
        ))
        .build(ctx);

    let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
    proxy.send_event(UserEvent::Debug(true)).unwrap();
}

mod game_pad;

fn create_screen(
    ctx: &mut Context,
    textures: &Textures,
    screen_id: &mut Id,
    parent: Id,
    style: &Style,
) {
    *screen_id = ctx.reserve();

    let _screen = ctx
        .create_control()
        .parent(*screen_id)
        .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
        .child(ctx, |cb, _| {
            cb.graphic(Texture::new(textures.screen, [0.0, 0.0, 1.0, 1.0]))
        })
        .build(ctx);

    let gamepad = cfg!(target_os = "android");
    if gamepad {
        let mut create_button = |graphic, [ax, ay]: [f32; 2], [x, y]: [f32; 2]| -> Id {
            let w = 200.0 / 2.0;
            let h = 200.0 / 2.0;
            ctx.create_control()
                .parent(*screen_id)
                .margins([-w + x, -h + y, w + x, h + y])
                .anchors([ax, ay, ax, ay])
                .graphic(graphic)
                .build(ctx)
        };

        let cross = create_button(style.gamepad.cross.clone(), [0.20, 1.0], [0.0, -150.0]);
        let r = create_button(Graphic::None, [0.20, 1.0], [40.0, -150.0]);
        let l = create_button(Graphic::None, [0.20, 1.0], [-40.0, -150.0]);
        let u = create_button(Graphic::None, [0.20, 1.0], [0.0, -190.0]);
        let d = create_button(Graphic::None, [0.20, 1.0], [0.0, -110.0]);

        let a = create_button(style.gamepad.a.clone(), [1.0, 1.0], [-50.0, -170.0]);
        let b = create_button(style.gamepad.b.clone(), [1.0, 1.0], [-100.0, -140.0]);
        let select = create_button(style.gamepad.select.clone(), [0.4, 1.0], [0.0, -60.0]);
        let start = create_button(style.gamepad.start.clone(), [0.6, 1.0], [0.0, -60.0]);
        let buttons = [r, l, u, d, a, b, select, start, cross];

        ctx.create_control_reserved(*screen_id)
            .parent(parent)
            .graphic(style.background.clone())
            .behaviour(game_pad::GamePad::new(buttons))
            .build(ctx);
    } else {
        ctx.create_control_reserved(*screen_id)
            .parent(parent)
            .graphic(style.background.clone())
            .build(ctx);
    }
}
