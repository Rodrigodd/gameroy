use std::{cell::RefCell, rc::Rc};

use giui::{
    graphics::{Graphic, Icon, Texture},
    layouts::{FitGraphic, HBoxLayout, MarginLayout, VBoxLayout},
    style::ButtonStyle,
    text::Text,
    widgets::{Button, ButtonGroup, OnKeyboardEvent, TabButton},
    BuilderContext, Context, Gui, Id, RectFill,
};
use winit::event_loop::EventLoopProxy;

use crate::{
    emulator::Bool,
    event_table::EventTable,
    style::Style,
    ui::{Textures, Ui},
    widget::{
        menu::{create_menu, MenuOption},
        ScreenLayout, SplitView,
    },
    EmulatorEvent, UserEvent,
};

mod disassembler_viewer;
mod game_pad;
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
            let km = &crate::config::config().keymap;
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
                                .send(EmulatorEvent::Rewind(matches!(event, Pressed(_)).into()))
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

fn send_emu<'a>(ctx: &'a mut Context, event: EmulatorEvent) {
    ctx.get::<flume::Sender<EmulatorEvent>>()
        .send(event)
        .unwrap()
}

fn create_screen(
    ctx: &mut Context,
    textures: &Textures,
    screen_id: &mut Id,
    parent: Id,
    style: &Style,
) {
    *screen_id = ctx.reserve();
    let screen = ctx.reserve();
    let mut layout = ScreenLayout::new((160, 144));

    let gamepad = cfg!(target_os = "android");
    if gamepad {
        let scale_factor = ctx.scale_factor() as f32;
        let mut create_control = |graphic, [ax, ay]: [f32; 2], [x, y]: [f32; 2]| -> Id {
            let w = 200.0 / 2.0;
            let h = 200.0 / 2.0;
            ctx.create_control()
                .parent(*screen_id)
                .margins([-w + x, -h + y, w + x, h + y])
                .anchors([ax, ay, ax, ay])
                .graphic(graphic)
                .build(ctx)
        };

        let cross_anchor = [0.20, 1.0];
        let cross_margin = [0.0, -190.0];
        // let _cross = create_control(style.gamepad.cross.clone(), cross_anchor, cross_margin);

        let [su, sd, sl, sr, scenter] = match &style.gamepad.cross {
            Graphic::Icon(icon) => {
                let mut section = |section: [f32; 4]| {
                    let section = section.map(|x| x / 212.0);
                    let uv_rect = [
                        icon.uv_rect[0] + icon.uv_rect[2] * section[0],
                        icon.uv_rect[1] + icon.uv_rect[3] * section[1],
                        icon.uv_rect[2] * section[2],
                        icon.uv_rect[3] * section[3],
                    ];
                    let size = [icon.size[0] * section[2], icon.size[1] * section[3]];
                    let graphic = Graphic::Icon(Icon::new(icon.texture, uv_rect, size));
                    let offset = [
                        (section[0] + section[2] / 2.0) - 0.5,
                        (section[1] + section[3] / 2.0) - 0.5,
                    ];
                    create_control(
                        graphic,
                        cross_anchor,
                        [
                            cross_margin[0] + offset[0] * icon.size[0] / scale_factor,
                            cross_margin[1] + offset[1] * icon.size[1] / scale_factor,
                        ],
                    )
                };

                // using the 212x212 cross texture as reference.
                let u = section([66.0, 0.00, 80.0, 66.0]);
                let d = section([66.0, 146., 80.0, 66.0]);
                let l = section([0.00, 66.0, 66.0, 80.0]);
                let r = section([146., 66.0, 66.0, 80.0]);

                let center = section([66.0, 66.0, 80.0, 80.0]);

                [u, d, l, r, center]
            }

            _ => panic!("expected gamepad.cross to be Texture"),
        };

        let a = create_control(style.gamepad.a.clone(), [1.0, 1.0], [-50.0, -210.0]);
        let b = create_control(style.gamepad.b.clone(), [1.0, 1.0], [-100.0, -180.0]);
        let ab = create_control(
            style.gamepad.ab.clone(),
            [1.0, 1.0],
            [-75.0 - 15.0, -195.0 - 25.0],
        );
        let select = create_control(style.gamepad.select.clone(), [0.4, 1.0], [0.0, -80.0]);
        let start = create_control(style.gamepad.start.clone(), [0.6, 1.0], [0.0, -80.0]);

        let [r, rd, d, ld, l, lu, u, ru] = std::array::from_fn(|i| {
            let angle = (i as f32 / 8.0) * std::f32::consts::TAU;
            let dist = game_pad::GamePad::MAX_DIST;
            let offset = [angle.cos() * dist, angle.sin() * dist];
            create_control(
                Graphic::None,
                cross_anchor,
                [cross_margin[0] + offset[0], cross_margin[1] + offset[1]],
            )
        });

        let foward_button = ctx
            .create_control()
            .parent(screen)
            .graphic(style.button_panel.clone())
            .layout(MarginLayout::new([12.0; 4]))
            .child(ctx, |cb, _| {
                cb.graphic(style.foward_icon.clone()).layout(FitGraphic)
            })
            .build(ctx);

        let rewind_button = ctx
            .create_control()
            .parent(screen)
            .graphic(style.button_panel.clone())
            .layout(MarginLayout::new([12.0; 4]))
            .child(ctx, |cb, _| {
                cb.graphic(style.rewind_icon.clone()).layout(FitGraphic)
            })
            .build(ctx);

        layout.foward_button = Some(foward_button);
        layout.rewind_button = Some(rewind_button);

        let mut joypad = vec![
            (r, 1 << 0),
            (l, 1 << 1),
            (u, 1 << 2),
            (d, 1 << 3),
            (a, 1 << 4),
            (b, 1 << 5),
            (select, 1 << 6),
            (start, 1 << 7),
        ];

        joypad.extend_from_slice(&[
            (ru, joypad[0].1 | joypad[2].1),
            (lu, joypad[1].1 | joypad[2].1),
            (rd, joypad[0].1 | joypad[3].1),
            (ld, joypad[1].1 | joypad[3].1),
            (ab, joypad[4].1 | joypad[5].1),
        ]);

        use EmulatorEvent::*;
        fn bx<T>(x: T) -> Box<T> {
            Box::new(x)
        }
        let other: Vec<game_pad::OtherButton> = vec![
            (foward_button, bx(|v, ctx| send_emu(ctx, FrameLimit(!v)))),
            (rewind_button, bx(|v, ctx| send_emu(ctx, Rewind(v.into())))),
        ];

        let sprites = [sr, sl, su, sd, a, b, select, start, scenter];

        ctx.create_control_reserved(*screen_id)
            .parent(parent)
            .graphic(style.background.clone())
            .behaviour(game_pad::GamePad::new(joypad, other, sprites))
            .build(ctx);
    } else {
        ctx.create_control_reserved(*screen_id)
            .parent(parent)
            .graphic(style.background.clone())
            .build(ctx);
    }

    let _screen = ctx
        .create_control_reserved(screen)
        .parent(*screen_id)
        .layout(layout)
        .child(ctx, |cb, _| {
            cb.graphic(Texture::new(textures.screen, [0.0, 0.0, 1.0, 1.0]))
        })
        .build(ctx);

    ctx.move_to_back(screen);

    let menu = cfg!(target_os = "android");
    if menu {
        let _open_menu = ctx
            .create_control()
            .parent(*screen_id)
            .layout(MarginLayout::new([12.0; 4]))
            .child(ctx, |cb, _| {
                cb.graphic(style.menu_icon.clone()).layout(FitGraphic)
            })
            .fill_x(RectFill::ShrinkEnd)
            .fill_y(RectFill::ShrinkEnd)
            .behaviour(Button::new(
                Rc::new(ButtonStyle {
                    normal: style.button_panel.clone().with_alpha(128),
                    hover: style.button_panel.clone().with_alpha(200),
                    pressed: style.button_panel.clone().with_alpha(255),
                    focus: style.button_panel.clone().with_alpha(150),
                }),
                true,
                move |_, ctx| {
                    let style = ctx.get::<Style>().clone();
                    fn option<'a>(
                        a: &'a str,
                        b: impl FnMut(&mut Context) + 'static,
                    ) -> MenuOption<'a> {
                        (a, Box::new(b))
                    }
                    send_emu(ctx, EmulatorEvent::Pause);
                    let options = vec![
                        option("Save State", |ctx| send_emu(ctx, EmulatorEvent::SaveState)),
                        option("Load State", |ctx| send_emu(ctx, EmulatorEvent::LoadState)),
                        option("Rewind", |ctx| {
                            send_emu(ctx, EmulatorEvent::Rewind(Bool::Toggle))
                        }),
                        option("Exit Game", |ctx| {
                            ctx.get::<EventLoopProxy<UserEvent>>()
                                .send_event(UserEvent::GoToRomList)
                                .unwrap();
                        }),
                    ];
                    let on_close = |ctx: &mut Context| send_emu(ctx, EmulatorEvent::Resume);
                    create_menu(options, on_close, ctx, &style);
                },
            ))
            .build(ctx);
    }
}
