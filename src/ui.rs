use std::{
    any::Any,
    cell::RefCell,
    rc::Rc,
    sync::{mpsc::SyncSender, Arc},
};

use crui::{
    font::Fonts,
    graphics::Texture,
    layouts::{FitText, HBoxLayout, MarginLayout, VBoxLayout},
    render::GuiRenderer,
    text::Text,
    widgets::{ButtonGroup, OnKeyboardEvent, ScrollBar, ScrollView, TabButton, ViewLayout},
    BuilderContext, Gui, GuiRender, Id,
};
use gameroy::{debugger::Debugger, gameboy::GameBoy};
use parking_lot::Mutex;
use sprite_render::{Camera, GLSpriteRender, SpriteInstance, SpriteRender};
use winit::{
    dpi::PhysicalSize,
    event::WindowEvent,
    event_loop::{ControlFlow, EventLoopProxy},
    window::{Window, WindowId},
};

use crate::{
    event_table::EventTable, layout::PixelPerfectLayout, split_view::SplitView, style::Style,
    AppState, EmulatorEvent, UserEvent, SCREEN_HEIGHT, SCREEN_WIDTH,
};

mod disassembler_viewer;
mod ppu_viewer;

struct Render<'a>(&'a mut GLSpriteRender);
impl<'a> GuiRenderer for Render<'a> {
    fn update_font_texure(&mut self, font_texture: u32, rect: [u32; 4], data_tex: &[u8]) {
        let mut data = Vec::with_capacity(data_tex.len() * 4);
        for byte in data_tex.iter() {
            data.extend([0xff, 0xff, 0xff, *byte].iter());
        }
        self.0.update_texture(
            font_texture,
            &data,
            Some([rect[0], rect[1], rect[2] - rect[0], rect[3] - rect[1]]),
        );
    }
    fn resize_font_texture(&mut self, font_texture: u32, new_size: [u32; 2]) {
        self.0
            .resize_texture(font_texture, new_size[0], new_size[1], &[]);
    }
}

#[derive(Clone)]
pub struct Textures {
    pub white: u32,
    pub screen: u32,
    pub tilemap: u32,
    pub background: u32,
    pub window: u32,
}

pub struct Ui {
    pub gui: Gui,
    gui_render: GuiRender,
    render: GLSpriteRender,
    camera: Camera,
    pub style: Style,
    pub event_table: Rc<RefCell<EventTable>>,
    pub textures: Textures,
    pub is_animating: bool,
    pub force_render: bool,
}
impl Ui {
    pub fn new(window: &Window) -> Self {
        // create the render and camera, and a texture for the glyphs rendering
        let mut render = GLSpriteRender::new(window, true).unwrap();
        let camera = {
            let size = window.inner_size();
            let width = size.width;
            let height = size.height;
            Camera::new(width, height, height as f32)
        };
        let font_texture = render.new_texture(128, 128, &[], false);

        let mut fonts = Fonts::new();
        let style = Style::load(&mut fonts, &mut render).unwrap();

        let textures = Textures {
            white: render.new_texture(1, 1, &[255, 255, 255, 255], false),
            screen: render.new_texture(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32, &[], false),
            tilemap: render.new_texture(128, 192, &[], false),
            background: render.new_texture(256, 256, &[], false),
            window: render.new_texture(256, 256, &[], false),
        };

        // create the gui, and the gui_render
        let gui = Gui::new(0.0, 0.0, fonts);
        let gui_render = GuiRender::new(font_texture, textures.white, [128, 128]);

        let mut ui = Self {
            gui,
            gui_render,
            render,
            camera,
            style,
            textures: textures.clone(),
            event_table: Rc::new(RefCell::new(EventTable::new())),
            is_animating: false,
            force_render: true,
        };

        ui.resize(window.inner_size(), window.id());

        ui
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>, window: WindowId) {
        self.render.resize(window, size.width, size.height);
        self.camera.resize(size.width, size.height);
        let width = size.width as f32;
        let height = size.height as f32;
        self.gui.resize(width, height);
        self.camera.set_width(width);
        self.camera.set_height(height);
        self.camera.set_position(width / 2.0, height / 2.0);
    }

    pub fn clear(&mut self) {
        self.gui.clear_controls()
    }

    pub fn new_events(&mut self, control: &mut ControlFlow, window: &Window) {
        *control = match self.gui.handle_scheduled_event() {
            Some(time) => ControlFlow::WaitUntil(time),
            None => ControlFlow::Wait,
        };
        self.update_window(window);
    }

    pub fn window_event(&mut self, event: &WindowEvent, window: &Window) {
        // gui receive events
        self.gui.handle_event(event);
        self.update_window(window);
    }

    fn update_window(&mut self, window: &Window) {
        if self.gui.render_is_dirty() || self.is_animating {
            window.request_redraw();
        }
        if let Some(cursor) = self.gui.cursor_change() {
            window.set_cursor_icon(cursor);
        }
    }

    pub fn update_screen_texture(&mut self, img_data: &[u8]) {
        self.render
            .update_texture(self.textures.screen, &img_data, None);
    }

    pub fn update_texture(&mut self, texture: u32, img_data: &[u8]) {
        self.render.update_texture(texture, &img_data, None);
    }

    pub fn render(&mut self, window_id: WindowId) {
        let mut ctx = self.gui.get_render_context();
        let (sprites, is_anim) = self.gui_render.render(&mut ctx, Render(&mut self.render));
        self.is_animating = is_anim || self.force_render;
        let mut renderer = self.render.render(window_id);
        renderer.clear_screen(&[0.0, 0.0, 0.0, 1.0]);
        renderer.draw_sprites(
            &mut self.camera,
            &sprites
                .iter()
                .map(|x| {
                    let width = x.rect[2] - x.rect[0];
                    let height = x.rect[3] - x.rect[1];
                    SpriteInstance {
                        scale: [width, height],
                        angle: 0.0,
                        uv_rect: x.uv_rect,
                        color: x.color.to_array(),
                        pos: [x.rect[0] + width / 2.0, x.rect[1] + height / 2.0],
                        texture: x.texture,
                    }
                })
                .collect::<Vec<_>>(),
        );

        renderer.finish();
    }

    pub fn notify<E: crate::event_table::Event>(&mut self, event: E) {
        self.event_table
            .borrow_mut()
            .notify::<E>(event, &mut self.gui.get_context());
    }

    pub fn get<T: Any>(&mut self) -> &mut T {
        self.gui.get_mut()
    }
}

pub fn create_emulator_ui(
    ui: &mut Ui,
    proxy: EventLoopProxy<UserEvent>,
    gb: Arc<parking_lot::lock_api::Mutex<parking_lot::RawMutex, GameBoy>>,
    debugger: Arc<parking_lot::lock_api::Mutex<parking_lot::RawMutex, Debugger>>,
    emu_channel: SyncSender<EmulatorEvent>,
    app_state: AppState,
) {
    ui.gui.set(ui.textures.clone());
    ui.gui.set::<EventLoopProxy<UserEvent>>(proxy);
    ui.gui.set::<Arc<Mutex<GameBoy>>>(gb);
    ui.gui.set::<Arc<Mutex<Debugger>>>(debugger);
    ui.gui.set(emu_channel);
    let debug = app_state.debug;
    ui.gui.set(app_state);

    create_gui(
        &mut ui.gui,
        &ui.textures,
        ui.event_table.clone(),
        &ui.style,
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
            use crui::KeyboardEvent::*;
            use winit::event::VirtualKeyCode::*;
            let sender = ctx.get::<SyncSender<EmulatorEvent>>().clone();
            let debug = ctx.get::<crate::AppState>().debug;
            let app_state = ctx.get_mut::<crate::AppState>();
            let mut set_key = |key: u8, value: bool| {
                app_state.joypad = (app_state.joypad & !(1 << key)) | ((!value as u8) << key)
            };
            match event {
                Pressed(Right) => set_key(0, true), // Left
                Release(Right) => set_key(0, false),
                Pressed(Left) => set_key(1, true), // Right
                Release(Left) => set_key(1, false),
                Pressed(Up) => set_key(2, true), // Up
                Release(Up) => set_key(2, false),
                Pressed(Down) => set_key(3, true), // Down
                Release(Down) => set_key(3, false),
                Pressed(A) => set_key(4, true), // A
                Release(A) => set_key(4, false),
                Pressed(S) => set_key(5, true), // B
                Release(S) => set_key(5, false),
                Pressed(Back) => set_key(6, true), // Select
                Release(Back) => set_key(6, false),
                Pressed(Return) => set_key(7, true), // Start
                Release(Return) => set_key(7, false),
                event => {
                    if debug {
                        match event {
                            Pressed(F5) => {
                                sender.send(EmulatorEvent::SaveState).unwrap();
                            }
                            Pressed(F6) => {
                                sender.send(EmulatorEvent::LoadState).unwrap();
                            }
                            Pressed(F7) => {
                                sender.send(EmulatorEvent::StepBack).unwrap();
                            }
                            Pressed(F8) => {
                                sender.send(EmulatorEvent::Step).unwrap();
                            }
                            Pressed(F9) => {
                                sender.send(EmulatorEvent::Run).unwrap();
                            }
                            Pressed(F12) => {
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
                            Pressed(F5) => {
                                sender.send(EmulatorEvent::SaveState).unwrap();
                            }
                            Pressed(F6) => {
                                sender.send(EmulatorEvent::LoadState).unwrap();
                            }
                            Pressed(F12) => {
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
                            Pressed(LShift) | Release(LShift) => sender
                                .send(EmulatorEvent::FrameLimit(!matches!(event, Pressed(_))))
                                .unwrap(),
                            Pressed(R) | Release(R) => sender
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
        gui.create_control_reserved(screen_id)
            .parent(root)
            .graphic(style.background.clone())
            .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
            .child(gui, |cb, _| {
                cb.graphic(Texture::new(textures.screen, [0.0, 0.0, 1.0, 1.0]))
            })
            .build(gui);
        gui.set_focus(Some(screen_id));
    }
}

fn close_debug_panel(
    ctx: &mut crui::Context,
    textures: &Textures,
    split_view: &mut crui::Id,
    screen_id: &mut crui::Id,
    root: crui::Id,
    style: &Style,
) {
    ctx.remove(*split_view);
    *split_view = ctx.reserve();
    *screen_id = ctx.reserve();
    ctx.create_control_reserved(*screen_id)
        .parent(root)
        .graphic(style.background.clone())
        .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
        .child(ctx, |cb, _| {
            cb.graphic(Texture::new(textures.screen, [0.0, 0.0, 1.0, 1.0]))
        })
        .build(ctx);
    ctx.set_focus(*screen_id);
    let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
    proxy.send_event(UserEvent::Debug(false)).unwrap();
}

fn open_debug_panel(
    ctx: &mut crui::Context,
    textures: &Textures,
    split_view: crui::Id,
    root: crui::Id,
    style: &Style,
    screen_id: &mut crui::Id,
    event_table: Rc<RefCell<EventTable>>,
) {
    ctx.create_control_reserved(split_view)
        .parent(root)
        .graphic(style.split_background.clone())
        .behaviour_and_layout(SplitView::new(0.333, 4.0, [2.0; 4], false))
        .build(ctx);
    ctx.remove(*screen_id);

    // create screen
    *screen_id = ctx.reserve();
    ctx.create_control_reserved(*screen_id)
        .parent(split_view)
        .graphic(style.background.clone())
        .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
        .child(ctx, |cb, _| {
            cb.graphic(Texture::new(textures.screen, [0.0, 0.0, 1.0, 1.0]))
        })
        .build(ctx);

    // create debug panel
    let debug_panel = ctx
        .create_control()
        .layout(VBoxLayout::default())
        .parent(split_view)
        .build(ctx);

    let tab_header = ctx
        .create_control()
        .parent(debug_panel)
        .layout(HBoxLayout::default())
        .min_size([16.0, 16.0])
        .build(ctx);

    let tab_page = ctx
        .create_control()
        .parent(debug_panel)
        .expand_y(true)
        .build(ctx);

    let tab_group = ButtonGroup::new(|_, _| ());

    let disas_page = ctx.create_control().parent(tab_page).build(ctx);
    disassembler_viewer::build(disas_page, ctx, &mut *event_table.borrow_mut(), &style);
    let _disas_tab = ctx
        .create_control()
        .parent(tab_header)
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "disassembly".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
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
    ppu_viewer::build(
        ppu_page,
        ctx,
        &mut *event_table.borrow_mut(),
        &style,
        textures,
    );
    let _ppu_tab = ctx
        .create_control()
        .parent(tab_header)
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "ppu".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
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

fn scroll_viewer(
    ctx: &mut dyn BuilderContext,
    scroll_view: Id,
    content: Id,
    style: &Style,
) -> crui::ControlBuilder {
    let view = ctx
        .create_control()
        .parent(scroll_view)
        .layout(ViewLayout::new(false, true))
        .build(ctx);
    let _content = ctx
        .create_control_reserved(content)
        .parent(view)
        .layout(VBoxLayout::new(10.0, [2.0; 4], -1))
        .build(ctx);
    let v_handle = ctx.reserve();
    let v_scroll = ctx
        .create_control()
        .parent(scroll_view)
        .behaviour(ScrollBar::new(
            v_handle,
            scroll_view,
            true,
            style.scrollbar.clone(),
        ))
        .min_size([12.0, 12.0])
        .build(ctx);
    let v_handle = ctx
        .create_control_reserved(v_handle)
        .parent(v_scroll)
        .build(ctx);
    ctx.create_control_reserved(scroll_view)
        .behaviour_and_layout(ScrollView::new(
            view,
            content,
            None,
            Some((v_scroll, v_handle)),
        ))
}
