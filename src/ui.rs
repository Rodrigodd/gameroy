use std::{any::Any, cell::RefCell, rc::Rc, sync::mpsc::SyncSender};

use crate::{
    disassembler_viewer, event_table::EventTable, layout::PixelPerfectLayout,
    split_view::SplitView, style::Style, EmulatorEvent, UserEvent, SCREEN_HEIGHT, SCREEN_WIDTH,
};
use crui::{
    font::Fonts, graphics::Texture, render::GuiRenderer, widgets::OnKeyboardEvent, Gui, GuiRender,
};
use sprite_render::{Camera, GLSpriteRender, SpriteInstance, SpriteRender};
use winit::{
    dpi::PhysicalSize,
    event::WindowEvent,
    event_loop::{ControlFlow, EventLoop, EventLoopProxy},
    window::{Window, WindowBuilder, WindowId},
};

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

pub struct Ui {
    gui: Gui,
    gui_render: GuiRender,
    render: GLSpriteRender,
    camera: Camera,
    style: Style,
    pub event_table: Rc<RefCell<EventTable>>,
    pub screen_texture: u32,
    pub is_animating: bool,
    pub force_render: bool,
}
impl Ui {
    pub fn new<T>(wb: WindowBuilder, event_loop: &EventLoop<T>) -> (Self, Window) {
        // create the render and camera, and a texture for the glyphs rendering
        let (window, mut render) = GLSpriteRender::new(wb, event_loop, true);
        let camera = {
            let size = window.inner_size();
            let width = size.width;
            let height = size.height;
            Camera::new(width, height, height as f32)
        };
        let font_texture = render.new_texture(128, 128, &[], false);

        let mut fonts = Fonts::new();
        let style = Style::load(&mut fonts, &mut render).unwrap();

        let screen_texture =
            render.new_texture(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32, &[], false);
        let white_texture = render.new_texture(1, 1, &[255, 255, 255, 255], false);

        // create the gui, and the gui_render
        let gui = Gui::new(0.0, 0.0, fonts);
        let gui_render = GuiRender::new(font_texture, white_texture, [128, 128]);

        let mut ui = Self {
            gui,
            gui_render,
            render,
            camera,
            style,
            screen_texture,
            event_table: Rc::new(RefCell::new(EventTable::new())),
            is_animating: false,
            force_render: true,
        };

        create_gui(
            &mut ui.gui,
            screen_texture,
            ui.event_table.clone(),
            &ui.style,
        );

        ui.resize(window.inner_size(), window.id());

        (ui, window)
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

    pub fn frame_update(&mut self, img_data: &[u8]) {
        self.render
            .update_texture(self.screen_texture, &img_data, None);
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

    pub fn insert<T: Any>(&mut self, value: T) {
        self.gui.set(value);
    }
}

pub fn create_gui(
    gui: &mut Gui,
    screen_texture: u32,
    event_table: Rc<RefCell<EventTable>>,
    style: &Style,
) {
    let mut screen_id = gui.reserve_id();
    let mut split_view = gui.reserve_id();
    let root = gui.reserve_id();

    let sty = style.clone();
    gui.create_control_reserved(root)
        .behaviour(OnKeyboardEvent(move |event, _, ctx| {
            use crui::BuilderContext;
            use crui::KeyboardEvent::*;
            use winit::event::VirtualKeyCode::*;
            let sender = ctx.get::<SyncSender<EmulatorEvent>>().clone();
            let debug = ctx.get::<crate::AppState>().debug;
            if debug {
                match event {
                    Pressed(F7) => {
                        sender.send(EmulatorEvent::Step).unwrap();
                    }
                    Pressed(F9) => {
                        sender.send(EmulatorEvent::Run).unwrap();
                    }
                    Pressed(F12) => {
                        ctx.remove(split_view);
                        split_view = ctx.reserve();

                        screen_id = ctx.reserve();
                        ctx.create_control_reserved(screen_id)
                            .parent(root)
                            .graphic(sty.background.clone())
                            .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
                            .child(ctx, |cb, _| {
                                cb.graphic(
                                    Texture::new(screen_texture, [0.0, 0.0, 1.0, 1.0]).into(),
                                )
                            })
                            .build(ctx);
                        ctx.set_focus(screen_id);

                        let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
                        proxy.send_event(UserEvent::Debug(false)).unwrap();
                    }
                    _ => {}
                }
            } else {
                let app_state = ctx.get_mut::<crate::AppState>();
                let mut set_key = |key: u8, value: bool| {
                    app_state.joypad = (app_state.joypad & !(1 << key)) | ((!value as u8) << key)
                };
                match event {
                    Pressed(Right) => set_key(0, true),
                    Release(Right) => set_key(0, false),
                    Pressed(Left) => set_key(1, true),
                    Release(Left) => set_key(1, false),
                    Pressed(Up) => set_key(2, true),
                    Release(Up) => set_key(2, false),
                    Pressed(Down) => set_key(3, true),
                    Release(Down) => set_key(3, false),
                    Pressed(Z) => set_key(4, true),
                    Release(Z) => set_key(4, false),
                    Pressed(X) => set_key(5, true),
                    Release(X) => set_key(5, false),
                    Pressed(Return) => set_key(6, true),
                    Release(Return) => set_key(6, false),
                    Pressed(S) => set_key(7, true),
                    Release(S) => set_key(7, false),
                    Pressed(F12) => {
                        let mut split_layout = SplitView::new(4.0, [2.0; 4], false);
                        split_layout.split = 0.5;

                        ctx.create_control_reserved(split_view)
                            .parent(root)
                            .graphic(sty.split_background.clone())
                            .behaviour_and_layout(split_layout)
                            .build(ctx);

                        // TODO: instead of destroying the screen control, and creating it again, I
                        // need to look if I could create a 'set_parent' method. But that may be
                        // complicated
                        ctx.remove(screen_id);

                        // TODO: maybe there should be a replace_control method?
                        screen_id = ctx.reserve();
                        ctx.create_control_reserved(screen_id)
                            .parent(split_view)
                            .graphic(sty.background.clone())
                            .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
                            .child(ctx, |cb, _| {
                                cb.graphic(
                                    Texture::new(screen_texture, [0.0, 0.0, 1.0, 1.0]).into(),
                                )
                            })
                            .build(ctx);
                        ctx.set_focus(screen_id);
                        disassembler_viewer::build(
                            split_view,
                            ctx,
                            &mut *event_table.borrow_mut(),
                            &sty,
                        );

                        let proxy = ctx.get::<EventLoopProxy<UserEvent>>();
                        proxy.send_event(UserEvent::Debug(true)).unwrap();
                    }
                    Pressed(LShift) | Release(LShift) => sender
                        .send(EmulatorEvent::FrameLimit(!matches!(event, Pressed(_))))
                        .unwrap(),

                    _ => {}
                }
            }
            true
        }))
        .build(gui);

    gui.create_control_reserved(screen_id)
        .parent(root)
        .graphic(style.background.clone())
        .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
        .child(gui, |cb, _| {
            cb.graphic(Texture::new(screen_texture, [0.0, 0.0, 1.0, 1.0]).into())
        })
        .build(gui);
    gui.set_focus(Some(screen_id));
}
