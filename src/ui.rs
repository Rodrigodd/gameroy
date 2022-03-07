use std::{any::Any, cell::RefCell, rc::Rc};

use crui::{
    font::Fonts,
    layouts::VBoxLayout,
    render::GuiRenderer,
    widgets::{ListBuilder, ScrollBar, ScrollView, ViewLayout},
    BuilderContext, ControlBuilder, Gui, GuiRender, Id,
};

use sprite_render::{Camera, GLSpriteRender, SpriteInstance, SpriteRender};
use winit::{
    dpi::PhysicalSize,
    event::WindowEvent,
    event_loop::{ControlFlow, EventLoopProxy},
    window::{Window, WindowId},
};

use crate::{event_table::EventTable, style::Style, SCREEN_HEIGHT, SCREEN_WIDTH, UserEvent};

mod emulator_ui;
pub use emulator_ui::create_emulator_ui;

mod rom_loading_ui;
pub use rom_loading_ui::create_rom_loading_ui;

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
    pub fn new(window: &Window, proxy: EventLoopProxy<UserEvent>) -> Self {
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
        let mut gui = Gui::new(0.0, 0.0, fonts);
        let gui_render = GuiRender::new(font_texture, textures.white, [128, 128]);

        gui.set(proxy);
        gui.set(textures.clone());
        gui.set(style.clone());

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

pub fn list(
    cb: ControlBuilder,
    ctx: &mut (impl BuilderContext + ?Sized),
    style: &Style,
    list_builder: impl ListBuilder + 'static,
) -> ControlBuilder {
    use crui::widgets::List;
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
        [10.0, 0.0, 0.0, 0.0],
        view,
        v_scroll_bar,
        v_scroll_bar_handle,
        h_scroll_bar,
        h_scroll_bar_handle,
        list_builder,
    ))
}
