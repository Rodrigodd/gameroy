use std::sync::Arc;

use crui::graphics::Texture;
use crui::layouts::VBoxLayout;
use crui::widgets::{ScrollBar, ScrollView, ViewLayout};
use crui::{Behaviour, BuilderContext, Id};
use gameroy::gameboy::GameBoy;
use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

use crate::event_table::{EventTable, FrameUpdated, Handle};
use crate::ui::Textures;

struct PpuViewer {
    _emulator_updated_event: Handle<FrameUpdated>,
}
impl Behaviour for PpuViewer {
    fn on_event(&mut self, event: Box<dyn std::any::Any>, _this: Id, ctx: &mut crui::Context) {
        if event.is::<FrameUpdated>() {
            let textures = ctx.get::<Textures>();
            const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];

            let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
            let proxy = &ctx.get::<EventLoopProxy<UserEvent>>();
            // TODO: maybe I could clone the gameboy's ppu instead of keeping it lock for so long.
            let gb = gb.lock();

            let mut tiles = vec![255; 128 * 194 * 4];
            gameroy::ppu::draw_tiles(
                &gb.ppu,
                &mut |x, y, c| {
                    let i = (x + y * 128) as usize * 4;
                    tiles[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                },
                gb.ppu.bgp,
            );
            proxy
                .send_event(UserEvent::UpdateTexture(
                    textures.tilemap,
                    tiles.into_boxed_slice(),
                ))
                .unwrap();

            let mut background = vec![255; 256 * 256 * 4];
            gameroy::ppu::draw_background(&gb.ppu, &mut |x, y, c| {
                let i = (x + y * 256) as usize * 4;
                background[i..i + 3].copy_from_slice(&COLOR[c as usize]);
            });
            proxy
                .send_event(UserEvent::UpdateTexture(
                    textures.background,
                    background.into_boxed_slice(),
                ))
                .unwrap();

            let mut window = vec![255; 256 * 256 * 4];
            gameroy::ppu::draw_window(&gb.ppu, &mut |x, y, c| {
                let i = (x + y * 256) as usize * 4;
                window[i..i + 3].copy_from_slice(&COLOR[c as usize]);
            });
            proxy
                .send_event(UserEvent::UpdateTexture(
                    textures.window,
                    window.into_boxed_slice(),
                ))
                .unwrap();
        }
    }
}

pub fn build(
    parent: Id,
    ctx: &mut dyn BuilderContext,
    event_table: &mut EventTable,
    style: &Style,
    textures: &Textures,
) {
    let ppu_viewer = ctx.reserve();
    ctx.create_control_reserved(ppu_viewer)
        .parent(parent)
        .behaviour(PpuViewer {
            _emulator_updated_event: event_table.register(ppu_viewer),
        })
        .build(ctx);

    let scroll_view = ctx.reserve();
    let view = ctx
        .create_control()
        .parent(scroll_view)
        .layout(ViewLayout::new(false, true))
        .build(ctx);
    let content = ctx
        .create_control()
        .parent(view)
        .layout(VBoxLayout::new(2.0, [2.0; 4], -1))
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
        .parent(ppu_viewer)
        .behaviour_and_layout(ScrollView::new(
            view,
            content,
            None,
            Some((v_scroll, v_handle)),
        ))
        .build(ctx);

    ctx.create_control()
        .parent(content)
        .graphic(Texture::new(textures.tilemap, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([128.0, 196.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);

    ctx.create_control()
        .parent(content)
        .graphic(Texture::new(textures.background, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([256.0, 256.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);

    ctx.create_control()
        .parent(content)
        .graphic(Texture::new(textures.window, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([256.0, 256.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);
}
