use std::sync::Arc;

use crui::graphics::Texture;
use crui::layouts::VBoxLayout;
use crui::{Behaviour, BuilderContext, Id};
use gameroy::gameboy::GameBoy;
use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

use crate::event_table::{EventTable, Handle, FrameUpdated};
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
            gameroy::ppu::draw_background(
                &gb.ppu,
                &mut |x, y, c| {
                    let i = (x + y * 256) as usize * 4;
                    background[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                },
            );
            proxy
                .send_event(UserEvent::UpdateTexture(
                    textures.background,
                    background.into_boxed_slice(),
                ))
                .unwrap();

            let mut window = vec![255; 256 * 256 * 4];
            gameroy::ppu::draw_window(
                &gb.ppu,
                &mut |x, y, c| {
                    let i = (x + y * 256) as usize * 4;
                    window[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                },
            );
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
    _style: &Style,
    textures: &Textures,
) {
    let ppu_viewer = ctx.reserve();
    ctx.create_control_reserved(ppu_viewer)
        .parent(parent)
        .layout(VBoxLayout::default())
        .behaviour(PpuViewer {
            _emulator_updated_event: event_table.register(ppu_viewer),
        })
        .build(ctx);

    ctx.create_control()
        .parent(ppu_viewer)
        .graphic(Texture::new(textures.tilemap, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([128.0, 196.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);

    ctx.create_control()
        .parent(ppu_viewer)
        .graphic(Texture::new(textures.background, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([256.0, 256.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);

    ctx.create_control()
        .parent(ppu_viewer)
        .graphic(Texture::new(textures.window, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([256.0, 256.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);
}
