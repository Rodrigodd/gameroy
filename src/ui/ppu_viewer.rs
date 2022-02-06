use std::sync::Arc;

use crui::graphics::Texture;
use crui::layouts::VBoxLayout;
use crui::{Behaviour, BuilderContext, Id};
use gameroy::gameboy::GameBoy;
use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

use crate::event_table::{EmulatorUpdated, EventTable, Handle};

struct PpuViewer {
    tilemap_texture: u32,
    _emulator_updated_event: Handle<EmulatorUpdated>,
}
impl Behaviour for PpuViewer {
    fn on_event(&mut self, event: Box<dyn std::any::Any>, _this: Id, ctx: &mut crui::Context) {
        if event.is::<EmulatorUpdated>() {
            const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
            let img_data = {
                let mut img_data = vec![255; 128 * 194 * 4];
                let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
                let gb = gb.lock();
                gameroy::ppu::draw_tiles(
                    &gb.ppu,
                    &mut |x, y, c| {
                        let i = (x + y * 128) as usize * 4;
                        img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                    },
                    gb.ppu.bgp,
                );
                img_data
            };
            println!("send update event!!");
            ctx.get::<EventLoopProxy<UserEvent>>()
                .send_event(UserEvent::UpdateTexture(
                    self.tilemap_texture,
                    img_data.into_boxed_slice(),
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
    tilemap_texture: u32,
) {
    let ppu_viewer = ctx.reserve();
    ctx.create_control_reserved(ppu_viewer)
        .parent(parent)
        .layout(VBoxLayout::default())
        .behaviour(PpuViewer {
            tilemap_texture,
            _emulator_updated_event: event_table.register(ppu_viewer),
        })
        .build(ctx);

    ctx.create_control()
        .parent(parent)
        .graphic(Texture::new(tilemap_texture, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([128.0, 196.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);
}
