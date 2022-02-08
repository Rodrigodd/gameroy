use std::sync::Arc;

use crui::graphics::Texture;
use crui::layouts::{HBoxLayout, VBoxLayout};
use crui::text::Text;
use crui::widgets::{ScrollBar, ScrollView, ViewLayout};
use crui::{Behaviour, BuilderContext, Context, Id, InputFlags, MouseEvent};
use gameroy::gameboy::GameBoy;
use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

use crate::style::Style;
use crate::UserEvent;

use crate::event_table::{EmulatorUpdated, EventTable, FrameUpdated, Handle};
use crate::ui::Textures;

struct TilemapViewer {
    info_text: Id,
    tilemap: Id,
    width: u8,
    height: u8,
    info: Box<dyn FnMut(u8, u8, &mut Context) -> String>,
}
impl Behaviour for TilemapViewer {
    fn input_flags(&self) -> InputFlags {
        InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: crui::MouseInfo, _this: Id, ctx: &mut crui::Context) {
        match mouse.event {
            MouseEvent::Enter => {}
            MouseEvent::Exit => {}
            MouseEvent::Down(_) => {}
            MouseEvent::Up(_) => {}
            MouseEvent::Moved => {
                let tilemap = ctx.get_rect(self.tilemap);
                let rel_x = (mouse.pos[0] - tilemap[0]) / (tilemap[2] - tilemap[0]);
                let rel_y = (mouse.pos[1] - tilemap[1]) / (tilemap[3] - tilemap[1]);

                if rel_x >= 0.0 && rel_x < 1.0 && rel_y >= 0.0 && rel_y < 1.0 {
                    let x = (rel_x * self.width as f32) as u8;
                    let y = (rel_y * self.height as f32) as u8;

                    let info = (self.info)(x, y, ctx);
                    ctx.get_graphic_mut(self.info_text).set_text(&info);
                }
            }
            MouseEvent::None => {}
        }
    }
}

struct PpuViewer {
    _frame_updated_event: Handle<FrameUpdated>,
    _emulator_updated_event: Handle<EmulatorUpdated>,
}
impl Behaviour for PpuViewer {
    fn on_event(&mut self, event: Box<dyn std::any::Any>, _this: Id, ctx: &mut crui::Context) {
        if event.is::<FrameUpdated>() || event.is::<EmulatorUpdated>() {
            let textures = ctx.get::<Textures>();
            const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];

            let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
            let proxy = &ctx.get::<EventLoopProxy<UserEvent>>();
            // TODO: maybe I could clone the gameboy's ppu instead of keeping it lock for so long.
            let gb = gb.lock();

            if event.is::<EmulatorUpdated>() {
                let mut debug_screen = vec![255; 160 * 144 * 4];
                let screen = &gb.ppu.screen;
                let curr_i = (gb.ppu.ly as usize * 160 + gb.ppu.curr_x as usize).min(160 * 144);

                for i in 0..curr_i {
                    let c = screen[i];
                    let i = i * 4;
                    debug_screen[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                }

                const OLD_COLOR: [[u8; 3]; 4] = [[0, 0, 200], [0, 0, 110], [0, 0, 85], [0, 0, 60]];
                for i in curr_i..screen.len() {
                    let c = screen[i];
                    let i = i * 4;
                    debug_screen[i..i + 3].copy_from_slice(&OLD_COLOR[c as usize]);
                }

                proxy
                    .send_event(UserEvent::UpdateTexture(
                        textures.screen,
                        debug_screen.into_boxed_slice(),
                    ))
                    .unwrap();
            }

            let mut tiles = vec![255; 128 * 194 * 4];
            gameroy::ppu::draw_tiles(
                &gb.ppu,
                &mut |x, y, c| {
                    let i = (x + y * 128) as usize * 4;
                    let color = COLOR[c as usize];
                    tiles[i..i + 3].copy_from_slice(&color);
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
            _frame_updated_event: event_table.register(ppu_viewer),
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

    build_tilemap_viewer(ctx, textures.tilemap, style, content, 16, 24, |x, y, _| {
        format!(
            "tile number: {:02x}\nx: {:02x} y: {:02x}",
            y as u16 * 16 + x as u16,
            x,
            y
        )
    });

    build_tilemap_viewer(
        ctx,
        textures.background,
        style,
        content,
        32,
        32,
        |x, y, ctx| {
            let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
            let ppu = &gb.lock().ppu;

            let i = y as u16 * 32 + x as u16;
            let address = if ppu.lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
            let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

            // if is using 8800 method
            if ppu.lcdc & 0x10 == 0 {
                tile += 0x100;
                if tile >= 0x180 {
                    tile -= 0x100;
                }
            }
            format!("tile number: {:02x}\nx: {:02x} y: {:02x}", tile, x, y)
        },
    );

    build_tilemap_viewer(ctx, textures.window, style, content, 32, 32, |x, y, ctx| {
        let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
        let ppu = &gb.lock().ppu;

        let i = y as u16 * 32 + x as u16;
        let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
        let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

        // if is using 8800 method
        if ppu.lcdc & 0x10 == 0 {
            tile += 0x100;
            if tile >= 0x180 {
                tile -= 0x100;
            }
        }
        format!("tile number: {:02x}\nx: {:02x} y: {:02x}", tile, x, y)
    });
}

fn build_tilemap_viewer(
    ctx: &mut dyn BuilderContext,
    texture: u32,
    style: &Style,
    parent: Id,
    width: u8,
    height: u8,
    info: impl Fn(u8, u8, &mut Context) -> String + 'static,
) {
    let tilemap_viewer = ctx.reserve();
    let tilemap = ctx
        .create_control()
        .parent(tilemap_viewer)
        .graphic(Texture::new(texture, [0.0, 0.0, 1.0, 1.0]))
        .min_size([2.0 * 8.0 * width as f32, 2.0 * 8.0 * height as f32])
        .expand_x(true)
        .fill_x(crui::RectFill::ShrinkStart)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);
    let tilemap_text = ctx
        .create_control()
        .parent(tilemap_viewer)
        .graphic(Text::new(String::new(), (0, 0), style.text_style.clone()))
        .min_size([140.0, 16.0])
        .build(ctx);
    ctx.create_control_reserved(tilemap_viewer)
        .parent(parent)
        .layout(HBoxLayout::default())
        .behaviour(TilemapViewer {
            width,
            height,
            tilemap,
            info_text: tilemap_text,
            info: Box::new(info),
        })
        .build(ctx);
}
