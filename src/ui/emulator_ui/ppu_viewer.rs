use std::sync::Arc;

use crui::{
    graphics::{Graphic, Texture},
    layouts::{FitText, GridLayout, HBoxLayout, VBoxLayout},
    text::Text,
    Behaviour, BuilderContext, Color, Context, Id, InputFlags, MouseEvent,
};
use gameroy::gameboy::GameBoy;
use parking_lot::Mutex;
use winit::event_loop::EventLoopProxy;

use crate::{
    event_table::{EmulatorUpdated, EventTable, FrameUpdated, Handle},
    style::Style,
    ui::Textures,
    UserEvent,
};

use crate::ui::scroll_viewer;

struct TilemapViewer {
    info_text: Id,
    tilemap: Id,
    width: u8,
    height: u8,
    info: Box<dyn FnMut(u8, u8, &mut Context) -> String>,
}
impl Behaviour for TilemapViewer {
    fn on_start(&mut self, _this: Id, ctx: &mut Context) {
        let info = (self.info)(0, 0, ctx);
        ctx.get_graphic_mut(self.info_text).set_text(&info);
    }

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
    oam_sprites: [[Id; 2]; 40],
    buffer_sprites: [[Id; 2]; 10],
    _frame_updated_event: Handle<FrameUpdated>,
    _emulator_updated_event: Handle<EmulatorUpdated>,
}
impl PpuViewer {
    fn update(&mut self, ctx: &mut Context, emulator_updated: bool) {
        let textures = ctx.get::<Textures>();
        const COLOR: [[u8; 3]; 4] = [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
        let gb = ctx.get::<Arc<Mutex<GameBoy>>>();
        let proxy = &ctx.get::<EventLoopProxy<UserEvent>>();
        let ppu = gb.lock().ppu.borrow().clone();
        if emulator_updated {
            let mut debug_screen = vec![255; 160 * 144 * 4];
            let screen = &ppu.screen;
            let curr_i = (ppu.ly as usize * 160 + ppu.curr_x.min(160) as usize).min(160 * 144);

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
        gameroy::gameboy::ppu::draw_tiles(
            &ppu,
            &mut |x, y, c| {
                let i = (x + y * 128) as usize * 4;
                let color = COLOR[c as usize];
                tiles[i..i + 3].copy_from_slice(&color);
            },
            ppu.bgp,
        );
        proxy
            .send_event(UserEvent::UpdateTexture(
                textures.tilemap,
                tiles.into_boxed_slice(),
            ))
            .unwrap();
        let mut background = vec![255; 256 * 256 * 4];
        gameroy::gameboy::ppu::draw_background(&ppu, &mut |x, y, c| {
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
        gameroy::gameboy::ppu::draw_window(&ppu, &mut |x, y, c| {
            let i = (x + y * 256) as usize * 4;
            window[i..i + 3].copy_from_slice(&COLOR[c as usize]);
        });
        proxy
            .send_event(UserEvent::UpdateTexture(
                textures.window,
                window.into_boxed_slice(),
            ))
            .unwrap();

        for (i, &[view, text]) in self.oam_sprites.iter().enumerate() {
            let i = i * 4;
            let data = &ppu.oam[i..i + 4];
            let sy = data[0];
            let sx = data[1];
            let tile = data[2];
            let flags = data[3];

            // let palette = if flags & 0x10 != 0 {
            //     ppu.obp1
            // } else {
            //     ppu.obp0
            // };

            let x = tile % 16;
            let y = tile / 16;
            let uv_rect = [x as f32 / 16.0, y as f32 / 24.0, 1.0 / 16.0, 1.0 / 24.0];

            if let Graphic::Texture(t) = ctx.get_graphic_mut(view) {
                if sy < 16 || sx < 8 {
                    t.color = 0xff0000ff.into();
                } else {
                    t.color = Color::WHITE;
                }
                t.uv_rect = uv_rect;
            }

            ctx.get_graphic_mut(text).set_text(&format!(
                "x: {:02x} y: {:02x}\ntile: {:02x}\nflag: {:02x}",
                sx, sy, tile, flags
            ))
        }

        for (i, &[view, text]) in self.buffer_sprites.iter().enumerate() {
            let gameroy::gameboy::ppu::Sprite {
                sx,
                sy,
                tile,
                flags,
            } = ppu.sprite_buffer[i];

            // let palette = if flags & 0x10 != 0 {
            //     ppu.obp1
            // } else {
            //     ppu.obp0
            // };

            let x = tile % 16;
            let y = tile / 16;
            let uv_rect = [x as f32 / 16.0, y as f32 / 24.0, 1.0 / 16.0, 1.0 / 24.0];

            if let Graphic::Texture(t) = ctx.get_graphic_mut(view) {
                if i >= ppu.sprite_buffer_len as usize {
                    t.color = 0xff0000ff.into();
                } else {
                    t.color = Color::WHITE;
                }
                t.uv_rect = uv_rect;
            }

            ctx.get_graphic_mut(text).set_text(&format!(
                "x: {:02x} y: {:02x}\ntile: {:02x}\nflag: {:02x}",
                sx, sy, tile, flags
            ))
        }
    }
}
impl Behaviour for PpuViewer {
    fn on_event(&mut self, event: Box<dyn std::any::Any>, _this: Id, ctx: &mut crui::Context) {
        if event.is::<FrameUpdated>() || event.is::<EmulatorUpdated>() {
            let emulator_updated = event.is::<EmulatorUpdated>();
            self.update(ctx, emulator_updated);
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
    let scroll_view = ctx.reserve();
    let content = ctx.reserve();

    scroll_viewer(ctx, scroll_view, content, style)
        .parent(ppu_viewer)
        .build(ctx);

    build_tilemap_viewer(ctx, textures.tilemap, style, content, 16, 24, |x, y, _| {
        format!(
            "tilemap:\ntile number: {:02x}\nx: {:02x} y: {:02x}",
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
            let gb = ctx.get::<Arc<Mutex<GameBoy>>>().lock();
            let ppu = gb.ppu.borrow();

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
            format!(
                "background:\ntile number: {:02x}\nx: {:02x} y: {:02x}",
                tile, x, y
            )
        },
    );

    build_tilemap_viewer(ctx, textures.window, style, content, 32, 32, |x, y, ctx| {
        let gb = ctx.get::<Arc<Mutex<GameBoy>>>().lock();
        let ppu = gb.ppu.borrow();

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
        format!(
            "window:\ntile number: {:02x}\nx: {:02x} y: {:02x}",
            tile, x, y
        )
    });

    let oam_viewer = ctx.reserve();
    ctx.create_control()
        .parent(content)
        .layout(VBoxLayout::default())
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "Sprites in OAM".to_string(),
                (-1, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
        })
        .child_reserved(oam_viewer, ctx, |cb, _| {
            cb.layout(GridLayout::new([2.0; 2], [2.0; 4], 8))
        })
        .build(ctx);

    let oam_sprites: [[Id; 2]; 40] = [(); 40].map(|_| [ctx.reserve(), ctx.reserve()]);

    for &[view, text] in &oam_sprites {
        let tile = ctx
            .create_control()
            .parent(oam_viewer)
            .layout(VBoxLayout::default())
            .graphic(style.background.clone())
            .build(ctx);
        ctx.create_control_reserved(view)
            .parent(tile)
            .graphic(Texture::new(textures.tilemap, [0.0; 4]))
            .min_size([6.0 * 8.0, 6.0 * 8.0])
            .fill_x(crui::RectFill::ShrinkCenter)
            .fill_y(crui::RectFill::ShrinkCenter)
            .build(ctx);
        ctx.create_control_reserved(text)
            .parent(tile)
            .graphic(Text::new(
                "x: 10 y: 10\ntile: aa\nflag: 00".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
            .build(ctx);
    }

    let buffer_viewer = ctx.reserve();
    ctx.create_control()
        .parent(content)
        .layout(VBoxLayout::default())
        .child(ctx, |cb, _| {
            cb.graphic(Text::new(
                "Sprites in Buffer".to_string(),
                (-1, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
        })
        .child_reserved(buffer_viewer, ctx, |cb, _| {
            cb.layout(GridLayout::new([2.0; 2], [2.0; 4], 5))
        })
        .build(ctx);

    let buffer_sprites: [[Id; 2]; 10] = [(); 10].map(|_| [ctx.reserve(), ctx.reserve()]);

    for &[view, text] in &buffer_sprites {
        let tile = ctx
            .create_control()
            .parent(buffer_viewer)
            .layout(VBoxLayout::default())
            .graphic(style.background.clone())
            .build(ctx);
        ctx.create_control_reserved(view)
            .parent(tile)
            .graphic(Texture::new(textures.tilemap, [0.0; 4]))
            .min_size([6.0 * 8.0, 6.0 * 8.0])
            .fill_x(crui::RectFill::ShrinkCenter)
            .fill_y(crui::RectFill::ShrinkCenter)
            .build(ctx);
        ctx.create_control_reserved(text)
            .parent(tile)
            .graphic(Text::new(
                "x: 10 y: 10\ntile: aa\nflag: 00".to_string(),
                (0, 0),
                style.text_style.clone(),
            ))
            .layout(FitText)
            .build(ctx);
    }

    ctx.create_control_reserved(ppu_viewer)
        .parent(parent)
        .behaviour(PpuViewer {
            oam_sprites,
            buffer_sprites,
            _frame_updated_event: event_table.register(ppu_viewer),
            _emulator_updated_event: event_table.register(ppu_viewer),
        })
        .build(ctx);
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
    let tilemap_text = ctx
        .create_control()
        .parent(tilemap_viewer)
        .graphic(Text::new(String::new(), (-1, -1), style.text_style.clone()))
        .min_size([140.0, 16.0])
        .build(ctx);
    let tilemap = ctx
        .create_control()
        .parent(tilemap_viewer)
        .graphic(Texture::new(texture, [0.0, 0.0, 1.0, 1.0]))
        .min_size([2.0 * 8.0 * width as f32, 2.0 * 8.0 * height as f32])
        .expand_x(true)
        .fill_x(crui::RectFill::ShrinkStart)
        .fill_y(crui::RectFill::ShrinkCenter)
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
