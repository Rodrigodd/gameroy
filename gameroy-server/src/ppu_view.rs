//! Renders the PPU viewer's images.
//!
//! "No rendering dependency" means no GPU, windowing or graphics API — not "no
//! pixel loops". The core already exposes `draw_tiles`/`draw_background`/
//! `draw_window` as plain callbacks over palette indices, so doing this here costs
//! nothing extra and avoids shipping VRAM, OAM and the whole PPU state to every
//! front-end that wants to show a tilemap.

use gameroy::gameboy::GameBoy;
use gameroy::gameboy::ppu::{draw_background, draw_tiles, draw_window};

use crate::protocol::{IndexedImage, PpuViewer, SpriteInfo};

const TILES_W: u16 = 128;
const TILES_H: u16 = 192;
const MAP_SIZE: u16 = 256;

fn render(width: u16, height: u16, f: impl FnOnce(&mut dyn FnMut(i32, i32, u8))) -> IndexedImage {
    let mut data = vec![0u8; width as usize * height as usize];
    {
        let mut put = |x: i32, y: i32, color: u8| {
            if x < 0 || y < 0 || x >= width as i32 || y >= height as i32 {
                return;
            }
            data[y as usize * width as usize + x as usize] = color;
        };
        f(&mut put);
    }
    IndexedImage {
        width,
        height,
        data,
    }
}

pub fn capture(gb: &GameBoy) -> PpuViewer {
    let ppu = gb.ppu.borrow();

    let tiles = render(TILES_W, TILES_H, |put| {
        draw_tiles(&ppu, &mut |x, y, c| put(x, y, c), ppu.bgp)
    });
    let background = render(MAP_SIZE, MAP_SIZE, |put| {
        draw_background(&ppu, &mut |x, y, c| put(x, y, c))
    });
    let window = render(MAP_SIZE, MAP_SIZE, |put| {
        draw_window(&ppu, &mut |x, y, c| put(x, y, c))
    });

    // OAM is 40 entries of 4 bytes: y, x, tile, flags.
    let oam = ppu
        .oam
        .chunks_exact(4)
        .map(|e| SpriteInfo {
            sy: e[0],
            sx: e[1],
            tile: e[2],
            flags: e[3],
        })
        .collect();

    let sprite_buffer = ppu
        .sprite_buffer
        .iter()
        .map(|s| SpriteInfo {
            sx: s.sx,
            sy: s.sy,
            tile: s.tile,
            flags: s.flags,
        })
        .collect();

    PpuViewer {
        tiles,
        background,
        window,
        oam,
        sprite_buffer,
        sprite_buffer_len: ppu.sprite_buffer_len,
    }
}
