//! Turning what the server sends into what the panels show.
//!
//! Pure functions over protocol types, deliberately outside any Slint callback:
//! this is the part of the front-end worth testing, and a callback cannot be
//! called from a test without a window.

use gameroy_server::protocol::{CpuState, IndexedImage, PartialScreen, PpuState, SpriteInfo};
use slint::{Rgb8Pixel, SharedPixelBuffer};

use crate::SpriteCell;

/// The four Game Boy shades, lightest first, as the old front-end had them.
pub const PALETTE: [u8; 4] = [255, 180, 100, 0];

/// The same four shades tinted blue, for the part of a paused screen that the
/// frame in progress has not reached yet and so still holds the previous frame.
/// Blue because the Game Boy's own output is grey, so nothing else can be.
const STALE_PALETTE: [(u8, u8, u8); 4] =
    [(120, 140, 255), (85, 100, 190), (48, 58, 125), (14, 18, 60)];

pub fn format_cpu(cpu: &CpuState, clock: u64, frame: u32) -> String {
    let flag = |bit: u8, name: char| if cpu.f & (1 << bit) != 0 { name } else { '-' };
    format!(
        "clock {clock}\n\
         frame {frame}\n\
         \n\
         AF {:02x}{:02x}  {}{}{}{}\n\
         BC {:02x}{:02x}\n\
         DE {:02x}{:02x}\n\
         HL {:02x}{:02x}\n\
         SP {:04x}\n\
         PC {:04x}\n\
         \n\
         IME {:?}\n\
         IF {:02x}  IE {:02x}\n\
         bank {:x}/{:x}",
        cpu.a,
        cpu.f,
        flag(7, 'Z'),
        flag(6, 'N'),
        flag(5, 'H'),
        flag(4, 'C'),
        cpu.b,
        cpu.c,
        cpu.d,
        cpu.e,
        cpu.h,
        cpu.l,
        cpu.sp,
        cpu.pc,
        cpu.ime,
        cpu.interrupt_flag,
        cpu.interrupt_enabled,
        cpu.banks.0,
        cpu.banks.1,
    )
}

/// Renders breakpoint flags as `wrxj`, dashes for the ones that are off. Formatting
/// is a front-end concern: the server sends the bitmask.
pub fn format_break_flags(flags: u8) -> String {
    // Mirrors gameroy::debugger::break_flags.
    const WRITE: u8 = 1 << 0;
    const READ: u8 = 1 << 1;
    const EXECUTE: u8 = 1 << 2;
    const JUMP: u8 = 1 << 3;
    let bit = |f: u8, c: char| if flags & f != 0 { c } else { '-' };
    [
        bit(WRITE, 'w'),
        bit(READ, 'r'),
        bit(EXECUTE, 'x'),
        bit(JUMP, 'j'),
    ]
    .iter()
    .collect()
}

pub fn format_ppu(ppu: &PpuState) -> String {
    format!(
        "LCDC {:02x}  STAT {:02x}\n\
         SCY  {:02x}  SCX  {:02x}\n\
         LY   {:02x}  LYC  {:02x}\n\
         LX   {:>3}\n\
         BGP  {:02x}\n\
         OBP0 {:02x}  OBP1 {:02x}\n\
         WY   {:02x}  WX   {:02x}\n\
         WYC  {:02x}\n\
         state {}\n\
         next  {}",
        ppu.lcdc,
        ppu.stat,
        ppu.scy,
        ppu.scx,
        ppu.ly,
        ppu.lyc,
        ppu.lx,
        ppu.bgp,
        ppu.obp0,
        ppu.obp1,
        ppu.wy,
        ppu.wx,
        ppu.wyc,
        ppu.state,
        ppu.next_clock_count,
    )
}

/// The paused screen, with the pixels this frame has not reached yet tinted.
///
/// Those hold whatever the previous frame left there, so showing them in the
/// normal palette would be showing stale output as if it were current -- the
/// distinction is the entire point of the view.
pub fn partial_screen_image(screen: &PartialScreen) -> slint::Image {
    let img = &screen.pixels;
    let mut buffer = SharedPixelBuffer::<Rgb8Pixel>::new(img.width as u32, img.height as u32);
    let drawn = screen.drawn_pixels as usize;
    for (i, (out, &shade)) in buffer
        .make_mut_slice()
        .iter_mut()
        .zip(img.data.iter())
        .enumerate()
    {
        let shade = (shade & 3) as usize;
        *out = if i < drawn {
            let l = PALETTE[shade];
            Rgb8Pixel::new(l, l, l)
        } else {
            let (r, g, b) = STALE_PALETTE[shade];
            Rgb8Pixel::new(r, g, b)
        };
    }
    slint::Image::from_rgb8(buffer)
}

pub fn sprite_cells(sprites: &[SpriteInfo]) -> Vec<SpriteCell> {
    sprites
        .iter()
        .enumerate()
        .map(|(i, s)| SpriteCell {
            tile: s.tile as i32,
            text: format!(
                "{i:>2}  x {:>3}  y {:>3}\ntile {:02x}  flags {:02x}",
                s.sx, s.sy, s.tile, s.flags
            )
            .into(),
            offscreen: s.is_offscreen(),
        })
        .collect()
}

/// Expands a palette-index image into RGB for display. The server ships indices,
/// so the palette stays a front-end decision.
pub fn indexed_to_image(img: &IndexedImage) -> slint::Image {
    let mut buffer = SharedPixelBuffer::<Rgb8Pixel>::new(img.width as u32, img.height as u32);
    for (out, &shade) in buffer.make_mut_slice().iter_mut().zip(img.data.iter()) {
        let l = PALETTE[(shade & 3) as usize];
        *out = Rgb8Pixel::new(l, l, l);
    }
    slint::Image::from_rgb8(buffer)
}

/// One pixel FIFO as a hex run, labelled so the two lines read as a pair.
///
/// The FIFOs hold 2-bit pixels and are usually short or empty; an empty one says
/// so rather than rendering as a bare label.
pub fn format_fifo(label: &str, pixels: &[u8]) -> String {
    if pixels.is_empty() {
        return format!("{label}: empty");
    }
    let mut out = format!("{label}:");
    for p in pixels {
        out.push_str(&format!(" {p:02x}"));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_a_fifo_and_says_so_when_empty() {
        assert_eq!(format_fifo("bg ", &[]), "bg : empty");
        assert_eq!(format_fifo("obj", &[0, 3, 10]), "obj: 00 03 0a");
    }

    #[test]
    fn break_flags_render_as_wrxj() {
        assert_eq!(format_break_flags(0), "----");
        assert_eq!(format_break_flags(0b1111), "wrxj");
        assert_eq!(format_break_flags(0b0100), "--x-");
    }

    /// The grid slices the atlas by `tile`, so it has to survive the trip.
    #[test]
    fn sprite_cells_carry_the_tile_and_flag_offscreen_ones() {
        let sprites = [
            SpriteInfo {
                sx: 80,
                sy: 64,
                tile: 0x2a,
                flags: 0,
            },
            // Parked off the top, which is how a game hides a sprite.
            SpriteInfo {
                sx: 0,
                sy: 0,
                tile: 0x01,
                flags: 0,
            },
        ];
        let cells = sprite_cells(&sprites);
        assert_eq!(cells[0].tile, 0x2a);
        assert!(!cells[0].offscreen);
        assert!(cells[1].offscreen);
        assert!(cells[0].text.contains("2a"));
    }
}
