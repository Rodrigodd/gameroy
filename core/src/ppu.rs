use crate::gameboy::GameBoy;
use crate::save_state::{LoadStateError, SaveState};

#[derive(PartialEq, Eq)]
pub struct Ppu {
    /// The current screen been render.
    /// Each pixel is a shade of gray, from 0 to 3
    pub screen: [u8; 144 * 160],
    /// sprites that will be rendered in the next mode 3 scanline
    pub sprite_buffer: [[u8; 4]; 10],
    /// the length of the sprite_buffer
    pub sprite_buffer_len: u8,
    /// Window Internal Line Counter
    pub wyc: u8,
    /// FF40: LCD Control Register
    pub lcdc: u8,
    /// FF41: LCD Status Register
    pub stat: u8,
    /// FF42: Scroll Y Register
    pub scy: u8,
    /// FF43: Scroll x Register
    pub scx: u8,
    /// FF44: LCDC Y-Coordinate
    pub ly: u8,
    /// FF45: LY Compare
    pub lyc: u8,
    /// FF47: BG & Window Pallete Data
    pub bgp: u8,
    /// FF4A: Window Y Position
    pub wy: u8,
    /// FF4B: Window X Position
    pub wx: u8,
}
impl SaveState for Ppu {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.screen.save_state(data)?;
        self.sprite_buffer.save_state(data)?;
        self.sprite_buffer_len.save_state(data)?;
        self.wyc.save_state(data)?;
        self.lcdc.save_state(data)?;
        self.stat.save_state(data)?;
        self.scy.save_state(data)?;
        self.scx.save_state(data)?;
        self.ly.save_state(data)?;
        self.lyc.save_state(data)?;
        self.bgp.save_state(data)?;
        self.wy.save_state(data)?;
        self.wx.save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        self.screen.load_state(data)?;
        self.sprite_buffer.load_state(data)?;
        self.sprite_buffer_len.load_state(data)?;
        self.wyc.load_state(data)?;
        self.lcdc.load_state(data)?;
        self.stat.load_state(data)?;
        self.scy.load_state(data)?;
        self.scx.load_state(data)?;
        self.ly.load_state(data)?;
        self.lyc.load_state(data)?;
        self.bgp.load_state(data)?;
        self.wy.load_state(data)?;
        self.wx.load_state(data)?;
        Ok(())
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            screen: [0; 144 * 160],
            sprite_buffer: Default::default(),
            sprite_buffer_len: Default::default(),
            wyc: Default::default(),
            lcdc: Default::default(),
            stat: Default::default(),
            scy: Default::default(),
            scx: Default::default(),
            ly: Default::default(),
            lyc: Default::default(),
            bgp: Default::default(),
            wy: Default::default(),
            wx: Default::default(),
        }
    }
}

impl Ppu {
    /// Set the ppu's lcdc.
    pub fn set_lcdc(&mut self, lcdc: u8) {
        self.lcdc = lcdc;
    }

    /// Get a reference to the ppu's lcdc.
    pub fn lcdc(&self) -> u8 {
        self.lcdc
    }

    /// Set the ppu's stat.
    pub fn set_stat(&mut self, stat: u8) {
        self.stat = stat;
    }

    /// Get a reference to the ppu's stat.
    pub fn stat(&self) -> u8 {
        self.stat
    }

    /// Set the ppu's scy.
    pub fn set_scy(&mut self, scy: u8) {
        self.scy = scy;
    }

    /// Get a reference to the ppu's scy.
    pub fn scy(&self) -> u8 {
        self.scy
    }

    /// Set the ppu's scx.
    pub fn set_scx(&mut self, scx: u8) {
        self.scx = scx;
    }

    /// Get a reference to the ppu's scx.
    pub fn scx(&self) -> u8 {
        self.scx
    }

    /// Set the ppu's ly.
    pub fn set_ly(&mut self, ly: u8) {
        self.ly = ly;
    }

    /// Get a reference to the ppu's ly.
    pub fn ly(&self) -> u8 {
        self.ly
    }

    /// Set the ppu's lyc.
    pub fn set_lyc(&mut self, lyc: u8) {
        self.lyc = lyc;
    }

    /// Get a reference to the ppu's lyc.
    pub fn lyc(&self) -> u8 {
        self.lyc
    }

    /// Set the ppu's bgp.
    pub fn set_bgp(&mut self, bgp: u8) {
        self.bgp = bgp;
    }

    /// Get a reference to the ppu's bgp.
    pub fn bgp(&self) -> u8 {
        self.bgp
    }

    /// Set the ppu's wx.
    pub fn set_wx(&mut self, wx: u8) {
        self.wx = wx;
    }

    /// Get a reference to the ppu's wx.
    pub fn wx(&self) -> u8 {
        self.wx
    }

    /// Set the ppu's wy.
    pub fn set_wy(&mut self, wy: u8) {
        self.wy = wy;
    }

    /// Get a reference to the ppu's wy.
    pub fn wy(&self) -> u8 {
        self.wy
    }

    fn search_objects(&mut self, memory: &mut [u8]) {
        self.sprite_buffer_len = 0;
        let sprite_height = if self.lcdc & 0x04 != 0 { 16 } else { 8 };
        for i in 0..40 {
            let i = 0xFE00 + i as usize * 4;
            let data = &memory[i..i + 4];
            let sy = data[0];
            let sx = data[1];
            let t = data[2];
            let f = data[3];

            if sx > 0
                && self.ly as u16 + 16 >= sy as u16
                && self.ly as u16 + 16 < sy as u16 + sprite_height
            {
                self.sprite_buffer[self.sprite_buffer_len as usize] = [sy, sx, t, f];
                self.sprite_buffer_len += 1;
            }
            if self.sprite_buffer_len == 10 {
                break;
            }
        }
        // sort buffer by priority, in asce
        self.sprite_buffer[0..self.sprite_buffer_len as usize].reverse();
        self.sprite_buffer[0..self.sprite_buffer_len as usize].sort_by_key(|x| !x[1]);
    }

    pub fn update(gb: &mut GameBoy) {
        use crate::consts;
        gb.ppu.ly = ((gb.clock_count / 456) % 153) as u8;
        let lx = gb.clock_count % 456;

        let set_stat_int = |s: &mut Self, memory: &mut [u8], i: u8| {
            if s.stat & (1 << i) != 0 {
                memory[consts::IF as usize] |= 1 << 1;
            }
        };
        let set_mode = |s: &mut Self, memory: &mut [u8], mode: u8| {
            debug_assert!(mode <= 3);
            s.stat = (s.stat & !0b11) | mode;
            // object search at mode 2
            if mode == 2 {
                s.search_objects(memory);
            }
            if mode == 0 {
                draw_scan_line(memory, s);
            }
        };

        // LY==LYC Interrupt
        let ly = gb.ppu.ly;
        let lyc = gb.ppu.lyc;
        if ly != 0 && (lx == 4 && ly == lyc || ly == 153 && lx == 12 && 0 == lyc) {
            // STAT Coincidente Flag
            gb.ppu.stat |= 1 << 2;
            // LY == LYC STAT Interrupt
            set_stat_int(&mut gb.ppu, &mut gb.memory, 6)
        }

        let mode = gb.ppu.stat & 0b11;
        match mode {
            0 if gb.ppu.ly == 144 => {
                set_mode(&mut gb.ppu, &mut gb.memory, 1);
                // V-Blank Interrupt
                if let Some(mut v_blank) = gb.v_blank.take() {
                    v_blank(gb);
                    gb.v_blank = Some(v_blank);
                }
                gb.memory[consts::IF as usize] |= 1 << 0;
                // Mode 1 STAT Interrupt
                set_stat_int(&mut gb.ppu, &mut gb.memory, 4);
            }
            0 | 1 => {
                if mode == 0 && lx < 80 || mode == 1 && gb.ppu.ly < 144 {
                    if mode == 1 {
                        gb.ppu.wyc = 0;
                    }
                    set_mode(&mut gb.ppu, &mut gb.memory, 2);
                    // Mode 2 STAT Interrupt
                    set_stat_int(&mut gb.ppu, &mut gb.memory, 5);
                }
            }
            2 if lx >= 80 => {
                set_mode(&mut gb.ppu, &mut gb.memory, 3);
            }
            3 if lx >= 80 + 172 => {
                set_mode(&mut gb.ppu, &mut gb.memory, 0);
                // Mode 0 STAT Interrupt
                set_stat_int(&mut gb.ppu, &mut gb.memory, 3);
            }
            4..=255 => unreachable!(),
            _ => {}
        }
    }
}

pub fn draw_tile(
    rom: &[u8],
    draw_pixel: &mut impl FnMut(i32, i32, u8),
    tx: i32,
    ty: i32,
    index: usize,
    pallete: u8,
    alpha: bool,
) {
    let i = index * 0x10 + 0x8000;
    for y in 0..8 {
        let a = rom[i + y as usize * 2];
        let b = rom[i + y as usize * 2 + 1];
        for x in 0..8 {
            let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
            if alpha && color == 0 {
                continue;
            }
            let color = (pallete >> (color * 2)) & 0b11;
            draw_pixel(tx + x, ty + y, color);
        }
    }
}

pub fn draw_tiles(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8), pallete: u8) {
    for i in 0..0x180 {
        let tx = 8 * (i % 16);
        let ty = 8 * (i / 16);

        draw_tile(rom, draw_pixel, tx, ty, i as usize, pallete, false);
    }
}

pub fn draw_background(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8), pallete: u8) {
    for i in 0..(32 * 32) {
        let t = rom[0x9800 + i as usize];
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);

        draw_tile(rom, draw_pixel, tx, ty, t as usize, pallete, false);
    }
}

pub fn draw_window(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8), pallete: u8) {
    for i in 0..(32 * 32) {
        let t = rom[0x9C00 + i as usize];
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);

        draw_tile(rom, draw_pixel, tx, ty, t as usize, pallete, false);
    }
}

pub fn draw_sprites(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..40 {
        let i = 0xFE00 + i as usize * 4;
        let data = &rom[i..i + 4];
        let sy = data[0] as i32 - 16;
        let sx = data[1] as i32 - 8;
        let t = data[2];
        let f = data[3];

        let pallete = if f & 0x10 != 0 {
            // OBP1
            rom[0xFF49]
        } else {
            // OBP0
            rom[0xFF48]
        };

        if sy < 0 || sx < 0 {
            continue;
        }
        draw_tile(rom, draw_pixel, sx, sy, t as usize, pallete, true);
    }
}

pub fn draw_screen(rom: &[u8], ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    // Draw Background
    if true {
        let scx = ppu.scx();
        let scy = ppu.scy();
        let xs = scx / 8;
        let ys = scy / 8;
        for y in ys..ys + 19 {
            for x in xs..xs + 21 {
                let tx = 8 * x as i32 - scx as i32;
                let ty = 8 * y as i32 - scy as i32;
                let x = x % 32;
                let y = y % 32;
                let i = x as usize + y as usize * 32;
                // BG Tile Map Select
                let address = if ppu.lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
                let mut tile = rom[address + i as usize] as usize;

                // if is using 8800 method
                if ppu.lcdc & 0x10 == 0 {
                    tile += 0x100;
                    if tile >= 0x180 {
                        tile -= 0x100;
                    }
                }

                draw_tile(rom, draw_pixel, tx, ty, tile, ppu.bgp, false);
            }
        }
    }
    // Draw Window, if enabled
    if ppu.lcdc & 0x20 != 0 {
        let wx = ppu.wx();
        let wy = ppu.wy();
        for y in 0..19 - wy / 8 {
            for x in 0..21 - wx / 8 {
                let tx = 8 * x as i32 + wx as i32;
                let ty = 8 * y as i32 + wy as i32;
                let x = x % 32;
                let y = y % 32;
                let i = x as usize + y as usize * 32;
                // BG Tile Map Select
                let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
                let mut tile = rom[address + i as usize] as usize;

                // if is using 8800 method
                if ppu.lcdc & 0x10 == 0 {
                    tile += 0x100;
                    if tile >= 0x180 {
                        tile -= 0x100;
                    }
                }

                draw_tile(rom, draw_pixel, tx, ty, tile, ppu.bgp, false);
            }
        }
    }
    // Draw Sprites, if enabled
    if ppu.lcdc & 0x02 != 0 {
        draw_sprites(rom, draw_pixel);
    }
}

pub fn draw_scan_line(rom: &[u8], ppu: &mut Ppu) {
    // Draw background
    if ppu.lcdc & 0x01 != 0 {
        // (py, px) is a pixel in the background map
        // (lx, ly) is a pixel in the lcd screen
        let ly = ppu.ly;
        let py = ((ppu.scy as u16 + ppu.ly as u16) % 256) as u8;
        for lx in 0..160 {
            let px = ((lx as u16 + ppu.scx as u16) % 256) as u8;

            let i = (px as usize / 8) + (py as usize / 8) * 32;

            // BG Tile Map Select
            let address = if ppu.lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
            let mut tile = rom[address + i as usize] as usize;

            // if is using 8800 method
            if ppu.lcdc & 0x10 == 0 {
                tile += 0x100;
                if tile >= 0x180 {
                    tile -= 0x100;
                }
            }

            {
                let pallete = ppu.bgp;
                let alpha = false;
                let i = tile * 0x10 + 0x8000;
                let y = py % 8;
                let a = rom[i + y as usize * 2];
                let b = rom[i + y as usize * 2 + 1];
                let x = px % 8;
                let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
                if alpha && color == 0 {
                    continue;
                }
                let color = (pallete >> (color * 2)) & 0b11;
                // draw_pixel(lx as i32, ly as i32, color);
                ppu.screen[ly as usize * 160 + lx as usize] = color;
            };
        }
    } else {
        let ly = ppu.ly;
        ppu.screen[ly as usize * 160..(ly as usize + 1) * 160].copy_from_slice(&[0; 160]);
    }

    // Draw window
    if ppu.lcdc & 0x21 == 0x21 && ppu.ly >= ppu.wy && ppu.wx <= 166 {
        // (py, px) is a pixel in the window map
        // (lx, ly) is a pixel in the lcd screen
        let ly = ppu.ly;
        let py = ppu.wyc;
        ppu.wyc += 1;
        for lx in ppu.wx.saturating_sub(7)..160 {
            let px = lx + 7 - ppu.wx;

            let i = (px as usize / 8) + (py as usize / 8) * 32;

            // BG Tile Map Select
            let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
            let mut tile = rom[address + i as usize] as usize;

            // if is using 8800 method
            if ppu.lcdc & 0x10 == 0 {
                tile += 0x100;
                if tile >= 0x180 {
                    tile -= 0x100;
                }
            }

            {
                let pallete = ppu.bgp;
                let alpha = false;
                let i = tile * 0x10 + 0x8000;
                let y = py % 8;
                let a = rom[i + y as usize * 2];
                let b = rom[i + y as usize * 2 + 1];
                let x = px % 8;
                let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
                if alpha && color == 0 {
                    continue;
                }
                let color = (pallete >> (color * 2)) & 0b11;
                // draw_pixel(lx as i32, ly as i32, color);
                ppu.screen[ly as usize * 160 + lx as usize] = color;
            };
        }
    }

    // Draw Sprites, if enabled
    if ppu.lcdc & 0x02 != 0 {
        for &[sy, sx, t, flags] in &ppu.sprite_buffer[0..ppu.sprite_buffer_len as usize] {
            let sy = sy as i32 - 16;
            let sx = sx as i32 - 8;

            // Y-Flip
            let py = if flags & 0x40 != 0 {
                let height = if ppu.lcdc & 0x04 != 0 { 16 } else { 8 };
                height - 1 - (ppu.ly as i32 - sy)
            } else {
                ppu.ly as i32 - sy
            };

            let tile = if ppu.lcdc & 0x04 != 0 {
                // sprite with 2 tiles of height
                ((t & !1) + py as u8 / 8) as usize
            } else {
                t as usize
            };

            let pallete = if flags & 0x10 != 0 {
                // OBP1
                rom[0xFF49]
            } else {
                // OBP0
                rom[0xFF48]
            };

            {
                let y = py % 8;
                let i = tile * 0x10 + 0x8000;
                let a = rom[i + y as usize * 2];
                let b = rom[i + y as usize * 2 + 1];
                let ly = ppu.ly;
                for x in 0.max(-sx)..8.min(160 - sx) {
                    let lx = sx + x;
                    // X-Flip
                    let x = if flags & 0x20 != 0 { x } else { 7 - x };
                    let color = (((b >> x) << 1) & 0b10) | ((a >> x) & 0b1);
                    if color == 0 {
                        continue;
                    }
                    // Object to Background Priority
                    if flags & 0x80 != 0 && ppu.screen[ly as usize * 160 + lx as usize] != 0 {
                        continue;
                    }
                    let color = (pallete >> (color * 2)) & 0b11;
                    ppu.screen[ly as usize * 160 + lx as usize] = color;
                }
            };
        }
    }
}
