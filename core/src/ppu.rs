pub struct Ppu {
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
}

pub fn draw_tile(
    rom: &[u8],
    draw_pixel: &mut impl FnMut(i32, i32, u8),
    tx: i32,
    ty: i32,
    index: usize,
) {
    let i = index * 0x10 + 0x8000;
    for y in 0..8 {
        let a = rom[i + y as usize * 2];
        let b = rom[i + y as usize * 2 + 1];
        for x in 0..8 {
            let color = (((a >> (7 - x)) << 1) & 0b10) | ((b >> (7 - x)) & 0b1);
            draw_pixel(tx + x, ty + y, color);
        }
    }
}

pub fn draw_tiles(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..0x180 {
        let tx = 8 * (i % 16);
        let ty = 8 * (i / 16);

        draw_tile(
            rom,
            draw_pixel,
            tx,
            ty,
            i as usize,
        );
    }
}

pub fn draw_background(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..(32 * 32) {
        let t = rom[0x9800 + i as usize];
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);

        draw_tile(
            rom,
            draw_pixel,
            tx,
            ty,
            t as usize,
        );
    }
}

pub fn draw_window(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..(32 * 32) {
        let t = rom[0x9C00 + i as usize];
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);

        draw_tile(
            rom,
            draw_pixel,
            tx,
            ty,
            t as usize,
        );
    }
}

pub fn draw_sprites(rom: &[u8], draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..40 {
        let i = 0xFE00 + i as usize * 4;
        let data = &rom[i..i + 4];
        let sy = data[0] as i32 - 16;
        let sx = data[1] as i32 - 8;
        let t = data[2];
        let _f = data[3];

        if sy < 0 || sx < 0 {
            continue;
        }
        draw_tile(
            rom,
            draw_pixel,
            sx,
            sy,
            t as usize,
        );
    }
}

pub fn draw_screen(rom: &[u8], ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
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
            let t = rom[0x9800 + i as usize];

            draw_tile(
                rom,
                draw_pixel,
                tx,
                ty,
                t as usize
            );
        }
    }
    draw_sprites(rom, draw_pixel);
}



