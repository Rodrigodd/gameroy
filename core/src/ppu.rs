use crate::gameboy::GameBoy;
use crate::save_state::{LoadStateError, SaveState};

#[derive(PartialEq, Eq, Default, Clone, Debug)]
struct PixelFifo {
    queue: [u8; 16],
    /// next position to push
    head: u8,
    /// next position to pop
    tail: u8,
}
impl SaveState for PixelFifo {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.queue.save_state(data)?;
        self.head.save_state(data)?;
        self.tail.save_state(data)?;

        Ok(())
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        self.queue.load_state(data)?;
        self.head.load_state(data)?;
        self.tail.load_state(data)?;

        Ok(())
    }
}
impl PixelFifo {
    fn is_empty(&self) -> bool {
        self.head == self.tail
    }

    fn clear(&mut self) {
        self.head = 0;
        self.tail = 0;
    }

    fn push_background(&mut self, tile_low: u8, tile_hight: u8) {
        for i in (0..8).rev() {
            let color = (((tile_hight >> i) & 0x01) << 1) | ((tile_low >> i) & 0x01);
            debug_assert!(color < 4);
            let pixel = color;
            self.queue[self.head as usize] = pixel;
            self.head = (self.head + 1) % self.queue.len() as u8;
            debug_assert_ne!(self.head, self.tail);
        }
    }

    fn push_sprite(
        &mut self,
        tile_low: u8,
        tile_hight: u8,
        cut_off: u8,
        palette: bool,
        background_priority: bool,
    ) {
        let pixel = |x| {
            let color: u8 = (((tile_hight >> x) & 0x01) << 1) | ((tile_low >> x) & 0x01);
            debug_assert!(color < 4);
            let pixel = color | ((background_priority as u8) << 3) | ((palette as u8) << 4);
            pixel
        };

        let mut cursor = self.tail;
        let mut x = 8 - cut_off;
        // overwrite pixels in fifo, but only if 0
        while cursor != self.head && x != 0 {
            x -= 1;
            let color = self.queue[cursor as usize] & 0b11;
            if color == 0 {
                self.queue[cursor as usize] = pixel(x);
            }
            cursor = (cursor + 1) % self.queue.len() as u8;
        }
        // write remained
        for x in (0..x).rev() {
            self.queue[self.head as usize] = pixel(x);
            self.head = (self.head + 1) % self.queue.len() as u8;
            debug_assert_ne!(self.head, self.tail);
        }
    }

    fn pop_front(&mut self) -> Option<u8> {
        if self.is_empty() {
            return None;
        }
        let v = self.queue[self.tail as usize];
        self.tail = (self.tail + 1) % self.queue.len() as u8;
        Some(v)
    }
}

#[derive(PartialEq, Eq, Default, Clone, Copy, Debug)]
pub struct Sprite {
    pub sx: u8,
    pub sy: u8,
    pub tile: u8,
    pub flags: u8,
}
impl SaveState for Sprite {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        [self.sx, self.sy, self.tile, self.flags].save_state(data)
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        let mut t = [0u8; 4];
        t.load_state(data)?;
        let [sx, sy, t, flags] = t;
        *self = Self {
            sx,
            sy,
            tile: t,
            flags,
        };
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Ppu {
    /// 8000-9FFF: Video RAM
    pub vram: [u8; 0x2000],
    /// FE00-FE9F: Sprite Attribute table
    pub oam: [u8; 0xA0],

    /// The current screen been render.
    /// Each pixel is a shade of gray, from 0 to 3
    pub screen: [u8; 144 * 160],
    /// sprites that will be rendered in the next mode 3 scanline
    pub sprite_buffer: [Sprite; 10],
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
    /// FF48:
    pub obp0: u8,
    /// FF49:
    pub obp1: u8,
    /// FF4A: Window Y Position
    pub wy: u8,
    /// FF4B: Window X Position
    pub wx: u8,

    /// Last clock cycle where the PPU was updated
    last_clock_count: u64,
    /// The current internal clock of the PPU, used to indicate the current ly and lx.
    internal_clock: u64,

    background_fifo: PixelFifo,
    sprite_fifo: PixelFifo,

    // pixel fetcher
    fetcher_step: u8,
    // the first push to fifo in a scanline is skipped
    fetcher_skipped_first_push: bool,
    /// If it is currentlly fetching a sprite (None for is not featching)
    sprite_fetching: bool,
    fetcher_cycle: bool,
    /// the tile x position that the pixel fetcher is in
    fetcher_x: u8,
    fetch_tile_number: u8,
    fetch_tile_data_low: u8,
    fetch_tile_data_hight: u8,

    reach_window: bool,
    is_in_window: bool,

    // the current x position in the current scanline
    pub curr_x: u8,
    // the number of pixels left to be discarted in the start of the current scanline
    discarting: u8,
}

impl std::fmt::Debug for Ppu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ppu")
            .field("vram", &"[...]")
            .field("oam", &"[...]")
            .field("screen", &"[...]")
            .field("sprite_buffer", &self.sprite_buffer)
            .field("sprite_buffer_len", &self.sprite_buffer_len)
            .field("wyc", &self.wyc)
            .field("lcdc", &self.lcdc)
            .field("stat", &self.stat)
            .field("scy", &self.scy)
            .field("scx", &self.scx)
            .field("ly", &self.ly)
            .field("lyc", &self.lyc)
            .field("bgp", &self.bgp)
            .field("obp0", &self.obp0)
            .field("obp1", &self.obp1)
            .field("wy", &self.wy)
            .field("wx", &self.wx)
            .field("last_clock_count", &self.last_clock_count)
            .field("internal_clock", &self.internal_clock)
            .field("background_fifo", &self.background_fifo)
            .field("sprite_fifo", &self.sprite_fifo)
            .field("fetcher_step", &self.fetcher_step)
            .field(
                "fetcher_skipped_first_push",
                &self.fetcher_skipped_first_push,
            )
            .field("sprite_fetching", &self.sprite_fetching)
            .field("fetcher_cycle", &self.fetcher_cycle)
            .field("fetcher_x", &self.fetcher_x)
            .field("fetch_tile_number", &self.fetch_tile_number)
            .field("fetch_tile_data_low", &self.fetch_tile_data_low)
            .field("fetch_tile_data_hight", &self.fetch_tile_data_hight)
            .field("reach_window", &self.reach_window)
            .field("is_in_window", &self.is_in_window)
            .field("curr_x", &self.curr_x)
            .field("discarting", &self.discarting)
            .finish()
    }
}
impl SaveState for Ppu {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.vram.save_state(data)?;
        self.oam.save_state(data)?;

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
        self.last_clock_count.save_state(data)?;
        self.internal_clock.save_state(data)?;

        self.background_fifo.save_state(data)?;
        self.sprite_fifo.save_state(data)?;

        self.fetcher_step.save_state(data)?;
        self.fetcher_x.save_state(data)?;
        self.fetch_tile_number.save_state(data)?;
        self.fetch_tile_data_low.save_state(data)?;
        self.fetch_tile_data_hight.save_state(data)?;

        [
            &self.reach_window,
            &self.is_in_window,
            &self.fetcher_skipped_first_push,
            &self.sprite_fetching,
            &self.fetcher_cycle,
        ]
        .save_state(data)?;

        self.curr_x.save_state(data)?;
        self.discarting.save_state(data)?;

        Ok(())
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        self.vram.load_state(data)?;
        self.oam.load_state(data)?;

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
        self.last_clock_count.load_state(data)?;
        self.internal_clock.load_state(data)?;

        self.background_fifo.load_state(data)?;
        self.sprite_fifo.load_state(data)?;

        self.fetcher_step.load_state(data)?;
        self.fetcher_x.load_state(data)?;
        self.fetch_tile_number.load_state(data)?;
        self.fetch_tile_data_low.load_state(data)?;
        self.fetch_tile_data_hight.load_state(data)?;

        [
            &mut self.reach_window,
            &mut self.is_in_window,
            &mut self.fetcher_skipped_first_push,
            &mut self.sprite_fetching,
            &mut self.fetcher_cycle,
        ]
        .load_state(data)?;

        self.curr_x.load_state(data)?;
        self.discarting.load_state(data)?;

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
            obp0: Default::default(),
            obp1: Default::default(),
            wy: Default::default(),
            wx: Default::default(),
            vram: [0; 0x2000],
            oam: [0; 0xA0],
            last_clock_count: 0,
            internal_clock: 0,
            background_fifo: Default::default(),
            sprite_fifo: Default::default(),
            sprite_fetching: false,
            fetcher_cycle: false,
            fetcher_step: 0,
            fetcher_skipped_first_push: false,
            fetcher_x: 0,
            fetch_tile_number: 0,
            fetch_tile_data_low: 0,
            fetch_tile_data_hight: 0,
            reach_window: false,
            is_in_window: false,
            curr_x: 0,
            discarting: 0,
        }
    }
}

impl Ppu {
    pub fn write(gb: &mut GameBoy, address: u8, value: u8) {
        // for now, ppu update on read or write,should never trigger a interrupt.
        let (v, s) = Self::update(gb);
        debug_assert!(!v && !s);
        let this = &mut *gb.ppu.borrow_mut();
        match address {
            0x40 => {
                if value & 0x80 != this.lcdc & 0x80 {
                    if value & 0x80 == 0 {
                        // disable ppu
                        this.ly = 0;
                        this.internal_clock = 0;
                        // set to mode 0
                        this.stat &= !0b11;
                    } else {
                        debug_assert_eq!(this.ly, 0);
                        debug_assert_eq!(this.internal_clock, 0);
                        debug_assert_eq!(this.stat & 0b11, 0b00);
                        this.stat |= 0b10;
                    }
                }
                this.lcdc = value
            }
            0x41 => this.stat = (value & !0b11) | (this.stat & 0b11),
            0x42 => this.scy = value,
            0x43 => this.scx = value,
            0x44 => {} // ly is read only
            0x45 => this.lyc = value,
            0x47 => this.bgp = value,
            0x48 => this.obp0 = value,
            0x49 => this.obp1 = value,
            0x4A => this.wy = value,
            0x4B => this.wx = value,
            _ => unreachable!(),
        }
    }

    pub fn read(gb: &GameBoy, address: u8) -> u8 {
        let this = gb.ppu.borrow();
        match address {
            0x40 => this.lcdc,
            0x41 => {
                drop(this);
                let (v, s) = Self::update(gb);
                debug_assert!(!v && !s);
                gb.ppu.borrow().stat
            }
            0x42 => this.scy,
            0x43 => this.scx,
            0x44 => {
                drop(this);
                let (v, s) = Self::update(gb);
                debug_assert!(!v && !s);
                gb.ppu.borrow().ly
            }
            0x45 => this.lyc,
            0x47 => this.bgp,
            0x48 => this.obp0,
            0x49 => this.obp1,
            0x4A => this.wy,
            0x4B => this.wx,
            _ => unreachable!(),
        }
    }

    fn search_objects(&mut self) {
        self.sprite_buffer_len = 0;
        let sprite_height = if self.lcdc & 0x04 != 0 { 16 } else { 8 };
        for i in 0..40 {
            let i = i as usize * 4;
            let data = &self.oam[i..i + 4];
            let sy = data[0];
            let sx = data[1];
            let t = data[2];
            let flags = data[3];

            if sx > 0
                && self.ly as u16 + 16 >= sy as u16
                && self.ly as u16 + 16 < sy as u16 + sprite_height
            {
                self.sprite_buffer[self.sprite_buffer_len as usize] = Sprite {
                    sy,
                    sx,
                    tile: t,
                    flags,
                };
                self.sprite_buffer_len += 1;
            }
            if self.sprite_buffer_len == 10 {
                break;
            }
        }
        // sort buffer by priority, in increasing order
        // lower x position, has greater priority
        self.sprite_buffer[0..self.sprite_buffer_len as usize].reverse();
        self.sprite_buffer[0..self.sprite_buffer_len as usize].sort_by_key(|x| !x.sx);
    }

    /// Update the ppu from its state in the last update (in last_clock_count) to its expected
    /// currrently state (in gb.clock_count). Returns a tuple (v_blank_int, stat_int) telling if
    /// there are triggered interrupts in this update.
    #[must_use]
    pub fn update(gb: &GameBoy) -> (bool, bool) {
        let ppu = &mut *gb.ppu.borrow_mut();

        if ppu.lcdc & 0x80 == 0 {
            // ppu is disabled
            ppu.last_clock_count = gb.clock_count;
            return (false, false);
        }

        let mut v_blank_interrupt = false;
        let mut stat_interrupt = false;
        for _clock in ppu.last_clock_count..gb.clock_count {
            ppu.ly = ((ppu.internal_clock / 456) % 154) as u8;
            let lx = ppu.internal_clock % 456;
            ppu.internal_clock += 1;

            let mut set_stat_int = |ppu: &mut Ppu, i: u8| {
                if ppu.stat & (1 << i) != 0 {
                    stat_interrupt = true;
                }
            };

            // LY==LYC Interrupt
            let ly = ppu.ly;
            let lyc = ppu.lyc;
            if ly != 0 && (lx == 4 && ly == lyc || ly == 153 && lx == 12 && 0 == lyc) {
                // STAT Coincidente Flag
                ppu.stat |= 1 << 2;
                // LY == LYC STAT Interrupt
                set_stat_int(ppu, 6)
            }

            let mut set_mode = |ppu: &mut Ppu, mode: u8| {
                debug_assert!(mode <= 3);
                ppu.stat = (ppu.stat & !0b11) | mode;

                match mode {
                    // H-Blank
                    0 => {
                        // draw_scan_line(memory, s);
                        if ppu.is_in_window {
                            ppu.wyc += 1;
                        }
                        // Mode 0 STAT Interrupt
                        set_stat_int(ppu, 3);
                    }
                    // V-Blank
                    1 => {
                        ppu.wyc = 0;
                        ppu.reach_window = false;
                        // Mode 1 STAT Interrupt
                        set_stat_int(ppu, 4);
                        // Entering VBlank state triggers the OAM interrupt (I discovery this in
                        // SameBoy source code)
                        set_stat_int(ppu, 5);
                    }
                    // OAM search
                    2 => {
                        ppu.search_objects();
                        // Mode 2 STAT Interrupt
                        set_stat_int(ppu, 5);
                    }
                    // Draw
                    3 => {
                        ppu.background_fifo.clear();
                        ppu.sprite_fifo.clear();

                        ppu.fetcher_step = 0;
                        ppu.fetcher_skipped_first_push = false;
                        ppu.sprite_fetching = false;
                        ppu.fetcher_cycle = false;
                        ppu.fetcher_x = 0;
                        if ppu.wy == ppu.ly {
                            ppu.reach_window = true;
                        }
                        ppu.is_in_window = false;
                        ppu.curr_x = 0;
                        ppu.discarting = ppu.scx % 8;
                    }
                    4..=255 => unreachable!(),
                }
            };

            // mode transition
            match ppu.stat & 0b11 {
                // hblank
                0 => {
                    if ppu.ly == 144 {
                        set_mode(ppu, 1);

                        // V-Blank Interrupt
                        v_blank_interrupt = true;
                    } else if lx == 0 {
                        set_mode(ppu, 2);
                    }
                }
                // vblank
                1 => {
                    if ppu.ly == 0 {
                        ppu.wyc = 0;
                        set_mode(ppu, 2);
                    }
                }
                // searching objects
                2 => {
                    if lx == 80 {
                        set_mode(ppu, 3);
                    }
                }
                // drawing
                3 => {
                    if ppu.curr_x == 160 {
                        set_mode(ppu, 0);
                    }
                }

                4..=255 => unreachable!(),
            }
            // mode operation
            match ppu.stat & 0b11 {
                0 => {}
                1 => {}
                2 => {}
                3 => {
                    let is_in_window = ppu.is_in_window;

                    let sprite_enable = ppu.lcdc & 0x02 != 0;
                    if ppu.discarting == 0
                        && !ppu.sprite_fetching
                        && ppu.sprite_buffer_len != 0
                        && sprite_enable
                    {
                        let sprite = ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1];
                        if sprite.sx <= ppu.curr_x + 8 {
                            ppu.sprite_fetching = true;
                            ppu.sprite_buffer_len -= 1;
                            ppu.fetcher_step = 0;
                            ppu.fetcher_cycle = false;
                            break;
                        }
                    }

                    ppu.fetcher_cycle = !ppu.fetcher_cycle;
                    // Pixel Fetching
                    if ppu.fetcher_cycle {
                        if ppu.sprite_fetching {
                            let sprite = ppu.sprite_buffer[ppu.sprite_buffer_len as usize];
                            let tall = ppu.lcdc & 0x04;
                            let flip_y = sprite.flags & 0x40;
                            let flip_x = sprite.flags & 0x20 != 0;
                            let py = if flip_y != 0 {
                                let height = if tall != 0 { 16 } else { 8 };
                                (height - 1) - (ly + 16 - sprite.sy)
                            } else {
                                ly + 16 - sprite.sy
                            };

                            // for steps 1 and 2
                            let tile = ppu.fetch_tile_number as u16;
                            let address = tile * 0x10 + 0x8000;
                            let offset = (py % 8) as u16 * 2;
                            let fetch_tile_address = address + offset;

                            match ppu.fetcher_step {
                                // fetch tile number
                                0 => {
                                    let tile = if tall != 0 {
                                        // sprite with 2 tiles of height
                                        (sprite.tile & !1) + py / 8
                                    } else {
                                        sprite.tile
                                    };

                                    ppu.fetch_tile_number = tile;
                                    ppu.fetcher_step = 1;
                                }
                                // fetch tile data (low)
                                1 => {
                                    ppu.fetch_tile_data_low =
                                        ppu.vram[fetch_tile_address as usize - 0x8000];
                                    ppu.fetcher_step = 2;
                                }
                                // fetch tile data (hight)
                                2 => {
                                    ppu.fetch_tile_data_hight =
                                        ppu.vram[fetch_tile_address as usize + 1 - 0x8000];

                                    ppu.fetcher_step = 3;
                                }
                                // push to fifo
                                3 => {
                                    let tile_low = if flip_x {
                                        ppu.fetch_tile_data_low.reverse_bits()
                                    } else {
                                        ppu.fetch_tile_data_low
                                    };
                                    let tile_hight = if flip_x {
                                        ppu.fetch_tile_data_hight.reverse_bits()
                                    } else {
                                        ppu.fetch_tile_data_hight
                                    };
                                    let cut_off = if sprite.sx < 8 { 8 - sprite.sx } else { 0 };
                                    ppu.sprite_fifo.push_sprite(
                                        tile_low,
                                        tile_hight,
                                        cut_off,
                                        sprite.flags & 0x10 != 0,
                                        sprite.flags & 0x80 != 0,
                                    );
                                    ppu.fetcher_step = 0;
                                    ppu.sprite_fetching = false;
                                }
                                4..=255 => unreachable!(),
                            }
                        } else {
                            let fetch_tile_address =
                                |ppu: &mut Ppu, is_in_window: bool, ly: u8| -> u16 {
                                    let mut tile = ppu.fetch_tile_number as u16;
                                    if ppu.lcdc & 0x10 == 0 {
                                        tile += 0x100;
                                        if tile >= 0x180 {
                                            tile -= 0x100;
                                        }
                                    }
                                    let address = tile * 0x10 + 0x8000;
                                    let offset = if is_in_window {
                                        2 * (ppu.wyc as u16 % 8)
                                    } else {
                                        2 * ((ly.wrapping_add(ppu.scy) & 0xff) % 8) as u16
                                    };
                                    let fetch_tile_address = address + offset;
                                    fetch_tile_address
                                };

                            match ppu.fetcher_step {
                                // fetch tile number
                                0 => {
                                    let tile_map = if !is_in_window {
                                        if ppu.lcdc & 0x08 != 0 {
                                            0x9C00
                                        } else {
                                            0x9800
                                        }
                                    } else {
                                        if ppu.lcdc & 0x40 != 0 {
                                            0x9C00
                                        } else {
                                            0x9800
                                        }
                                    };

                                    let tx = if is_in_window {
                                        ppu.fetcher_x
                                    } else {
                                        (ppu.fetcher_x + (ppu.scx / 8)) & 0x1f
                                    };
                                    let ty = if is_in_window {
                                        ppu.wyc / 8
                                    } else {
                                        (ly.wrapping_add(ppu.scy) & 0xff) / 8
                                    };

                                    let offset = (32 * ty as u16 + tx as u16) & 0x03ff;
                                    ppu.fetch_tile_number =
                                        ppu.vram[(tile_map + offset) as usize - 0x8000];
                                    ppu.fetcher_step = 1;
                                }
                                // fetch tile data (low)
                                1 => {
                                    let fetch_tile_address =
                                        fetch_tile_address(ppu, is_in_window, ly);
                                    ppu.fetch_tile_data_low =
                                        ppu.vram[fetch_tile_address as usize - 0x8000];
                                    ppu.fetcher_step = 2;
                                }
                                // fetch tile data (hight)
                                2 => {
                                    let fetch_tile_address =
                                        fetch_tile_address(ppu, is_in_window, ly);
                                    ppu.fetch_tile_data_hight =
                                        ppu.vram[fetch_tile_address as usize + 1 - 0x8000];

                                    if !ppu.fetcher_skipped_first_push {
                                        ppu.fetcher_skipped_first_push = true;
                                        ppu.fetcher_step = 0;
                                    } else {
                                        ppu.fetcher_step = 3;
                                    }
                                }
                                // push to fifo
                                3 => {
                                    if !ppu.background_fifo.is_empty() {
                                        ppu.fetcher_step = 3;
                                    } else {
                                        let low = ppu.fetch_tile_data_low;
                                        let hight = ppu.fetch_tile_data_hight;
                                        ppu.background_fifo.push_background(low, hight);
                                        ppu.fetcher_x += 1;
                                        ppu.fetcher_step = 0;
                                    }
                                }
                                4..=255 => unreachable!(),
                            }
                        }
                    }

                    if !ppu.sprite_fetching {
                        let window_enabled = ppu.lcdc & 0x20 != 0;
                        if !ppu.is_in_window
                            && window_enabled
                            && ppu.reach_window
                            && ppu.curr_x >= ppu.wx.saturating_sub(7)
                        {
                            ppu.is_in_window = true;
                            if !ppu.sprite_fetching {
                                ppu.fetcher_step = 0;
                            }
                            ppu.discarting = 0;
                            ppu.fetcher_x = 0;
                            ppu.background_fifo.clear();
                        }

                        if let Some(pixel) = ppu.background_fifo.pop_front() {
                            if ppu.discarting > 0 {
                                ppu.discarting -= 1;
                                // Discart a pixel. Used for scrolling the background.
                            } else {
                                let i = (ly as usize) * 160 + ppu.curr_x as usize;
                                let background_enable = ppu.lcdc & 0x01 != 0;
                                let bcolor = if background_enable { pixel & 0b11 } else { 0 };

                                // background color, with pallete applied
                                let palette = ppu.bgp;
                                let mut color = (palette >> (bcolor * 2)) & 0b11;

                                let sprite_pixel = ppu.sprite_fifo.pop_front();
                                if let Some(sprite_pixel) = sprite_pixel {
                                    let scolor = sprite_pixel & 0b11;
                                    let background_priority = (sprite_pixel >> 3) & 0x01 != 0;
                                    if scolor == 0 || background_priority && bcolor != 0 {
                                        // use background color
                                    } else {
                                        // use sprite color
                                        let palette = (sprite_pixel >> 4) & 0x1;
                                        let palette = [ppu.obp0, ppu.obp1][palette as usize];
                                        color = (palette >> (scolor * 2)) & 0b11;
                                    }
                                }
                                debug_assert!(color < 4);
                                ppu.screen[i] = color;
                                ppu.curr_x += 1;
                            }
                        }
                    }
                }
                4..=255 => unreachable!(),
            }
        }
        ppu.last_clock_count = gb.clock_count;
        (v_blank_interrupt, stat_interrupt)
    }
}

pub fn draw_tile(
    ppu: &Ppu,
    draw_pixel: &mut impl FnMut(i32, i32, u8),
    tx: i32,
    ty: i32,
    index: usize,
    palette: u8,
    alpha: bool,
) {
    let i = index * 0x10;
    for y in 0..8 {
        let a = ppu.vram[i + y as usize * 2];
        let b = ppu.vram[i + y as usize * 2 + 1];
        for x in 0..8 {
            let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
            if alpha && color == 0 {
                continue;
            }
            let color = (palette >> (color * 2)) & 0b11;
            draw_pixel(tx + x, ty + y, color);
        }
    }
}

pub fn draw_tiles(ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8), palette: u8) {
    for i in 0..0x180 {
        let tx = 8 * (i % 16);
        let ty = 8 * (i / 16);

        draw_tile(ppu, draw_pixel, tx, ty, i as usize, palette, false);
    }
}

pub fn draw_background(ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..(32 * 32) {
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);
        // BG Tile Map Select
        let address = if ppu.lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
        let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

        // if is using 8800 method
        if ppu.lcdc & 0x10 == 0 {
            tile += 0x100;
            if tile >= 0x180 {
                tile -= 0x100;
            }
        }

        draw_tile(ppu, draw_pixel, tx, ty, tile, ppu.bgp, false);
    }
}

pub fn draw_window(ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..(32 * 32) {
        let tx = 8 * (i % 32);
        let ty = 8 * (i / 32);
        // BG Tile Map Select
        let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
        let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

        // if is using 8800 method
        if ppu.lcdc & 0x10 == 0 {
            tile += 0x100;
            if tile >= 0x180 {
                tile -= 0x100;
            }
        }

        draw_tile(ppu, draw_pixel, tx, ty, tile, ppu.bgp, false);
    }
}

pub fn draw_sprites(ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    for i in 0..40 {
        let i = i as usize * 4;
        let data = &ppu.oam[i..i + 4];
        let sy = data[0] as i32 - 16;
        let sx = data[1] as i32 - 8;
        let t = data[2];
        let f = data[3];

        let palette = if f & 0x10 != 0 { ppu.obp1 } else { ppu.obp0 };

        if sy < 0 || sx < 0 {
            continue;
        }
        draw_tile(ppu, draw_pixel, sx, sy, t as usize, palette, true);
    }
}

pub fn draw_screen(ppu: &Ppu, draw_pixel: &mut impl FnMut(i32, i32, u8)) {
    // Draw Background
    if true {
        let scx = ppu.scx;
        let scy = ppu.scy;
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
                let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

                // if is using 8800 method
                if ppu.lcdc & 0x10 == 0 {
                    tile += 0x100;
                    if tile >= 0x180 {
                        tile -= 0x100;
                    }
                }

                draw_tile(ppu, draw_pixel, tx, ty, tile, ppu.bgp, false);
            }
        }
    }
    // Draw Window, if enabled
    if ppu.lcdc & 0x20 != 0 {
        let wx = ppu.wx;
        let wy = ppu.wy;
        for y in 0..19 - wy / 8 {
            for x in 0..21 - wx / 8 {
                let tx = 8 * x as i32 + wx as i32;
                let ty = 8 * y as i32 + wy as i32;
                let x = x % 32;
                let y = y % 32;
                let i = x as usize + y as usize * 32;
                // BG Tile Map Select
                let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
                let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

                // if is using 8800 method
                if ppu.lcdc & 0x10 == 0 {
                    tile += 0x100;
                    if tile >= 0x180 {
                        tile -= 0x100;
                    }
                }

                draw_tile(ppu, draw_pixel, tx, ty, tile, ppu.bgp, false);
            }
        }
    }
    // Draw Sprites, if enabled
    if ppu.lcdc & 0x02 != 0 {
        draw_sprites(ppu, draw_pixel);
    }
}

pub fn draw_scan_line(ppu: &mut Ppu) {
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
            let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

            // if is using 8800 method
            if ppu.lcdc & 0x10 == 0 {
                tile += 0x100;
                if tile >= 0x180 {
                    tile -= 0x100;
                }
            }

            {
                let palette = ppu.bgp;
                let alpha = false;
                let i = tile * 0x10;
                let y = py % 8;
                let a = ppu.vram[i + y as usize * 2];
                let b = ppu.vram[i + y as usize * 2 + 1];
                let x = px % 8;
                let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
                if alpha && color == 0 {
                    continue;
                }
                let color = (palette >> (color * 2)) & 0b11;
                // draw_pixel(lx as i32, ly as i32, color);
                debug_assert!(color < 4);
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
        for lx in ppu.wx.saturating_sub(7)..160 {
            let px = lx + 7 - ppu.wx;

            let i = (px as usize / 8) + (py as usize / 8) * 32;

            // BG Tile Map Select
            let address = if ppu.lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
            let mut tile = ppu.vram[address - 0x8000 + i as usize] as usize;

            // if is using 8800 method
            if ppu.lcdc & 0x10 == 0 {
                tile += 0x100;
                if tile >= 0x180 {
                    tile -= 0x100;
                }
            }

            {
                let palette = ppu.bgp;
                let alpha = false;
                let i = tile * 0x10;
                let y = py % 8;
                let a = ppu.vram[i + y as usize * 2];
                let b = ppu.vram[i + y as usize * 2 + 1];
                let x = px % 8;
                let color = (((b >> (7 - x)) << 1) & 0b10) | ((a >> (7 - x)) & 0b1);
                if alpha && color == 0 {
                    continue;
                }
                let color = (palette >> (color * 2)) & 0b11;
                // draw_pixel(lx as i32, ly as i32, color);
                debug_assert!(color < 4);
                ppu.screen[ly as usize * 160 + lx as usize] = color;
            };
        }
    }

    // Draw Sprites, if enabled
    if ppu.lcdc & 0x02 != 0 {
        for &Sprite {
            sy,
            sx,
            tile: t,
            flags,
        } in &ppu.sprite_buffer[0..ppu.sprite_buffer_len as usize]
        {
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

            let palette = if flags & 0x10 != 0 {
                ppu.obp1
            } else {
                ppu.obp0
            };

            {
                let y = py % 8;
                let i = tile * 0x10;
                let a = ppu.vram[i + y as usize * 2];
                let b = ppu.vram[i + y as usize * 2 + 1];
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
                    let color = (palette >> (color * 2)) & 0b11;
                    debug_assert!(color < 4);
                    ppu.screen[ly as usize * 160 + lx as usize] = color;
                }
            };
        }
    }
}
