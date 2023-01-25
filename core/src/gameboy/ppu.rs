use crate::{
    gameboy::GameBoy,
    save_state::{LoadStateError, SaveState, SaveStateContext},
};

#[derive(PartialEq, Eq, Default, Clone, Debug)]
pub struct PixelFifo {
    queue: [u8; 16],
    /// next position to push
    head: u8,
    /// next position to pop
    tail: u8,
}
impl SaveState for PixelFifo {
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl std::io::Write,
    ) -> Result<(), std::io::Error> {
        self.queue.save_state(ctx, data)?;
        self.head.save_state(ctx, data)?;
        self.tail.save_state(ctx, data)?;

        Ok(())
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl std::io::Read,
    ) -> Result<(), LoadStateError> {
        self.queue.load_state(ctx, data)?;
        self.head.load_state(ctx, data)?;
        self.tail.load_state(ctx, data)?;

        Ok(())
    }
}
impl PixelFifo {
    pub fn iter(&self) -> impl Iterator<Item = u8> + '_ {
        let tail = self.tail as usize;
        let head = self.head as usize;

        if tail <= head {
            self.queue[tail..head]
                .iter()
                .cloned()
                .chain(self.queue[0..0].iter().cloned())
        } else {
            self.queue[tail..]
                .iter()
                .cloned()
                .chain(self.queue[..head].iter().cloned())
        }
    }

    pub fn len(&self) -> usize {
        let tail = self.tail as usize;
        let head = self.head as usize;

        if tail <= head {
            head - tail
        } else {
            self.queue.len() - tail + head
        }
    }

    pub fn is_empty(&self) -> bool {
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
        palette: bool,
        background_priority: bool,
    ) {
        let pixel = |x| {
            let color: u8 = (((tile_hight >> x) & 0x01) << 1) | ((tile_low >> x) & 0x01);
            debug_assert!(color < 4);

            color | ((background_priority as u8) << 3) | ((palette as u8) << 4)
        };

        let mut cursor = self.tail;
        let mut x = 8u8;
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
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl std::io::Write,
    ) -> Result<(), std::io::Error> {
        [self.sx, self.sy, self.tile, self.flags].save_state(ctx, data)
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl std::io::Read,
    ) -> Result<(), LoadStateError> {
        let mut t = [0u8; 4];
        t.load_state(ctx, data)?;
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

    /// The cycle in which the last DMA transfer was requested.
    dma_started: u64,
    /// If the DMA is running, including the initial delay.
    dma_running: bool,
    /// Oam read is blocked
    dma_block_oam: bool,

    oam_read_block: bool,
    oam_write_block: bool,
    vram_read_block: bool,
    vram_write_block: bool,

    /// The current screen been render.
    /// Each pixel is a shade of gray, from 0 to 3
    pub screen: [u8; 144 * 160],
    /// sprites that will be rendered in the next mode 3 scanline
    pub sprite_buffer: [Sprite; 10],
    /// the length of the `sprite_buffer`
    pub sprite_buffer_len: u8,
    /// Window Internal Line Counter
    pub wyc: u8,

    /// FF40: LCD Control Register
    pub lcdc: u8,
    /// FF41: LCD Status Register
    /// Bit 6 - LYC=LY STAT Interrupt source
    /// Bit 5 - Mode 2 OAM STAT Interrupt source
    /// Bit 4 - Mode 1 VBlank STAT Interrupt source
    /// Bit 3 - Mode 0 HBlank STAT Interrupt source
    /// Bit 2 - LYC=LY Flag
    /// Bit 1-0 - Mode Flag
    pub stat: u8,
    /// FF42: Scroll Y Register
    pub scy: u8,
    /// FF43: Scroll x Register
    pub scx: u8,
    /// FF44: LCDC Y-Coordinate
    pub ly: u8,
    /// FF45: LY Compare
    pub lyc: u8,
    /// FF47: BG & Window Palette Data
    pub bgp: u8,
    /// FF48:
    pub obp0: u8,
    /// FF49:
    pub obp1: u8,
    /// FF4A: Window Y Position
    pub wy: u8,
    /// FF4B: Window X Position
    pub wx: u8,

    pub state: u8,
    /// When making the LY==LYC comparison, uses this value instead of ly to control the comparison
    /// timing. This is 0xFF if this will not update the stat.
    ly_for_compare: u8,

    stat_signal: bool,
    ly_compare_signal: bool,
    /// use this value instead of the current stat mode when controlling the stat interrupt signal,
    /// to control the timing. 0xff means that this will not trigger a interrupt.
    stat_mode_for_interrupt: u8,

    /// Next clock cycle where the PPU will be updated
    pub next_clock_count: u64,
    /// The clock count in which the current scanline has started.
    pub line_start_clock_count: u64,

    pub background_fifo: PixelFifo,
    pub sprite_fifo: PixelFifo,

    // pixel fetcher
    fetcher_step: u8,
    /// the tile x position that the pixel fetcher is in
    fetcher_x: u8,
    fetch_tile_number: u8,
    fetch_tile_data_low: u8,
    fetch_tile_data_hight: u8,

    sprite_tile_address: u16,
    sprite_tile_data_low: u8,
    sprite_tile_data_hight: u8,

    reach_window: bool,
    is_in_window: bool,

    /// Sprites at 0 cause a extra delay in the sprite fetching.
    sprite_at_0_penalty: u8,

    /// The x position of the next screen pixel to be draw in the current scanline
    pub screen_x: u8,
    /// The x position in the current scanline, from -(8 + scx%8) to 160. Negative values
    /// (represented by positives between 241 and 255) are use for detecting sprites that starts
    /// to the left of the screen, and for discarding pixels for scrolling.
    scanline_x: u8,
}

impl std::fmt::Debug for Ppu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ppu")
            .field("vram", &"[...]")
            .field("oam", &"[...]")
            .field("screen", &"[...]")
            .field("dma_started", &self.dma_started)
            .field("dma_running", &self.dma_running)
            .field("dma_block_oam", &self.dma_block_oam)
            .field("oam_read_block", &self.oam_read_block)
            .field("oam_write_block", &self.oam_write_block)
            .field("vram_read_block", &self.vram_read_block)
            .field("vram_write_block", &self.vram_write_block)
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
            .field("state", &self.state)
            .field("ly_for_compare", &self.ly_for_compare)
            .field("stat_signal", &self.stat_signal)
            .field("ly_compare_signal", &self.ly_compare_signal)
            .field("stat_mode_for_interrupt", &self.stat_mode_for_interrupt)
            .field("next_clock_count", &self.next_clock_count)
            .field("line_start_clock_count", &self.line_start_clock_count)
            .field("background_fifo", &self.background_fifo)
            .field("sprite_fifo", &self.sprite_fifo)
            .field("fetcher_step", &self.fetcher_step)
            .field("fetcher_x", &self.fetcher_x)
            .field("fetch_tile_number", &self.fetch_tile_number)
            .field("fetch_tile_data_low", &self.fetch_tile_data_low)
            .field("fetch_tile_data_hight", &self.fetch_tile_data_hight)
            .field("sprite_tile_address", &self.sprite_tile_address)
            .field("sprite_tile_data_low", &self.sprite_tile_data_low)
            .field("sprite_tile_data_hight", &self.sprite_tile_data_hight)
            .field("reach_window", &self.reach_window)
            .field("is_in_window", &self.is_in_window)
            .field("sprite_at_0_penalty", &self.sprite_at_0_penalty)
            .field("screen_x", &self.screen_x)
            .field("scanline_x", &self.scanline_x)
            .finish()
    }
}
crate::save_state!(Ppu, self, data {
    self.vram;
    self.oam;

    self.dma_started;

    self.screen;
    self.sprite_buffer;
    self.sprite_buffer_len;
    self.wyc;

    self.lcdc;
    self.stat;
    self.scy;
    self.scx;
    self.ly;
    self.lyc;
    self.bgp;
    self.obp0;
    self.obp1;
    self.wy;
    self.wx;

    self.state;
    self.ly_for_compare;

    self.stat_mode_for_interrupt;

    self.next_clock_count;
    self.line_start_clock_count;

    self.background_fifo;
    self.sprite_fifo;

    self.fetcher_step;
    self.fetcher_x;
    self.fetch_tile_number;
    self.fetch_tile_data_low;
    self.fetch_tile_data_hight;

    self.sprite_tile_address;
    self.sprite_tile_data_low;
    self.sprite_tile_data_hight;

    self.sprite_at_0_penalty;

    self.screen_x;
    self.scanline_x;

    bitset [
        self.dma_running,
        self.dma_block_oam,
        self.oam_read_block,
        self.oam_write_block,
        self.vram_read_block,
        self.vram_write_block
    ];
    bitset [
        self.stat_signal,
        self.ly_compare_signal,
        self.reach_window,
        self.is_in_window
    ];
});

impl Default for Ppu {
    fn default() -> Self {
        Self {
            vram: [0; 0x2000],
            oam: [0; 0xA0],
            dma_started: 0x7fff_ffff_ffff_ffff,
            dma_running: false,
            dma_block_oam: false,
            oam_read_block: false,
            oam_write_block: false,
            vram_read_block: false,
            vram_write_block: false,
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
            ly_for_compare: 0,
            state: 0,
            stat_signal: false,
            ly_compare_signal: false,
            stat_mode_for_interrupt: 0xff,
            next_clock_count: 0,
            line_start_clock_count: 0,
            background_fifo: Default::default(),
            sprite_fifo: Default::default(),
            fetcher_step: 0,
            fetcher_x: 0,
            fetch_tile_number: 0,
            fetch_tile_data_low: 0,
            fetch_tile_data_hight: 0,
            sprite_tile_address: 0,
            sprite_tile_data_low: 0,
            sprite_tile_data_hight: 0,
            reach_window: false,
            is_in_window: false,
            sprite_at_0_penalty: 0,
            screen_x: 0,
            scanline_x: 0,
        }
    }
}

impl Ppu {
    pub fn reset_after_boot(&mut self) {
        let mut ppu_state = &include_bytes!("../../after_boot/ppu.sav")[..];
        let ctx = &mut SaveStateContext::default();
        *self = Self {
            #[rustfmt::skip]
            vram: {
                let mut vram = [0; 0x2000];
                vram.load_state(ctx, &mut ppu_state).unwrap();
                vram

            },
            oam: {
                let mut oam = [0; 0xA0];
                oam.load_state(ctx, &mut ppu_state).unwrap();
                oam
            },
            dma_started: 0x7fff_ffff_ffff_ffff,
            dma_running: false,
            dma_block_oam: false,
            oam_read_block: false,
            oam_write_block: false,
            vram_read_block: false,
            vram_write_block: false,
            screen: {
                let mut screen = [0; 0x5A00];
                screen.load_state(ctx, &mut ppu_state).unwrap();
                screen
            },
            sprite_buffer: [Sprite::default(); 10],
            sprite_buffer_len: 0,
            wyc: 0,
            lcdc: 0x91,
            stat: 0x05,
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bgp: 0xfc,
            obp0: 0,
            obp1: 0,
            wy: 0,
            wx: 0,
            state: 23,
            ly_for_compare: 0,
            next_clock_count: 23_440_377,
            line_start_clock_count: 23_435_361,

            background_fifo: PixelFifo::default(),
            sprite_fifo: PixelFifo::default(),

            fetcher_step: 0x03,
            fetcher_x: 0x14,
            fetch_tile_number: 0,
            fetch_tile_data_low: 0,
            fetch_tile_data_hight: 0,

            sprite_tile_address: 0,
            sprite_tile_data_low: 0,
            sprite_tile_data_hight: 0,

            reach_window: true,
            is_in_window: false,
            stat_signal: false,
            ly_compare_signal: false,
            stat_mode_for_interrupt: 1,

            sprite_at_0_penalty: 0,

            screen_x: 0xa0,
            scanline_x: 0x00,
        }
    }
    pub fn write(gb: &mut GameBoy, address: u8, value: u8) {
        match address {
            0x40 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                if value & 0x80 != this.lcdc & 0x80 {
                    if value & 0x80 == 0 {
                        // disable ppu
                        this.ly = 0;
                        this.line_start_clock_count = 0;
                        // set to mode 0
                        this.stat &= !0b11;
                        this.state = 0;
                    } else {
                        // enable ppu
                        debug_assert_eq!(this.ly, 0);
                        this.ly_for_compare = 0;
                        debug_assert_eq!(this.stat & 0b11, 0b00);
                        this.next_clock_count = gb.clock_count;
                    }
                }
                this.lcdc = value
            }
            0x41 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.stat = 0x80 | (value & !0b111) | (this.stat & 0b111)
            }
            0x42 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.scy = value
            }
            0x43 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.scx = value
            }
            0x44 => {} // ly is read only
            0x45 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.lyc = value
            }
            0x47 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.bgp = value
            }
            0x48 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.obp0 = value
            }
            0x49 => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.obp1 = value
            }
            0x4A => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.wy = value
            }
            0x4B => {
                gb.update();
                let this = &mut *gb.ppu.borrow_mut();
                this.wx = value
            }
            _ => unreachable!(),
        }
    }

    pub fn read(gb: &GameBoy, address: u8) -> u8 {
        let this = gb.ppu.borrow();
        match address {
            0x40 => this.lcdc,
            0x41 => {
                drop(this);
                gb.update();
                gb.ppu.borrow().stat | 0x80
            }
            0x42 => this.scy,
            0x43 => this.scx,
            0x44 => {
                drop(this);
                gb.update();
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

            if self.ly as u16 + 16 >= sy as u16 && self.ly as u16 + 16 < sy as u16 + sprite_height {
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

    fn update_dma(gb: &GameBoy, ppu: &mut Ppu, clock_count: u64) {
        if ppu.dma_running {
            let elapsed = clock_count.wrapping_sub(ppu.dma_started);
            if elapsed >= 8 {
                ppu.dma_block_oam = true;
            }
            // 8 cycles delay + 160 machine cycles
            if elapsed >= 8 + 160 * 4 {
                // Finish running
                ppu.dma_block_oam = false;
                ppu.dma_running = false;

                // copy memory
                let mut value = gb.dma;
                if value >= 0xFE {
                    value -= 0x20;
                }
                let start = (value as u16) << 8;
                for (i, j) in (0x00..=0x9F).zip(start..=start + 0x9F) {
                    // avoid borrowing the ppu twice
                    let value = match j {
                        0x8000..=0x9FFF => ppu.vram[j as usize - 0x8000],
                        j => gb.read(j),
                    };
                    ppu.oam[i] = value;
                }
            }
        }
    }

    pub fn start_dma(gb: &mut GameBoy, value: u8) {
        gb.update();
        gb.dma = value;
        let ppu = &mut *gb.ppu.borrow_mut();
        ppu.dma_started = gb.clock_count;
        if ppu.dma_running {
            // HACK: if a DMA requested was make right before this one, this dma_started
            // rewritten would cancel the oam_block of that DMA. To fix this, I will hackly
            // block the oam here, but this will block the oam 4 cycles early, but I think
            // this will not be observable.
            ppu.dma_block_oam = true;
        }
        ppu.dma_running = true;
    }

    pub fn read_oam(gb: &GameBoy, address: u16) -> u8 {
        gb.update();
        let ppu = &mut *gb.ppu.borrow_mut();
        if ppu.dma_block_oam || ppu.oam_read_block {
            0xff
        } else {
            ppu.oam[address as usize - 0xFE00]
        }
    }

    pub fn write_oam(gb: &mut GameBoy, address: u16, value: u8) {
        gb.update();
        let ppu = &mut *gb.ppu.borrow_mut();
        if !ppu.dma_block_oam && !ppu.oam_write_block {
            ppu.oam[address as usize - 0xFE00] = value;
        }
    }

    pub fn read_vram(gb: &GameBoy, address: u16) -> u8 {
        gb.update();
        let ppu = &mut *gb.ppu.borrow_mut();
        if ppu.vram_read_block {
            0xff
        } else {
            ppu.vram[address as usize - 0x8000]
        }
    }

    pub fn write_vram(gb: &mut GameBoy, address: u16, value: u8) {
        gb.update();
        let ppu = &mut *gb.ppu.borrow_mut();
        if !ppu.vram_write_block {
            ppu.vram[address as usize - 0x8000] = value;
        }
    }

    pub fn update(gb: &GameBoy) -> (bool, bool) {
        // Most of the ppu behaviour is based on the LIJI32/SameBoy including all of the timing,
        // and most of the implementation.

        let ppu = &mut *gb.ppu.borrow_mut();
        if ppu.lcdc & 0x80 == 0 {
            // ppu is disabled
            ppu.next_clock_count = gb.clock_count;
            Self::update_dma(gb, ppu, gb.clock_count);
            return (false, false);
        }

        let mut state = ppu.state;
        let mut stat_interrupt = false;
        let mut vblank_interrupt = false;

        ppu.update_stat(&mut stat_interrupt);

        if ppu.next_clock_count >= gb.clock_count {
            Self::update_dma(gb, ppu, gb.clock_count);
        }

        while ppu.next_clock_count < gb.clock_count {
            Self::update_dma(gb, ppu, ppu.next_clock_count);
            // println!("state: {}", state);
            match state {
                // turn on
                0 => {
                    ppu.ly = 0;

                    ppu.set_stat_mode(0);
                    ppu.stat_mode_for_interrupt = 0;

                    ppu.reach_window = false;
                    ppu.screen_x = 0;

                    ppu.oam_read_block = false;
                    ppu.oam_write_block = false;
                    ppu.vram_read_block = false;
                    ppu.vram_write_block = false;

                    // In the SameBoy, there is a delay of 1 T-cycle beetween this state and the
                    // next. I change to 0 because in the Sameboy the ppu activation happens 11
                    // T-cycles after the opcode read, but in my emulator the delay is 12 T-cycles
                    // (3 M-cycles). SameBoy must have a better write timing?
                    ppu.next_clock_count += 0;
                    state = 1;
                }
                // 1
                1 => {
                    ppu.line_start_clock_count = ppu.next_clock_count - 8;
                    ppu.wyc = 0xff;
                    ppu.next_clock_count += 76;
                    state = 2;
                }
                // 77
                2 => {
                    ppu.oam_write_block = true;
                    ppu.next_clock_count += 2;
                    state = 3;
                }
                // 79
                3 => {
                    ppu.oam_read_block = true;
                    ppu.oam_write_block = true;
                    ppu.vram_read_block = true;
                    ppu.vram_write_block = true;

                    ppu.set_stat_mode(3);
                    ppu.stat_mode_for_interrupt = 3;

                    ppu.next_clock_count += 2;
                    state = 4;
                }
                // 81
                4 => {
                    ppu.next_clock_count += 3;
                    state = 5;
                }
                // 84
                5 => {
                    // goto mode_3_start
                    state = 10;
                }

                // start_line
                6 => {
                    ppu.line_start_clock_count = ppu.next_clock_count;
                    ppu.screen_x = 0;
                    ppu.next_clock_count += 3;
                    state = 7;
                }
                // 3
                7 => {
                    ppu.oam_read_block = true;

                    if ppu.ly == 0 {
                        ppu.ly_for_compare = 0;
                        ppu.set_stat_mode(0);
                        ppu.stat_mode_for_interrupt = 0xff;
                    } else {
                        ppu.ly_for_compare = 0xFF;
                        ppu.set_stat_mode(0);
                        ppu.stat_mode_for_interrupt = 2;
                    }
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 1;
                    state = 8;
                }
                // 4
                8 => {
                    ppu.oam_write_block = true;

                    ppu.ly_for_compare = ppu.ly;

                    ppu.set_stat_mode(2);
                    ppu.stat_mode_for_interrupt = 2;
                    ppu.update_stat(&mut stat_interrupt);
                    ppu.stat_mode_for_interrupt = 0xff;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.search_objects();

                    ppu.next_clock_count += 76;
                    state = 39;
                }
                // 80
                39 => {
                    ppu.oam_read_block = true;
                    ppu.oam_write_block = false;
                    ppu.vram_read_block = true;
                    ppu.vram_write_block = false;

                    ppu.next_clock_count += 4;
                    state = 9;
                }
                // 84
                9 => {
                    debug_assert_eq!(ppu.next_clock_count - ppu.line_start_clock_count, 84);
                    ppu.set_stat_mode(3);
                    ppu.stat_mode_for_interrupt = 3;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.oam_read_block = true;
                    ppu.oam_write_block = true;
                    ppu.vram_read_block = true;
                    ppu.vram_write_block = true;

                    ppu.next_clock_count += 5;
                    state = 10;
                }
                // mode_3_start
                10 => {
                    ppu.background_fifo.clear();
                    ppu.sprite_fifo.clear();

                    // fill background fifo with 8 dummy pixels
                    ppu.background_fifo.push_background(0x00, 0x00);

                    ppu.fetcher_step = 0;
                    ppu.fetcher_x = 0;
                    if ppu.wy == ppu.ly {
                        ppu.reach_window = true;
                    }
                    ppu.is_in_window = false;
                    ppu.scanline_x = -((ppu.scx % 8 + 8) as i8) as u8;
                    ppu.sprite_at_0_penalty = (ppu.scx % 8).min(5);
                    // ppu.sprite_at_0_penalty = 4;

                    state = 27;
                }
                // Loop for every line from 0 to 144
                27 => {
                    // Check for window activation
                    let window_enabled = ppu.lcdc & 0x20 != 0;
                    if !ppu.is_in_window && ppu.reach_window && window_enabled {
                        let mut should_activate = false;
                        if ppu.wx == 0 {
                            let cmp = [-7i8, -9, -10, -11, -12, -13, -14, -14];
                            if ppu.scanline_x == cmp[(ppu.scx % 8) as usize] as u8 {
                                should_activate = true;
                            }
                        // else if wx166_glitch
                        } else if ppu.wx < 166 {
                            if ppu.wx == ppu.scanline_x.wrapping_add(7) {
                                should_activate = true;
                            } else if ppu.wx == ppu.scanline_x.wrapping_add(6) {
                                // TODO: && !wx_just_changed
                                should_activate = true;
                                if ppu.screen_x > 0 {
                                    ppu.screen_x -= 1;
                                }
                            }
                        }

                        if should_activate {
                            // wrapping add, because wyc starts at -1
                            ppu.wyc = ppu.wyc.wrapping_add(1);
                            if ppu.wx == 0 && ppu.scx % 8 != 0 {
                                // wait 1
                                ppu.next_clock_count += 1;
                            }
                            state = 28;
                            continue;
                        } else if ppu.wx == 166 && ppu.wx == ppu.scanline_x + 7 {
                            ppu.wyc += 1;
                        }
                    }
                    state = 29;
                }
                // active window
                28 => {
                    ppu.is_in_window = true;
                    ppu.fetcher_x = 0;
                    ppu.fetcher_step = 0;
                    ppu.background_fifo.clear();

                    state = 29;
                }
                29 => {
                    // Handle sprites

                    // discart already handled sprites
                    // TODO: discovery why this is necessary (blinded copied from SameBoy)
                    while ppu.sprite_buffer_len > 0
                        && (ppu.scanline_x < 160 || ppu.scanline_x >= (-8i8) as u8)
                        && ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1].sx
                            < ppu.scanline_x.wrapping_add(8)
                    {
                        ppu.sprite_buffer_len -= 1;
                    }

                    // fetch sprites
                    // ppu.sprite_fetching = true;
                    state = 30;
                }
                // while there are sprites to be fetch...
                30 => {
                    let sprite_enabled = ppu.lcdc & 0x02 != 0;
                    if ppu.sprite_buffer_len > 0
                        && sprite_enabled
                        && ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1].sx
                            == ppu.scanline_x.wrapping_add(8)
                    {
                        // continue loop
                        state = 31;
                    } else {
                        // exit loop
                        state = 24;
                    }
                }
                // while there are background pixels or don't reach a fetcher step...
                31 => {
                    if ppu.background_fifo.is_empty() || ppu.fetcher_step < 5 {
                        tick_pixel_fetcher(ppu, ppu.ly);
                        // wait 1
                        ppu.next_clock_count += 1;
                        // if abort_sprite_feching { goto aborted }
                    } else {
                        state = 32;
                    }
                }
                32 => {
                    // TODO: handle extra penalty sprite at 0
                    if ppu.sprite_at_0_penalty != 0
                        && ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1].sx == 0
                    {
                        // wait sprite_at_0_penalty
                        ppu.next_clock_count += ppu.sprite_at_0_penalty as u64;
                        ppu.sprite_at_0_penalty = 0;
                        state = 37;
                        continue;
                    }

                    state = 38;
                }
                37 => {
                    // if abort_sprite_feching { goto aborted }

                    state = 38;
                }
                38 => {
                    // wait 1
                    tick_pixel_fetcher(ppu, ppu.ly);
                    ppu.next_clock_count += 1;
                    state = 36;
                }
                36 => {
                    // if abort_sprite_feching { goto aborted }

                    tick_pixel_fetcher(ppu, ppu.ly);
                    ppu.sprite_tile_address = {
                        let tall = ppu.lcdc & 0x04 != 0;
                        let sprite = ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1];
                        let flip_y = sprite.flags & 0x40 != 0;

                        let height = if tall { 0xF } else { 0x7 };
                        let mut py = ppu.ly.wrapping_sub(sprite.sy) & height;
                        if flip_y {
                            py = (!py) & height;
                        }

                        let tile = if tall { sprite.tile & !1 } else { sprite.tile };
                        tile as u16 * 0x10 + py as u16 * 2
                    };

                    // wait 2
                    ppu.next_clock_count += 2;
                    state = 33;
                }
                33 => {
                    // if abort_sprite_feching { goto aborted }

                    ppu.sprite_tile_data_low = ppu.vram[ppu.sprite_tile_address as usize];

                    // wait 2
                    ppu.next_clock_count += 2;
                    state = 34;
                }
                34 => {
                    // if abort_sprite_feching { goto aborted }

                    ppu.sprite_tile_data_hight = ppu.vram[ppu.sprite_tile_address as usize + 1];

                    // ppu.sprite_fetching = false;

                    // wait 1
                    ppu.next_clock_count += 1;
                    state = 35;
                }
                35 => {
                    let sprite = ppu.sprite_buffer[ppu.sprite_buffer_len as usize - 1];
                    let flip_x = sprite.flags & 0x20 != 0;
                    let tile_low = if flip_x {
                        ppu.sprite_tile_data_low.reverse_bits()
                    } else {
                        ppu.sprite_tile_data_low
                    };
                    let tile_hight = if flip_x {
                        ppu.sprite_tile_data_hight.reverse_bits()
                    } else {
                        ppu.sprite_tile_data_hight
                    };
                    ppu.sprite_fifo.push_sprite(
                        tile_low,
                        tile_hight,
                        sprite.flags & 0x10 != 0,
                        sprite.flags & 0x80 != 0,
                    );
                    ppu.sprite_buffer_len -= 1;

                    // loop again
                    state = 30;
                }
                24 => {
                    output_pixel(ppu);
                    tick_pixel_fetcher(ppu, ppu.ly);

                    debug_assert!(ppu.screen_x <= 160);
                    if ppu.screen_x == 160 {
                        // exit mode 3
                        state = 11;
                    } else {
                        ppu.next_clock_count += 1;
                        state = 27;
                    }
                }
                11 => {
                    ppu.oam_read_block = false;
                    ppu.oam_write_block = false;
                    ppu.vram_read_block = false;
                    ppu.vram_write_block = false;

                    ppu.set_stat_mode(0);
                    ppu.stat_mode_for_interrupt = 0;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 1;
                    state = 12;
                }
                12 => {
                    ppu.next_clock_count += 2;
                    state = 13;
                }
                13 => {
                    let elapsed = ppu.next_clock_count - ppu.line_start_clock_count;
                    ppu.next_clock_count += 456 - elapsed - 2;
                    state = 26;
                }
                26 => {
                    if ppu.lcdc & 0x20 != 0 && ppu.wy == ppu.ly {
                        ppu.reach_window = true;
                    }

                    ppu.next_clock_count += 2;
                    state = 14;
                }
                14 => {
                    ppu.ly += 1;
                    if ppu.ly == 144 {
                        // goto vblank
                        state = 15;
                    } else {
                        // goto start_line
                        state = 6;
                    }
                }
                // start_vblank_line
                15 => {
                    if ppu.ly == 153 {
                        // goto last_vblank_line
                        state = 18;
                        continue;
                    }
                    ppu.ly_for_compare = 0xFF;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 2;
                    state = 16;
                }
                // 2
                16 => {
                    if ppu.ly == 144 && !ppu.stat_signal && ppu.stat & 0x20 != 0 {
                        stat_interrupt = true;
                    }

                    ppu.next_clock_count += 2;
                    state = 17;
                }
                // 4
                17 => {
                    ppu.ly_for_compare = ppu.ly;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 0;
                    state = 40;
                }
                40 => {
                    if ppu.ly == 144 {
                        ppu.set_stat_mode(1);
                        vblank_interrupt = true;
                        if !ppu.stat_signal && ppu.stat & 0x20 != 0 {
                            stat_interrupt = true;
                        }
                        ppu.stat_mode_for_interrupt = 1;
                        ppu.update_stat(&mut stat_interrupt);
                    }

                    ppu.next_clock_count += 456 - 4;
                    state = 25;
                }
                25 => {
                    ppu.ly += 1;
                    // goto start_vblank_line
                    state = 15;
                }
                // last_vblank_line
                18 => {
                    ppu.ly = 153;
                    ppu.ly_for_compare = 0xFF;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 6;
                    state = 19;
                }
                // 6
                19 => {
                    ppu.ly = 0;
                    ppu.ly_for_compare = 153;
                    ppu.update_stat(&mut stat_interrupt);
                    ppu.next_clock_count += 2;
                    state = 20;
                }
                // 8
                20 => {
                    ppu.ly = 0;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 4;
                    state = 21;
                }
                // 12
                21 => {
                    ppu.ly_for_compare = 0;
                    ppu.update_stat(&mut stat_interrupt);

                    ppu.next_clock_count += 12;
                    state = 22;
                }
                // 24
                22 => {
                    ppu.next_clock_count += 456 - 24;
                    state = 23;
                }
                // 0
                23 => {
                    ppu.ly = 0;
                    ppu.reach_window = false;

                    ppu.wyc = 0xff;

                    // goto start_line
                    state = 6;
                }
                _ => unreachable!(),
            }
        }
        ppu.state = state;
        (vblank_interrupt, stat_interrupt)
    }

    fn set_stat_mode(&mut self, mode: u8) {
        self.stat = (self.stat & !0b11) | mode;
    }

    fn update_stat(&mut self, stat_interrupt: &mut bool) {
        let stat_mode = self.stat_mode_for_interrupt;
        let mut stat_line = false;

        match stat_mode {
            0 => stat_line |= self.stat & 0x08 != 0,
            1 => {
                // VBlank also trigger OAM STAT interrupt
                stat_line |= self.stat & 0x30 != 0;
            }
            2 => stat_line |= self.stat & 0x20 != 0,
            3 => {}
            255 => {}
            4..=254 => unreachable!(),
        }

        // LY==LYC
        self.stat &= !0x04;
        if self.ly_for_compare == self.lyc {
            self.ly_compare_signal = true;
            // STAT Coincident Flag
            self.stat |= 0x04;
        } else if self.ly_for_compare != 0xff {
            self.ly_compare_signal = false;
        }
        // LY == LYC STAT Interrupt
        stat_line |= (self.stat & (1 << 6) != 0) && self.ly_compare_signal;

        // on rising edge
        if !self.stat_signal && stat_line {
            *stat_interrupt = true;
        }

        self.stat_signal = stat_line;
    }
}

fn tick_pixel_fetcher(ppu: &mut Ppu, ly: u8) {
    let is_in_window = ppu.is_in_window;

    let fetch_tile_address = |ppu: &mut Ppu, is_in_window: bool, ly: u8| -> u16 {
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
            2 * (ly.wrapping_add(ppu.scy) % 8) as u16
        };
        address + offset
    };

    let push_to_fifo = |ppu: &mut Ppu| {
        if ppu.background_fifo.is_empty() {
            let low = ppu.fetch_tile_data_low;
            let hight = ppu.fetch_tile_data_hight;
            ppu.background_fifo.push_background(low, hight);
            ppu.fetcher_step = 0;
        }
    };

    match ppu.fetcher_step {
        0 => {}
        // fetch tile number
        1 => {
            #[allow(clippy::collapsible_else_if)]
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
                ((ppu.scx.wrapping_add(ppu.scanline_x).wrapping_add(8)) / 8) & 0x1f
            };
            let ty = if is_in_window {
                ppu.wyc / 8
            } else {
                ly.wrapping_add(ppu.scy) / 8
            };

            let offset = (32 * ty as u16 + tx as u16) & 0x03ff;
            ppu.fetch_tile_number = ppu.vram[(tile_map + offset) as usize - 0x8000];
        }
        2 => {}
        // fetch tile data (low)
        3 => {
            let fetch_tile_address = fetch_tile_address(ppu, is_in_window, ly);
            ppu.fetch_tile_data_low = ppu.vram[fetch_tile_address as usize - 0x8000];
        }
        4 => {}
        // fetch tile data (hight)
        5 => {
            let fetch_tile_address = fetch_tile_address(ppu, is_in_window, ly);
            ppu.fetch_tile_data_hight = ppu.vram[fetch_tile_address as usize + 1 - 0x8000];
            if ppu.is_in_window {
                ppu.fetcher_x += 1;
            }

            ppu.fetcher_step += 1;
            push_to_fifo(ppu);
            // the step may change to 0, and must not be increase at the end of this function
            return;
        }
        // push to fifo
        6 | 7 => {
            push_to_fifo(ppu);
            // the step may change to 0, and must not be increase at the end of this function
            return;
        }
        8..=255 => unreachable!(),
    }
    ppu.fetcher_step += 1;
}

fn output_pixel(ppu: &mut Ppu) {
    if let Some(pixel) = ppu.background_fifo.pop_front() {
        let sprite_pixel = ppu.sprite_fifo.pop_front();

        // scanline_x values greater or equal than 160 are interpreted as negative (for scrolling)
        // or are out of bounds.
        if ppu.scanline_x >= 160 {
            // Discart the pixel. Used for scrolling the background.
            ppu.scanline_x = ppu.scanline_x.wrapping_add(1);
            return;
        }

        let i = (ppu.ly as usize) * 160 + ppu.screen_x as usize;
        let background_enable = ppu.lcdc & 0x01 != 0;
        let bcolor = if background_enable { pixel & 0b11 } else { 0 };

        // background color, with pallete applied
        let palette = ppu.bgp;
        let mut color = (palette >> (bcolor * 2)) & 0b11;

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
        ppu.screen_x += 1;
        ppu.scanline_x += 1;
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
                let mut tile = ppu.vram[address - 0x8000 + i] as usize;

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
                let mut tile = ppu.vram[address - 0x8000 + i] as usize;

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
            let mut tile = ppu.vram[address - 0x8000 + i] as usize;

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
            let mut tile = ppu.vram[address - 0x8000 + i] as usize;

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
