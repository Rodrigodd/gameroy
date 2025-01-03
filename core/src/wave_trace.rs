//! Trace the GameBoy state to a VCD wave file for debugging. Its main purpose is to compare the
//! emulator's state with the state of the [DMG-SIM](https://github.com/msinger/dmg-sim) and
//! [dmgcpu](https://github.com/emu-russia/dmgcpu) Verilog simulations.

use std::cell::{Cell, RefCell};
use std::fs::File;
use std::io::BufWriter;

use crate::gameboy::cpu::Cpu;
use crate::gameboy::ppu::Ppu;
use crate::gameboy::timer::Timer;
use crate::gameboy::GameBoy;

// NOTE: The actual clock period should be 1/(2^22 Hz) = 238.419 ns, but msinger's
// dmg-sim (the verilog simulation I am comparing to) uses a period of 240 ns.
const TIMESCALE: u64 = 1; // ns
const CYCLE_PERIOD: u64 = 240 / TIMESCALE;

// Offset the timestampst to make it align with the other simulations.
const OFFSET: i64 = (-5625669120) / TIMESCALE as i64;

// Convert clock count to timestamp
fn clock_to_timestamp(clock: u64) -> u64 {
    (clock * CYCLE_PERIOD).saturating_add_signed(OFFSET)
}

type MaxWidth = u16;
type WireIndex = u8;

macro_rules! decl_regs {
    ($name:ident, $module:literal, $var:ident : $type:ty, $($reg:ident, $width:expr => $value:expr;)*) => {
        pub struct $name {
            $($reg: WireIndex,)*
        }
        impl $name {
            fn new(writer: &mut MyWriter) -> std::io::Result<Self> {
                writer.add_module($module)?;
                let this = Self {
                    $($reg: writer.add_wire($width, stringify!($reg))?,)*
                };
                writer.close_module()?;
                Ok(this)
            }

            fn trace(&self, clock_count: u64, writer: &mut MyWriter, $var: $type) -> std::io::Result<()> {
                $(writer.change(clock_count, self.$reg, ($value) as MaxWidth)?;)*
                Ok(())
            }
        }
    };
}

decl_regs! {
    GameboyRegs, "gameboy", gb: &GameBoy,
    serial_data, 8 => gb.serial.borrow().serial_data;
    serial_control, 8 => gb.serial.borrow().serial_control;
    joypad_io, 8 => gb.joypad_io;
    joypad, 8 => gb.joypad;
    interrupt_flag, 8 => gb.interrupt_flag.get();
    dma, 8 => gb.dma;
    interrupt_enabled, 8 => gb.interrupt_enabled;
    v_blank_trigger, 1 => gb.v_blank_trigger.get() as u8;
}

decl_regs! {
    CpuRegs, "cpu", cpu: &Cpu,
    f, 8 => cpu.f.0;
    a, 8 => cpu.a;
    c, 8 => cpu.c;
    b, 8 => cpu.b;
    e, 8 => cpu.e;
    d, 8 => cpu.d;
    l, 8 => cpu.l;
    h, 8 => cpu.h;
    sp, 16 => cpu.sp;
    pc, 16 => cpu.pc;
    ime, 2 => cpu.ime;
    state, 2 => cpu.state;
    halt_bug, 1 => cpu.halt_bug;
    op, 8 => cpu.op;
}

decl_regs! {
    PpuRegs, "ppu", ppu: &Ppu,
    lcdc, 8 => ppu.lcdc;
    stat, 8 => ppu.stat;
    scy, 8 => ppu.scy;
    scx, 8 => ppu.scx;
    ly, 8 => ppu.ly;
    lyc, 8 => ppu.lyc;
    bgp, 8 => ppu.bgp;
    obp0, 8 => ppu.obp0;
    obp1, 8 => ppu.obp1;
    wy, 8 => ppu.wy;
    wx, 8 => ppu.wx;

    state, 8 => ppu.state;
    ly_for_compare, 8 => ppu.ly_for_compare;
    stat_signal, 1 => ppu.stat_signal;
    ly_compare_signal, 1 => ppu.ly_compare_signal;
    stat_mode_for_interrupt, 2 => ppu.stat_mode_for_interrupt;
    scanline_x, 8 => ppu.scanline_x;
}

decl_regs! {
    TimerRegs, "timer", timer: &Timer,
    div, 16 => timer.div;
    tima, 8 => timer.tima;
    tma, 8 => timer.tma;
    tac, 8 => timer.tac;
    loading, 8 => timer.loading;
}

struct Wire {
    id: vcd::IdCode,
    value: std::cell::Cell<MaxWidth>,
    width: u8,
    name: String,
}

struct MyWriter {
    writer: vcd::Writer<BufWriter<File>>,
    // Buffer of (timestamp, wire, value)
    buffer: Vec<(u64, WireIndex, MaxWidth)>,
    wires: Vec<Wire>,
    last_commit: u64,
}
impl MyWriter {
    fn new(filename: &str) -> std::io::Result<Self> {
        let file = std::fs::File::create(filename).unwrap();
        let buf = std::io::BufWriter::new(file);
        let mut writer = vcd::Writer::new(buf);

        writer.timescale(TIMESCALE as u32, vcd::TimescaleUnit::NS)?;

        Ok(Self {
            writer,
            buffer: Vec::new(),
            wires: Vec::new(),
            last_commit: 0,
        })
    }

    fn add_module(&mut self, name: &str) -> std::io::Result<()> {
        self.writer.add_module(name)
    }

    fn close_module(&mut self) -> std::io::Result<()> {
        self.writer.upscope()
    }

    fn add_wire(&mut self, width: u8, name: &str) -> std::io::Result<WireIndex> {
        let id = self.writer.add_wire(width as u32, name)?;
        let index = self.wires.len();

        if index > WireIndex::MAX as usize {
            panic!("Too many wires");
        }

        let wire = Wire {
            id,
            width,
            value: 0x8000.into(),
            name: name.to_string(),
        };

        self.wires.push(wire);

        Ok(index as WireIndex)
    }

    fn change(
        &mut self,
        clock_count: u64,
        index: WireIndex,
        value: MaxWidth,
    ) -> std::io::Result<()> {
        let timestamp = clock_to_timestamp(clock_count);
        self.change_ns(timestamp, index, value)
    }

    #[inline(never)]
    fn change_ns(
        &mut self,
        timestamp: u64,
        index: WireIndex,
        value: MaxWidth,
    ) -> std::io::Result<()> {
        let wire = self.wires.get(index as usize).unwrap();
        debug_assert!(
            self.last_commit <= timestamp,
            "{} < {} at {}",
            self.last_commit,
            timestamp,
            wire.name
        );

        if wire.value.get() == value {
            return Ok(());
        }
        wire.value.set(value);

        if self
            .buffer
            .last()
            .map_or(false, |&(t, i, _)| t == timestamp && i == index)
        {
            self.buffer.pop();
        }
        self.buffer.push((timestamp, index, value));

        Ok(())
    }

    fn begin(&mut self) -> std::io::Result<()> {
        self.writer.enddefinitions()?;
        self.writer.begin(vcd::SimulationCommand::Dumpvars)?;
        Ok(())
    }

    fn commit(&mut self) -> std::io::Result<()> {
        self.buffer
            .sort_unstable_by_key(|(timestamp, _, _)| *timestamp);

        for (timestamp, index, value) in self.buffer.drain(..) {
            if timestamp != self.last_commit {
                debug_assert!(
                    self.last_commit <= timestamp,
                    "{} < {}",
                    self.last_commit,
                    timestamp
                );
                self.writer.timestamp(timestamp)?;
                self.last_commit = timestamp;
            }

            let wire = self.wires.get(index as usize).unwrap();
            if wire.width == 1 {
                self.writer.change_scalar(wire.id, value == 1)?;
            } else {
                self.writer
                    .change_vector(wire.id, to_bits(wire.width, value))?;
            }
        }

        Ok(())
    }
}

pub struct WaveTrace {
    writer: RefCell<MyWriter>,
    last_clock_count: Cell<u64>,
    clk: WireIndex,
    address_bus: WireIndex,
    data_bus: WireIndex,
    read: WireIndex,
    write: WireIndex,
    gameboy_regs: GameboyRegs,
    cpu_regs: CpuRegs,
    ppu_regs: PpuRegs,
    timer_regs: TimerRegs,
}
impl WaveTrace {
    pub fn new() -> std::io::Result<Self> {
        let filename = "wave_trace/trace.vcd";

        // create dir if not exists
        std::fs::create_dir_all("wave_trace").unwrap();

        let mut writer = MyWriter::new(filename)?;

        writer.add_module("gameroy")?;

        let clk = writer.add_wire(1, "clk")?;
        let address_bus = writer.add_wire(16, "address_bus")?;
        let data_bus = writer.add_wire(8, "data_bus")?;
        let read = writer.add_wire(1, "read")?;
        let write = writer.add_wire(1, "write")?;

        let gameboy_regs = GameboyRegs::new(&mut writer)?;
        let cpu_regs = CpuRegs::new(&mut writer)?;
        let ppu_regs = PpuRegs::new(&mut writer)?;
        let timer_regs = TimerRegs::new(&mut writer)?;

        writer.close_module()?;

        writer.begin()?;

        let this = Self {
            writer: RefCell::new(writer),
            last_clock_count: u64::MAX.into(),
            clk,
            address_bus,
            data_bus,
            read,
            write,
            gameboy_regs,
            cpu_regs,
            ppu_regs,
            timer_regs,
        };

        Ok(this)
    }

    pub fn trace_gameboy(&self, clock_count: u64, gameboy: &GameBoy) -> std::io::Result<()> {
        self.trace_gameboy_ex(clock_count, gameboy, None)
    }

    pub fn trace_gameboy_ex(
        &self,
        clock_count: u64,
        gameboy: &GameBoy,
        bus: Option<(u16, u8, bool)>,
    ) -> std::io::Result<()> {
        let mut writer = self.writer.borrow_mut();

        if self.last_clock_count.get() != u64::MAX {
            for c in self.last_clock_count.get()..clock_count {
                let t = clock_to_timestamp(c);
                let delta = CYCLE_PERIOD / 2;
                writer.change_ns(t + delta, self.clk, 0)?;
                writer.change_ns(t + 2 * delta, self.clk, 1)?;
            }
        } else {
            writer.change(clock_count, self.clk, 1)?;
        }

        if let Some(bus) = bus {
            writer.change(clock_count - 4, self.address_bus, bus.0 as MaxWidth)?;
            writer.change(clock_count - 4, self.data_bus, bus.1 as MaxWidth)?;
            writer.change(clock_count - 4, self.read, !bus.2 as MaxWidth)?;
            writer.change(clock_count - 4, self.write, bus.2 as MaxWidth)?;
        } else {
            writer.change(clock_count - 4, self.read, 0)?;
            writer.change(clock_count - 4, self.write, 0)?;
        }

        self.last_clock_count.set(clock_count);

        self.gameboy_regs.trace(clock_count, &mut writer, gameboy)?;
        self.cpu_regs
            .trace(clock_count, &mut writer, &gameboy.cpu)?;

        Ok(())
    }

    pub fn trace_ppu(&self, clock_count: u64, ppu: &Ppu) -> std::io::Result<()> {
        let mut writer = self.writer.borrow_mut();

        self.ppu_regs.trace(clock_count, &mut writer, ppu)?;

        Ok(())
    }

    pub fn trace_timer(&self, clock_count: u64, timer: &Timer) -> std::io::Result<()> {
        let mut writer = self.writer.borrow_mut();

        self.timer_regs.trace(clock_count, &mut writer, timer)?;

        Ok(())
    }

    pub fn buffer_size(&self) -> usize {
        self.writer.borrow().buffer.len() * std::mem::size_of::<(u64, WireIndex, MaxWidth)>()
    }

    pub fn commit(&self) -> std::io::Result<()> {
        self.writer.borrow_mut().commit()
    }
}

fn to_bits(n: u8, value: MaxWidth) -> impl Iterator<Item = vcd::Value> {
    (0..n).rev().map(move |i| ((value >> i) & 1 == 1).into())
}
