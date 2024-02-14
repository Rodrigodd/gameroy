use std::cell::RefCell;
use std::fs::File;
use std::io::BufWriter;

use crate::gameboy::GameBoy;

// NOTE: The actual clock period should be 1/(2^22 Hz) = 476.837 ns, but msinger's
// dmg-sim (the verilog simulation I am comparing to) uses a period of 244 ns.
const CYCLE_PERIOD: u64 = 244;

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {
        $sub
    };
}

// declare a const array, while deducing the length from the number of elements.
macro_rules! const_array {
    ($ident:ident: $type:ty, $($value:expr,)*) => {
        const $ident: [$type; {0usize $(+ replace_expr!($value 1usize))*}]= [$($value),*];
    };
}

const_array!(
    PPU_REGS: (u16, &str),
    (0xff40, "LCDC_ff40"),
    (0xff41, "STAT_ff41"),
    (0xff42, "SCY_ff42"),
    (0xff43, "SCX_ff43"),
    (0xff44, "LY_ff44"),
    (0xff45, "LYC_ff45"),
    // (0xff47, "BGP_ff47"),
    // (0xff48, "OBP0_ff48"),
    // (0xff49, "OBP1_ff49"),
    // (0xff4a, "WY_ff4a"),
    // (0xff4b, "WX_ff4b"),
);

pub struct CpuRegs {
    f: Wire,
    a: Wire,
    c: Wire,
    b: Wire,
    e: Wire,
    d: Wire,
    l: Wire,
    h: Wire,
    sp: Wire,
    pc: Wire,
    ime: Wire,
    state: Wire,
    halt_bug: Wire,
}

struct Wire {
    id: vcd::IdCode,
    value: std::cell::Cell<u16>,
    width: u8,
}

struct MyWriter {
    writer: vcd::Writer<BufWriter<File>>,
}
impl MyWriter {
    fn new(filename: &str) -> std::io::Result<Self> {
        let file = std::fs::File::create(filename).unwrap();
        let buf = std::io::BufWriter::new(file);
        let mut writer = vcd::Writer::new(buf);

        writer.timescale(CYCLE_PERIOD as u32, vcd::TimescaleUnit::NS)?; // 4.096 MHz
                                                                        //
        Ok(Self { writer })
    }

    fn add_module(&mut self, name: &str) -> std::io::Result<()> {
        self.writer.add_module(name)
    }

    fn close_module(&mut self) -> std::io::Result<()> {
        self.writer.upscope()
    }

    fn add_wire(&mut self, width: u8, name: &str) -> std::io::Result<Wire> {
        let id = self.writer.add_wire(width as u32, name)?;
        Ok(Wire {
            id,
            width,
            value: 0x8000.into(),
        })
    }

    fn change(&mut self, wire: &Wire, value: u16) -> std::io::Result<()> {
        if wire.value.get() == value {
            return Ok(());
        }
        wire.value.set(value);

        if wire.width == 1 {
            self.writer.change_scalar(wire.id, value != 0)?;
        } else {
            self.writer
                .change_vector(wire.id, to_bits(wire.width, value))?;
        }
        Ok(())
    }

    fn timestamp(&mut self, time: u64) -> std::io::Result<()> {
        self.writer.timestamp(time)
    }

    fn begin(&mut self) -> std::io::Result<()> {
        self.writer.enddefinitions()?;
        self.writer.begin(vcd::SimulationCommand::Dumpvars)?;
        Ok(())
    }
}

pub(crate) struct VcdWriter {
    writer: RefCell<MyWriter>,
    cpu_regs: CpuRegs,
    ppu_regs: [(u16, Wire); PPU_REGS.len()],
}
impl VcdWriter {
    pub fn new() -> std::io::Result<Self> {
        // let timestamp = std::time::SystemTime::now()
        //     .duration_since(std::time::UNIX_EPOCH)
        //     .unwrap()
        //     .as_secs();
        // let filename = format!("vcd_trace/{}.vcd", timestamp);
        let filename = "vcd_trace/trace.vcd";

        // create dir if not exists
        std::fs::create_dir_all("vcd_trace").unwrap();

        let mut writer = MyWriter::new(filename)?;

        writer.add_module("gameroy")?;

        writer.add_module("cpu")?;
        let cpu_regs = CpuRegs {
            f: writer.add_wire(8, "f")?,
            a: writer.add_wire(8, "a")?,
            c: writer.add_wire(8, "c")?,
            b: writer.add_wire(8, "b")?,
            e: writer.add_wire(8, "e")?,
            d: writer.add_wire(8, "d")?,
            l: writer.add_wire(8, "l")?,
            h: writer.add_wire(8, "h")?,
            sp: writer.add_wire(16, "sp")?,
            pc: writer.add_wire(16, "pc")?,
            ime: writer.add_wire(2, "ime")?,
            state: writer.add_wire(2, "state")?,
            halt_bug: writer.add_wire(1, "halt_bug")?,
        };
        writer.close_module()?;

        writer.add_module("ppu")?;

        let ppu_regs = PPU_REGS.map(|(addr, name)| {
            let id = writer.add_wire(8, name).unwrap();
            (addr, id)
        });

        writer.close_module()?;

        writer.close_module()?;

        writer.begin()?;

        let this = Self {
            writer: RefCell::new(writer),
            cpu_regs,
            ppu_regs,
        };

        Ok(this)
    }

    pub fn trace(&self, gb: &GameBoy) -> std::io::Result<()> {
        let mut writer = self.writer.borrow_mut();

        writer.timestamp(gb.clock_count)?;

        {
            let cpu = &gb.cpu;
            writer.change(&self.cpu_regs.f, cpu.f.0 as u16)?;
            writer.change(&self.cpu_regs.a, cpu.a as u16)?;
            writer.change(&self.cpu_regs.c, cpu.c as u16)?;
            writer.change(&self.cpu_regs.b, cpu.b as u16)?;
            writer.change(&self.cpu_regs.e, cpu.e as u16)?;
            writer.change(&self.cpu_regs.d, cpu.d as u16)?;
            writer.change(&self.cpu_regs.l, cpu.l as u16)?;
            writer.change(&self.cpu_regs.h, cpu.h as u16)?;
            writer.change(&self.cpu_regs.sp, cpu.sp)?;
            writer.change(&self.cpu_regs.pc, cpu.pc)?;
            writer.change(&self.cpu_regs.ime, cpu.ime as u16)?;
            writer.change(&self.cpu_regs.state, cpu.state as u16)?;
            writer.change(&self.cpu_regs.halt_bug, cpu.halt_bug as u16)?;
        }

        for (addr, id) in self.ppu_regs.iter() {
            writer.change(id, gb.read(*addr) as u16)?;
        }

        Ok(())
    }
}

fn to_bits(n: u8, value: u16) -> impl Iterator<Item = vcd::Value> {
    (0..n).rev().map(move |i| ((value >> i) & 1 == 1).into())
}
