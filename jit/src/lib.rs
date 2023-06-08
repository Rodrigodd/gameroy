use dynasmrt::ExecutableBuffer;
use gameroy::{
    consts::{self, CB_CLOCK, CLOCK, CLOCK_SPEED, LEN},
    disassembler::{Address, Cursor},
    gameboy::{cpu::CpuState, GameBoy},
    interpreter::Interpreter,
};
use std::{
    collections::HashMap,
    hash::{BuildHasher, Hasher},
};

use self::x64::BlockCompiler;

#[cfg(target_os = "windows")]
mod windows;

mod x64;

pub struct Block {
    _start_address: u16,
    _length: u16,
    initial_block_clock_cycles: u32,
    max_clock_cycles: u32,
    fn_ptr: unsafe extern "sysv64" fn(&mut GameBoy),
    pub _compiled_code: ExecutableBuffer,
    bytes: usize,

    cleared_flags: usize,
    partially_cleared_flags: usize,
    non_cleared_flags: usize,
}

impl Block {
    #[inline(never)]
    fn call(&self, gb: &mut GameBoy) {
        // SAFETY: As long as `Block`s are only generated from BlockCompiler::compile, and
        // Self::_compiled_code is not mutated, self.fn_ptr should be pointing to a valid x64
        // function.
        unsafe { (self.fn_ptr)(gb) }
    }
}

/// A block of instruction to be compiled.
struct BlockTrace {
    instrs: Vec<Instr>,
    length: u16,
    // Pairs of (instr index, cycles count) of points where the next_interrupt is checked. It is
    // often after a write.
    interrupt_checks: Vec<(u16, u32)>,
}

pub struct CompilerOpts {
    pub flags_analysis: bool,
}

struct Instr {
    op: [u8; 3],
    pc: u16,
    bank: u16,
    curr_clock_count: u32,
}

fn trace_a_block(gb: &GameBoy) -> BlockTrace {
    let bank = gb.cartridge.curr_bank();

    let cursor = Cursor {
        bank0: bank.0,
        bank: Some(bank.1),
        pc: gb.cpu.pc,
        reg_a: None,
    };

    let mut cursors = vec![cursor];

    let mut interrupt_checks = Vec::new();

    let mut mark_check = |instrs: &Vec<Instr>, max_clock_cycles: &mut u32| {
        interrupt_checks.push((instrs.len() as u16 - 1, *max_clock_cycles));
    };

    let mut curr_clock_count = 0;
    let mut length = 0;

    let mut instrs = Vec::new();

    while let Some(cursor) = cursors.pop() {
        let (op, len) = cursor.get_op(gb);
        length += len as u16;
        curr_clock_count += if op[0] == 0xcb {
            CB_CLOCK[op[1] as usize] as u32
        } else {
            CLOCK[op[0] as usize] as u32
        };

        instrs.push(Instr {
            op,
            pc: cursor.pc,
            bank: if cursor.pc <= 0x3FFF {
                cursor.bank0
            } else {
                cursor.bank.unwrap()
            },
            curr_clock_count,
        });

        // after writing to RAM, a next_interrupt check is emmited.
        let write_ram = if op[0] == 0xcb {
            consts::CB_WRITE_RAM[op[1] as usize]
        } else {
            consts::WRITE_RAM[op[0] as usize]
        };
        if write_ram {
            mark_check(&instrs, &mut curr_clock_count);
        }

        if [
            0x18, 0xc3, 0xc7, 0xc9, 0xcd, 0xcf, 0xd7, 0xd7, 0xe7, 0xe9, 0xef, 0xff, 0xff,
        ]
        .contains(&op[0])
        {
            break;
        }

        let (step, _jump) = gameroy::disassembler::compute_step(len, cursor, &op, &gb.cartridge);

        let step = match step {
            Some(step) if step.pc < 0x4000 && step.bank0 == bank.0 || step.bank == Some(bank.1) => {
                step
            }
            _ => break,
        };

        let (op, len) = step.get_op(gb);
        if step.pc + len as u16 > 0x8000 {
            break;
        }

        if [0x10, 0x76].contains(&op[0]) {
            break;
        }

        cursors.push(step);
    }

    mark_check(&instrs, &mut curr_clock_count);

    BlockTrace {
        instrs,
        length,
        interrupt_checks,
    }
}

pub struct NoHashHasher(u64);
impl Hasher for NoHashHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, _: &[u8]) {
        panic!("should only be hashing u16")
    }

    fn write_u16(&mut self, i: u16) {
        self.0 = (self.0 << 16) | i as u64;
    }
}
impl BuildHasher for NoHashHasher {
    type Hasher = Self;

    fn build_hasher(&self) -> Self::Hasher {
        Self(0)
    }
}

#[derive(Default)]
struct Stats {
    cycles_compiled: u64,
    cycles_interpreted: u64,
    cycles_on_ram: u64,
    blocks_runned: u64,
    fallbacks_on_ram: u64,
    fallbacks_on_halt: u64,
    fallbacks_on_interrupt: u64,
    fallbacks_other: u64,
}
impl Drop for JitCompiler {
    fn drop(&mut self) {
        let total_cycles = self.stats.cycles_compiled + self.stats.cycles_interpreted;
        let total_queries = self.stats.blocks_runned
            + self.stats.fallbacks_on_ram
            + self.stats.fallbacks_on_halt
            + self.stats.fallbacks_on_interrupt
            + self.stats.fallbacks_other;

        let compiled_bytes = self.blocks.values().map(|block| block.bytes).sum::<usize>();

        let cleared_flags = self
            .blocks
            .values()
            .map(|block| block.cleared_flags)
            .sum::<usize>();

        let partially_cleared_flags = self
            .blocks
            .values()
            .map(|block| block.partially_cleared_flags)
            .sum::<usize>();

        let non_cleared_flags = self
            .blocks
            .values()
            .map(|block| block.non_cleared_flags)
            .sum::<usize>();

        let total_flags = cleared_flags + partially_cleared_flags + non_cleared_flags;

        println!(
            r#"Statistics:
    compiled:    {:10} cycles ({:.2}%)
    interpreted: {:10} cycles ({:.2}%)
    ram:         {:10} cycles ({:.2}%)
    blocks runned:          {:10} ({:.2}%)
    fallbacks on ram:       {:10} ({:.2}%)
    fallbacks on halt:      {:10} ({:.2}%)
    fallbacks on interrupt: {:10} ({:.2}%)
    fallbacks on other:     {:10} ({:.2}%)
    compiled bytes:         {:10} bytes
    cleared flags:          {:10} ({:.2}%)
    partially cleared flags:{:10} ({:.2}%)
    non cleared flags:      {:10} ({:.2}%)"#,
            self.stats.cycles_compiled,
            100.0 * self.stats.cycles_compiled as f64 / total_cycles as f64,
            self.stats.cycles_interpreted,
            100.0 * self.stats.cycles_interpreted as f64 / total_cycles as f64,
            self.stats.cycles_on_ram,
            100.0 * self.stats.cycles_on_ram as f64 / total_cycles as f64,
            self.stats.blocks_runned,
            100.0 * self.stats.blocks_runned as f64 / total_queries as f64,
            self.stats.fallbacks_on_ram,
            100.0 * self.stats.fallbacks_on_ram as f64 / total_queries as f64,
            self.stats.fallbacks_on_halt,
            100.0 * self.stats.fallbacks_on_halt as f64 / total_queries as f64,
            self.stats.fallbacks_on_interrupt,
            100.0 * self.stats.fallbacks_on_interrupt as f64 / total_queries as f64,
            self.stats.fallbacks_other,
            100.0 * self.stats.fallbacks_other as f64 / total_queries as f64,
            compiled_bytes,
            cleared_flags,
            100.0 * cleared_flags as f64 / total_flags as f64,
            partially_cleared_flags,
            100.0 * partially_cleared_flags as f64 / total_flags as f64,
            non_cleared_flags,
            100.0 * non_cleared_flags as f64 / total_flags as f64,
        );
    }
}

pub struct JitCompiler {
    pub blocks: HashMap<Address, Block, NoHashHasher>,
    stats: Stats,
    pub opts: CompilerOpts,
}

impl Default for JitCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl JitCompiler {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::with_hasher(NoHashHasher(0)),
            stats: Stats::default(),
            opts: CompilerOpts {
                flags_analysis: true,
            },
        }
    }

    pub fn get_block(&mut self, gb: &GameBoy) -> Option<&Block> {
        let pc = gb.cpu.pc;
        let bank = gb.cartridge.curr_bank();

        if pc >= 0x8000 {
            // don't compile code outside ROM
            return None;
        }

        let op = gb.cartridge.read(pc);

        // if STOP or HALT, fallback to interpreter
        if op == 0x10 || op == 0x76 {
            return None;
        }

        let len = LEN[op as usize];

        if pc < 0x4000 && pc + len as u16 >= 0x4000 {
            return None;
        }
        if pc + len as u16 >= 0x8000 {
            return None;
        }

        let address = Address::from_pc(bank, pc)?;
        Some(
            self.blocks
                .entry(address)
                .or_insert_with(|| BlockCompiler::new(gb).compile_block(&self.opts)),
        )
    }

    pub fn interpret_block(&mut self, gb: &mut GameBoy) {
        let on_ram = gb.cpu.pc >= 0x8000;

        let mut stats = std::mem::take(&mut self.stats);

        let block = self.get_block(gb);
        let next_interrupt = gb.next_interrupt.get();
        let start_clock = gb.clock_count;

        let block = match block {
            Some(block) => 'block: {
                if gb.cpu.state != CpuState::Running {
                    stats.fallbacks_on_halt += 1;
                    break 'block None;
                }

                let next_check = gb.clock_count + block.initial_block_clock_cycles as u64 + 4;
                if next_interrupt <= next_check {
                    stats.fallbacks_on_interrupt += 1;
                    break 'block None;
                }

                stats.blocks_runned += 1;
                Some(block)
            }
            None => {
                if on_ram {
                    stats.fallbacks_on_ram += 1;
                } else {
                    stats.fallbacks_other += 1;
                }
                None
            }
        };

        match block {
            Some(block) => {
                // let bank = gb.cartridge.curr_bank();
                // println!(
                //     "running {:02x} {:04x} ({})",
                //     if block._start_address <= 0x3FFF {
                //         bank.0
                //     } else {
                //         bank.1
                //     },
                //     block._start_address,
                //     gb.clock_count,
                // );
                block.call(gb);
                debug_assert!(gb.clock_count != start_clock);

                stats.cycles_compiled += gb.clock_count - start_clock;

                // assert that no interrupt happened inside the block (unless it happend in a write
                // of the last instruction).
                debug_assert!(
                    gb.clock_count - 24 < gb.next_interrupt.get(),
                    "{} < {}",
                    gb.clock_count,
                    next_interrupt
                );
            }
            _ => {
                // println!("interpr {:04x} ({})", gb.cpu.pc, gb.clock_count);

                // avoid being stuck here for to long
                let timeout = gb.clock_count + CLOCK_SPEED / 60;

                let mut on_halt = 0;

                let mut inter = Interpreter(gb);
                loop {
                    let op = inter.0.read(inter.0.cpu.pc);

                    let now = inter.0.clock_count;
                    let is_halt = inter.0.cpu.state == CpuState::Halt;
                    inter.interpret_op();
                    let elapsed = inter.0.clock_count - now;
                    if is_halt {
                        on_halt += elapsed;
                    }

                    let is_jump = [
                        0xc2, 0xc3, 0xca, 0xd2, 0xda, 0xe9, 0x18, 0x20, 0x28, 0x30, 0x38, 0xc4,
                        0xcc, 0xcd, 0xd4, 0xdc, 0xc0, 0xc8, 0xc9, 0xd0, 0xd8, 0xd9, 0xc7, 0xcf,
                        0xd7, 0xdf, 0xe7, 0xef, 0xf7, 0xff,
                    ]
                    .contains(&op);

                    let is_interrupt = [0x40, 0x48, 0x50, 0x58, 0x60].contains(&inter.0.cpu.pc);

                    if is_interrupt
                        || is_jump && inter.0.cpu.pc < 0x8000
                        || inter.0.clock_count > timeout
                    {
                        stats.cycles_interpreted += inter.0.clock_count - start_clock - on_halt;
                        if on_ram {
                            stats.cycles_on_ram += inter.0.clock_count - start_clock - on_halt;
                        }
                        break;
                    }
                }
            }
        }

        self.stats = stats;
    }
}
