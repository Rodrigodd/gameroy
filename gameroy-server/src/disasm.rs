//! Windowed access to the disassembly trace.
//!
//! `Trace::directives` is a `BTreeSet` that grows as code is discovered, and the
//! old front-end cloned the whole thing on every pause and binary-searched it.
//! In-process that was wasteful; across a serialized boundary it is impossible.
//!
//! So the session keeps a flat index alongside it, rebuilt only when the trace
//! actually grows, and ships a window of lines around the PC.

use gameroy::disassembler::{Address as CoreAddress, Directive, disassembly_opcode};
use gameroy::gameboy::GameBoy;

use crate::protocol::{Address, DisasmLine, DisassemblyWindow};

#[derive(Default)]
pub struct DisasmIndex {
    /// Flattened `trace.directives`, in address order.
    directives: Vec<Directive>,
    /// Number of directives the index was built from. The trace only grows, so a
    /// length change is a sufficient and very cheap staleness check.
    built_from_len: usize,
    generation: u32,
}

impl DisasmIndex {
    /// Rebuilds only when the trace has grown. Called on every pause, so the
    /// common path must stay a single length comparison.
    pub fn sync(&mut self, gb: &GameBoy) {
        let trace = gb.trace.borrow();
        if trace.directives.len() == self.built_from_len && !self.directives.is_empty() {
            return;
        }
        self.directives = trace.directives.iter().cloned().collect();
        self.built_from_len = self.directives.len();
        self.generation = self.generation.wrapping_add(1);
    }

    /// A window of `lines` entries centred on the current PC.
    pub fn window_around_pc(&self, gb: &GameBoy, lines: u16) -> DisassemblyWindow {
        let total = self.directives.len() as u32;
        if total == 0 || lines == 0 {
            return DisassemblyWindow {
                generation: self.generation,
                total,
                ..Default::default()
            };
        }

        let banks = gb.cartridge.curr_bank();
        let pc = gb.cpu.pc;
        let current = CoreAddress::from_pc(banks, pc);
        let pc_index = current.and_then(|addr| {
            self.directives
                .binary_search_by(|d| d.address.cmp(&addr))
                .ok()
                .map(|i| i as u32)
        });

        // Centre on the PC when it is traced; otherwise show the start, which is
        // what the user sees before any code has been executed.
        let centre = pc_index.unwrap_or(0) as usize;
        let half = lines as usize / 2;
        let start = centre.saturating_sub(half);
        let end = (start + lines as usize).min(self.directives.len());
        let start = end.saturating_sub(lines as usize).min(start);

        let trace = gb.trace.borrow();
        let rendered = self.directives[start..end]
            .iter()
            .map(|d| {
                let mut text = String::new();
                let len = (d.len as usize).min(d.op.len());
                // Resolve jump targets to label names where the trace knows one,
                // exactly as the old viewer's text did.
                let _ = disassembly_opcode(
                    d.address.address,
                    &d.op[..len],
                    |target| match CoreAddress::from_pc(banks, target)
                        .and_then(|a| trace.labels.get(&a))
                    {
                        Some(label) => label.name.clone(),
                        None => format!("${target:04x}"),
                    },
                    &mut text,
                );
                DisasmLine {
                    address: Address {
                        bank: d.address.bank,
                        address: d.address.address,
                    },
                    pc: d.address.address,
                    label: trace.labels.get(&d.address).map(|l| l.name.clone()),
                    text: text.trim_end().to_string(),
                    is_current: current == Some(d.address),
                }
            })
            .collect();

        DisassemblyWindow {
            start_index: start as u32,
            total,
            generation: self.generation,
            lines: rendered,
            pc_index,
        }
    }
}
