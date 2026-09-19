//! Windowed access to the disassembly trace.
//!
//! `Trace::directives` is a `BTreeSet` that grows as code is discovered, and the
//! old front-end cloned the whole thing on every pause and binary-searched it.
//! In-process that was wasteful; across a serialized boundary it is impossible.
//!
//! So the session keeps a flat index alongside it, rebuilt only when the trace
//! actually grows, and serves windows out of that: one centred on the PC on every
//! pause, and whatever else the client's scrollbar asks for.

use gameroy::disassembler::{Address as CoreAddress, Directive, disassembly_opcode};
use gameroy::gameboy::GameBoy;

use crate::protocol::{Address, DisasmAnchor, DisasmLine, DisassemblyWindow};

/// Marks where the jump operand starts and ends in a rendered line.
///
/// `disassembly_opcode` takes the operand text from a closure and formats it into
/// the output, so the closure cannot see where it landed. Wrapping the name in
/// two bytes the disassembler can never emit and searching for them afterwards is
/// the only way to recover the span. They never reach the wire.
const LINK_START: char = '\x01';
const LINK_END: char = '\x02';

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

    /// Throws the index away when a different machine takes over.
    ///
    /// Without this a second ROM whose trace happens to reach the same length as
    /// the first is displayed with the first one's rows, because `sync`
    /// short-circuits on exactly that comparison.
    pub fn reset(&mut self) {
        self.directives.clear();
        self.built_from_len = 0;
        self.generation = self.generation.wrapping_add(1);
    }

    /// The row index of `addr`, or of the first row after it.
    fn index_of(&self, addr: CoreAddress) -> Option<u32> {
        if self.directives.is_empty() {
            return None;
        }
        Some(
            match self.directives.binary_search_by(|d| d.address.cmp(&addr)) {
                Ok(i) => i as u32,
                // Not the start of a traced opcode; the nearest row after it is the
                // closest thing to what the user pointed at.
                Err(i) => (i.min(self.directives.len() - 1)) as u32,
            },
        )
    }

    fn pc_index(&self, gb: &GameBoy) -> Option<u32> {
        let current = CoreAddress::from_pc(gb.cartridge.curr_bank(), gb.cpu.pc)?;
        self.directives
            .binary_search_by(|d| d.address.cmp(&current))
            .ok()
            .map(|i| i as u32)
    }

    /// A window of `lines` rows positioned by `anchor`.
    pub fn window(&self, gb: &GameBoy, anchor: DisasmAnchor, lines: u16) -> DisassemblyWindow {
        let total = self.directives.len() as u32;
        let pc_index = self.pc_index(gb);
        if total == 0 || lines == 0 {
            return DisassemblyWindow {
                generation: self.generation,
                total,
                pc_index,
                ..Default::default()
            };
        }

        let focus = match anchor {
            DisasmAnchor::Index(i) => Some(i.min(total - 1)),
            DisasmAnchor::Address(a) => self.index_of(CoreAddress {
                bank: a.bank,
                address: a.address,
            }),
            // Before any code has run the PC may not be traced; the start of the
            // listing is the useful thing to show then.
            DisasmAnchor::Pc => Some(pc_index.unwrap_or(0)),
        };

        // An index anchor names the first row; everything else is centred on it.
        let start = match anchor {
            DisasmAnchor::Index(i) => i as usize,
            _ => (focus.unwrap_or(0) as usize).saturating_sub(lines as usize / 2),
        };
        let end = (start + lines as usize).min(self.directives.len());
        let start = end.saturating_sub(lines as usize).min(start);

        DisassemblyWindow {
            start_index: start as u32,
            total,
            generation: self.generation,
            lines: self.render(gb, start, end),
            pc_index,
            focus_index: focus,
        }
    }

    fn render(&self, gb: &GameBoy, start: usize, end: usize) -> Vec<DisasmLine> {
        let banks = gb.cartridge.curr_bank();
        let current = CoreAddress::from_pc(banks, gb.cpu.pc);
        let trace = gb.trace.borrow();

        self.directives[start..end]
            .iter()
            .map(|d| {
                // `trace.jumps` is keyed on the *opcode* address and holds the
                // destination the tracer actually followed. Re-deriving it with
                // `Address::from_pc` instead, as this used to, mislabels every
                // call into a bank that is not currently mapped.
                let destination = trace.jumps.get(&d.address).copied();

                let mut text = String::new();
                let len = (d.len as usize).min(d.op.len());
                let mut linked = false;
                let _ = disassembly_opcode(
                    d.address.address,
                    &d.op[..len],
                    |target| {
                        let Some(dest) = destination else {
                            // A 16-bit immediate the tracer did not follow, so
                            // there is nowhere to go; render it as a plain number.
                            return format!("${target:04x}");
                        };
                        linked = true;
                        // A jump whose destination has no label is still a jump.
                        // The old viewer unwrapped here and would panic.
                        let name = match trace.labels.get(&dest) {
                            Some(label) => label.name.clone(),
                            None => format!("${target:04x}"),
                        };
                        format!("{LINK_START}{name}{LINK_END}")
                    },
                    &mut text,
                );

                let (mnemonic, mut operand_prefix, mut link_text, operand_suffix) =
                    split(&text, linked);

                // `RST nnH` is written as a fixed string, so the closure above is
                // never called for it even though the tracer did record a jump.
                // Naming the destination here keeps every row that goes somewhere
                // clickable, and keeps `jump_target` and `link_text` in step.
                if link_text.is_empty()
                    && let Some(dest) = destination
                {
                    link_text = match trace.labels.get(&dest) {
                        Some(label) => label.name.clone(),
                        None => format!("${:04x}", cpu_address(dest)),
                    };
                    // `split` trims the line, so the appended name would other-
                    // wise butt straight up against the operand: `RST 30HHandler`.
                    if !operand_prefix.is_empty() {
                        operand_prefix.push(' ');
                    }
                }

                DisasmLine {
                    address: Address {
                        bank: d.address.bank,
                        address: d.address.address,
                    },
                    pc: cpu_address(d.address),
                    label: trace.labels.get(&d.address).map(|l| l.name.clone()),
                    mnemonic,
                    operand_prefix,
                    link_text,
                    operand_suffix,
                    jump_target: destination.map(|a| Address {
                        bank: a.bank,
                        address: a.address,
                    }),
                    jump_index: destination.and_then(|a| self.index_of(a)),
                    is_current: current == Some(d.address),
                }
            })
            .collect()
    }
}

/// The CPU address a directive lives at.
///
/// Bank 0 is mapped at 0x0000 and every other bank at 0x4000, regardless of what
/// happens to be paged in right now. This is what `break` and `runto` take, so
/// showing the raw in-bank offset instead -- as this used to for an unmapped
/// bank -- hands the user an address their own commands would reject.
fn cpu_address(addr: CoreAddress) -> u16 {
    if addr.bank == 0 {
        addr.address
    } else {
        addr.address + 0x4000
    }
}

/// Splits a rendered line into its coloured parts, consuming the link sentinels.
///
/// Returns `(mnemonic, operand_prefix, link_text, operand_suffix)`.
fn split(text: &str, linked: bool) -> (String, String, String, String) {
    let text = text.trim_end();
    let (head, link, tail) = match (linked, text.find(LINK_START), text.find(LINK_END)) {
        (true, Some(s), Some(e)) if s < e => (
            &text[..s],
            text[s + LINK_START.len_utf8()..e].to_string(),
            text[e + LINK_END.len_utf8()..].to_string(),
        ),
        _ => (text, String::new(), String::new()),
    };

    // The mnemonic is the leading word; whatever follows is operand text.
    match head.find(' ') {
        Some(i) => (
            head[..i].to_string(),
            head[i..].trim_start().to_string(),
            link,
            tail,
        ),
        None => (head.to_string(), String::new(), link, tail),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_a_line_with_a_jump_operand() {
        let text = format!("jp   nz, {LINK_START}Main{LINK_END}");
        let (m, prefix, link, suffix) = split(&text, true);
        assert_eq!(m, "jp");
        assert_eq!(prefix, "nz, ");
        assert_eq!(link, "Main");
        assert_eq!(suffix, "");
    }

    #[test]
    fn splits_a_line_without_one() {
        let (m, prefix, link, suffix) = split("ld   a, $ff  ", false);
        assert_eq!(m, "ld");
        assert_eq!(prefix, "a, $ff");
        assert!(link.is_empty() && suffix.is_empty());
    }

    #[test]
    fn splits_an_operandless_line() {
        let (m, prefix, link, suffix) = split("nop", false);
        assert_eq!(m, "nop");
        assert!(prefix.is_empty() && link.is_empty() && suffix.is_empty());
    }

    /// A sentinel that somehow survives without its partner must not slice the
    /// string at a nonsense offset.
    #[test]
    fn a_half_marked_line_falls_back_to_plain_text() {
        let text = format!("jp {LINK_START}Main");
        let (m, prefix, link, _) = split(&text, true);
        assert_eq!(m, "jp");
        assert!(link.is_empty());
        assert!(prefix.contains("Main"));
    }

    #[test]
    fn bank_zero_keeps_its_address_and_others_move_to_0x4000() {
        assert_eq!(
            cpu_address(CoreAddress {
                bank: 0,
                address: 0x0150
            }),
            0x0150
        );
        assert_eq!(
            cpu_address(CoreAddress {
                bank: 1,
                address: 0x0a12
            }),
            0x4a12
        );
        // The bug this replaces: a bank that is not currently paged in used to
        // report the raw in-bank offset.
        assert_eq!(
            cpu_address(CoreAddress {
                bank: 7,
                address: 0x0a12
            }),
            0x4a12
        );
    }
}
