use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    fmt::Write,
    ops::Range,
};

use crate::{
    consts,
    gameboy::{cartridge::Cartridge, GameBoy},
};

struct ReallySigned(i8);

impl fmt::LowerHex for ReallySigned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prefix = if f.alternate() { "0x" } else { "" };
        let bare_hex = format!("{:x}", self.0.abs());
        f.pad_integral(self.0 >= 0, prefix, &bare_hex)
    }
}

/// A address in the ROM.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Address {
    /// The bank where this address belongs
    pub bank: u16,
    /// The address in the bank, in range 0x0000..=0x3FFF
    pub address: u16,
}
impl Address {
    pub fn new(bank: u16, address: u16) -> Self {
        assert!(
            (0x0000..=0x3FFF).contains(&address),
            "address {:04x} is out of rom address range",
            address
        );
        Self { bank, address }
    }

    pub fn from_pc(
        banks: (impl Into<Option<u16>>, impl Into<Option<u16>>),
        pc: u16,
    ) -> Option<Self> {
        let banks = (banks.0.into(), banks.1.into());
        match pc {
            // lower bank
            0x0000..=0x3FFF => banks.0.map(|bank| Self { bank, address: pc }),
            // upper bank
            0x4000..=0x7FFF => banks.1.map(|bank| Self {
                bank,
                address: pc - 0x4000,
            }),
            // out of ROM
            _ => None,
        }
    }

    fn to_pc(self) -> u16 {
        if self.bank == 0 {
            self.address
        } else {
            self.address + 0x4000
        }
    }

    fn as_cursor(&self, cart: &Cartridge) -> Cursor {
        let pc = self.to_pc();
        Cursor {
            bank0: cart.bank0_from_bank(self.bank),
            bank: Some(self.bank),
            pc,
            reg_a: None,
        }
    }

    fn from_cursor(jump: &Cursor) -> Option<Self> {
        let &Cursor {
            bank0, bank, pc, ..
        } = jump;
        Self::from_pc((bank0, bank), pc)
    }
}

pub struct Label {
    /// The address specifies the bank and memory address
    pub address: Address,
    pub name: String,
}
impl Label {
    /// Create a Label in a given address with a generate name, in the format "L<BANK:02x>_<ADDRESS:04x>".
    fn new(address: Address) -> Self {
        Self {
            address,
            name: format!("L{:02x}_{:04x}", address.bank, address.address),
        }
    }
}

pub struct Cursor {
    /// The currently active bank in the 0 to 3FFF range.
    pub bank0: u16,
    /// The currently active ROM bank, if know
    pub bank: Option<u16>,
    /// The program counter, can have any value in the entire range
    pub pc: u16,
    /// The currently value of register A, if know
    pub reg_a: Option<u8>,
}
impl Cursor {
    /// Return the opcode, and the length
    pub fn get_op(&self, rom: &GameBoy) -> ([u8; 3], u8) {
        let bank = self.bank.unwrap_or(self.bank0);

        let read_pc = |of| {
            if self.pc.wrapping_add(of) < 0x7FFF {
                // read ROM
                rom.cartridge.read_at_bank(bank, self.pc.wrapping_add(of))
            } else {
                // read RAM
                rom.read(self.pc.wrapping_add(of))
            }
        };

        let op = [read_pc(0), read_pc(1), read_pc(2)];
        let len = consts::LEN[op[0] as usize];

        (op, len)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Directive {
    /// The address where this directive disassemble from
    pub address: Address,
    /// The length of bytes this directive disassemble from
    pub len: u16,
    /// The instruction of the directive
    pub op: [u8; 3],
}

pub struct Trace {
    /// Disassembled directives
    pub directives: BTreeSet<Directive>,
    /// Ranges of memory where code are executed
    pub code_ranges: Vec<Range<Address>>,
    /// Map between a address and a label
    pub labels: BTreeMap<Address, Label>,
    /// Map from a opcode (like jp or call) to another address
    pub jumps: BTreeMap<Address, Address>,
}
impl Default for Trace {
    fn default() -> Self {
        Self::new()
    }
}
impl Trace {
    pub fn new() -> Self {
        Self {
            directives: BTreeSet::new(),
            code_ranges: Vec::new(),
            labels: Default::default(),
            jumps: Default::default(),
        }
    }

    /// Disassembly some opcodes above and below, respecting `code_ranges`
    pub fn print_around(
        &mut self,
        banks: (u16, u16),
        pc: u16,
        rom: &GameBoy,
        w: &mut impl Write,
    ) -> fmt::Result {
        self.trace_starting_at(rom, banks, pc, None);

        let curr = Address::from_pc(banks, pc).unwrap();

        let curr_range = match self.get_curr_code_range(curr) {
            Some(x) => x,
            None => return writeln!(w, "Outside ROM..."),
        };

        const LOOK_ABOVE: u16 = 5;

        let mut queue = [Address::new(0, 0); LOOK_ABOVE as usize];
        let mut i = 0;
        let mut c = 0;
        let mut pc = curr_range.start;
        while pc < curr {
            queue[i] = pc;
            i = (i + 1) % LOOK_ABOVE as usize;
            c += 1;
            let (_, len) = pc.as_cursor(&rom.cartridge).get_op(rom);
            pc.address += len as u16;
        }
        if c < LOOK_ABOVE {
            for _ in c..LOOK_ABOVE {
                writeln!(w)?;
            }
            pc = curr_range.start;
        } else {
            pc = queue[i];
        }
        let label = |pc, x| {
            if let Some(address) = self.jumps.get(&pc) {
                return self.labels.get(address).unwrap().name.clone();
            }
            format!("${:04x}", x)
        };
        while pc < curr {
            write!(w, "  {:02x}_{:04x}: ", pc.bank, pc.address)?;
            let (op, len) = pc.as_cursor(&rom.cartridge).get_op(rom);
            disassembly_opcode(pc.address, &op[0..len as usize], |x| label(pc, x), w)?;
            writeln!(w)?;
            pc.address += len as u16;
        }

        let mut pc = curr;
        write!(w, ">>{:02x}_{:04x}: ", pc.bank, pc.address)?;
        let (op, len) = pc.as_cursor(&rom.cartridge).get_op(rom);
        disassembly_opcode(pc.address, &op[0..len as usize], |x| label(pc, x), w)?;
        writeln!(w)?;
        pc.address += len as u16;

        for c in 0..LOOK_ABOVE {
            if pc >= curr_range.end {
                for _ in c..LOOK_ABOVE {
                    writeln!(w)?;
                }
                break;
            }
            write!(w, "  {:02x}_{:04x}: ", pc.bank, pc.address)?;
            let (op, len) = pc.as_cursor(&rom.cartridge).get_op(rom);
            disassembly_opcode(pc.address, &op[0..len as usize], |x| label(pc, x), w)?;
            writeln!(w)?;
            pc.address += len as u16;
        }
        Ok(())
    }

    pub fn is_already_traced(&self, address: Address) -> bool {
        self.get_curr_code_range(address).is_some()
    }

    pub fn trace_starting_at(
        &mut self,
        gameboy: &GameBoy,
        banks: (u16, u16),
        start: u16,
        label: Option<String>,
    ) {
        if let Some(label) = label {
            let address = Address::from_pc(banks, start).unwrap();
            if let Some(x) = self.add_label(address) {
                x.name = label
            }
        }

        let cursor = Cursor {
            bank0: banks.0,
            bank: Some(banks.1),
            pc: start,
            reg_a: None,
        };

        let mut cursors = vec![cursor];
        while !cursors.is_empty() {
            self.trace_once(gameboy, &mut cursors);
        }
    }

    fn get_curr_code_range(&self, address: Address) -> Option<Range<Address>> {
        self.code_ranges
            .binary_search_by(|range| {
                use std::cmp::Ordering;
                if address < range.start {
                    Ordering::Greater
                } else if address >= range.end {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .map(|i| self.code_ranges[i].clone())
            .ok()
    }

    /// Insert a opcode to `Self::code_ranges`.
    /// Return true if the opcode was not added before.
    fn add_opcode(&mut self, address: Address, op: &[u8], len: u16) -> bool {
        let i = self.code_ranges.binary_search_by(|range| {
            use std::cmp::Ordering;
            if address < range.start {
                Ordering::Greater
            } else if address >= range.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        match i {
            Ok(_) => false,
            Err(i) => {
                let mut op_array = [0; 3];
                op_array[0..op.len()].copy_from_slice(op);
                self.directives.insert(Directive {
                    address,
                    len,
                    op: op_array,
                });

                // TODO: this min(0x3FFF) is a hack
                let address_end = Address::new(address.bank, (address.address + len).min(0x3FFF));
                let merge_previous = i > 0 && self.code_ranges[i - 1].end >= address;
                let merge_next =
                    i + 1 < self.code_ranges.len() && self.code_ranges[i].start <= address_end;

                if merge_previous && merge_next {
                    self.code_ranges[i - 1].end = self.code_ranges[i].end;
                    self.code_ranges.remove(i);
                } else if merge_previous {
                    self.code_ranges[i - 1].end = address_end;
                } else if merge_next {
                    self.code_ranges[i].start = address;
                } else {
                    self.code_ranges.insert(i, address..address_end);
                }
                true
            }
        }
    }

    fn add_label(&mut self, address: Address) -> Option<&mut Label> {
        Some(
            self.labels
                .entry(address)
                .or_insert_with(|| Label::new(address)),
        )
    }

    fn add_jump(&mut self, from: Address, to: Address) {
        if let Some(x) = self.add_label(to) {
            let to = x.address;
            self.jumps.insert(from, to);
        }
    }

    /// Pop a PC from '`cursors`, compute next possible PC values, and push to 'cursors'
    fn trace_once(&mut self, rom: &GameBoy, cursors: &mut Vec<Cursor>) {
        let cursor = cursors.pop().unwrap();

        let Some(address) = Address::from_cursor(&cursor) else {
            return
        };

        let (op, len) = cursor.get_op(rom);

        if !self.add_opcode(address, &op[0..len as usize], len as u16) {
            return;
        }

        let (step, jump) = compute_step(len, cursor, &op, &rom.cartridge);
        cursors.extend(step);

        let Some(jump) = jump else { return };

        if let Some(to) = Address::from_cursor(&jump) {
            self.add_jump(address, to);
            cursors.push(jump);
        }
    }

    pub fn fmt(&self, rom: &GameBoy, f: &mut impl Write) -> fmt::Result {
        for range in self.code_ranges.iter() {
            let mut pc = range.start;
            loop {
                if pc >= range.end {
                    break;
                }
                let (op, len) = pc.as_cursor(&rom.cartridge).get_op(rom);
                if let Some(label) = self.labels.get(&pc) {
                    writeln!(f, "{}:", label.name)?;
                }
                write!(f, "    ")?;
                write!(f, "{:02x}_{:04x}: ", pc.bank, pc.address)?;
                disassembly_opcode(
                    pc.to_pc(),
                    &op,
                    |x| {
                        if let Some(address) = self.jumps.get(&pc) {
                            return self.labels.get(address).unwrap().name.clone();
                        }
                        format!("${:04x}", x)
                    },
                    f,
                )?;
                writeln!(f)?;
                pc.address += len as u16;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

/// Return a (step, jump) pair.
pub fn compute_step(
    len: u8,
    curr: Cursor,
    op: &[u8],
    cart: &Cartridge,
) -> (Option<Cursor>, Option<Cursor>) {
    let Cursor {
        pc,
        bank0,
        bank,
        reg_a,
    } = curr;
    let step = move || {
        if pc < 0x4000 && pc + len as u16 >= 0x4000 {
            return None;
        }
        if pc + len as u16 >= 0x8000 {
            return None;
        }
        Some(Cursor {
            bank0,
            bank,
            pc: pc + len as u16,
            reg_a,
        })
    };
    let jump = move |dest: u16| {
        let bank = if cart.num_banks() == 2 {
            // If there is only one switchable bank, it will be the active.
            Some(1)
        } else if dest < 0x4000 {
            // if it jump to the bank 0, the program can switch banks.
            None
        } else {
            // if it continue in this bank, it is unlikely that it will switch banks.
            bank
        };
        Some(Cursor {
            bank0,
            bank,
            pc: dest,
            reg_a: None,
        })
    };
    match op[0] {
        0xC3 => {
            // JP $aaaa
            let dest = u16::from_le_bytes([op[1], op[2]]);
            (None, jump(dest))
        }
        0xE9 => {
            // JP (HL)
            // I can't predict where this will jump to
            (None, None)
        }
        x if x & 0b11100111 == 0b11000010 => {
            // JP cc, $aaaa
            let dest = u16::from_le_bytes([op[1], op[2]]);
            (step(), jump(dest))
        }
        0x18 => {
            // JR $rr
            let dest = pc
                .wrapping_add(len as u16)
                .wrapping_add_signed(op[1] as i8 as i16);
            (step(), jump(dest))
        }
        x if x & 0b1110_0111 == 0b0010_0000 => {
            // JR cc, $rr
            let dest = pc
                .wrapping_add(len as u16)
                .wrapping_add_signed(op[1] as i8 as i16);
            (step(), jump(dest))
        }
        x if x & 0b1100_1111 == 0b0000_1010 || x & 0b1111_1000 == 0b0111_1000 => {
            // LD A, ?
            (
                Some(Cursor {
                    bank0,
                    bank,
                    pc: pc.wrapping_add(len as u16),
                    reg_a: None,
                }),
                None,
            )
        }
        0x3e => {
            // LD A, d8
            (
                Some(Cursor {
                    bank0,
                    bank,
                    pc: pc.wrapping_add(len as u16),
                    reg_a: Some(op[1]),
                }),
                None,
            )
        }
        0xEA => {
            // LD (a16), A

            // If it write a know value of A to ROM region, the bank can be switched.
            let address = u16::from_le_bytes([op[1], op[2]]);

            match (reg_a, address) {
                (Some(value), 0x0000..=0x7FFF) => {
                    let bank = cart.bank_from_write(bank.unwrap_or(bank0), address, value);
                    let bank0 = cart.bank0_from_bank(bank);
                    (
                        Some(Cursor {
                            bank0,
                            bank: (bank != bank0).then_some(bank),
                            pc: pc.wrapping_add(len as u16),
                            reg_a,
                        }),
                        None,
                    )
                }
                (None, 0x0000..=0x7FFF) => (
                    Some(Cursor {
                        bank0,
                        bank,
                        pc: pc.wrapping_add(len as u16),
                        reg_a: None,
                    }),
                    None,
                ),
                _ => (
                    Some(Cursor {
                        bank0,
                        bank,
                        pc: pc.wrapping_add(len as u16),
                        reg_a,
                    }),
                    None,
                ),
            }
        }
        0x08 => {
            // LD (a16), SP
            let address = u16::from_le_bytes([op[1], op[2]]);
            let step = if (0x0000..=0x7FFF).contains(&address) {
                Some(Cursor {
                    bank0,
                    bank: None,
                    pc: pc.wrapping_add(len as u16),
                    reg_a,
                })
            } else {
                step()
            };
            (step, None)
        }
        x if x == 0x36
            || x & 0b1100_1111 == 0b0000_0010
            || x != 0x76 && x & 0b1111_1000 == 0b0111_0000
            || [0xc5, 0xd5, 0xe5, 0xf5].contains(&x) =>
        {
            // LD (?), ? or PUSH ?
            // this could switch banks, but is unlikely to happen when inside a switchable bank.
            let bank = if pc < 0x4000 { None } else { bank };
            (
                Some(Cursor {
                    bank0,
                    bank,
                    pc: pc.wrapping_add(len as u16),
                    reg_a,
                }),
                None,
            )
        }
        x if x == 0xCD || x & 0b11100111 == 0b11000100 => {
            // CALL $aaaa or CALL cc, $aaaa
            let dest = u16::from_le_bytes([op[1], op[2]]);
            // the call could switch bank, but is unlikely to happen when inside a switchable bank.
            let bank = if pc >= 0x4000 || dest >= 0x4000 {
                bank
            } else {
                None
            };
            (
                Some(Cursor {
                    bank0,
                    bank,
                    pc: pc.wrapping_add(len as u16),
                    reg_a: None,
                }),
                jump(dest),
            )
        }
        0xC9 | 0xD9 => {
            // RET or RETI
            (None, None)
        }
        x if x & 0b11000111 == 0b11000111 => {
            // RST n
            let dest = (x & 0b00111000) as u16;
            (
                // TODO: some RST (and maybe others calls too) discard the return address in the
                // calee, so tracing the next instruction here would be wrong.
                step(),
                jump(dest),
            )
        }
        _ => (step(), None),
    }
}

pub fn disassembly_opcode(
    pc: u16,
    op: &[u8],
    label: impl FnOnce(u16) -> String,
    w: &mut impl Write,
) -> fmt::Result {
    let len = op.len() as u16;
    let op = {
        let mut a = [0; 3];
        a[0..op.len()].copy_from_slice(op);
        a
    };
    match op[0] {
        0x00 => write!(w, "NOP  "),
        0x01 => write!(w, "LD   BC, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0x02 => write!(w, "LD   (BC), A "),
        0x03 => write!(w, "INC  BC "),
        0x04 => write!(w, "INC  B "),
        0x05 => write!(w, "DEC  B "),
        0x06 => write!(w, "LD   B, ${:02x} ", op[1]),
        0x07 => write!(w, "RLCA "),
        0x08 => write!(
            w,
            "LD   ({}), SP ",
            label(u16::from_le_bytes([op[1], op[2]]))
        ),
        0x09 => write!(w, "ADD  HL, BC "),
        0x0a => write!(w, "LD   A, (BC) "),
        0x0b => write!(w, "DEC  BC "),
        0x0c => write!(w, "INC  C "),
        0x0d => write!(w, "DEC  C "),
        0x0e => write!(w, "LD   C, ${:02x} ", op[1]),
        0x0f => write!(w, "RRCA "),
        0x10 => write!(w, "STOP 0 "),
        0x11 => write!(w, "LD   DE, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0x12 => write!(w, "LD   (DE), A "),
        0x13 => write!(w, "INC  DE "),
        0x14 => write!(w, "INC  D "),
        0x15 => write!(w, "DEC  D "),
        0x16 => write!(w, "LD   D, ${:02x} ", op[1]),
        0x17 => write!(w, "RLA  "),
        0x18 => write!(
            w,
            "JR   {} ",
            label(((pc + len) as i16 + op[1] as i8 as i16) as u16)
        ),
        0x19 => write!(w, "ADD  HL, DE "),
        0x1a => write!(w, "LD   A, (DE) "),
        0x1b => write!(w, "DEC  DE "),
        0x1c => write!(w, "INC  E "),
        0x1d => write!(w, "DEC  E "),
        0x1e => write!(w, "LD   E, ${:02x} ", op[1]),
        0x1f => write!(w, "RRA  "),
        0x20 => write!(
            w,
            "JR   NZ, {} ",
            label(((pc + len) as i16 + op[1] as i8 as i16) as u16)
        ),
        0x21 => write!(w, "LD   HL, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0x22 => write!(w, "LD   (HL+), A "),
        0x23 => write!(w, "INC  HL "),
        0x24 => write!(w, "INC  H "),
        0x25 => write!(w, "DEC  H "),
        0x26 => write!(w, "LD   H, ${:02x} ", op[1]),
        0x27 => write!(w, "DAA  "),
        0x28 => write!(
            w,
            "JR   Z, {} ",
            label(((pc + len) as i16 + op[1] as i8 as i16) as u16)
        ),
        0x29 => write!(w, "ADD  HL, HL "),
        0x2a => write!(w, "LD   A, (HL+) "),
        0x2b => write!(w, "DEC  HL "),
        0x2c => write!(w, "INC  L "),
        0x2d => write!(w, "DEC  L "),
        0x2e => write!(w, "LD   L, ${:02x} ", op[1]),
        0x2f => write!(w, "CPL  "),
        0x30 => write!(
            w,
            "JR   NC, {} ",
            label(((pc + len) as i16 + op[1] as i8 as i16) as u16)
        ),
        0x31 => write!(w, "LD   SP, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0x32 => write!(w, "LD   (HL-), A "),
        0x33 => write!(w, "INC  SP "),
        0x34 => write!(w, "INC  (HL) "),
        0x35 => write!(w, "DEC  (HL) "),
        0x36 => write!(w, "LD   (HL), ${:02x} ", op[1]),
        0x37 => write!(w, "SCF  "),
        0x38 => write!(
            w,
            "JR   C, {} ",
            label(((pc + len) as i16 + op[1] as i8 as i16) as u16)
        ),
        0x39 => write!(w, "ADD  HL, SP "),
        0x3a => write!(w, "LD   A, (HL-) "),
        0x3b => write!(w, "DEC  SP "),
        0x3c => write!(w, "INC  A "),
        0x3d => write!(w, "DEC  A "),
        0x3e => write!(w, "LD   A, ${:02x} ", op[1]),
        0x3f => write!(w, "CCF  "),
        0x40 => write!(w, "LD   B, B "),
        0x41 => write!(w, "LD   B, C "),
        0x42 => write!(w, "LD   B, D "),
        0x43 => write!(w, "LD   B, E "),
        0x44 => write!(w, "LD   B, H "),
        0x45 => write!(w, "LD   B, L "),
        0x46 => write!(w, "LD   B, (HL) "),
        0x47 => write!(w, "LD   B, A "),
        0x48 => write!(w, "LD   C, B "),
        0x49 => write!(w, "LD   C, C "),
        0x4a => write!(w, "LD   C, D "),
        0x4b => write!(w, "LD   C, E "),
        0x4c => write!(w, "LD   C, H "),
        0x4d => write!(w, "LD   C, L "),
        0x4e => write!(w, "LD   C, (HL) "),
        0x4f => write!(w, "LD   C, A "),
        0x50 => write!(w, "LD   D, B "),
        0x51 => write!(w, "LD   D, C "),
        0x52 => write!(w, "LD   D, D "),
        0x53 => write!(w, "LD   D, E "),
        0x54 => write!(w, "LD   D, H "),
        0x55 => write!(w, "LD   D, L "),
        0x56 => write!(w, "LD   D, (HL) "),
        0x57 => write!(w, "LD   D, A "),
        0x58 => write!(w, "LD   E, B "),
        0x59 => write!(w, "LD   E, C "),
        0x5a => write!(w, "LD   E, D "),
        0x5b => write!(w, "LD   E, E "),
        0x5c => write!(w, "LD   E, H "),
        0x5d => write!(w, "LD   E, L "),
        0x5e => write!(w, "LD   E, (HL) "),
        0x5f => write!(w, "LD   E, A "),
        0x60 => write!(w, "LD   H, B "),
        0x61 => write!(w, "LD   H, C "),
        0x62 => write!(w, "LD   H, D "),
        0x63 => write!(w, "LD   H, E "),
        0x64 => write!(w, "LD   H, H "),
        0x65 => write!(w, "LD   H, L "),
        0x66 => write!(w, "LD   H, (HL) "),
        0x67 => write!(w, "LD   H, A "),
        0x68 => write!(w, "LD   L, B "),
        0x69 => write!(w, "LD   L, C "),
        0x6a => write!(w, "LD   L, D "),
        0x6b => write!(w, "LD   L, E "),
        0x6c => write!(w, "LD   L, H "),
        0x6d => write!(w, "LD   L, L "),
        0x6e => write!(w, "LD   L, (HL) "),
        0x6f => write!(w, "LD   L, A "),
        0x70 => write!(w, "LD   (HL), B "),
        0x71 => write!(w, "LD   (HL), C "),
        0x72 => write!(w, "LD   (HL), D "),
        0x73 => write!(w, "LD   (HL), E "),
        0x74 => write!(w, "LD   (HL), H "),
        0x75 => write!(w, "LD   (HL), L "),
        0x76 => write!(w, "HALT "),
        0x77 => write!(w, "LD   (HL), A "),
        0x78 => write!(w, "LD   A, B "),
        0x79 => write!(w, "LD   A, C "),
        0x7a => write!(w, "LD   A, D "),
        0x7b => write!(w, "LD   A, E "),
        0x7c => write!(w, "LD   A, H "),
        0x7d => write!(w, "LD   A, L "),
        0x7e => write!(w, "LD   A, (HL) "),
        0x7f => write!(w, "LD   A, A "),
        0x80 => write!(w, "ADD  A, B "),
        0x81 => write!(w, "ADD  A, C "),
        0x82 => write!(w, "ADD  A, D "),
        0x83 => write!(w, "ADD  A, E "),
        0x84 => write!(w, "ADD  A, H "),
        0x85 => write!(w, "ADD  A, L "),
        0x86 => write!(w, "ADD  A, (HL) "),
        0x87 => write!(w, "ADD  A, A "),
        0x88 => write!(w, "ADC  A, B "),
        0x89 => write!(w, "ADC  A, C "),
        0x8a => write!(w, "ADC  A, D "),
        0x8b => write!(w, "ADC  A, E "),
        0x8c => write!(w, "ADC  A, H "),
        0x8d => write!(w, "ADC  A, L "),
        0x8e => write!(w, "ADC  A, (HL) "),
        0x8f => write!(w, "ADC  A, A "),
        0x90 => write!(w, "SUB  B "),
        0x91 => write!(w, "SUB  C "),
        0x92 => write!(w, "SUB  D "),
        0x93 => write!(w, "SUB  E "),
        0x94 => write!(w, "SUB  H "),
        0x95 => write!(w, "SUB  L "),
        0x96 => write!(w, "SUB  (HL) "),
        0x97 => write!(w, "SUB  A "),
        0x98 => write!(w, "SBC  A, B "),
        0x99 => write!(w, "SBC  A, C "),
        0x9a => write!(w, "SBC  A, D "),
        0x9b => write!(w, "SBC  A, E "),
        0x9c => write!(w, "SBC  A, H "),
        0x9d => write!(w, "SBC  A, L "),
        0x9e => write!(w, "SBC  A, (HL) "),
        0x9f => write!(w, "SBC  A, A "),
        0xa0 => write!(w, "AND  B "),
        0xa1 => write!(w, "AND  C "),
        0xa2 => write!(w, "AND  D "),
        0xa3 => write!(w, "AND  E "),
        0xa4 => write!(w, "AND  H "),
        0xa5 => write!(w, "AND  L "),
        0xa6 => write!(w, "AND  (HL) "),
        0xa7 => write!(w, "AND  A "),
        0xa8 => write!(w, "XOR  B "),
        0xa9 => write!(w, "XOR  C "),
        0xaa => write!(w, "XOR  D "),
        0xab => write!(w, "XOR  E "),
        0xac => write!(w, "XOR  H "),
        0xad => write!(w, "XOR  L "),
        0xae => write!(w, "XOR  (HL) "),
        0xaf => write!(w, "XOR  A "),
        0xb0 => write!(w, "OR   B "),
        0xb1 => write!(w, "OR   C "),
        0xb2 => write!(w, "OR   D "),
        0xb3 => write!(w, "OR   E "),
        0xb4 => write!(w, "OR   H "),
        0xb5 => write!(w, "OR   L "),
        0xb6 => write!(w, "OR   (HL) "),
        0xb7 => write!(w, "OR   A "),
        0xb8 => write!(w, "CP   B "),
        0xb9 => write!(w, "CP   C "),
        0xba => write!(w, "CP   D "),
        0xbb => write!(w, "CP   E "),
        0xbc => write!(w, "CP   H "),
        0xbd => write!(w, "CP   L "),
        0xbe => write!(w, "CP   (HL) "),
        0xbf => write!(w, "CP   A "),
        0xc0 => write!(w, "RET  NZ "),
        0xc1 => write!(w, "POP  BC "),
        0xc2 => write!(w, "JP   NZ, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xc3 => write!(w, "JP   {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xc4 => write!(w, "CALL NZ, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xc5 => write!(w, "PUSH BC "),
        0xc6 => write!(w, "ADD  A, ${:02x} ", op[1]),
        0xc7 => write!(w, "RST  00H "),
        0xc8 => write!(w, "RET  Z "),
        0xc9 => write!(w, "RET  "),
        0xca => write!(w, "JP   Z, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xcb => disassembly_opcode_cr(op[1], w),
        0xcc => write!(w, "CALL Z, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xcd => write!(w, "CALL {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xce => write!(w, "ADC  A, ${:02x} ", op[1]),
        0xcf => write!(w, "RST  08H "),
        0xd0 => write!(w, "RET  NC "),
        0xd1 => write!(w, "POP  DE "),
        0xd2 => write!(w, "JP   NC, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xd3 => write!(w, "     "),
        0xd4 => write!(w, "CALL NC, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xd5 => write!(w, "PUSH DE "),
        0xd6 => write!(w, "SUB  ${:02x} ", op[1]),
        0xd7 => write!(w, "RST  10H "),
        0xd8 => write!(w, "RET  C "),
        0xd9 => write!(w, "RETI "),
        0xda => write!(w, "JP   C, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xdb => write!(w, "     "),
        0xdc => write!(w, "CALL C, {} ", label(u16::from_le_bytes([op[1], op[2]]))),
        0xdd => write!(w, "     "),
        0xde => write!(w, "SBC  A, ${:02x} ", op[1]),
        0xdf => write!(w, "RST  18H "),
        0xe0 => write!(w, "LDH  ({}), A ", label(0xFF00 | op[1] as u16)),
        0xe1 => write!(w, "POP  HL "),
        0xe2 => write!(w, "LD   (C), A "),
        0xe3 => write!(w, "     "),
        0xe4 => write!(w, "     "),
        0xe5 => write!(w, "PUSH HL "),
        0xe6 => write!(w, "AND  ${:02x} ", op[1]),
        0xe7 => write!(w, "RST  20H "),
        0xe8 => write!(w, "ADD  SP, ${:+02x} ", ReallySigned(op[1] as i8)),
        0xe9 => write!(w, "JP   (HL) "),
        0xea => write!(
            w,
            "LD   ({}), A ",
            label(u16::from_le_bytes([op[1], op[2]]))
        ),
        0xeb => write!(w, "     "),
        0xec => write!(w, "     "),
        0xed => write!(w, "     "),
        0xee => write!(w, "XOR  ${:02x} ", op[1]),
        0xef => write!(w, "RST  28H "),
        0xf0 => write!(w, "LDH  A, ({}) ", label(0xFF00 | op[1] as u16)),
        0xf1 => write!(w, "POP  AF "),
        0xf2 => write!(w, "LD   A, (C) "),
        0xf3 => write!(w, "DI   "),
        0xf4 => write!(w, "     "),
        0xf5 => write!(w, "PUSH AF "),
        0xf6 => write!(w, "OR   ${:02x} ", op[1]),
        0xf7 => write!(w, "RST  30H "),
        0xf8 => write!(w, "LD   HL, SP+${:+02x} ", ReallySigned(op[1] as i8)),
        0xf9 => write!(w, "LD   SP, HL "),
        0xfa => write!(
            w,
            "LD   A, ({}) ",
            label(u16::from_le_bytes([op[1], op[2]]))
        ),
        0xfb => write!(w, "EI   "),
        0xfc => write!(w, "     "),
        0xfd => write!(w, "     "),
        0xfe => write!(w, "CP   ${:02x} ", op[1]),
        0xff => write!(w, "RST  38H "),
    }
}

fn disassembly_opcode_cr(op: u8, w: &mut impl Write) -> fmt::Result {
    match op {
        0x00 => write!(w, "RLC  B "),
        0x01 => write!(w, "RLC  C "),
        0x02 => write!(w, "RLC  D "),
        0x03 => write!(w, "RLC  E "),
        0x04 => write!(w, "RLC  H "),
        0x05 => write!(w, "RLC  L "),
        0x06 => write!(w, "RLC  (HL) "),
        0x07 => write!(w, "RLC  A "),
        0x08 => write!(w, "RRC  B "),
        0x09 => write!(w, "RRC  C "),
        0x0a => write!(w, "RRC  D "),
        0x0b => write!(w, "RRC  E "),
        0x0c => write!(w, "RRC  H "),
        0x0d => write!(w, "RRC  L "),
        0x0e => write!(w, "RRC  (HL) "),
        0x0f => write!(w, "RRC  A "),
        0x10 => write!(w, "RL   B "),
        0x11 => write!(w, "RL   C "),
        0x12 => write!(w, "RL   D "),
        0x13 => write!(w, "RL   E "),
        0x14 => write!(w, "RL   H "),
        0x15 => write!(w, "RL   L "),
        0x16 => write!(w, "RL   (HL) "),
        0x17 => write!(w, "RL   A "),
        0x18 => write!(w, "RR   B "),
        0x19 => write!(w, "RR   C "),
        0x1a => write!(w, "RR   D "),
        0x1b => write!(w, "RR   E "),
        0x1c => write!(w, "RR   H "),
        0x1d => write!(w, "RR   L "),
        0x1e => write!(w, "RR   (HL) "),
        0x1f => write!(w, "RR   A "),
        0x20 => write!(w, "SLA  B "),
        0x21 => write!(w, "SLA  C "),
        0x22 => write!(w, "SLA  D "),
        0x23 => write!(w, "SLA  E "),
        0x24 => write!(w, "SLA  H "),
        0x25 => write!(w, "SLA  L "),
        0x26 => write!(w, "SLA  (HL) "),
        0x27 => write!(w, "SLA  A "),
        0x28 => write!(w, "SRA  B "),
        0x29 => write!(w, "SRA  C "),
        0x2a => write!(w, "SRA  D "),
        0x2b => write!(w, "SRA  E "),
        0x2c => write!(w, "SRA  H "),
        0x2d => write!(w, "SRA  L "),
        0x2e => write!(w, "SRA  (HL) "),
        0x2f => write!(w, "SRA  A "),
        0x30 => write!(w, "SWAP B "),
        0x31 => write!(w, "SWAP C "),
        0x32 => write!(w, "SWAP D "),
        0x33 => write!(w, "SWAP E "),
        0x34 => write!(w, "SWAP H "),
        0x35 => write!(w, "SWAP L "),
        0x36 => write!(w, "SWAP (HL) "),
        0x37 => write!(w, "SWAP A "),
        0x38 => write!(w, "SRL  B "),
        0x39 => write!(w, "SRL  C "),
        0x3a => write!(w, "SRL  D "),
        0x3b => write!(w, "SRL  E "),
        0x3c => write!(w, "SRL  H "),
        0x3d => write!(w, "SRL  L "),
        0x3e => write!(w, "SRL  (HL) "),
        0x3f => write!(w, "SRL  A "),
        0x40 => write!(w, "BIT  0,B "),
        0x41 => write!(w, "BIT  0,C "),
        0x42 => write!(w, "BIT  0,D "),
        0x43 => write!(w, "BIT  0,E "),
        0x44 => write!(w, "BIT  0,H "),
        0x45 => write!(w, "BIT  0,L "),
        0x46 => write!(w, "BIT  0,(HL) "),
        0x47 => write!(w, "BIT  0,A "),
        0x48 => write!(w, "BIT  1,B "),
        0x49 => write!(w, "BIT  1,C "),
        0x4a => write!(w, "BIT  1,D "),
        0x4b => write!(w, "BIT  1,E "),
        0x4c => write!(w, "BIT  1,H "),
        0x4d => write!(w, "BIT  1,L "),
        0x4e => write!(w, "BIT  1,(HL) "),
        0x4f => write!(w, "BIT  1,A "),
        0x50 => write!(w, "BIT  2,B "),
        0x51 => write!(w, "BIT  2,C "),
        0x52 => write!(w, "BIT  2,D "),
        0x53 => write!(w, "BIT  2,E "),
        0x54 => write!(w, "BIT  2,H "),
        0x55 => write!(w, "BIT  2,L "),
        0x56 => write!(w, "BIT  2,(HL) "),
        0x57 => write!(w, "BIT  2,A "),
        0x58 => write!(w, "BIT  3,B "),
        0x59 => write!(w, "BIT  3,C "),
        0x5a => write!(w, "BIT  3,D "),
        0x5b => write!(w, "BIT  3,E "),
        0x5c => write!(w, "BIT  3,H "),
        0x5d => write!(w, "BIT  3,L "),
        0x5e => write!(w, "BIT  3,(HL) "),
        0x5f => write!(w, "BIT  3,A "),
        0x60 => write!(w, "BIT  4,B "),
        0x61 => write!(w, "BIT  4,C "),
        0x62 => write!(w, "BIT  4,D "),
        0x63 => write!(w, "BIT  4,E "),
        0x64 => write!(w, "BIT  4,H "),
        0x65 => write!(w, "BIT  4,L "),
        0x66 => write!(w, "BIT  4,(HL) "),
        0x67 => write!(w, "BIT  4,A "),
        0x68 => write!(w, "BIT  5,B "),
        0x69 => write!(w, "BIT  5,C "),
        0x6a => write!(w, "BIT  5,D "),
        0x6b => write!(w, "BIT  5,E "),
        0x6c => write!(w, "BIT  5,H "),
        0x6d => write!(w, "BIT  5,L "),
        0x6e => write!(w, "BIT  5,(HL) "),
        0x6f => write!(w, "BIT  5,A "),
        0x70 => write!(w, "BIT  6,B "),
        0x71 => write!(w, "BIT  6,C "),
        0x72 => write!(w, "BIT  6,D "),
        0x73 => write!(w, "BIT  6,E "),
        0x74 => write!(w, "BIT  6,H "),
        0x75 => write!(w, "BIT  6,L "),
        0x76 => write!(w, "BIT  6,(HL) "),
        0x77 => write!(w, "BIT  6,A "),
        0x78 => write!(w, "BIT  7,B "),
        0x79 => write!(w, "BIT  7,C "),
        0x7a => write!(w, "BIT  7,D "),
        0x7b => write!(w, "BIT  7,E "),
        0x7c => write!(w, "BIT  7,H "),
        0x7d => write!(w, "BIT  7,L "),
        0x7e => write!(w, "BIT  7,(HL) "),
        0x7f => write!(w, "BIT  7,A "),
        0x80 => write!(w, "RES  0,B "),
        0x81 => write!(w, "RES  0,C "),
        0x82 => write!(w, "RES  0,D "),
        0x83 => write!(w, "RES  0,E "),
        0x84 => write!(w, "RES  0,H "),
        0x85 => write!(w, "RES  0,L "),
        0x86 => write!(w, "RES  0,(HL) "),
        0x87 => write!(w, "RES  0,A "),
        0x88 => write!(w, "RES  1,B "),
        0x89 => write!(w, "RES  1,C "),
        0x8a => write!(w, "RES  1,D "),
        0x8b => write!(w, "RES  1,E "),
        0x8c => write!(w, "RES  1,H "),
        0x8d => write!(w, "RES  1,L "),
        0x8e => write!(w, "RES  1,(HL) "),
        0x8f => write!(w, "RES  1,A "),
        0x90 => write!(w, "RES  2,B "),
        0x91 => write!(w, "RES  2,C "),
        0x92 => write!(w, "RES  2,D "),
        0x93 => write!(w, "RES  2,E "),
        0x94 => write!(w, "RES  2,H "),
        0x95 => write!(w, "RES  2,L "),
        0x96 => write!(w, "RES  2,(HL) "),
        0x97 => write!(w, "RES  2,A "),
        0x98 => write!(w, "RES  3,B "),
        0x99 => write!(w, "RES  3,C "),
        0x9a => write!(w, "RES  3,D "),
        0x9b => write!(w, "RES  3,E "),
        0x9c => write!(w, "RES  3,H "),
        0x9d => write!(w, "RES  3,L "),
        0x9e => write!(w, "RES  3,(HL) "),
        0x9f => write!(w, "RES  3,A "),
        0xa0 => write!(w, "RES  4,B "),
        0xa1 => write!(w, "RES  4,C "),
        0xa2 => write!(w, "RES  4,D "),
        0xa3 => write!(w, "RES  4,E "),
        0xa4 => write!(w, "RES  4,H "),
        0xa5 => write!(w, "RES  4,L "),
        0xa6 => write!(w, "RES  4,(HL) "),
        0xa7 => write!(w, "RES  4,A "),
        0xa8 => write!(w, "RES  5,B "),
        0xa9 => write!(w, "RES  5,C "),
        0xaa => write!(w, "RES  5,D "),
        0xab => write!(w, "RES  5,E "),
        0xac => write!(w, "RES  5,H "),
        0xad => write!(w, "RES  5,L "),
        0xae => write!(w, "RES  5,(HL) "),
        0xaf => write!(w, "RES  5,A "),
        0xb0 => write!(w, "RES  6,B "),
        0xb1 => write!(w, "RES  6,C "),
        0xb2 => write!(w, "RES  6,D "),
        0xb3 => write!(w, "RES  6,E "),
        0xb4 => write!(w, "RES  6,H "),
        0xb5 => write!(w, "RES  6,L "),
        0xb6 => write!(w, "RES  6,(HL) "),
        0xb7 => write!(w, "RES  6,A "),
        0xb8 => write!(w, "RES  7,B "),
        0xb9 => write!(w, "RES  7,C "),
        0xba => write!(w, "RES  7,D "),
        0xbb => write!(w, "RES  7,E "),
        0xbc => write!(w, "RES  7,H "),
        0xbd => write!(w, "RES  7,L "),
        0xbe => write!(w, "RES  7,(HL) "),
        0xbf => write!(w, "RES  7,A "),
        0xc0 => write!(w, "SET  0,B "),
        0xc1 => write!(w, "SET  0,C "),
        0xc2 => write!(w, "SET  0,D "),
        0xc3 => write!(w, "SET  0,E "),
        0xc4 => write!(w, "SET  0,H "),
        0xc5 => write!(w, "SET  0,L "),
        0xc6 => write!(w, "SET  0,(HL) "),
        0xc7 => write!(w, "SET  0,A "),
        0xc8 => write!(w, "SET  1,B "),
        0xc9 => write!(w, "SET  1,C "),
        0xca => write!(w, "SET  1,D "),
        0xcb => write!(w, "SET  1,E "),
        0xcc => write!(w, "SET  1,H "),
        0xcd => write!(w, "SET  1,L "),
        0xce => write!(w, "SET  1,(HL) "),
        0xcf => write!(w, "SET  1,A "),
        0xd0 => write!(w, "SET  2,B "),
        0xd1 => write!(w, "SET  2,C "),
        0xd2 => write!(w, "SET  2,D "),
        0xd3 => write!(w, "SET  2,E "),
        0xd4 => write!(w, "SET  2,H "),
        0xd5 => write!(w, "SET  2,L "),
        0xd6 => write!(w, "SET  2,(HL) "),
        0xd7 => write!(w, "SET  2,A "),
        0xd8 => write!(w, "SET  3,B "),
        0xd9 => write!(w, "SET  3,C "),
        0xda => write!(w, "SET  3,D "),
        0xdb => write!(w, "SET  3,E "),
        0xdc => write!(w, "SET  3,H "),
        0xdd => write!(w, "SET  3,L "),
        0xde => write!(w, "SET  3,(HL) "),
        0xdf => write!(w, "SET  3,A "),
        0xe0 => write!(w, "SET  4,B "),
        0xe1 => write!(w, "SET  4,C "),
        0xe2 => write!(w, "SET  4,D "),
        0xe3 => write!(w, "SET  4,E "),
        0xe4 => write!(w, "SET  4,H "),
        0xe5 => write!(w, "SET  4,L "),
        0xe6 => write!(w, "SET  4,(HL) "),
        0xe7 => write!(w, "SET  4,A "),
        0xe8 => write!(w, "SET  5,B "),
        0xe9 => write!(w, "SET  5,C "),
        0xea => write!(w, "SET  5,D "),
        0xeb => write!(w, "SET  5,E "),
        0xec => write!(w, "SET  5,H "),
        0xed => write!(w, "SET  5,L "),
        0xee => write!(w, "SET  5,(HL) "),
        0xef => write!(w, "SET  5,A "),
        0xf0 => write!(w, "SET  6,B "),
        0xf1 => write!(w, "SET  6,C "),
        0xf2 => write!(w, "SET  6,D "),
        0xf3 => write!(w, "SET  6,E "),
        0xf4 => write!(w, "SET  6,H "),
        0xf5 => write!(w, "SET  6,L "),
        0xf6 => write!(w, "SET  6,(HL) "),
        0xf7 => write!(w, "SET  6,A "),
        0xf8 => write!(w, "SET  7,B "),
        0xf9 => write!(w, "SET  7,C "),
        0xfa => write!(w, "SET  7,D "),
        0xfb => write!(w, "SET  7,E "),
        0xfc => write!(w, "SET  7,H "),
        0xfd => write!(w, "SET  7,L "),
        0xfe => write!(w, "SET  7,(HL) "),
        0xff => write!(w, "SET  7,A "),
    }
}
