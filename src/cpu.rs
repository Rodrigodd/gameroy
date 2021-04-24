use std::fmt;

pub enum State {
    Running,
    Halt,
    Stopped,
}

impl Default for State {
    fn default() -> Self {
        Self::Running
    }
}

#[repr(C)]
pub struct Cpu {
    pub a: u8,
    pub f: Flags,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
    pub ime: bool,
    pub state: State,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            a: 0x01,
            f: Flags(0xB0),
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xd8,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x100,
            ime: false,
            state: State::Running,
        }
    }
}
impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "A {:02x} {:02x} F", self.a, self.f.0)?;
        writeln!(f, "B {:02x} {:02x} C", self.b, self.c)?;
        writeln!(f, "D {:02x} {:02x} E", self.d, self.e)?;
        writeln!(f, "H {:02x} {:02x} L", self.h, self.l)?;
        writeln!(f, "SP {:04x}", self.sp)?;
        writeln!(f, "PC {:04x}", self.pc)?;
        Ok(())
    }
}
impl Cpu {
    pub fn af(&self) -> u16 {
        u16::from_be_bytes([self.a, self.f.0])
    }
    pub fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b, self.c])
    }
    pub fn de(&self) -> u16 {
        u16::from_be_bytes([self.d, self.e])
    }
    pub fn hl(&self) -> u16 {
        u16::from_be_bytes([self.h, self.l])
    }

    pub fn set_af(&mut self, value: u16) {
        let [a, f] = value.to_be_bytes();
        self.a = a;
        self.f.0 = f & 0xf0;
    }
    pub fn set_bc(&mut self, value: u16) {
        let [b, c] = value.to_be_bytes();
        self.b = b;
        self.c = c;
    }
    pub fn set_de(&mut self, value: u16) {
        let [d, e] = value.to_be_bytes();
        self.d = d;
        self.e = e;
    }
    pub fn set_hl(&mut self, value: u16) {
        let [h, l] = value.to_be_bytes();
        self.h = h;
        self.l = l;
    }
}

#[derive(Default)]
pub struct Flags(pub u8);
impl Flags {
    pub fn z(&self) -> bool {
        (self.0 >> 7) & 1 != 0
    }
    pub fn def_z(&mut self, v: bool) {
        self.0 = (self.0 & !(1 << 7)) | (v as u8) << 7;
    }
    pub fn set_z(&mut self) {
        self.0 |= 1 << 7;
    }
    pub fn clr_z(&mut self) {
        self.0 &= !(1 << 7);
    }

    pub fn n(&self) -> bool {
        (self.0 >> 6) & 1 != 0
    }
    pub fn def_n(&mut self, v: bool) {
        self.0 = (self.0 & !(1 << 6)) | (v as u8) << 6;
    }
    pub fn set_n(&mut self) {
        self.0 |= 1 << 6;
    }
    pub fn clr_n(&mut self) {
        self.0 &= !(1 << 6);
    }

    pub fn h(&self) -> bool {
        (self.0 >> 5) & 1 != 0
    }
    pub fn def_h(&mut self, v: bool) {
        self.0 = (self.0 & !(1 << 5)) | (v as u8) << 5;
    }
    pub fn set_h(&mut self) {
        self.0 |= 1 << 5;
    }
    pub fn clr_h(&mut self) {
        self.0 &= !(1 << 5);
    }

    pub fn c(&self) -> bool {
        (self.0 >> 4) & 1 != 0
    }
    pub fn def_c(&mut self, v: bool) {
        self.0 = (self.0 & !(1 << 4)) | (v as u8) << 4;
    }
    pub fn set_c(&mut self) {
        self.0 |= 1 << 4;
    }
    pub fn clr_c(&mut self) {
        self.0 &= !(1 << 4);
    }
}
