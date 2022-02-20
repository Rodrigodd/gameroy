use std::fmt;

use crate::save_state::{LoadStateError, SaveState};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CpuState {
    Running = 0,
    Halt = 1,
    Stopped = 2,
}
impl SaveState for CpuState {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        (*self as u8).save_state(data)
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        let mut value = 0u8;
        value.load_state(data)?;
        *self = match value {
            0 => Self::Running,
            1 => Self::Halt,
            2 => Self::Stopped,
            x => return Err(LoadStateError::InvalidState(x)),
        };
        Ok(())
    }
}
impl Default for CpuState {
    fn default() -> Self {
        Self::Running
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ImeState {
    /// Interrupts are disable
    Disabled = 0,
    /// Interrupts are enable
    Enabled = 1,
    /// Interrupts will be enable after the next instruction
    ToBeEnable = 2,
}
impl SaveState for ImeState {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        (*self as u8).save_state(data)
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        let mut value = 0u8;
        value.load_state(data)?;
        *self = match value {
            0 => Self::Disabled,
            1 => Self::Enabled,
            2 => Self::ToBeEnable,
            x => return Err(LoadStateError::InvalidImeState(x)),
        };
        Ok(())
    }
}
impl Default for ImeState {
    fn default() -> Self {
        Self::Disabled
    }
}

#[repr(C)]
#[derive(Default, Debug, PartialEq, Eq)]
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
    pub ime: ImeState,
    pub state: CpuState,
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
impl SaveState for Cpu {
    fn save_state(&self, data: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.a.save_state(data)?;
        self.f.0.save_state(data)?;
        self.b.save_state(data)?;
        self.c.save_state(data)?;
        self.d.save_state(data)?;
        self.e.save_state(data)?;
        self.h.save_state(data)?;
        self.l.save_state(data)?;
        self.sp.save_state(data)?;
        self.pc.save_state(data)?;
        self.ime.save_state(data)?;
        self.state.save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl std::io::Read) -> Result<(), LoadStateError> {
        self.a.load_state(data)?;
        self.f.0.load_state(data)?;
        self.b.load_state(data)?;
        self.c.load_state(data)?;
        self.d.load_state(data)?;
        self.e.load_state(data)?;
        self.h.load_state(data)?;
        self.l.load_state(data)?;
        self.sp.load_state(data)?;
        self.pc.load_state(data)?;
        self.ime.load_state(data)?;
        self.state.load_state(data)?;
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

#[derive(Default, Debug, PartialEq, Eq)]
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
