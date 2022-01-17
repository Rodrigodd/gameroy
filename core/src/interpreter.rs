use crate::{
    consts,
    cpu::{CpuState, ImeState},
    gameboy::GameBoy,
};

#[derive(PartialEq, Eq)]
enum Condition {
    None,
    Z,
    NZ,
    C,
    NC,
}

#[derive(Clone, Copy)]
enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Im8,
    Im16,
    BC,
    DE,
    HL,
    SP,
    HLI,
    HLD,
}

enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    Im16,
}

fn n16(op: &[u8]) -> u16 {
    u16::from_le_bytes([op[1], op[2]])
}

fn sub16(a: u16, b: u16) -> u16 {
    a.wrapping_sub(b)
}

fn add16(a: u16, b: u16) -> u16 {
    a.wrapping_add(b)
}

fn sub(a: u8, b: u8) -> u8 {
    a.wrapping_sub(b)
}

fn add(a: u8, b: u8) -> u8 {
    a.wrapping_add(b)
}

/// A interpreter
pub struct Interpreter(pub GameBoy);
impl Interpreter {
    /// Interactive debug.
    /// Print the instruction around the current running instruction
    /// and the current state of the CPU.
    /// Wait for command in the stdin.
    pub fn debug(&mut self) {
        let mut input = String::new();
        input.clear();
        use std::io::Write;
        let pc = self.0.cpu.pc;
        let mut string = String::new();
        let std = std::io::stdout();
        let mut std = std.lock();
        self.0
            .trace
            .borrow_mut()
            .print_around(self.0.cartridge.curr_bank(), pc, &self.0, &mut string)
            .unwrap();
        writeln!(std, "{}", string).unwrap();
        writeln!(std, "{}", self.0.cpu).unwrap();
        writeln!(std, "clock: {}", self.0.clock_count).unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.split_ascii_whitespace().collect::<Vec<_>>();
        dbg!(&input);
        if input.is_empty() {
            self.interpret_op();
            return;
        }
        match input[0] {
            "runto" => {
                let pc = match u16::from_str_radix(input[1], 16) {
                    Ok(x) => x,
                    Err(_) => return,
                };
                while self.0.cpu.pc != pc {
                    self.interpret_op();
                }
            }
            "run" => {
                let clocks = match u64::from_str_radix(input[1], 10) {
                    Ok(x) => x,
                    Err(_) => return,
                };
                let target = self.0.clock_count + clocks;
                while self.0.clock_count < target {
                    self.interpret_op();
                }
            }
            _ => writeln!(std, "unkown command").unwrap(),
        }
    }

    /// Set the value of the cpu PC, but also update the dissasembly tracing
    fn jump_to(&mut self, address: u16) {
        let pc = self.0.cpu.pc;
        self.0.cpu.pc = address;
        let bank = self.0.cartridge.curr_bank();
        let mut trace = self.0.trace.borrow_mut();

        // check early if this address is already traced, and return if it is
        if address <= 0x3FFF && trace.is_already_traced(bank, address) {
            return;
        }

        if pc <= 0x3FFF && address > 0x3FFF {
            // if it is entring the ram, clear its trace, because the ram could have changed
            trace.clear_ram_trace();
        }

        trace.trace_starting_at(
            &self.0,
            bank,
            address,
            Some(format!("L{:02x}_{:04x}", bank, address)),
        );
    }

    fn jump(&mut self, c: Condition, address: u16) {
        // JP cc, nn
        use Condition::*;
        let c = match c {
            None => true,
            Z => self.0.cpu.f.z(),
            NZ => !self.0.cpu.f.z(),
            C => self.0.cpu.f.c(),
            NC => !self.0.cpu.f.c(),
        };
        if c {
            self.jump_to(address);
            let cycles = 16;
            self.0.tick(cycles);
        } else {
            self.jump_to(add16(self.0.cpu.pc, 3));
            let cycles = 12;
            self.0.tick(cycles);
        }
    }

    fn jump_rel(&mut self, c: Condition, address: i8) {
        // JP cc, nn
        use Condition::*;
        let c = match c {
            None => true,
            Z => self.0.cpu.f.z(),
            NZ => !self.0.cpu.f.z(),
            C => self.0.cpu.f.c(),
            NC => !self.0.cpu.f.c(),
        };
        self.jump_to(add16(self.0.cpu.pc, 2));
        if c {
            let pc = (self.0.cpu.pc as i16 + address as i16) as u16;
            self.jump_to(pc);
            let cycles = 12;
            self.0.tick(cycles);
        } else {
            let cycles = 8;
            self.0.tick(cycles);
        }
    }

    fn pushr(&mut self, value: u16) {
        let [lsp, msp] = value.to_le_bytes();
        self.0.write(sub16(self.0.cpu.sp, 1), msp);
        self.0.write(sub16(self.0.cpu.sp, 2), lsp);
        self.0.cpu.sp = sub16(self.0.cpu.sp, 2);
    }

    fn popr(&mut self) -> u16 {
        self.0.cpu.sp = add16(self.0.cpu.sp, 2);
        let lsp = self.0.read(sub16(self.0.cpu.sp, 2));
        let msp = self.0.read(sub16(self.0.cpu.sp, 1));
        u16::from_be_bytes([msp, lsp])
    }

    fn push(&mut self, reg: Reg16) {
        let r = match reg {
            Reg16::AF => self.0.cpu.af(),
            Reg16::BC => self.0.cpu.bc(),
            Reg16::DE => self.0.cpu.de(),
            Reg16::HL => self.0.cpu.hl(),
            _ => unreachable!(),
        };
        self.pushr(r);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn pop(&mut self, reg: Reg16) {
        let r = self.popr();
        match reg {
            Reg16::AF => self.0.cpu.set_af(r),
            Reg16::BC => self.0.cpu.set_bc(r),
            Reg16::DE => self.0.cpu.set_de(r),
            Reg16::HL => self.0.cpu.set_hl(r),
            _ => unreachable!(),
        }
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn call(&mut self, c: Condition, address: u16) {
        // JP cc, nn
        use Condition::*;
        let c = match c {
            None => true,
            Z => self.0.cpu.f.z(),
            NZ => !self.0.cpu.f.z(),
            C => self.0.cpu.f.c(),
            NC => !self.0.cpu.f.c(),
        };
        self.0.cpu.pc += 3;
        if c {
            self.pushr(self.0.cpu.pc);
            self.jump_to(address);
            let cycles = 24;
            self.0.tick(cycles);
        } else {
            let cycles = 12;
            self.0.tick(cycles);
        }
    }

    fn ret(&mut self, cond: Condition) {
        // JP cc, nn
        use Condition::*;
        let c = match cond {
            None => true,
            Z => self.0.cpu.f.z(),
            NZ => !self.0.cpu.f.z(),
            C => self.0.cpu.f.c(),
            NC => !self.0.cpu.f.c(),
        };
        self.0.cpu.pc = add16(self.0.cpu.pc, 1);
        if c {
            let address = self.popr();
            self.jump_to(address);
            let cycles = if cond == None { 16 } else { 20 };
            self.0.tick(cycles);
        } else {
            let cycles = 8;
            self.0.tick(cycles);
        }
    }

    fn read(&mut self, reg: Reg) -> u8 {
        match reg {
            Reg::A => self.0.cpu.a,
            Reg::B => self.0.cpu.b,
            Reg::C => self.0.cpu.c,
            Reg::D => self.0.cpu.d,
            Reg::E => self.0.cpu.e,
            Reg::H => self.0.cpu.h,
            Reg::L => self.0.cpu.l,
            Reg::Im8 => self.0.read(add16(self.0.cpu.pc, 1)),
            Reg::Im16 => {
                let adress = self.0.read16(add16(self.0.cpu.pc, 1));
                self.0.read(adress)
            }
            Reg::BC => self.0.read(self.0.cpu.bc()),
            Reg::DE => self.0.read(self.0.cpu.de()),
            Reg::HL => self.0.read(self.0.cpu.hl()),
            Reg::HLI => {
                let v = self.0.read(self.0.cpu.hl());
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
                v
            }
            Reg::HLD => {
                let v = self.0.read(self.0.cpu.hl());
                self.0.cpu.set_hl(sub16(self.0.cpu.hl(), 1));
                v
            }
            Reg::SP => unreachable!(),
        }
    }

    fn write(&mut self, reg: Reg, value: u8) {
        match reg {
            Reg::A => self.0.cpu.a = value,
            Reg::B => self.0.cpu.b = value,
            Reg::C => self.0.cpu.c = value,
            Reg::D => self.0.cpu.d = value,
            Reg::E => self.0.cpu.e = value,
            Reg::H => self.0.cpu.h = value,
            Reg::L => self.0.cpu.l = value,
            Reg::Im8 => self.0.write(add16(self.0.cpu.pc, 1), value),
            Reg::Im16 => {
                let adress = self.0.read16(add16(self.0.cpu.pc, 1));
                self.0.write(adress, value)
            }
            Reg::BC => self.0.write(self.0.cpu.bc(), value),
            Reg::DE => self.0.write(self.0.cpu.de(), value),
            Reg::HL => self.0.write(self.0.cpu.hl(), value),
            Reg::HLI => {
                self.0.write(self.0.cpu.hl(), value);
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
            }
            Reg::HLD => {
                self.0.write(self.0.cpu.hl(), value);
                self.0.cpu.set_hl(sub16(self.0.cpu.hl(), 1));
            }
            Reg::SP => unreachable!(),
        }
    }

    fn load(&mut self, dst: Reg, src: Reg) {
        let v = self.read(src);
        self.write(dst, v);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn loadh(&mut self, dst: Reg, src: Reg) {
        let src = match src {
            Reg::A => self.0.cpu.a,
            Reg::C => self.0.read(0xFF00 | self.0.cpu.c as u16),
            Reg::Im8 => {
                let r8 = self.0.read(add16(self.0.cpu.pc, 1));
                self.0.read(0xFF00 | r8 as u16)
            }
            _ => unreachable!(),
        };

        match dst {
            Reg::A => self.0.cpu.a = src,
            Reg::C => self.0.write(0xFF00 | self.0.cpu.c as u16, src),
            Reg::Im8 => {
                let r8 = self.0.read(add16(self.0.cpu.pc, 1));
                self.0.write(0xFF00 | r8 as u16, src);
            }
            _ => unreachable!(),
        }
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn load16(&mut self, dst: Reg16, src: Reg16) {
        let src = match src {
            Reg16::BC => self.0.cpu.bc(),
            Reg16::DE => self.0.cpu.de(),
            Reg16::HL => self.0.cpu.hl(),
            Reg16::SP => self.0.cpu.sp,
            Reg16::Im16 => self.0.read16(self.0.cpu.pc + 1),
            _ => unreachable!(),
        };
        match dst {
            Reg16::BC => self.0.cpu.set_bc(src),
            Reg16::DE => self.0.cpu.set_de(src),
            Reg16::HL => self.0.cpu.set_hl(src),
            Reg16::SP => self.0.cpu.sp = src,
            Reg16::Im16 => {
                let adress = self.0.read16(self.0.cpu.pc + 1);
                self.0.write16(adress, src)
            }
            _ => unreachable!(),
        }
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn inc(&mut self, reg: Reg) {
        let reg = match reg {
            Reg::A => &mut self.0.cpu.a,
            Reg::B => &mut self.0.cpu.b,
            Reg::C => &mut self.0.cpu.c,
            Reg::D => &mut self.0.cpu.d,
            Reg::E => &mut self.0.cpu.e,
            Reg::H => &mut self.0.cpu.h,
            Reg::L => &mut self.0.cpu.l,
            Reg::BC => {
                self.0.cpu.set_bc(add16(self.0.cpu.bc(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::DE => {
                self.0.cpu.set_de(add16(self.0.cpu.de(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::HL => {
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::SP => {
                self.0.cpu.sp = add16(self.0.cpu.sp, 1);
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            _ => unreachable!(),
        };
        *reg = add(*reg, 1);
        self.0.cpu.f.def_z(*reg == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(*reg & 0x0f == 0x0);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn dec(&mut self, reg: Reg) {
        let reg = match reg {
            Reg::A => &mut self.0.cpu.a,
            Reg::B => &mut self.0.cpu.b,
            Reg::C => &mut self.0.cpu.c,
            Reg::D => &mut self.0.cpu.d,
            Reg::E => &mut self.0.cpu.e,
            Reg::H => &mut self.0.cpu.h,
            Reg::L => &mut self.0.cpu.l,
            Reg::BC => {
                self.0.cpu.set_bc(sub16(self.0.cpu.bc(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::DE => {
                self.0.cpu.set_de(sub16(self.0.cpu.de(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::HL => {
                self.0.cpu.set_hl(sub16(self.0.cpu.hl(), 1));
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            Reg::SP => {
                self.0.cpu.sp = sub16(self.0.cpu.sp, 1);
                let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
                self.0.tick(cycles);
                self.0.cpu.pc = add16(
                    self.0.cpu.pc,
                    consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
                );
                return;
            }
            _ => unreachable!(),
        };
        *reg = sub(*reg, 1);
        self.0.cpu.f.def_z(*reg == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h(*reg & 0x0f == 0xf);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn add(&mut self, reg: Reg) {
        let v = self.read(reg);
        let (r, o) = self.0.cpu.a.overflowing_add(v);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_c(o);
        self.0.cpu.f.def_h((self.0.cpu.a & 0xF) + (v & 0xF) > 0xF);
        self.0.cpu.a = r;
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn adc(&mut self, reg: Reg) {
        let a = self.0.cpu.a as u16;
        let c = self.0.cpu.f.c() as u16;
        let v = self.read(reg) as u16;
        let r = a + v + c;
        self.0.cpu.f.def_z(r & 0xFF == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h((a & 0xF) + (v & 0xF) + c > 0xF);
        self.0.cpu.f.def_c(r > 0xff);
        self.0.cpu.a = (r & 0xff) as u8;
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn sub(&mut self, reg: Reg) {
        let v = self.read(reg);
        let (r, o) = self.0.cpu.a.overflowing_sub(v);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h((self.0.cpu.a & 0xF) < (v & 0xF));
        self.0.cpu.f.def_c(o);
        self.0.cpu.a = r;
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn sbc(&mut self, reg: Reg) {
        let a = self.0.cpu.a as i16;
        let c = self.0.cpu.f.c() as i16;
        let v = self.read(reg) as i16;
        let r = a - v - c;
        self.0.cpu.f.def_z(r & 0xFF == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h((a & 0xF) < (v & 0xF) + c);
        self.0.cpu.f.def_c(r < 0x0);
        self.0.cpu.a = (r & 0xff) as u8;
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn and(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a = self.0.cpu.a & v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_c();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.set_h();
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn or(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a = self.0.cpu.a | v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_c();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn xor(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a = self.0.cpu.a ^ v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_c();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn cp(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.f.def_z(self.0.cpu.a == v);
        self.0.cpu.f.def_c(self.0.cpu.a < v);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h(self.0.cpu.a & 0xF < v & 0xF);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn add16(&mut self, b: Reg16) {
        let b = match b {
            Reg16::BC => self.0.cpu.bc(),
            Reg16::DE => self.0.cpu.de(),
            Reg16::HL => self.0.cpu.hl(),
            Reg16::SP => self.0.cpu.sp,
            Reg16::Im16 | Reg16::AF => unreachable!(),
        };
        let (r, o) = self.0.cpu.hl().overflowing_add(b);
        self.0.cpu.f.clr_n();
        self.0
            .cpu
            .f
            .def_h((self.0.cpu.hl() & 0x0FFF) + (b & 0x0FFF) > 0xFFF);
        self.0.cpu.f.def_c(o);
        self.0.cpu.set_hl(r);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
    }

    fn rst(&mut self, address: u8) {
        self.0.cpu.pc = add16(
            self.0.cpu.pc,
            consts::LEN[self.0.read(self.0.cpu.pc) as usize] as u16,
        );
        self.pushr(self.0.cpu.pc);
        let cycles = consts::CLOCK[self.0.read(self.0.cpu.pc) as usize];
        self.0.tick(cycles);
        self.jump_to(address as u16);
    }

    fn bit(&mut self, bit: u8, reg: Reg) {
        let r = self.read(reg);
        self.0.cpu.f.def_z((r & (1 << bit)) == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.set_h();
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn set(&mut self, bit: u8, reg: Reg) {
        let mut r = self.read(reg);
        r = r | (1 << bit);
        self.write(reg, r);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn res(&mut self, bit: u8, reg: Reg) {
        let mut r = self.read(reg);
        r = r & !(1 << bit);
        self.write(reg, r);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn rlc(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = r.rotate_left(1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(r & 0x1 != 0);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn rl(&mut self, reg: Reg) {
        let c = self.0.cpu.f.c() as u8;
        let mut r = self.read(reg);
        self.0.cpu.f.def_c(r & 0x80 != 0);
        r = r << 1 | c;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn rrc(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = r.rotate_right(1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(r & 0x80 != 0);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn rr(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = self.0.cpu.f.c() as u8;
        self.0.cpu.f.def_c(r & 0x01 != 0);
        r = r >> 1 | c << 7;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn sla(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = r & 0x80 != 0;
        r = r << 1;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(c);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn sra(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        self.0.cpu.f.def_c(r & 0x01 != 0);
        r = (r & 0x80) | (r >> 1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn swap(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = ((r & 0x0F) << 4) | ((r & 0xF0) >> 4);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.clr_c();
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    fn srl(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = r & 0x01 != 0;
        r = r >> 1;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(c);
        self.0.cpu.pc += 2;
        let cycles = if let Reg::HL = reg { 16 } else { 8 };
        self.0.tick(cycles);
    }

    pub fn will_read_from(&self) -> (u8, [u16; 2]) {
        let op = self.0.read(self.0.cpu.pc);
        let none = (0, [0, 0]);
        let some = |x| (1, [x, 0]);
        match op {
            0x0a => some(self.0.cpu.bc()),
            0x1a => some(self.0.cpu.de()),
            0x2a | 0x3a | 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e | 0x86 | 0x8e | 0x96
            | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe => some(self.0.cpu.hl()),
            0xcb => match self.0.read(add16(self.0.cpu.pc, 1)) {
                0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x36 | 0x3e | 0x46 | 0x4e | 0x56
                | 0x5e | 0x66 | 0x6e | 0x76 | 0x7e | 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae
                | 0xb6 | 0xbe | 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe => {
                    some(self.0.cpu.hl())
                }
                _ => none,
            },
            0xf0 => {
                let r8 = self.0.read(add16(self.0.cpu.pc, 1));
                some(0xff00 | r8 as u16)
            }
            0xf2 => some(0xff00 | self.0.cpu.c as u16),
            0xfa => some(self.0.read16(add16(self.0.cpu.pc, 1))),
            0xc0 | 0xc1 | 0xc8 | 0xc9 | 0xd0 | 0xd1 | 0xd8 | 0xd9 | 0xe1 | 0xf1 => {
                // POP or RET
                (2, [add16(self.0.cpu.sp, 1), self.0.cpu.sp])
            }
            _ => none,
        }
    }

    pub fn will_write_to(&self) -> (u8, [u16; 2]) {
        let op = self.0.read(self.0.cpu.pc);
        let none = (0, [0, 0]);
        let some = |x| (1, [x, 0]);
        match op {
            0x02 => some(self.0.cpu.bc()),
            0x08 => {
                let adress = self.0.read16(self.0.cpu.pc + 1);
                (2, [adress, add16(adress, 1)])
            }
            0x12 => some(self.0.cpu.de()),
            0x22 | 0x32 | 0x34 | 0x35 | 0x36 | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x77 => {
                // LD (HL), .. or INC (HL) or etc.
                some(self.0.cpu.hl())
            }
            0xc4 | 0xc5 | 0xcd | 0xcf | 0xd4 | 0xd5 | 0xd7 | 0xd8 | 0xd9 | 0xe5 | 0xe7 | 0xef
            | 0xf5 | 0xf7 | 0xff | 0xdc => {
                // PUSH .. or CALL .. or RST
                (2, [sub16(self.0.cpu.sp, 1), sub16(self.0.cpu.sp, 2)])
            }
            0xe0 => {
                let r8 = self.0.read(add16(self.0.cpu.pc, 1));
                some(0xFF00 | r8 as u16)
            }
            0xe2 => some(0xFF00 | self.0.cpu.c as u16),
            0xea => some(self.0.read16(add16(self.0.cpu.pc, 1))),
            _ => none,
        }
    }

    pub fn will_jump_to(&self) -> Option<u16> {
        let pc = self.0.cpu.pc;
        let op = &[
            self.0.read(pc),
            self.0.read(add16(pc, 1)),
            self.0.read(add16(pc, 2)),
        ];
        let len = consts::LEN[op[0] as usize];
        match op[0] {
            0xC3 => {
                // JP $aaaa
                let dest = u16::from_le_bytes([op[1], op[2]]);
                Some(dest)
            }
            0xE9 => {
                // JP (HL)
                Some(self.0.cpu.hl())
            }
            x if x & 0b11100111 == 0b11000010 => {
                // JP cc, $aaaa
                let dest = u16::from_le_bytes([op[1], op[2]]);
                Some(dest)
            }
            0x18 => {
                // JR $rr
                let dest = ((pc + len as u16) as i16 + op[1] as i8 as i16) as u16;
                Some(dest)
            }
            x if x & 0b1110_0111 == 0b0010_0000 => {
                // JR cc, $rr
                let dest = ((pc + len as u16) as i16 + op[1] as i8 as i16) as u16;
                Some(dest)
            }
            x if x == 0xCD || x & 0b11100111 == 0b11000100 => {
                // CALL $aaaa or CALL cc, $aaaa
                let dest = u16::from_le_bytes([op[1], op[2]]);
                Some(dest)
            }
            0xC9 | 0xD9 => {
                let dest = u16::from_le_bytes([
                    self.0.read(sub16(self.0.cpu.sp, 1)),
                    self.0.read(sub16(self.0.cpu.sp, 2)),
                ]);
                Some(dest)
            }
            x if x & 0b11000111 == 0b11000111 => {
                // RST n
                let dest = (x & 0b00111000) as u16;
                Some(dest)
            }
            _ => None,
        }
    }

    pub fn interpret_op(&mut self) {
        let interrupts: u8 = self.0.memory[consts::IF] & self.0.memory[consts::IE];
        if interrupts != 0 {
            if self.0.cpu.ime == ImeState::Enabled {
                self.0.cpu.ime = ImeState::Disabled;
                self.0.memory[consts::IF] = 0;
                let address = [0x40, 0x48, 0x50, 0x58, 0x60][interrupts.trailing_zeros() as usize];
                self.pushr(self.0.cpu.pc);
                self.jump_to(address);
                self.0.tick(20);
            }
            if self.0.cpu.state == CpuState::Halt {
                self.0.tick(4);
                self.0.cpu.state = CpuState::Running;
            }
        }
        if self.0.cpu.ime == ImeState::ToBeEnable {
            self.0.cpu.ime = ImeState::Enabled;
        }

        if self.0.cpu.state != CpuState::Running {
            self.0.tick(4);
            return;
        }

        use Condition::*;
        let op = &[
            self.0.read(self.0.cpu.pc),
            self.0.read(add16(self.0.cpu.pc, 1)),
            self.0.read(add16(self.0.cpu.pc, 2)),
        ];
        let trace = false;
        if trace {
            println!(
                "{:04x}: {:02x} {:04x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
                self.0.cpu.pc,
                op[0],
                self.0.cpu.sp,
                self.0.cpu.a,
                self.0.cpu.f.0,
                self.0.cpu.b,
                self.0.cpu.c,
                self.0.cpu.d,
                self.0.cpu.e,
                self.0.cpu.h,
                self.0.cpu.l,
            );
        }
        match op[0] {
            0x00 => {
                // NOP 1:4 - - - -
            }
            0x01 => {
                // LD BC,d16 3:12 - - - -
                return self.load16(Reg16::BC, Reg16::Im16);
            }
            0x02 => {
                // LD (BC),A 1:8 - - - -
                return self.load(Reg::BC, Reg::A);
            }
            0x03 => {
                // INC BC 1:8 - - - -
                return self.inc(Reg::BC);
            }
            0x04 => {
                // INC B 1:4 Z 0 H -
                return self.inc(Reg::B);
            }
            0x05 => {
                // DEC B 1:4 Z 1 H -
                return self.dec(Reg::B);
            }
            0x06 => {
                // LD B,d8 2:8 - - - -
                return self.load(Reg::B, Reg::Im8);
            }
            0x07 => {
                // RLCA 1:4 0 0 0 C
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.clr_h();
                self.0.cpu.f.def_c(self.0.cpu.a & 0x80 != 0);
                self.0.cpu.a = self.0.cpu.a.rotate_left(1);
            }
            0x08 => {
                // LD (a16),SP 3:20 - - - -
                return self.load16(Reg16::Im16, Reg16::SP);
            }
            0x09 => {
                // ADD HL,BC 1:8 - 0 H C
                return self.add16(Reg16::BC);
            }
            0x0a => {
                // LD A,(BC) 1:8 - - - -
                return self.load(Reg::A, Reg::BC);
            }
            0x0b => {
                // DEC BC 1:8 - - - -
                return self.dec(Reg::BC);
            }
            0x0c => {
                // INC C 1:4 Z 0 H -
                return self.inc(Reg::C);
            }
            0x0d => {
                // DEC C 1:4 Z 1 H -
                return self.dec(Reg::C);
            }
            0x0e => {
                // LD C,d8 2:8 - - - -
                return self.load(Reg::C, Reg::Im8);
            }
            0x0f => {
                // RRCA 1:4 0 0 0 C
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.clr_h();
                self.0.cpu.f.def_c(self.0.cpu.a & 0x01 != 0);
                self.0.cpu.a = self.0.cpu.a.rotate_right(1);
            }
            0x10 => {
                // STOP 0 2:4 - - - -
                self.0.cpu.state = CpuState::Stopped;
            }
            0x11 => {
                // LD DE,d16 3:12 - - - -
                return self.load16(Reg16::DE, Reg16::Im16);
            }
            0x12 => {
                // LD (DE),A 1:8 - - - -
                return self.load(Reg::DE, Reg::A);
            }
            0x13 => {
                // INC DE 1:8 - - - -
                return self.inc(Reg::DE);
            }
            0x14 => {
                // INC D 1:4 Z 0 H -
                return self.inc(Reg::D);
            }
            0x15 => {
                // DEC D 1:4 Z 1 H -
                return self.dec(Reg::D);
            }
            0x16 => {
                // LD D,d8 2:8 - - - -
                return self.load(Reg::D, Reg::Im8);
            }
            0x17 => {
                // RLA 1:4 0 0 0 C
                let c = self.0.cpu.f.c() as u8;
                self.0.cpu.f.def_c(self.0.cpu.a & 0x80 != 0);
                self.0.cpu.a = self.0.cpu.a << 1 | c;
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.clr_h();
            }
            0x18 => {
                // JR r8 2:12 - - - -
                return self.jump_rel(None, op[1] as i8);
            }
            0x19 => {
                // ADD HL,DE 1:8 - 0 H C
                return self.add16(Reg16::DE);
            }
            0x1a => {
                // LD A,(DE) 1:8 - - - -
                return self.load(Reg::A, Reg::DE);
            }
            0x1b => {
                // DEC DE 1:8 - - - -
                return self.dec(Reg::DE);
            }
            0x1c => {
                // INC E 1:4 Z 0 H -
                return self.inc(Reg::E);
            }
            0x1d => {
                // DEC E 1:4 Z 1 H -
                return self.dec(Reg::E);
            }
            0x1e => {
                // LD E,d8 2:8 - - - -
                return self.load(Reg::E, Reg::Im8);
            }
            0x1f => {
                // RRA 1:4 0 0 0 C
                let c = self.0.cpu.f.c() as u8;
                self.0.cpu.f.def_c(self.0.cpu.a & 0x01 != 0);
                self.0.cpu.a = self.0.cpu.a >> 1 | c << 7;
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.clr_h();
            }
            0x20 => {
                // JR NZ,r8 2:12/8 - - - -
                return self.jump_rel(NZ, op[1] as i8);
            }
            0x21 => {
                // LD HL,d16 3:12 - - - -
                return self.load16(Reg16::HL, Reg16::Im16);
            }
            0x22 => {
                // LD (HL+),A 1:8 - - - -
                return self.load(Reg::HLI, Reg::A);
            }
            0x23 => {
                // INC HL 1:8 - - - -
                return self.inc(Reg::HL);
            }
            0x24 => {
                // INC H 1:4 Z 0 H -
                return self.inc(Reg::H);
            }
            0x25 => {
                // DEC H 1:4 Z 1 H -
                return self.dec(Reg::H);
            }
            0x26 => {
                // LD H,d8 2:8 - - - -
                return self.load(Reg::H, Reg::Im8);
            }
            0x27 => {
                // DAA 1:4 Z - 0 C
                if !self.0.cpu.f.n() {
                    if self.0.cpu.f.c() || self.0.cpu.a > 0x99 {
                        self.0.cpu.a = add(self.0.cpu.a, 0x60);
                        self.0.cpu.f.set_c();
                    }
                    if self.0.cpu.f.h() || (self.0.cpu.a & 0x0F) > 0x09 {
                        self.0.cpu.a = add(self.0.cpu.a, 0x6);
                    }
                } else {
                    if self.0.cpu.f.c() {
                        self.0.cpu.a = sub(self.0.cpu.a, 0x60);
                    }
                    if self.0.cpu.f.h() {
                        self.0.cpu.a = sub(self.0.cpu.a, 0x6);
                    }
                }
                self.0.cpu.f.def_z(self.0.cpu.a == 0);
                self.0.cpu.f.clr_h();
            }
            0x28 => {
                // JR Z,r8 2:12/8 - - - -
                return self.jump_rel(Z, op[1] as i8);
            }
            0x29 => {
                // ADD HL,HL 1:8 - 0 H C
                return self.add16(Reg16::HL);
            }
            0x2a => {
                // LD A,(HL+) 1:8 - - - -
                return self.load(Reg::A, Reg::HLI);
            }
            0x2b => {
                // DEC HL 1:8 - - - -
                return self.dec(Reg::HL);
            }
            0x2c => {
                // INC L 1:4 Z 0 H -
                return self.inc(Reg::L);
            }
            0x2d => {
                // DEC L 1:4 Z 1 H -
                return self.dec(Reg::L);
            }
            0x2e => {
                // LD L,d8 2:8 - - - -
                return self.load(Reg::L, Reg::Im8);
            }
            0x2f => {
                // CPL 1:4 - 1 1 -
                self.0.cpu.a = !self.0.cpu.a;
                self.0.cpu.f.set_n();
                self.0.cpu.f.set_h();
            }
            0x30 => {
                // JR NC,r8 2:12/8 - - - -
                return self.jump_rel(NC, op[1] as i8);
            }
            0x31 => {
                // LD SP,d16 3:12 - - - -
                return self.load16(Reg16::SP, Reg16::Im16);
            }
            0x32 => {
                // LD (HL-),A 1:8 - - - -
                return self.load(Reg::HLD, Reg::A);
            }
            0x33 => {
                // INC SP 1:8 - - - -
                return self.inc(Reg::SP);
            }
            0x34 => {
                // INC (HL) 1:12 Z 0 H -
                let mut reg = self.0.read(self.0.cpu.hl());
                reg = add(reg, 1);
                self.0.write(self.0.cpu.hl(), reg);
                self.0.cpu.f.def_z(reg == 0);
                self.0.cpu.f.clr_n();
                self.0.cpu.f.def_h(reg & 0x0f == 0);
            }
            0x35 => {
                // DEC (HL) 1:12 Z 1 H -
                let mut reg = self.0.read(self.0.cpu.hl());
                reg = sub(reg, 1);
                self.0.write(self.0.cpu.hl(), reg);
                self.0.cpu.f.def_z(reg == 0);
                self.0.cpu.f.set_n();
                self.0.cpu.f.def_h(reg & 0x0f == 0xf);
            }
            0x36 => {
                // LD (HL),d8 2:12 - - - -
                return self.load(Reg::HL, Reg::Im8);
            }
            0x37 => {
                // SCF 1:4 - 0 0 1
                self.0.cpu.f.clr_h();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.set_c();
            }
            0x38 => {
                // JR C,r8 2:12/8 - - - -
                return self.jump_rel(C, op[1] as i8);
            }
            0x39 => {
                // ADD HL,SP 1:8 - 0 H C
                return self.add16(Reg16::SP);
            }
            0x3a => {
                // LD A,(HL-) 1:8 - - - -
                return self.load(Reg::A, Reg::HLD);
            }
            0x3b => {
                // DEC SP 1:8 - - - -
                return self.dec(Reg::SP);
            }
            0x3c => {
                // INC A 1:4 Z 0 H -
                return self.inc(Reg::A);
            }
            0x3d => {
                // DEC A 1:4 Z 1 H -
                return self.dec(Reg::A);
            }
            0x3e => {
                // LD A,d8 2:8 - - - -
                return self.load(Reg::A, Reg::Im8);
            }
            0x3f => {
                // CCF 1:4 - 0 0 C
                self.0.cpu.f.clr_n();
                self.0.cpu.f.clr_h();
                self.0.cpu.f.def_c(!self.0.cpu.f.c());
            }
            0x40 => {
                // LD B,B 1:4 - - - -
                return self.load(Reg::B, Reg::B);
            }
            0x41 => {
                // LD B,C 1:4 - - - -
                return self.load(Reg::B, Reg::C);
            }
            0x42 => {
                // LD B,D 1:4 - - - -
                return self.load(Reg::B, Reg::D);
            }
            0x43 => {
                // LD B,E 1:4 - - - -
                return self.load(Reg::B, Reg::E);
            }
            0x44 => {
                // LD B,H 1:4 - - - -
                return self.load(Reg::B, Reg::H);
            }
            0x45 => {
                // LD B,L 1:4 - - - -
                return self.load(Reg::B, Reg::L);
            }
            0x46 => {
                // LD B,(HL) 1:8 - - - -
                return self.load(Reg::B, Reg::HL);
            }
            0x47 => {
                // LD B,A 1:4 - - - -
                return self.load(Reg::B, Reg::A);
            }
            0x48 => {
                // LD C,B 1:4 - - - -
                return self.load(Reg::C, Reg::B);
            }
            0x49 => {
                // LD C,C 1:4 - - - -
                return self.load(Reg::C, Reg::C);
            }
            0x4a => {
                // LD C,D 1:4 - - - -
                return self.load(Reg::C, Reg::D);
            }
            0x4b => {
                // LD C,E 1:4 - - - -
                return self.load(Reg::C, Reg::E);
            }
            0x4c => {
                // LD C,H 1:4 - - - -
                return self.load(Reg::C, Reg::H);
            }
            0x4d => {
                // LD C,L 1:4 - - - -
                return self.load(Reg::C, Reg::L);
            }
            0x4e => {
                // LD C,(HL) 1:8 - - - -
                return self.load(Reg::C, Reg::HL);
            }
            0x4f => {
                // LD C,A 1:4 - - - -
                return self.load(Reg::C, Reg::A);
            }
            0x50 => {
                // LD D,B 1:4 - - - -
                return self.load(Reg::D, Reg::B);
            }
            0x51 => {
                // LD D,C 1:4 - - - -
                return self.load(Reg::D, Reg::C);
            }
            0x52 => {
                // LD D,D 1:4 - - - -
                return self.load(Reg::D, Reg::D);
            }
            0x53 => {
                // LD D,E 1:4 - - - -
                return self.load(Reg::D, Reg::E);
            }
            0x54 => {
                // LD D,H 1:4 - - - -
                return self.load(Reg::D, Reg::H);
            }
            0x55 => {
                // LD D,L 1:4 - - - -
                return self.load(Reg::D, Reg::L);
            }
            0x56 => {
                // LD D,(HL) 1:8 - - - -
                return self.load(Reg::D, Reg::HL);
            }
            0x57 => {
                // LD D,A 1:4 - - - -
                return self.load(Reg::D, Reg::A);
            }
            0x58 => {
                // LD E,B 1:4 - - - -
                return self.load(Reg::E, Reg::B);
            }
            0x59 => {
                // LD E,C 1:4 - - - -
                return self.load(Reg::E, Reg::C);
            }
            0x5a => {
                // LD E,D 1:4 - - - -
                return self.load(Reg::E, Reg::D);
            }
            0x5b => {
                // LD E,E 1:4 - - - -
                return self.load(Reg::E, Reg::E);
            }
            0x5c => {
                // LD E,H 1:4 - - - -
                return self.load(Reg::E, Reg::H);
            }
            0x5d => {
                // LD E,L 1:4 - - - -
                return self.load(Reg::E, Reg::L);
            }
            0x5e => {
                // LD E,(HL) 1:8 - - - -
                return self.load(Reg::E, Reg::HL);
            }
            0x5f => {
                // LD E,A 1:4 - - - -
                return self.load(Reg::E, Reg::A);
            }
            0x60 => {
                // LD H,B 1:4 - - - -
                return self.load(Reg::H, Reg::B);
            }
            0x61 => {
                // LD H,C 1:4 - - - -
                return self.load(Reg::H, Reg::C);
            }
            0x62 => {
                // LD H,D 1:4 - - - -
                return self.load(Reg::H, Reg::D);
            }
            0x63 => {
                // LD H,E 1:4 - - - -
                return self.load(Reg::H, Reg::E);
            }
            0x64 => {
                // LD H,H 1:4 - - - -
                return self.load(Reg::H, Reg::H);
            }
            0x65 => {
                // LD H,L 1:4 - - - -
                return self.load(Reg::H, Reg::L);
            }
            0x66 => {
                // LD H,(HL) 1:8 - - - -
                return self.load(Reg::H, Reg::HL);
            }
            0x67 => {
                // LD H,A 1:4 - - - -
                return self.load(Reg::H, Reg::A);
            }
            0x68 => {
                // LD L,B 1:4 - - - -
                return self.load(Reg::L, Reg::B);
            }
            0x69 => {
                // LD L,C 1:4 - - - -
                return self.load(Reg::L, Reg::C);
            }
            0x6a => {
                // LD L,D 1:4 - - - -
                return self.load(Reg::L, Reg::D);
            }
            0x6b => {
                // LD L,E 1:4 - - - -
                return self.load(Reg::L, Reg::E);
            }
            0x6c => {
                // LD L,H 1:4 - - - -
                return self.load(Reg::L, Reg::H);
            }
            0x6d => {
                // LD L,L 1:4 - - - -
                return self.load(Reg::L, Reg::L);
            }
            0x6e => {
                // LD L,(HL) 1:8 - - - -
                return self.load(Reg::L, Reg::HL);
            }
            0x6f => {
                // LD L,A 1:4 - - - -
                return self.load(Reg::L, Reg::A);
            }
            0x70 => {
                // LD (HL),B 1:8 - - - -
                return self.load(Reg::HL, Reg::B);
            }
            0x71 => {
                // LD (HL),C 1:8 - - - -
                return self.load(Reg::HL, Reg::C);
            }
            0x72 => {
                // LD (HL),D 1:8 - - - -
                return self.load(Reg::HL, Reg::D);
            }
            0x73 => {
                // LD (HL),E 1:8 - - - -
                return self.load(Reg::HL, Reg::E);
            }
            0x74 => {
                // LD (HL),H 1:8 - - - -
                return self.load(Reg::HL, Reg::H);
            }
            0x75 => {
                // LD (HL),L 1:8 - - - -
                return self.load(Reg::HL, Reg::L);
            }
            0x76 => {
                // HALT 1:4 - - - -
                self.0.cpu.state = CpuState::Halt;
            }
            0x77 => {
                // LD (HL),A 1:8 - - - -
                return self.load(Reg::HL, Reg::A);
            }
            0x78 => {
                // LD A,B 1:4 - - - -
                return self.load(Reg::A, Reg::B);
            }
            0x79 => {
                // LD A,C 1:4 - - - -
                return self.load(Reg::A, Reg::C);
            }
            0x7a => {
                // LD A,D 1:4 - - - -
                return self.load(Reg::A, Reg::D);
            }
            0x7b => {
                // LD A,E 1:4 - - - -
                return self.load(Reg::A, Reg::E);
            }
            0x7c => {
                // LD A,H 1:4 - - - -
                return self.load(Reg::A, Reg::H);
            }
            0x7d => {
                // LD A,L 1:4 - - - -
                return self.load(Reg::A, Reg::L);
            }
            0x7e => {
                // LD A,(HL) 1:8 - - - -
                return self.load(Reg::A, Reg::HL);
            }
            0x7f => {
                // LD A,A 1:4 - - - -
                return self.load(Reg::A, Reg::A);
            }
            0x80 => {
                // ADD A,B 1:4 Z 0 H C
                return self.add(Reg::B);
            }
            0x81 => {
                // ADD A,C 1:4 Z 0 H C
                return self.add(Reg::C);
            }
            0x82 => {
                // ADD A,D 1:4 Z 0 H C
                return self.add(Reg::D);
            }
            0x83 => {
                // ADD A,E 1:4 Z 0 H C
                return self.add(Reg::E);
            }
            0x84 => {
                // ADD A,H 1:4 Z 0 H C
                return self.add(Reg::H);
            }
            0x85 => {
                // ADD A,L 1:4 Z 0 H C
                return self.add(Reg::L);
            }
            0x86 => {
                // ADD A,(HL) 1:8 Z 0 H C
                return self.add(Reg::HL);
            }
            0x87 => {
                // ADD A,A 1:4 Z 0 H C
                return self.add(Reg::A);
            }
            0x88 => {
                // ADC A,B 1:4 Z 0 H C
                return self.adc(Reg::B);
            }
            0x89 => {
                // ADC A,C 1:4 Z 0 H C
                return self.adc(Reg::C);
            }
            0x8a => {
                // ADC A,D 1:4 Z 0 H C
                return self.adc(Reg::D);
            }
            0x8b => {
                // ADC A,E 1:4 Z 0 H C
                return self.adc(Reg::E);
            }
            0x8c => {
                // ADC A,H 1:4 Z 0 H C
                return self.adc(Reg::H);
            }
            0x8d => {
                // ADC A,L 1:4 Z 0 H C
                return self.adc(Reg::L);
            }
            0x8e => {
                // ADC A,(HL) 1:8 Z 0 H C
                return self.adc(Reg::HL);
            }
            0x8f => {
                // ADC A,A 1:4 Z 0 H C
                return self.adc(Reg::A);
            }
            0x90 => {
                // SUB B 1:4 Z 1 H C
                return self.sub(Reg::B);
            }
            0x91 => {
                // SUB C 1:4 Z 1 H C
                return self.sub(Reg::C);
            }
            0x92 => {
                // SUB D 1:4 Z 1 H C
                return self.sub(Reg::D);
            }
            0x93 => {
                // SUB E 1:4 Z 1 H C
                return self.sub(Reg::E);
            }
            0x94 => {
                // SUB H 1:4 Z 1 H C
                return self.sub(Reg::H);
            }
            0x95 => {
                // SUB L 1:4 Z 1 H C
                return self.sub(Reg::L);
            }
            0x96 => {
                // SUB (HL) 1:8 Z 1 H C
                return self.sub(Reg::HL);
            }
            0x97 => {
                // SUB A 1:4 Z 1 H C
                return self.sub(Reg::A);
            }
            0x98 => {
                // SBC A,B 1:4 Z 1 H C
                return self.sbc(Reg::B);
            }
            0x99 => {
                // SBC A,C 1:4 Z 1 H C
                return self.sbc(Reg::C);
            }
            0x9a => {
                // SBC A,D 1:4 Z 1 H C
                return self.sbc(Reg::D);
            }
            0x9b => {
                // SBC A,E 1:4 Z 1 H C
                return self.sbc(Reg::E);
            }
            0x9c => {
                // SBC A,H 1:4 Z 1 H C
                return self.sbc(Reg::H);
            }
            0x9d => {
                // SBC A,L 1:4 Z 1 H C
                return self.sbc(Reg::L);
            }
            0x9e => {
                // SBC A,(HL) 1:8 Z 1 H C
                return self.sbc(Reg::HL);
            }
            0x9f => {
                // SBC A,A 1:4 Z 1 H C
                return self.sbc(Reg::A);
            }
            0xa0 => {
                // AND B 1:4 Z 0 1 0
                return self.and(Reg::B);
            }
            0xa1 => {
                // AND C 1:4 Z 0 1 0
                return self.and(Reg::C);
            }
            0xa2 => {
                // AND D 1:4 Z 0 1 0
                return self.and(Reg::D);
            }
            0xa3 => {
                // AND E 1:4 Z 0 1 0
                return self.and(Reg::E);
            }
            0xa4 => {
                // AND H 1:4 Z 0 1 0
                return self.and(Reg::H);
            }
            0xa5 => {
                // AND L 1:4 Z 0 1 0
                return self.and(Reg::L);
            }
            0xa6 => {
                // AND (HL) 1:8 Z 0 1 0
                return self.and(Reg::HL);
            }
            0xa7 => {
                // AND A 1:4 Z 0 1 0
                return self.and(Reg::A);
            }
            0xa8 => {
                // XOR B 1:4 Z 0 0 0
                return self.xor(Reg::B);
            }
            0xa9 => {
                // XOR C 1:4 Z 0 0 0
                return self.xor(Reg::C);
            }
            0xaa => {
                // XOR D 1:4 Z 0 0 0
                return self.xor(Reg::D);
            }
            0xab => {
                // XOR E 1:4 Z 0 0 0
                return self.xor(Reg::E);
            }
            0xac => {
                // XOR H 1:4 Z 0 0 0
                return self.xor(Reg::H);
            }
            0xad => {
                // XOR L 1:4 Z 0 0 0
                return self.xor(Reg::L);
            }
            0xae => {
                // XOR (HL) 1:8 Z 0 0 0
                return self.xor(Reg::HL);
            }
            0xaf => {
                // XOR A 1:4 Z 0 0 0
                return self.xor(Reg::A);
            }
            0xb0 => {
                // OR B 1:4 Z 0 0 0
                return self.or(Reg::B);
            }
            0xb1 => {
                // OR C 1:4 Z 0 0 0
                return self.or(Reg::C);
            }
            0xb2 => {
                // OR D 1:4 Z 0 0 0
                return self.or(Reg::D);
            }
            0xb3 => {
                // OR E 1:4 Z 0 0 0
                return self.or(Reg::E);
            }
            0xb4 => {
                // OR H 1:4 Z 0 0 0
                return self.or(Reg::H);
            }
            0xb5 => {
                // OR L 1:4 Z 0 0 0
                return self.or(Reg::L);
            }
            0xb6 => {
                // OR (HL) 1:8 Z 0 0 0
                return self.or(Reg::HL);
            }
            0xb7 => {
                // OR A 1:4 Z 0 0 0
                return self.or(Reg::A);
            }
            0xb8 => {
                // CP B 1:4 Z 1 H C
                return self.cp(Reg::B);
            }
            0xb9 => {
                // CP C 1:4 Z 1 H C
                return self.cp(Reg::C);
            }
            0xba => {
                // CP D 1:4 Z 1 H C
                return self.cp(Reg::D);
            }
            0xbb => {
                // CP E 1:4 Z 1 H C
                return self.cp(Reg::E);
            }
            0xbc => {
                // CP H 1:4 Z 1 H C
                return self.cp(Reg::H);
            }
            0xbd => {
                // CP L 1:4 Z 1 H C
                return self.cp(Reg::L);
            }
            0xbe => {
                // CP (HL) 1:8 Z 1 H C
                return self.cp(Reg::HL);
            }
            0xbf => {
                // CP A 1:4 Z 1 H C
                return self.cp(Reg::A);
            }
            0xc0 => {
                // RET NZ 1:20/8 - - - -
                return self.ret(NZ);
            }
            0xc1 => {
                // POP BC 1:12 - - - -
                return self.pop(Reg16::BC);
            }
            0xc2 => {
                // JP NZ,a16 3:16/12 - - - -
                return self.jump(NZ, n16(op));
            }
            0xc3 => {
                // JP a16 3:16 - - - -
                return self.jump(None, n16(op));
            }
            0xc4 => {
                // CALL NZ,a16 3:24/12 - - - -
                return self.call(NZ, n16(op));
            }
            0xc5 => {
                // PUSH BC 1:16 - - - -
                return self.push(Reg16::BC);
            }
            0xc6 => {
                // ADD A,d8 2:8 Z 0 H C
                return self.add(Reg::Im8);
            }
            0xc7 => {
                // RST 00H 1:16 - - - -
                return self.rst(0x00);
            }
            0xc8 => {
                // RET Z 1:20/8 - - - -
                return self.ret(Z);
            }
            0xc9 => {
                // RET 1:16 - - - -
                return self.ret(None);
            }
            0xca => {
                // JP Z,a16 3:16/12 - - - -
                return self.jump(Z, n16(op));
            }
            0xcb => {
                // PREFIX CB 1:4 - - - -
                return self.interpret_op_cb();
            }
            0xcc => {
                // CALL Z,a16 3:24/12 - - - -
                return self.call(Z, n16(op));
            }
            0xcd => {
                // CALL a16 3:24 - - - -
                return self.call(None, n16(op));
            }
            0xce => {
                // ADC A,d8 2:8 Z 0 H C
                return self.adc(Reg::Im8);
            }
            0xcf => {
                // RST 08H 1:16 - - - -
                return self.rst(0x08);
            }
            0xd0 => {
                // RET NC 1:20/8 - - - -
                return self.ret(NC);
            }
            0xd1 => {
                // POP DE 1:12 - - - -
                return self.pop(Reg16::DE);
            }
            0xd2 => {
                // JP NC,a16 3:16/12 - - - -
                return self.jump(NC, n16(op));
            }
            0xd3 => {
                //
            }
            0xd4 => {
                // CALL NC,a16 3:24/12 - - - -
                return self.call(NC, n16(op));
            }
            0xd5 => {
                // PUSH DE 1:16 - - - -
                return self.push(Reg16::DE);
            }
            0xd6 => {
                // SUB d8 2:8 Z 1 H C
                return self.sub(Reg::Im8);
            }
            0xd7 => {
                // RST 10H 1:16 - - - -
                return self.rst(0x10);
            }
            0xd8 => {
                // RET C 1:20/8 - - - -
                return self.ret(C);
            }
            0xd9 => {
                // RETI 1:16 - - - -
                self.ret(None);
                self.0.cpu.ime = ImeState::ToBeEnable;
                return;
            }
            0xda => {
                // JP C,a16 3:16/12 - - - -
                return self.jump(C, n16(op));
            }
            0xdb => {
                //
            }
            0xdc => {
                // CALL C,a16 3:24/12 - - - -
                return self.call(C, n16(op));
            }
            0xdd => {
                //
            }
            0xde => {
                // SBC A,d8 2:8 Z 1 H C
                return self.sbc(Reg::Im8);
            }
            0xdf => {
                // RST 18H 1:16 - - - -
                return self.rst(0x18);
            }
            0xe0 => {
                // LDH (a8),A 2:12 - - - -
                return self.loadh(Reg::Im8, Reg::A);
            }
            0xe1 => {
                // POP HL 1:12 - - - -
                return self.pop(Reg16::HL);
            }
            0xe2 => {
                // LD (C),A 2:8 - - - -
                return self.loadh(Reg::C, Reg::A);
            }
            0xe3 => {
                //
            }
            0xe4 => {
                //
            }
            0xe5 => {
                // PUSH HL 1:16 - - - -
                return self.push(Reg16::HL);
            }
            0xe6 => {
                // AND d8 2:8 Z 0 1 0
                return self.and(Reg::Im8);
            }
            0xe7 => {
                // RST 20H 1:16 - - - -
                return self.rst(0x20);
            }
            0xe8 => {
                // ADD SP,r8 2:16 0 0 H C
                let r;
                let c;
                let h;
                if (op[1] as i8) >= 0 {
                    c = ((self.0.cpu.sp & 0xFF) + op[1] as u16) > 0xFF;
                    h = ((self.0.cpu.sp & 0x0F) as u8 + (op[1] & 0xF)) > 0x0F;
                    r = add16(self.0.cpu.sp, op[1] as u16);
                } else {
                    r = sub16(self.0.cpu.sp, -(op[1] as i8) as u16);
                    c = (r & 0xFF) <= (self.0.cpu.sp & 0xFF);
                    h = (r & 0x0F) <= (self.0.cpu.sp & 0x0F);
                }
                self.0.cpu.sp = r;
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.def_c(c);
                self.0.cpu.f.def_h(h);
            }
            0xe9 => {
                // JP (HL) 1:4 - - - -
                self.jump_to(self.0.cpu.hl());
                let cycles = 4;
                self.0.tick(cycles);
                return;
            }
            0xea => {
                // LD (a16),A 3:16 - - - -
                return self.load(Reg::Im16, Reg::A);
            }
            0xeb => {
                //
            }
            0xec => {
                //
            }
            0xed => {
                //
            }
            0xee => {
                // XOR d8 2:8 Z 0 0 0
                return self.xor(Reg::Im8);
            }
            0xef => {
                // RST 28H 1:16 - - - -
                return self.rst(0x28);
            }
            0xf0 => {
                // LDH A,(a8) 2:12 - - - -
                return self.loadh(Reg::A, Reg::Im8);
            }
            0xf1 => {
                // POP AF 1:12 Z N H C
                return self.pop(Reg16::AF);
            }
            0xf2 => {
                // LD A,(C) 2:8 - - - -
                return self.loadh(Reg::A, Reg::C);
            }
            0xf3 => {
                // DI 1:4 - - - -
                self.0.cpu.ime = ImeState::Disabled;
            }
            0xf4 => {
                //
            }
            0xf5 => {
                // PUSH AF 1:16 - - - -
                return self.push(Reg16::AF);
            }
            0xf6 => {
                // OR d8 2:8 Z 0 0 0
                return self.or(Reg::Im8);
            }
            0xf7 => {
                // RST 30H 1:16 - - - -
                return self.rst(0x30);
            }
            0xf8 => {
                // LD HL,SP+r8 2:12 0 0 H C
                let r;
                let c;
                let h;
                if (op[1] as i8) >= 0 {
                    c = ((self.0.cpu.sp & 0xFF) + op[1] as u16) > 0xFF;
                    h = ((self.0.cpu.sp & 0x0F) as u8 + (op[1] & 0xF)) > 0x0F;
                    r = add16(self.0.cpu.sp, op[1] as u16);
                } else {
                    r = sub16(self.0.cpu.sp, -(op[1] as i8) as u16);
                    c = (r & 0xFF) <= (self.0.cpu.sp & 0xFF);
                    h = (r & 0x0F) <= (self.0.cpu.sp & 0x0F);
                }
                self.0.cpu.set_hl(r);
                self.0.cpu.f.clr_z();
                self.0.cpu.f.clr_n();
                self.0.cpu.f.def_h(h);
                self.0.cpu.f.def_c(c);
            }
            0xf9 => {
                // LD SP,HL 1:8 - - - -
                return self.load16(Reg16::SP, Reg16::HL);
            }
            0xfa => {
                // LD A,(a16) 3:16 - - - -
                return self.load(Reg::A, Reg::Im16);
            }
            0xfb => {
                // EI 1:4 - - - -
                // TODO: this need to be delayed by one instruction
                self.0.cpu.ime = ImeState::ToBeEnable;
            }
            0xfc => {
                //
            }
            0xfd => {
                //
            }
            0xfe => {
                // CP d8 2:8 Z 1 H C
                return self.cp(Reg::Im8);
            }
            0xff => {
                // RST 38H 1:16 - - - -
                return self.rst(0x38);
            }
        }
        self.0.cpu.pc = add16(self.0.cpu.pc, consts::LEN[op[0] as usize] as u16);
        let cycles = consts::CLOCK[op[0] as usize];
        self.0.tick(cycles);
    }

    fn interpret_op_cb(&mut self) {
        let op = &[
            self.0.read(self.0.cpu.pc + 1),
            self.0.read(self.0.cpu.pc + 2),
        ];
        match op[0] {
            0x00 => {
                // RLC B 2:8 Z 0 0 C
                return self.rlc(Reg::B);
            }
            0x01 => {
                // RLC C 2:8 Z 0 0 C
                return self.rlc(Reg::C);
            }
            0x02 => {
                // RLC D 2:8 Z 0 0 C
                return self.rlc(Reg::D);
            }
            0x03 => {
                // RLC E 2:8 Z 0 0 C
                return self.rlc(Reg::E);
            }
            0x04 => {
                // RLC H 2:8 Z 0 0 C
                return self.rlc(Reg::H);
            }
            0x05 => {
                // RLC L 2:8 Z 0 0 C
                return self.rlc(Reg::L);
            }
            0x06 => {
                // RLC (HL) 2:16 Z 0 0 C
                return self.rlc(Reg::HL);
            }
            0x07 => {
                // RLC A 2:8 Z 0 0 C
                return self.rlc(Reg::A);
            }
            0x08 => {
                // RRC B 2:8 Z 0 0 C
                return self.rrc(Reg::B);
            }
            0x09 => {
                // RRC C 2:8 Z 0 0 C
                return self.rrc(Reg::C);
            }
            0x0a => {
                // RRC D 2:8 Z 0 0 C
                return self.rrc(Reg::D);
            }
            0x0b => {
                // RRC E 2:8 Z 0 0 C
                return self.rrc(Reg::E);
            }
            0x0c => {
                // RRC H 2:8 Z 0 0 C
                return self.rrc(Reg::H);
            }
            0x0d => {
                // RRC L 2:8 Z 0 0 C
                return self.rrc(Reg::L);
            }
            0x0e => {
                // RRC (HL) 2:16 Z 0 0 C
                return self.rrc(Reg::HL);
            }
            0x0f => {
                // RRC A 2:8 Z 0 0 C
                return self.rrc(Reg::A);
            }
            0x10 => {
                // RL B 2:8 Z 0 0 C
                return self.rl(Reg::B);
            }
            0x11 => {
                // RL C 2:8 Z 0 0 C
                return self.rl(Reg::C);
            }
            0x12 => {
                // RL D 2:8 Z 0 0 C
                return self.rl(Reg::D);
            }
            0x13 => {
                // RL E 2:8 Z 0 0 C
                return self.rl(Reg::E);
            }
            0x14 => {
                // RL H 2:8 Z 0 0 C
                return self.rl(Reg::H);
            }
            0x15 => {
                // RL L 2:8 Z 0 0 C
                return self.rl(Reg::L);
            }
            0x16 => {
                // RL (HL) 2:16 Z 0 0 C
                return self.rl(Reg::HL);
            }
            0x17 => {
                // RL A 2:8 Z 0 0 C
                return self.rl(Reg::A);
            }
            0x18 => {
                // RR B 2:8 Z 0 0 C
                return self.rr(Reg::B);
            }
            0x19 => {
                // RR C 2:8 Z 0 0 C
                return self.rr(Reg::C);
            }
            0x1a => {
                // RR D 2:8 Z 0 0 C
                return self.rr(Reg::D);
            }
            0x1b => {
                // RR E 2:8 Z 0 0 C
                return self.rr(Reg::E);
            }
            0x1c => {
                // RR H 2:8 Z 0 0 C
                return self.rr(Reg::H);
            }
            0x1d => {
                // RR L 2:8 Z 0 0 C
                return self.rr(Reg::L);
            }
            0x1e => {
                // RR (HL) 2:16 Z 0 0 C
                return self.rr(Reg::HL);
            }
            0x1f => {
                // RR A 2:8 Z 0 0 C
                return self.rr(Reg::A);
            }
            0x20 => {
                // SLA B 2:8 Z 0 0 C
                return self.sla(Reg::B);
            }
            0x21 => {
                // SLA C 2:8 Z 0 0 C
                return self.sla(Reg::C);
            }
            0x22 => {
                // SLA D 2:8 Z 0 0 C
                return self.sla(Reg::D);
            }
            0x23 => {
                // SLA E 2:8 Z 0 0 C
                return self.sla(Reg::E);
            }
            0x24 => {
                // SLA H 2:8 Z 0 0 C
                return self.sla(Reg::H);
            }
            0x25 => {
                // SLA L 2:8 Z 0 0 C
                return self.sla(Reg::L);
            }
            0x26 => {
                // SLA (HL) 2:16 Z 0 0 C
                return self.sla(Reg::HL);
            }
            0x27 => {
                // SLA A 2:8 Z 0 0 C
                return self.sla(Reg::A);
            }
            0x28 => {
                // SRA B 2:8 Z 0 0 0
                return self.sra(Reg::B);
            }
            0x29 => {
                // SRA C 2:8 Z 0 0 0
                return self.sra(Reg::C);
            }
            0x2a => {
                // SRA D 2:8 Z 0 0 0
                return self.sra(Reg::D);
            }
            0x2b => {
                // SRA E 2:8 Z 0 0 0
                return self.sra(Reg::E);
            }
            0x2c => {
                // SRA H 2:8 Z 0 0 0
                return self.sra(Reg::H);
            }
            0x2d => {
                // SRA L 2:8 Z 0 0 0
                return self.sra(Reg::L);
            }
            0x2e => {
                // SRA (HL) 2:16 Z 0 0 0
                return self.sra(Reg::HL);
            }
            0x2f => {
                // SRA A 2:8 Z 0 0 0
                return self.sra(Reg::A);
            }
            0x30 => {
                // SWAP B 2:8 Z 0 0 0
                return self.swap(Reg::B);
            }
            0x31 => {
                // SWAP C 2:8 Z 0 0 0
                return self.swap(Reg::C);
            }
            0x32 => {
                // SWAP D 2:8 Z 0 0 0
                return self.swap(Reg::D);
            }
            0x33 => {
                // SWAP E 2:8 Z 0 0 0
                return self.swap(Reg::E);
            }
            0x34 => {
                // SWAP H 2:8 Z 0 0 0
                return self.swap(Reg::H);
            }
            0x35 => {
                // SWAP L 2:8 Z 0 0 0
                return self.swap(Reg::L);
            }
            0x36 => {
                // SWAP (HL) 2:16 Z 0 0 0
                return self.swap(Reg::HL);
            }
            0x37 => {
                // SWAP A 2:8 Z 0 0 0
                return self.swap(Reg::A);
            }
            0x38 => {
                // SRL B 2:8 Z 0 0 C
                return self.srl(Reg::B);
            }
            0x39 => {
                // SRL C 2:8 Z 0 0 C
                return self.srl(Reg::C);
            }
            0x3a => {
                // SRL D 2:8 Z 0 0 C
                return self.srl(Reg::D);
            }
            0x3b => {
                // SRL E 2:8 Z 0 0 C
                return self.srl(Reg::E);
            }
            0x3c => {
                // SRL H 2:8 Z 0 0 C
                return self.srl(Reg::H);
            }
            0x3d => {
                // SRL L 2:8 Z 0 0 C
                return self.srl(Reg::L);
            }
            0x3e => {
                // SRL (HL) 2:16 Z 0 0 C
                return self.srl(Reg::HL);
            }
            0x3f => {
                // SRL A 2:8 Z 0 0 C
                return self.srl(Reg::A);
            }
            0x40 => {
                // BIT 0,B 2:8 Z 0 1 -
                return self.bit(0, Reg::B);
            }
            0x41 => {
                // BIT 0,C 2:8 Z 0 1 -
                return self.bit(0, Reg::C);
            }
            0x42 => {
                // BIT 0,D 2:8 Z 0 1 -
                return self.bit(0, Reg::D);
            }
            0x43 => {
                // BIT 0,E 2:8 Z 0 1 -
                return self.bit(0, Reg::E);
            }
            0x44 => {
                // BIT 0,H 2:8 Z 0 1 -
                return self.bit(0, Reg::H);
            }
            0x45 => {
                // BIT 0,L 2:8 Z 0 1 -
                return self.bit(0, Reg::L);
            }
            0x46 => {
                // BIT 0,(HL) 2:16 Z 0 1 -
                return self.bit(0, Reg::HL);
            }
            0x47 => {
                // BIT 0,A 2:8 Z 0 1 -
                return self.bit(0, Reg::A);
            }
            0x48 => {
                // BIT 1,B 2:8 Z 0 1 -
                return self.bit(1, Reg::B);
            }
            0x49 => {
                // BIT 1,C 2:8 Z 0 1 -
                return self.bit(1, Reg::C);
            }
            0x4a => {
                // BIT 1,D 2:8 Z 0 1 -
                return self.bit(1, Reg::D);
            }
            0x4b => {
                // BIT 1,E 2:8 Z 0 1 -
                return self.bit(1, Reg::E);
            }
            0x4c => {
                // BIT 1,H 2:8 Z 0 1 -
                return self.bit(1, Reg::H);
            }
            0x4d => {
                // BIT 1,L 2:8 Z 0 1 -
                return self.bit(1, Reg::L);
            }
            0x4e => {
                // BIT 1,(HL) 2:16 Z 0 1 -
                return self.bit(1, Reg::HL);
            }
            0x4f => {
                // BIT 1,A 2:8 Z 0 1 -
                return self.bit(1, Reg::A);
            }
            0x50 => {
                // BIT 2,B 2:8 Z 0 1 -
                return self.bit(2, Reg::B);
            }
            0x51 => {
                // BIT 2,C 2:8 Z 0 1 -
                return self.bit(2, Reg::C);
            }
            0x52 => {
                // BIT 2,D 2:8 Z 0 1 -
                return self.bit(2, Reg::D);
            }
            0x53 => {
                // BIT 2,E 2:8 Z 0 1 -
                return self.bit(2, Reg::E);
            }
            0x54 => {
                // BIT 2,H 2:8 Z 0 1 -
                return self.bit(2, Reg::H);
            }
            0x55 => {
                // BIT 2,L 2:8 Z 0 1 -
                return self.bit(2, Reg::L);
            }
            0x56 => {
                // BIT 2,(HL) 2:16 Z 0 1 -
                return self.bit(2, Reg::HL);
            }
            0x57 => {
                // BIT 2,A 2:8 Z 0 1 -
                return self.bit(2, Reg::A);
            }
            0x58 => {
                // BIT 3,B 2:8 Z 0 1 -
                return self.bit(3, Reg::B);
            }
            0x59 => {
                // BIT 3,C 2:8 Z 0 1 -
                return self.bit(3, Reg::C);
            }
            0x5a => {
                // BIT 3,D 2:8 Z 0 1 -
                return self.bit(3, Reg::D);
            }
            0x5b => {
                // BIT 3,E 2:8 Z 0 1 -
                return self.bit(3, Reg::E);
            }
            0x5c => {
                // BIT 3,H 2:8 Z 0 1 -
                return self.bit(3, Reg::H);
            }
            0x5d => {
                // BIT 3,L 2:8 Z 0 1 -
                return self.bit(3, Reg::L);
            }
            0x5e => {
                // BIT 3,(HL) 2:16 Z 0 1 -
                return self.bit(3, Reg::HL);
            }
            0x5f => {
                // BIT 3,A 2:8 Z 0 1 -
                return self.bit(3, Reg::A);
            }
            0x60 => {
                // BIT 4,B 2:8 Z 0 1 -
                return self.bit(4, Reg::B);
            }
            0x61 => {
                // BIT 4,C 2:8 Z 0 1 -
                return self.bit(4, Reg::C);
            }
            0x62 => {
                // BIT 4,D 2:8 Z 0 1 -
                return self.bit(4, Reg::D);
            }
            0x63 => {
                // BIT 4,E 2:8 Z 0 1 -
                return self.bit(4, Reg::E);
            }
            0x64 => {
                // BIT 4,H 2:8 Z 0 1 -
                return self.bit(4, Reg::H);
            }
            0x65 => {
                // BIT 4,L 2:8 Z 0 1 -
                return self.bit(4, Reg::L);
            }
            0x66 => {
                // BIT 4,(HL) 2:16 Z 0 1 -
                return self.bit(4, Reg::HL);
            }
            0x67 => {
                // BIT 4,A 2:8 Z 0 1 -
                return self.bit(4, Reg::A);
            }
            0x68 => {
                // BIT 5,B 2:8 Z 0 1 -
                return self.bit(5, Reg::B);
            }
            0x69 => {
                // BIT 5,C 2:8 Z 0 1 -
                return self.bit(5, Reg::C);
            }
            0x6a => {
                // BIT 5,D 2:8 Z 0 1 -
                return self.bit(5, Reg::D);
            }
            0x6b => {
                // BIT 5,E 2:8 Z 0 1 -
                return self.bit(5, Reg::E);
            }
            0x6c => {
                // BIT 5,H 2:8 Z 0 1 -
                return self.bit(5, Reg::H);
            }
            0x6d => {
                // BIT 5,L 2:8 Z 0 1 -
                return self.bit(5, Reg::L);
            }
            0x6e => {
                // BIT 5,(HL) 2:16 Z 0 1 -
                return self.bit(5, Reg::HL);
            }
            0x6f => {
                // BIT 5,A 2:8 Z 0 1 -
                return self.bit(5, Reg::A);
            }
            0x70 => {
                // BIT 6,B 2:8 Z 0 1 -
                return self.bit(6, Reg::B);
            }
            0x71 => {
                // BIT 6,C 2:8 Z 0 1 -
                return self.bit(6, Reg::C);
            }
            0x72 => {
                // BIT 6,D 2:8 Z 0 1 -
                return self.bit(6, Reg::D);
            }
            0x73 => {
                // BIT 6,E 2:8 Z 0 1 -
                return self.bit(6, Reg::E);
            }
            0x74 => {
                // BIT 6,H 2:8 Z 0 1 -
                return self.bit(6, Reg::H);
            }
            0x75 => {
                // BIT 6,L 2:8 Z 0 1 -
                return self.bit(6, Reg::L);
            }
            0x76 => {
                // BIT 6,(HL) 2:16 Z 0 1 -
                return self.bit(6, Reg::HL);
            }
            0x77 => {
                // BIT 6,A 2:8 Z 0 1 -
                return self.bit(6, Reg::A);
            }
            0x78 => {
                // BIT 7,B 2:8 Z 0 1 -
                return self.bit(7, Reg::B);
            }
            0x79 => {
                // BIT 7,C 2:8 Z 0 1 -
                return self.bit(7, Reg::C);
            }
            0x7a => {
                // BIT 7,D 2:8 Z 0 1 -
                return self.bit(7, Reg::D);
            }
            0x7b => {
                // BIT 7,E 2:8 Z 0 1 -
                return self.bit(7, Reg::E);
            }
            0x7c => {
                // BIT 7,H 2:8 Z 0 1 -
                return self.bit(7, Reg::H);
            }
            0x7d => {
                // BIT 7,L 2:8 Z 0 1 -
                return self.bit(7, Reg::L);
            }
            0x7e => {
                // BIT 7,(HL) 2:16 Z 0 1 -
                return self.bit(7, Reg::HL);
            }
            0x7f => {
                // BIT 7,A 2:8 Z 0 1 -
                return self.bit(7, Reg::A);
            }
            0x80 => {
                // RES 0,B 2:8 - - - -
                return self.res(0, Reg::B);
            }
            0x81 => {
                // RES 0,C 2:8 - - - -
                return self.res(0, Reg::C);
            }
            0x82 => {
                // RES 0,D 2:8 - - - -
                return self.res(0, Reg::D);
            }
            0x83 => {
                // RES 0,E 2:8 - - - -
                return self.res(0, Reg::E);
            }
            0x84 => {
                // RES 0,H 2:8 - - - -
                return self.res(0, Reg::H);
            }
            0x85 => {
                // RES 0,L 2:8 - - - -
                return self.res(0, Reg::L);
            }
            0x86 => {
                // RES 0,(HL) 2:16 - - - -
                return self.res(0, Reg::HL);
            }
            0x87 => {
                // RES 0,A 2:8 - - - -
                return self.res(0, Reg::A);
            }
            0x88 => {
                // RES 1,B 2:8 - - - -
                return self.res(1, Reg::B);
            }
            0x89 => {
                // RES 1,C 2:8 - - - -
                return self.res(1, Reg::C);
            }
            0x8a => {
                // RES 1,D 2:8 - - - -
                return self.res(1, Reg::D);
            }
            0x8b => {
                // RES 1,E 2:8 - - - -
                return self.res(1, Reg::E);
            }
            0x8c => {
                // RES 1,H 2:8 - - - -
                return self.res(1, Reg::H);
            }
            0x8d => {
                // RES 1,L 2:8 - - - -
                return self.res(1, Reg::L);
            }
            0x8e => {
                // RES 1,(HL) 2:16 - - - -
                return self.res(1, Reg::HL);
            }
            0x8f => {
                // RES 1,A 2:8 - - - -
                return self.res(1, Reg::A);
            }
            0x90 => {
                // RES 2,B 2:8 - - - -
                return self.res(2, Reg::B);
            }
            0x91 => {
                // RES 2,C 2:8 - - - -
                return self.res(2, Reg::C);
            }
            0x92 => {
                // RES 2,D 2:8 - - - -
                return self.res(2, Reg::D);
            }
            0x93 => {
                // RES 2,E 2:8 - - - -
                return self.res(2, Reg::E);
            }
            0x94 => {
                // RES 2,H 2:8 - - - -
                return self.res(2, Reg::H);
            }
            0x95 => {
                // RES 2,L 2:8 - - - -
                return self.res(2, Reg::L);
            }
            0x96 => {
                // RES 2,(HL) 2:16 - - - -
                return self.res(2, Reg::HL);
            }
            0x97 => {
                // RES 2,A 2:8 - - - -
                return self.res(2, Reg::A);
            }
            0x98 => {
                // RES 3,B 2:8 - - - -
                return self.res(3, Reg::B);
            }
            0x99 => {
                // RES 3,C 2:8 - - - -
                return self.res(3, Reg::C);
            }
            0x9a => {
                // RES 3,D 2:8 - - - -
                return self.res(3, Reg::D);
            }
            0x9b => {
                // RES 3,E 2:8 - - - -
                return self.res(3, Reg::E);
            }
            0x9c => {
                // RES 3,H 2:8 - - - -
                return self.res(3, Reg::H);
            }
            0x9d => {
                // RES 3,L 2:8 - - - -
                return self.res(3, Reg::L);
            }
            0x9e => {
                // RES 3,(HL) 2:16 - - - -
                return self.res(3, Reg::HL);
            }
            0x9f => {
                // RES 3,A 2:8 - - - - Ax
                return self.res(3, Reg::A);
            }
            0xa0 => {
                // RES 4,B 2:8 - - - -
                return self.res(4, Reg::B);
            }
            0xa1 => {
                // RES 4,C 2:8 - - - -
                return self.res(4, Reg::C);
            }
            0xa2 => {
                // RES 4,D 2:8 - - - -
                return self.res(4, Reg::D);
            }
            0xa3 => {
                // RES 4,E 2:8 - - - -
                return self.res(4, Reg::E);
            }
            0xa4 => {
                // RES 4,H 2:8 - - - -
                return self.res(4, Reg::H);
            }
            0xa5 => {
                // RES 4,L 2:8 - - - -
                return self.res(4, Reg::L);
            }
            0xa6 => {
                // RES 4,(HL) 2:16 - - - -
                return self.res(4, Reg::HL);
            }
            0xa7 => {
                // RES 4,A 2:8 - - - -
                return self.res(4, Reg::A);
            }
            0xa8 => {
                // RES 5,B 2:8 - - - -
                return self.res(5, Reg::B);
            }
            0xa9 => {
                // RES 5,C 2:8 - - - -
                return self.res(5, Reg::C);
            }
            0xaa => {
                // RES 5,D 2:8 - - - -
                return self.res(5, Reg::D);
            }
            0xab => {
                // RES 5,E 2:8 - - - -
                return self.res(5, Reg::E);
            }
            0xac => {
                // RES 5,H 2:8 - - - -
                return self.res(5, Reg::H);
            }
            0xad => {
                // RES 5,L 2:8 - - - -
                return self.res(5, Reg::L);
            }
            0xae => {
                // RES 5,(HL) 2:16 - - - -
                return self.res(5, Reg::HL);
            }
            0xaf => {
                // RES 5,A 2:8 - - - - Bx
                return self.res(5, Reg::A);
            }
            0xb0 => {
                // RES 6,B 2:8 - - - -
                return self.res(6, Reg::B);
            }
            0xb1 => {
                // RES 6,C 2:8 - - - -
                return self.res(6, Reg::C);
            }
            0xb2 => {
                // RES 6,D 2:8 - - - -
                return self.res(6, Reg::D);
            }
            0xb3 => {
                // RES 6,E 2:8 - - - -
                return self.res(6, Reg::E);
            }
            0xb4 => {
                // RES 6,H 2:8 - - - -
                return self.res(6, Reg::H);
            }
            0xb5 => {
                // RES 6,L 2:8 - - - -
                return self.res(6, Reg::L);
            }
            0xb6 => {
                // RES 6,(HL) 2:16 - - - -
                return self.res(6, Reg::HL);
            }
            0xb7 => {
                // RES 6,A 2:8 - - - -
                return self.res(6, Reg::A);
            }
            0xb8 => {
                // RES 7,B 2:8 - - - -
                return self.res(7, Reg::B);
            }
            0xb9 => {
                // RES 7,C 2:8 - - - -
                return self.res(7, Reg::C);
            }
            0xba => {
                // RES 7,D 2:8 - - - -
                return self.res(7, Reg::D);
            }
            0xbb => {
                // RES 7,E 2:8 - - - -
                return self.res(7, Reg::E);
            }
            0xbc => {
                // RES 7,H 2:8 - - - -
                return self.res(7, Reg::H);
            }
            0xbd => {
                // RES 7,L 2:8 - - - -
                return self.res(7, Reg::L);
            }
            0xbe => {
                // RES 7,(HL) 2:16 - - - -
                return self.res(7, Reg::HL);
            }
            0xbf => {
                // RES 7,A 2:8 - - - - Cx
                return self.res(7, Reg::A);
            }
            0xc0 => {
                // SET 0,B 2:8 - - - -
                return self.set(0, Reg::B);
            }
            0xc1 => {
                // SET 0,C 2:8 - - - -
                return self.set(0, Reg::C);
            }
            0xc2 => {
                // SET 0,D 2:8 - - - -
                return self.set(0, Reg::D);
            }
            0xc3 => {
                // SET 0,E 2:8 - - - -
                return self.set(0, Reg::E);
            }
            0xc4 => {
                // SET 0,H 2:8 - - - -
                return self.set(0, Reg::H);
            }
            0xc5 => {
                // SET 0,L 2:8 - - - -
                return self.set(0, Reg::L);
            }
            0xc6 => {
                // SET 0,(HL) 2:16 - - - -
                return self.set(0, Reg::HL);
            }
            0xc7 => {
                // SET 0,A 2:8 - - - -
                return self.set(0, Reg::A);
            }
            0xc8 => {
                // SET 1,B 2:8 - - - -
                return self.set(1, Reg::B);
            }
            0xc9 => {
                // SET 1,C 2:8 - - - -
                return self.set(1, Reg::C);
            }
            0xca => {
                // SET 1,D 2:8 - - - -
                return self.set(1, Reg::D);
            }
            0xcb => {
                // SET 1,E 2:8 - - - -
                return self.set(1, Reg::E);
            }
            0xcc => {
                // SET 1,H 2:8 - - - -
                return self.set(1, Reg::H);
            }
            0xcd => {
                // SET 1,L 2:8 - - - -
                return self.set(1, Reg::L);
            }
            0xce => {
                // SET 1,(HL) 2:16 - - - -
                return self.set(1, Reg::HL);
            }
            0xcf => {
                // SET 1,A 2:8 - - - - Dx
                return self.set(1, Reg::A);
            }
            0xd0 => {
                // SET 2,B 2:8 - - - -
                return self.set(2, Reg::B);
            }
            0xd1 => {
                // SET 2,C 2:8 - - - -
                return self.set(2, Reg::C);
            }
            0xd2 => {
                // SET 2,D 2:8 - - - -
                return self.set(2, Reg::D);
            }
            0xd3 => {
                // SET 2,E 2:8 - - - -
                return self.set(2, Reg::E);
            }
            0xd4 => {
                // SET 2,H 2:8 - - - -
                return self.set(2, Reg::H);
            }
            0xd5 => {
                // SET 2,L 2:8 - - - -
                return self.set(2, Reg::L);
            }
            0xd6 => {
                // SET 2,(HL) 2:16 - - - -
                return self.set(2, Reg::HL);
            }
            0xd7 => {
                // SET 2,A 2:8 - - - -
                return self.set(2, Reg::A);
            }
            0xd8 => {
                // SET 3,B 2:8 - - - -
                return self.set(3, Reg::B);
            }
            0xd9 => {
                // SET 3,C 2:8 - - - -
                return self.set(3, Reg::C);
            }
            0xda => {
                // SET 3,D 2:8 - - - -
                return self.set(3, Reg::D);
            }
            0xdb => {
                // SET 3,E 2:8 - - - -
                return self.set(3, Reg::E);
            }
            0xdc => {
                // SET 3,H 2:8 - - - -
                return self.set(3, Reg::H);
            }
            0xdd => {
                // SET 3,L 2:8 - - - -
                return self.set(3, Reg::L);
            }
            0xde => {
                // SET 3,(HL) 2:16 - - - -
                return self.set(3, Reg::HL);
            }
            0xdf => {
                // SET 3,A 2:8 - - - - Ex
                return self.set(3, Reg::A);
            }
            0xe0 => {
                // SET 4,B 2:8 - - - -
                return self.set(4, Reg::B);
            }
            0xe1 => {
                // SET 4,C 2:8 - - - -
                return self.set(4, Reg::C);
            }
            0xe2 => {
                // SET 4,D 2:8 - - - -
                return self.set(4, Reg::D);
            }
            0xe3 => {
                // SET 4,E 2:8 - - - -
                return self.set(4, Reg::E);
            }
            0xe4 => {
                // SET 4,H 2:8 - - - -
                return self.set(4, Reg::H);
            }
            0xe5 => {
                // SET 4,L 2:8 - - - -
                return self.set(4, Reg::L);
            }
            0xe6 => {
                // SET 4,(HL) 2:16 - - - -
                return self.set(4, Reg::HL);
            }
            0xe7 => {
                // SET 4,A 2:8 - - - -
                return self.set(4, Reg::A);
            }
            0xe8 => {
                // SET 5,B 2:8 - - - -
                return self.set(5, Reg::B);
            }
            0xe9 => {
                // SET 5,C 2:8 - - - -
                return self.set(5, Reg::C);
            }
            0xea => {
                // SET 5,D 2:8 - - - -
                return self.set(5, Reg::D);
            }
            0xeb => {
                // SET 5,E 2:8 - - - -
                return self.set(5, Reg::E);
            }
            0xec => {
                // SET 5,H 2:8 - - - -
                return self.set(5, Reg::H);
            }
            0xed => {
                // SET 5,L 2:8 - - - -
                return self.set(5, Reg::L);
            }
            0xee => {
                // SET 5,(HL) 2:16 - - - -
                return self.set(5, Reg::HL);
            }
            0xef => {
                // SET 5,A 2:8 - - - - Fx
                return self.set(5, Reg::A);
            }
            0xf0 => {
                // SET 6,B 2:8 - - - -
                return self.set(6, Reg::B);
            }
            0xf1 => {
                // SET 6,C 2:8 - - - -
                return self.set(6, Reg::C);
            }
            0xf2 => {
                // SET 6,D 2:8 - - - -
                return self.set(6, Reg::D);
            }
            0xf3 => {
                // SET 6,E 2:8 - - - -
                return self.set(6, Reg::E);
            }
            0xf4 => {
                // SET 6,H 2:8 - - - -
                return self.set(6, Reg::H);
            }
            0xf5 => {
                // SET 6,L 2:8 - - - -
                return self.set(6, Reg::L);
            }
            0xf6 => {
                // SET 6,(HL) 2:16 - - - -
                return self.set(6, Reg::HL);
            }
            0xf7 => {
                // SET 6,A 2:8 - - - -
                return self.set(6, Reg::A);
            }
            0xf8 => {
                // SET 7,B 2:8 - - - -
                return self.set(7, Reg::B);
            }
            0xf9 => {
                // SET 7,C 2:8 - - - -
                return self.set(7, Reg::C);
            }
            0xfa => {
                // SET 7,D 2:8 - - - -
                return self.set(7, Reg::D);
            }
            0xfb => {
                // SET 7,E 2:8 - - - -
                return self.set(7, Reg::E);
            }
            0xfc => {
                // SET 7,H 2:8 - - - -
                return self.set(7, Reg::H);
            }
            0xfd => {
                // SET 7,L 2:8 - - - -
                return self.set(7, Reg::L);
            }
            0xfe => {
                // SET 7,(HL) 2:16 - - - -
                return self.set(7, Reg::HL);
            }
            0xff => {
                // SET 7,A 2:8 - - - -
                return self.set(7, Reg::A);
            }
        }
    }
}
