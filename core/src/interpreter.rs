use std::ops::ControlFlow;

use crate::{
    consts,
    disassembler::Address,
    gameboy::{
        cpu::{CpuState, ImeState},
        GameBoy,
    },
};

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Condition {
    None,
    Z,
    NZ,
    C,
    NC,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Reg {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    Im16,
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
pub struct Interpreter<'a>(pub &'a mut GameBoy);
impl Interpreter<'_> {
    pub fn interpret_op(&mut self) {
        if let ControlFlow::Break(_) = self.handle_interrupt() {
            return;
        }

        if self.0.cpu.ime == ImeState::ToBeEnable {
            self.0.cpu.ime = ImeState::Enabled;
        }

        if self.0.cpu.state != CpuState::Running {
            return;
        }

        use Condition::*;
        let op = self.read_next_pc();
        let trace = false;
        if trace {
            println!(
                "{:04x}: {:02x} {:04x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
                self.0.cpu.pc,
                op,
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
        match op {
            // NOP 1:4 - - - -
            0x00 => self.nop(),
            // LD BC,d16 3:12 - - - -
            0x01 => self.load16(Reg16::BC, Reg16::Im16),
            // LD (BC),A 1:8 - - - -
            0x02 => self.load(Reg::BC, Reg::A),
            // INC BC 1:8 - - - -
            0x03 => self.inc(Reg::BC),
            // INC B 1:4 Z 0 H -
            0x04 => self.inc(Reg::B),
            // DEC B 1:4 Z 1 H -
            0x05 => self.dec(Reg::B),
            // LD B,d8 2:8 - - - -
            0x06 => self.load(Reg::B, Reg::Im8),
            // RLCA 1:4 0 0 0 C
            0x07 => self.rlca(),
            // LD (a16),SP 3:20 - - - -
            0x08 => self.load16(Reg16::Im16, Reg16::SP),
            // ADD HL,BC 1:8 - 0 H C
            0x09 => self.add16(Reg16::BC),
            // LD A,(BC) 1:8 - - - -
            0x0a => self.load(Reg::A, Reg::BC),
            // DEC BC 1:8 - - - -
            0x0b => self.dec(Reg::BC),
            // INC C 1:4 Z 0 H -
            0x0c => self.inc(Reg::C),
            // DEC C 1:4 Z 1 H -
            0x0d => self.dec(Reg::C),
            // LD C,d8 2:8 - - - -
            0x0e => self.load(Reg::C, Reg::Im8),
            // RRCA 1:4 0 0 0 C
            0x0f => self.rrca(),
            // STOP 0 2:4 - - - -
            0x10 => self.stop(),
            // LD DE,d16 3:12 - - - -
            0x11 => self.load16(Reg16::DE, Reg16::Im16),
            // LD (DE),A 1:8 - - - -
            0x12 => self.load(Reg::DE, Reg::A),
            // INC DE 1:8 - - - -
            0x13 => self.inc(Reg::DE),
            // INC D 1:4 Z 0 H -
            0x14 => self.inc(Reg::D),
            // DEC D 1:4 Z 1 H -
            0x15 => self.dec(Reg::D),
            // LD D,d8 2:8 - - - -
            0x16 => self.load(Reg::D, Reg::Im8),
            // RLA 1:4 0 0 0 C
            0x17 => self.rla(),
            // JR r8 2:12 - - - -
            0x18 => self.jump_rel(None),
            // ADD HL,DE 1:8 - 0 H C
            0x19 => self.add16(Reg16::DE),
            // LD A,(DE) 1:8 - - - -
            0x1a => self.load(Reg::A, Reg::DE),
            // DEC DE 1:8 - - - -
            0x1b => self.dec(Reg::DE),
            // INC E 1:4 Z 0 H -
            0x1c => self.inc(Reg::E),
            // DEC E 1:4 Z 1 H -
            0x1d => self.dec(Reg::E),
            // LD E,d8 2:8 - - - -
            0x1e => self.load(Reg::E, Reg::Im8),
            // RRA 1:4 0 0 0 C
            0x1f => self.rra(),
            // JR NZ,r8 2:12/8 - - - -
            0x20 => self.jump_rel(NZ),
            // LD HL,d16 3:12 - - - -
            0x21 => self.load16(Reg16::HL, Reg16::Im16),
            // LD (HL+),A 1:8 - - - -
            0x22 => self.load(Reg::HLI, Reg::A),
            // INC HL 1:8 - - - -
            0x23 => self.inc(Reg::HL),
            // INC H 1:4 Z 0 H -
            0x24 => self.inc(Reg::H),
            // DEC H 1:4 Z 1 H -
            0x25 => self.dec(Reg::H),
            // LD H,d8 2:8 - - - -
            0x26 => self.load(Reg::H, Reg::Im8),
            // DAA 1:4 Z - 0 C
            0x27 => self.daa(),
            // JR Z,r8 2:12/8 - - - -
            0x28 => self.jump_rel(Z),
            // ADD HL,HL 1:8 - 0 H C
            0x29 => self.add16(Reg16::HL),
            // LD A,(HL+) 1:8 - - - -
            0x2a => self.load(Reg::A, Reg::HLI),
            // DEC HL 1:8 - - - -
            0x2b => self.dec(Reg::HL),
            // INC L 1:4 Z 0 H -
            0x2c => self.inc(Reg::L),
            // DEC L 1:4 Z 1 H -
            0x2d => self.dec(Reg::L),
            // LD L,d8 2:8 - - - -
            0x2e => self.load(Reg::L, Reg::Im8),
            // CPL 1:4 - 1 1 -
            0x2f => self.cpl(),
            // JR NC,r8 2:12/8 - - - -
            0x30 => self.jump_rel(NC),
            // LD SP,d16 3:12 - - - -
            0x31 => self.load16(Reg16::SP, Reg16::Im16),
            // LD (HL-),A 1:8 - - - -
            0x32 => self.load(Reg::HLD, Reg::A),
            // INC SP 1:8 - - - -
            0x33 => self.inc(Reg::SP),
            // INC (HL) 1:12 Z 0 H -
            0x34 => self.inc16(Reg::HL),
            // DEC (HL) 1:12 Z 1 H -
            0x35 => self.dec16(Reg::HL),
            // LD (HL),d8 2:12 - - - -
            0x36 => self.load(Reg::HL, Reg::Im8),
            // SCF 1:4 - 0 0 1
            0x37 => self.scf(),
            // JR C,r8 2:12/8 - - - -
            0x38 => self.jump_rel(C),
            // ADD HL,SP 1:8 - 0 H C
            0x39 => self.add16(Reg16::SP),
            // LD A,(HL-) 1:8 - - - -
            0x3a => self.load(Reg::A, Reg::HLD),
            // DEC SP 1:8 - - - -
            0x3b => self.dec(Reg::SP),
            // INC A 1:4 Z 0 H -
            0x3c => self.inc(Reg::A),
            // DEC A 1:4 Z 1 H -
            0x3d => self.dec(Reg::A),
            // LD A,d8 2:8 - - - -
            0x3e => self.load(Reg::A, Reg::Im8),
            // CCF 1:4 - 0 0 C
            0x3f => self.ccf(),
            // LD B,B 1:4 - - - -
            0x40 => self.load(Reg::B, Reg::B),
            // LD B,C 1:4 - - - -
            0x41 => self.load(Reg::B, Reg::C),
            // LD B,D 1:4 - - - -
            0x42 => self.load(Reg::B, Reg::D),
            // LD B,E 1:4 - - - -
            0x43 => self.load(Reg::B, Reg::E),
            // LD B,H 1:4 - - - -
            0x44 => self.load(Reg::B, Reg::H),
            // LD B,L 1:4 - - - -
            0x45 => self.load(Reg::B, Reg::L),
            // LD B,(HL) 1:8 - - - -
            0x46 => self.load(Reg::B, Reg::HL),
            // LD B,A 1:4 - - - -
            0x47 => self.load(Reg::B, Reg::A),
            // LD C,B 1:4 - - - -
            0x48 => self.load(Reg::C, Reg::B),
            // LD C,C 1:4 - - - -
            0x49 => self.load(Reg::C, Reg::C),
            // LD C,D 1:4 - - - -
            0x4a => self.load(Reg::C, Reg::D),
            // LD C,E 1:4 - - - -
            0x4b => self.load(Reg::C, Reg::E),
            // LD C,H 1:4 - - - -
            0x4c => self.load(Reg::C, Reg::H),
            // LD C,L 1:4 - - - -
            0x4d => self.load(Reg::C, Reg::L),
            // LD C,(HL) 1:8 - - - -
            0x4e => self.load(Reg::C, Reg::HL),
            // LD C,A 1:4 - - - -
            0x4f => self.load(Reg::C, Reg::A),
            // LD D,B 1:4 - - - -
            0x50 => self.load(Reg::D, Reg::B),
            // LD D,C 1:4 - - - -
            0x51 => self.load(Reg::D, Reg::C),
            // LD D,D 1:4 - - - -
            0x52 => self.load(Reg::D, Reg::D),
            // LD D,E 1:4 - - - -
            0x53 => self.load(Reg::D, Reg::E),
            // LD D,H 1:4 - - - -
            0x54 => self.load(Reg::D, Reg::H),
            // LD D,L 1:4 - - - -
            0x55 => self.load(Reg::D, Reg::L),
            // LD D,(HL) 1:8 - - - -
            0x56 => self.load(Reg::D, Reg::HL),
            // LD D,A 1:4 - - - -
            0x57 => self.load(Reg::D, Reg::A),
            // LD E,B 1:4 - - - -
            0x58 => self.load(Reg::E, Reg::B),
            // LD E,C 1:4 - - - -
            0x59 => self.load(Reg::E, Reg::C),
            // LD E,D 1:4 - - - -
            0x5a => self.load(Reg::E, Reg::D),
            // LD E,E 1:4 - - - -
            0x5b => self.load(Reg::E, Reg::E),
            // LD E,H 1:4 - - - -
            0x5c => self.load(Reg::E, Reg::H),
            // LD E,L 1:4 - - - -
            0x5d => self.load(Reg::E, Reg::L),
            // LD E,(HL) 1:8 - - - -
            0x5e => self.load(Reg::E, Reg::HL),
            // LD E,A 1:4 - - - -
            0x5f => self.load(Reg::E, Reg::A),
            // LD H,B 1:4 - - - -
            0x60 => self.load(Reg::H, Reg::B),
            // LD H,C 1:4 - - - -
            0x61 => self.load(Reg::H, Reg::C),
            // LD H,D 1:4 - - - -
            0x62 => self.load(Reg::H, Reg::D),
            // LD H,E 1:4 - - - -
            0x63 => self.load(Reg::H, Reg::E),
            // LD H,H 1:4 - - - -
            0x64 => self.load(Reg::H, Reg::H),
            // LD H,L 1:4 - - - -
            0x65 => self.load(Reg::H, Reg::L),
            // LD H,(HL) 1:8 - - - -
            0x66 => self.load(Reg::H, Reg::HL),
            // LD H,A 1:4 - - - -
            0x67 => self.load(Reg::H, Reg::A),
            // LD L,B 1:4 - - - -
            0x68 => self.load(Reg::L, Reg::B),
            // LD L,C 1:4 - - - -
            0x69 => self.load(Reg::L, Reg::C),
            // LD L,D 1:4 - - - -
            0x6a => self.load(Reg::L, Reg::D),
            // LD L,E 1:4 - - - -
            0x6b => self.load(Reg::L, Reg::E),
            // LD L,H 1:4 - - - -
            0x6c => self.load(Reg::L, Reg::H),
            // LD L,L 1:4 - - - -
            0x6d => self.load(Reg::L, Reg::L),
            // LD L,(HL) 1:8 - - - -
            0x6e => self.load(Reg::L, Reg::HL),
            // LD L,A 1:4 - - - -
            0x6f => self.load(Reg::L, Reg::A),
            // LD (HL),B 1:8 - - - -
            0x70 => self.load(Reg::HL, Reg::B),
            // LD (HL),C 1:8 - - - -
            0x71 => self.load(Reg::HL, Reg::C),
            // LD (HL),D 1:8 - - - -
            0x72 => self.load(Reg::HL, Reg::D),
            // LD (HL),E 1:8 - - - -
            0x73 => self.load(Reg::HL, Reg::E),
            // LD (HL),H 1:8 - - - -
            0x74 => self.load(Reg::HL, Reg::H),
            // LD (HL),L 1:8 - - - -
            0x75 => self.load(Reg::HL, Reg::L),
            // HALT 1:4 - - - -
            0x76 => self.halt(),
            // LD (HL),A 1:8 - - - -
            0x77 => self.load(Reg::HL, Reg::A),
            // LD A,B 1:4 - - - -
            0x78 => self.load(Reg::A, Reg::B),
            // LD A,C 1:4 - - - -
            0x79 => self.load(Reg::A, Reg::C),
            // LD A,D 1:4 - - - -
            0x7a => self.load(Reg::A, Reg::D),
            // LD A,E 1:4 - - - -
            0x7b => self.load(Reg::A, Reg::E),
            // LD A,H 1:4 - - - -
            0x7c => self.load(Reg::A, Reg::H),
            // LD A,L 1:4 - - - -
            0x7d => self.load(Reg::A, Reg::L),
            // LD A,(HL) 1:8 - - - -
            0x7e => self.load(Reg::A, Reg::HL),
            // LD A,A 1:4 - - - -
            0x7f => self.load(Reg::A, Reg::A),
            // ADD A,B 1:4 Z 0 H C
            0x80 => self.add(Reg::B),
            // ADD A,C 1:4 Z 0 H C
            0x81 => self.add(Reg::C),
            // ADD A,D 1:4 Z 0 H C
            0x82 => self.add(Reg::D),
            // ADD A,E 1:4 Z 0 H C
            0x83 => self.add(Reg::E),
            // ADD A,H 1:4 Z 0 H C
            0x84 => self.add(Reg::H),
            // ADD A,L 1:4 Z 0 H C
            0x85 => self.add(Reg::L),
            // ADD A,(HL) 1:8 Z 0 H C
            0x86 => self.add(Reg::HL),
            // ADD A,A 1:4 Z 0 H C
            0x87 => self.add(Reg::A),
            // ADC A,B 1:4 Z 0 H C
            0x88 => self.adc(Reg::B),
            // ADC A,C 1:4 Z 0 H C
            0x89 => self.adc(Reg::C),
            // ADC A,D 1:4 Z 0 H C
            0x8a => self.adc(Reg::D),
            // ADC A,E 1:4 Z 0 H C
            0x8b => self.adc(Reg::E),
            // ADC A,H 1:4 Z 0 H C
            0x8c => self.adc(Reg::H),
            // ADC A,L 1:4 Z 0 H C
            0x8d => self.adc(Reg::L),
            // ADC A,(HL) 1:8 Z 0 H C
            0x8e => self.adc(Reg::HL),
            // ADC A,A 1:4 Z 0 H C
            0x8f => self.adc(Reg::A),
            // SUB B 1:4 Z 1 H C
            0x90 => self.sub(Reg::B),
            // SUB C 1:4 Z 1 H C
            0x91 => self.sub(Reg::C),
            // SUB D 1:4 Z 1 H C
            0x92 => self.sub(Reg::D),
            // SUB E 1:4 Z 1 H C
            0x93 => self.sub(Reg::E),
            // SUB H 1:4 Z 1 H C
            0x94 => self.sub(Reg::H),
            // SUB L 1:4 Z 1 H C
            0x95 => self.sub(Reg::L),
            // SUB (HL) 1:8 Z 1 H C
            0x96 => self.sub(Reg::HL),
            // SUB A 1:4 Z 1 H C
            0x97 => self.sub(Reg::A),
            // SBC A,B 1:4 Z 1 H C
            0x98 => self.sbc(Reg::B),
            // SBC A,C 1:4 Z 1 H C
            0x99 => self.sbc(Reg::C),
            // SBC A,D 1:4 Z 1 H C
            0x9a => self.sbc(Reg::D),
            // SBC A,E 1:4 Z 1 H C
            0x9b => self.sbc(Reg::E),
            // SBC A,H 1:4 Z 1 H C
            0x9c => self.sbc(Reg::H),
            // SBC A,L 1:4 Z 1 H C
            0x9d => self.sbc(Reg::L),
            // SBC A,(HL) 1:8 Z 1 H C
            0x9e => self.sbc(Reg::HL),
            // SBC A,A 1:4 Z 1 H C
            0x9f => self.sbc(Reg::A),
            // AND B 1:4 Z 0 1 0
            0xa0 => self.and(Reg::B),
            // AND C 1:4 Z 0 1 0
            0xa1 => self.and(Reg::C),
            // AND D 1:4 Z 0 1 0
            0xa2 => self.and(Reg::D),
            // AND E 1:4 Z 0 1 0
            0xa3 => self.and(Reg::E),
            // AND H 1:4 Z 0 1 0
            0xa4 => self.and(Reg::H),
            // AND L 1:4 Z 0 1 0
            0xa5 => self.and(Reg::L),
            // AND (HL) 1:8 Z 0 1 0
            0xa6 => self.and(Reg::HL),
            // AND A 1:4 Z 0 1 0
            0xa7 => self.and(Reg::A),
            // XOR B 1:4 Z 0 0 0
            0xa8 => self.xor(Reg::B),
            // XOR C 1:4 Z 0 0 0
            0xa9 => self.xor(Reg::C),
            // XOR D 1:4 Z 0 0 0
            0xaa => self.xor(Reg::D),
            // XOR E 1:4 Z 0 0 0
            0xab => self.xor(Reg::E),
            // XOR H 1:4 Z 0 0 0
            0xac => self.xor(Reg::H),
            // XOR L 1:4 Z 0 0 0
            0xad => self.xor(Reg::L),
            // XOR (HL) 1:8 Z 0 0 0
            0xae => self.xor(Reg::HL),
            // XOR A 1:4 Z 0 0 0
            0xaf => self.xor(Reg::A),
            // OR B 1:4 Z 0 0 0
            0xb0 => self.or(Reg::B),
            // OR C 1:4 Z 0 0 0
            0xb1 => self.or(Reg::C),
            // OR D 1:4 Z 0 0 0
            0xb2 => self.or(Reg::D),
            // OR E 1:4 Z 0 0 0
            0xb3 => self.or(Reg::E),
            // OR H 1:4 Z 0 0 0
            0xb4 => self.or(Reg::H),
            // OR L 1:4 Z 0 0 0
            0xb5 => self.or(Reg::L),
            // OR (HL) 1:8 Z 0 0 0
            0xb6 => self.or(Reg::HL),
            // OR A 1:4 Z 0 0 0
            0xb7 => self.or(Reg::A),
            // CP B 1:4 Z 1 H C
            0xb8 => self.cp(Reg::B),
            // CP C 1:4 Z 1 H C
            0xb9 => self.cp(Reg::C),
            // CP D 1:4 Z 1 H C
            0xba => self.cp(Reg::D),
            // CP E 1:4 Z 1 H C
            0xbb => self.cp(Reg::E),
            // CP H 1:4 Z 1 H C
            0xbc => self.cp(Reg::H),
            // CP L 1:4 Z 1 H C
            0xbd => self.cp(Reg::L),
            // CP (HL) 1:8 Z 1 H C
            0xbe => self.cp(Reg::HL),
            // CP A 1:4 Z 1 H C
            0xbf => self.cp(Reg::A),
            // RET NZ 1:20/8 - - - -
            0xc0 => self.ret(NZ),
            // POP BC 1:12 - - - -
            0xc1 => self.pop(Reg16::BC),
            // JP NZ,a16 3:16/12 - - - -
            0xc2 => self.jump(NZ),
            // JP a16 3:16 - - - -
            0xc3 => self.jump(None),
            // CALL NZ,a16 3:24/12 - - - -
            0xc4 => self.call(NZ),
            // PUSH BC 1:16 - - - -
            0xc5 => self.push(Reg16::BC),
            // ADD A,d8 2:8 Z 0 H C
            0xc6 => self.add(Reg::Im8),
            // RST 00H 1:16 - - - -
            0xc7 => self.rst(0x00),
            // RET Z 1:20/8 - - - -
            0xc8 => self.ret(Z),
            // RET 1:16 - - - -
            0xc9 => self.ret(None),
            // JP Z,a16 3:16/12 - - - -
            0xca => self.jump(Z),
            // PREFIX CB 1:4 - - - -
            0xcb => self.interpret_op_cb(),
            // CALL Z,a16 3:24/12 - - - -
            0xcc => self.call(Z),
            // CALL a16 3:24 - - - -
            0xcd => self.call(None),
            // ADC A,d8 2:8 Z 0 H C
            0xce => self.adc(Reg::Im8),
            // RST 08H 1:16 - - - -
            0xcf => self.rst(0x08),
            // RET NC 1:20/8 - - - -
            0xd0 => self.ret(NC),
            // POP DE 1:12 - - - -
            0xd1 => self.pop(Reg16::DE),
            // JP NC,a16 3:16/12 - - - -
            0xd2 => self.jump(NC),
            //
            0xd3 => self.invalid_opcode(op),
            // CALL NC,a16 3:24/12 - - - -
            0xd4 => self.call(NC),
            // PUSH DE 1:16 - - - -
            0xd5 => self.push(Reg16::DE),
            // SUB d8 2:8 Z 1 H C
            0xd6 => self.sub(Reg::Im8),
            // RST 10H 1:16 - - - -
            0xd7 => self.rst(0x10),
            // RET C 1:20/8 - - - -
            0xd8 => self.ret(C),
            // RETI 1:16 - - - -
            0xd9 => self.reti(),
            // JP C,a16 3:16/12 - - - -
            0xda => self.jump(C),
            //
            0xdb => self.invalid_opcode(op),
            // CALL C,a16 3:24/12 - - - -
            0xdc => self.call(C),
            //
            0xdd => self.invalid_opcode(op),
            // SBC A,d8 2:8 Z 1 H C
            0xde => self.sbc(Reg::Im8),
            // RST 18H 1:16 - - - -
            0xdf => self.rst(0x18),
            // LDH (a8),A 2:12 - - - -
            0xe0 => self.loadh(Reg::Im8, Reg::A),
            // POP HL 1:12 - - - -
            0xe1 => self.pop(Reg16::HL),
            // LD (C),A 2:8 - - - -
            0xe2 => self.loadh(Reg::C, Reg::A),
            //
            0xe3 => self.invalid_opcode(op),
            //
            0xe4 => self.invalid_opcode(op),
            // PUSH HL 1:16 - - - -
            0xe5 => self.push(Reg16::HL),
            // AND d8 2:8 Z 0 1 0
            0xe6 => self.and(Reg::Im8),
            // RST 20H 1:16 - - - -
            0xe7 => self.rst(0x20),
            // ADD SP,r8 2:16 0 0 H C
            0xe8 => self.add_sp(),
            // JP HL 1:4 - - - -
            0xe9 => self.jump_hl(),
            // LD (a16),A 3:16 - - - -
            0xea => self.load(Reg::Im16, Reg::A),
            //
            0xeb => self.invalid_opcode(op),
            //
            0xec => self.invalid_opcode(op),
            //
            0xed => self.invalid_opcode(op),
            // XOR d8 2:8 Z 0 0 0
            0xee => self.xor(Reg::Im8),
            // RST 28H 1:16 - - - -
            0xef => self.rst(0x28),
            // LDH A,(a8) 2:12 - - - -
            0xf0 => self.loadh(Reg::A, Reg::Im8),
            // POP AF 1:12 Z N H C
            0xf1 => self.pop(Reg16::AF),
            // LD A,(C) 2:8 - - - -
            0xf2 => self.loadh(Reg::A, Reg::C),
            // DI 1:4 - - - -
            0xf3 => self.di(),
            //
            0xf4 => self.invalid_opcode(op),
            // PUSH AF 1:16 - - - -
            0xf5 => self.push(Reg16::AF),
            // OR d8 2:8 Z 0 0 0
            0xf6 => self.or(Reg::Im8),
            // RST 30H 1:16 - - - -
            0xf7 => self.rst(0x30),
            // LD HL,SP+r8 2:12 0 0 H C
            0xf8 => self.ldhl_sp(),
            // LD SP,HL 1:8 - - - -
            0xf9 => self.load16(Reg16::SP, Reg16::HL),
            // LD A,(a16) 3:16 - - - -
            0xfa => self.load(Reg::A, Reg::Im16),
            // EI 1:4 - - - -
            0xfb => self.ei(),
            //
            0xfc => self.invalid_opcode(op),
            //
            0xfd => self.invalid_opcode(op),
            // CP d8 2:8 Z 1 H C
            0xfe => self.cp(Reg::Im8),
            // RST 38H 1:16 - - - -
            0xff => self.rst(0x38),
        }
    }

    pub fn handle_interrupt(&mut self) -> ControlFlow<()> {
        self.0.update_interrupt();

        if self.0.v_blank_trigger.get() {
            self.0.v_blank_trigger.set(false);
            self.0.call_v_blank_callback();
        }

        if self.0.cpu.state == CpuState::Halt {
            let mut until_interrupt = self
                .0
                .next_interrupt
                .get()
                .saturating_sub(self.0.clock_count);

            // TODO: ideally, this should be aware of the timeout of the main loop.
            if until_interrupt > consts::CLOCK_SPEED / 60 {
                until_interrupt = consts::CLOCK_SPEED / 60;
            }

            until_interrupt &= !0b11;
            self.0.tick(until_interrupt + 2);
        }

        self.0.update_interrupt();
        let interrupts: u8 = self.0.interrupt_flag.get() & self.0.interrupt_enabled;

        if self.0.cpu.state == CpuState::Halt {
            self.0.tick(2);
        }

        // TODO: I don't know the behaviour of Stopped state. Treating the same as Halt.
        if self.0.cpu.state == CpuState::Stopped {
            self.0.tick(2);
        }

        if interrupts != 0 {
            self.0.cpu.state = CpuState::Running;

            if self.0.cpu.ime == ImeState::Enabled {
                self.0.cpu.ime = ImeState::Disabled;
                self.0.tick(4);
                let mut interrupt = 8;
                let mut address = 0x00;
                // The push could overwrite IE, canceling the jump, but only it first write. PC is
                // set to 0x0000 instead.
                {
                    let value = self.0.cpu.pc;
                    let [lsb, msb] = value.to_le_bytes();
                    self.0.tick(4); // 1 M-cycle with SP in address buss
                    self.0.write(sub16(self.0.cpu.sp, 1), msb);

                    // NOTE: In this code we are doing two writes, and between them checking for
                    // interrupts. However write to PPU have some weird timings (involving
                    // time-travel), that makes necessary to always be some ticks between a write
                    // and a ppu update.
                    // As I don't know the precise timing of the interrupt read, I am updating the
                    // interrupt (that in turn update the ppu) 2 cycles after the last write, and 2
                    // cycles before the next one. This is enough to satify the weird timing needs.
                    self.0.tick(2); // 1 M-cycle with SP-1 in address buss

                    self.0.update_interrupt();
                    if self.0.interrupt_flag.get() & self.0.interrupt_enabled != 0 {
                        interrupt = (self.0.interrupt_flag.get() & self.0.interrupt_enabled)
                            .trailing_zeros() as usize;
                        address = [
                            0x40, // V-Blank
                            0x48, // STAT
                            0x50, // Timer
                            0x58, // Serial
                            0x60, // Joypad
                        ][interrupt];
                    }

                    self.0.tick(2); // 1 M-cycle with SP-1 in address buss

                    self.0.write(sub16(self.0.cpu.sp, 2), lsb);
                    self.0.tick(4); // 1 M-cycle with SP-2 in address buss
                    self.0.cpu.sp = sub16(self.0.cpu.sp, 2);
                };

                if interrupt != 8 {
                    self.0.update_interrupt();
                    self.0
                        .interrupt_flag
                        .set(self.0.interrupt_flag.get() & !(1 << interrupt));
                    self.jump_to(address);
                } else {
                    self.jump_to(0x0000);
                }
                self.0.tick(4);

                // return, to allow detecting the interrupt
                return ControlFlow::Break(());
            }
        }
        ControlFlow::Continue(())
    }

    pub fn interpret_op_cb(&mut self) {
        let op = self.read_next_pc();
        match op {
            // RLC B 2:8 Z 0 0 C
            0x00 => self.rlc(Reg::B),
            // RLC C 2:8 Z 0 0 C
            0x01 => self.rlc(Reg::C),
            // RLC D 2:8 Z 0 0 C
            0x02 => self.rlc(Reg::D),
            // RLC E 2:8 Z 0 0 C
            0x03 => self.rlc(Reg::E),
            // RLC H 2:8 Z 0 0 C
            0x04 => self.rlc(Reg::H),
            // RLC L 2:8 Z 0 0 C
            0x05 => self.rlc(Reg::L),
            // RLC (HL) 2:16 Z 0 0 C
            0x06 => self.rlc(Reg::HL),
            // RLC A 2:8 Z 0 0 C
            0x07 => self.rlc(Reg::A),
            // RRC B 2:8 Z 0 0 C
            0x08 => self.rrc(Reg::B),
            // RRC C 2:8 Z 0 0 C
            0x09 => self.rrc(Reg::C),
            // RRC D 2:8 Z 0 0 C
            0x0a => self.rrc(Reg::D),
            // RRC E 2:8 Z 0 0 C
            0x0b => self.rrc(Reg::E),
            // RRC H 2:8 Z 0 0 C
            0x0c => self.rrc(Reg::H),
            // RRC L 2:8 Z 0 0 C
            0x0d => self.rrc(Reg::L),
            // RRC (HL) 2:16 Z 0 0 C
            0x0e => self.rrc(Reg::HL),
            // RRC A 2:8 Z 0 0 C
            0x0f => self.rrc(Reg::A),
            // RL B 2:8 Z 0 0 C
            0x10 => self.rl(Reg::B),
            // RL C 2:8 Z 0 0 C
            0x11 => self.rl(Reg::C),
            // RL D 2:8 Z 0 0 C
            0x12 => self.rl(Reg::D),
            // RL E 2:8 Z 0 0 C
            0x13 => self.rl(Reg::E),
            // RL H 2:8 Z 0 0 C
            0x14 => self.rl(Reg::H),
            // RL L 2:8 Z 0 0 C
            0x15 => self.rl(Reg::L),
            // RL (HL) 2:16 Z 0 0 C
            0x16 => self.rl(Reg::HL),
            // RL A 2:8 Z 0 0 C
            0x17 => self.rl(Reg::A),
            // RR B 2:8 Z 0 0 C
            0x18 => self.rr(Reg::B),
            // RR C 2:8 Z 0 0 C
            0x19 => self.rr(Reg::C),
            // RR D 2:8 Z 0 0 C
            0x1a => self.rr(Reg::D),
            // RR E 2:8 Z 0 0 C
            0x1b => self.rr(Reg::E),
            // RR H 2:8 Z 0 0 C
            0x1c => self.rr(Reg::H),
            // RR L 2:8 Z 0 0 C
            0x1d => self.rr(Reg::L),
            // RR (HL) 2:16 Z 0 0 C
            0x1e => self.rr(Reg::HL),
            // RR A 2:8 Z 0 0 C
            0x1f => self.rr(Reg::A),
            // SLA B 2:8 Z 0 0 C
            0x20 => self.sla(Reg::B),
            // SLA C 2:8 Z 0 0 C
            0x21 => self.sla(Reg::C),
            // SLA D 2:8 Z 0 0 C
            0x22 => self.sla(Reg::D),
            // SLA E 2:8 Z 0 0 C
            0x23 => self.sla(Reg::E),
            // SLA H 2:8 Z 0 0 C
            0x24 => self.sla(Reg::H),
            // SLA L 2:8 Z 0 0 C
            0x25 => self.sla(Reg::L),
            // SLA (HL) 2:16 Z 0 0 C
            0x26 => self.sla(Reg::HL),
            // SLA A 2:8 Z 0 0 C
            0x27 => self.sla(Reg::A),
            // SRA B 2:8 Z 0 0 0
            0x28 => self.sra(Reg::B),
            // SRA C 2:8 Z 0 0 0
            0x29 => self.sra(Reg::C),
            // SRA D 2:8 Z 0 0 0
            0x2a => self.sra(Reg::D),
            // SRA E 2:8 Z 0 0 0
            0x2b => self.sra(Reg::E),
            // SRA H 2:8 Z 0 0 0
            0x2c => self.sra(Reg::H),
            // SRA L 2:8 Z 0 0 0
            0x2d => self.sra(Reg::L),
            // SRA (HL) 2:16 Z 0 0 0
            0x2e => self.sra(Reg::HL),
            // SRA A 2:8 Z 0 0 0
            0x2f => self.sra(Reg::A),
            // SWAP B 2:8 Z 0 0 0
            0x30 => self.swap(Reg::B),
            // SWAP C 2:8 Z 0 0 0
            0x31 => self.swap(Reg::C),
            // SWAP D 2:8 Z 0 0 0
            0x32 => self.swap(Reg::D),
            // SWAP E 2:8 Z 0 0 0
            0x33 => self.swap(Reg::E),
            // SWAP H 2:8 Z 0 0 0
            0x34 => self.swap(Reg::H),
            // SWAP L 2:8 Z 0 0 0
            0x35 => self.swap(Reg::L),
            // SWAP (HL) 2:16 Z 0 0 0
            0x36 => self.swap(Reg::HL),
            // SWAP A 2:8 Z 0 0 0
            0x37 => self.swap(Reg::A),
            // SRL B 2:8 Z 0 0 C
            0x38 => self.srl(Reg::B),
            // SRL C 2:8 Z 0 0 C
            0x39 => self.srl(Reg::C),
            // SRL D 2:8 Z 0 0 C
            0x3a => self.srl(Reg::D),
            // SRL E 2:8 Z 0 0 C
            0x3b => self.srl(Reg::E),
            // SRL H 2:8 Z 0 0 C
            0x3c => self.srl(Reg::H),
            // SRL L 2:8 Z 0 0 C
            0x3d => self.srl(Reg::L),
            // SRL (HL) 2:16 Z 0 0 C
            0x3e => self.srl(Reg::HL),
            // SRL A 2:8 Z 0 0 C
            0x3f => self.srl(Reg::A),
            // BIT 0,B 2:8 Z 0 1 -
            0x40 => self.bit(0, Reg::B),
            // BIT 0,C 2:8 Z 0 1 -
            0x41 => self.bit(0, Reg::C),
            // BIT 0,D 2:8 Z 0 1 -
            0x42 => self.bit(0, Reg::D),
            // BIT 0,E 2:8 Z 0 1 -
            0x43 => self.bit(0, Reg::E),
            // BIT 0,H 2:8 Z 0 1 -
            0x44 => self.bit(0, Reg::H),
            // BIT 0,L 2:8 Z 0 1 -
            0x45 => self.bit(0, Reg::L),
            // BIT 0,(HL) 2:16 Z 0 1 -
            0x46 => self.bit(0, Reg::HL),
            // BIT 0,A 2:8 Z 0 1 -
            0x47 => self.bit(0, Reg::A),
            // BIT 1,B 2:8 Z 0 1 -
            0x48 => self.bit(1, Reg::B),
            // BIT 1,C 2:8 Z 0 1 -
            0x49 => self.bit(1, Reg::C),
            // BIT 1,D 2:8 Z 0 1 -
            0x4a => self.bit(1, Reg::D),
            // BIT 1,E 2:8 Z 0 1 -
            0x4b => self.bit(1, Reg::E),
            // BIT 1,H 2:8 Z 0 1 -
            0x4c => self.bit(1, Reg::H),
            // BIT 1,L 2:8 Z 0 1 -
            0x4d => self.bit(1, Reg::L),
            // BIT 1,(HL) 2:16 Z 0 1 -
            0x4e => self.bit(1, Reg::HL),
            // BIT 1,A 2:8 Z 0 1 -
            0x4f => self.bit(1, Reg::A),
            // BIT 2,B 2:8 Z 0 1 -
            0x50 => self.bit(2, Reg::B),
            // BIT 2,C 2:8 Z 0 1 -
            0x51 => self.bit(2, Reg::C),
            // BIT 2,D 2:8 Z 0 1 -
            0x52 => self.bit(2, Reg::D),
            // BIT 2,E 2:8 Z 0 1 -
            0x53 => self.bit(2, Reg::E),
            // BIT 2,H 2:8 Z 0 1 -
            0x54 => self.bit(2, Reg::H),
            // BIT 2,L 2:8 Z 0 1 -
            0x55 => self.bit(2, Reg::L),
            // BIT 2,(HL) 2:16 Z 0 1 -
            0x56 => self.bit(2, Reg::HL),
            // BIT 2,A 2:8 Z 0 1 -
            0x57 => self.bit(2, Reg::A),
            // BIT 3,B 2:8 Z 0 1 -
            0x58 => self.bit(3, Reg::B),
            // BIT 3,C 2:8 Z 0 1 -
            0x59 => self.bit(3, Reg::C),
            // BIT 3,D 2:8 Z 0 1 -
            0x5a => self.bit(3, Reg::D),
            // BIT 3,E 2:8 Z 0 1 -
            0x5b => self.bit(3, Reg::E),
            // BIT 3,H 2:8 Z 0 1 -
            0x5c => self.bit(3, Reg::H),
            // BIT 3,L 2:8 Z 0 1 -
            0x5d => self.bit(3, Reg::L),
            // BIT 3,(HL) 2:16 Z 0 1 -
            0x5e => self.bit(3, Reg::HL),
            // BIT 3,A 2:8 Z 0 1 -
            0x5f => self.bit(3, Reg::A),
            // BIT 4,B 2:8 Z 0 1 -
            0x60 => self.bit(4, Reg::B),
            // BIT 4,C 2:8 Z 0 1 -
            0x61 => self.bit(4, Reg::C),
            // BIT 4,D 2:8 Z 0 1 -
            0x62 => self.bit(4, Reg::D),
            // BIT 4,E 2:8 Z 0 1 -
            0x63 => self.bit(4, Reg::E),
            // BIT 4,H 2:8 Z 0 1 -
            0x64 => self.bit(4, Reg::H),
            // BIT 4,L 2:8 Z 0 1 -
            0x65 => self.bit(4, Reg::L),
            // BIT 4,(HL) 2:16 Z 0 1 -
            0x66 => self.bit(4, Reg::HL),
            // BIT 4,A 2:8 Z 0 1 -
            0x67 => self.bit(4, Reg::A),
            // BIT 5,B 2:8 Z 0 1 -
            0x68 => self.bit(5, Reg::B),
            // BIT 5,C 2:8 Z 0 1 -
            0x69 => self.bit(5, Reg::C),
            // BIT 5,D 2:8 Z 0 1 -
            0x6a => self.bit(5, Reg::D),
            // BIT 5,E 2:8 Z 0 1 -
            0x6b => self.bit(5, Reg::E),
            // BIT 5,H 2:8 Z 0 1 -
            0x6c => self.bit(5, Reg::H),
            // BIT 5,L 2:8 Z 0 1 -
            0x6d => self.bit(5, Reg::L),
            // BIT 5,(HL) 2:16 Z 0 1 -
            0x6e => self.bit(5, Reg::HL),
            // BIT 5,A 2:8 Z 0 1 -
            0x6f => self.bit(5, Reg::A),
            // BIT 6,B 2:8 Z 0 1 -
            0x70 => self.bit(6, Reg::B),
            // BIT 6,C 2:8 Z 0 1 -
            0x71 => self.bit(6, Reg::C),
            // BIT 6,D 2:8 Z 0 1 -
            0x72 => self.bit(6, Reg::D),
            // BIT 6,E 2:8 Z 0 1 -
            0x73 => self.bit(6, Reg::E),
            // BIT 6,H 2:8 Z 0 1 -
            0x74 => self.bit(6, Reg::H),
            // BIT 6,L 2:8 Z 0 1 -
            0x75 => self.bit(6, Reg::L),
            // BIT 6,(HL) 2:16 Z 0 1 -
            0x76 => self.bit(6, Reg::HL),
            // BIT 6,A 2:8 Z 0 1 -
            0x77 => self.bit(6, Reg::A),
            // BIT 7,B 2:8 Z 0 1 -
            0x78 => self.bit(7, Reg::B),
            // BIT 7,C 2:8 Z 0 1 -
            0x79 => self.bit(7, Reg::C),
            // BIT 7,D 2:8 Z 0 1 -
            0x7a => self.bit(7, Reg::D),
            // BIT 7,E 2:8 Z 0 1 -
            0x7b => self.bit(7, Reg::E),
            // BIT 7,H 2:8 Z 0 1 -
            0x7c => self.bit(7, Reg::H),
            // BIT 7,L 2:8 Z 0 1 -
            0x7d => self.bit(7, Reg::L),
            // BIT 7,(HL) 2:16 Z 0 1 -
            0x7e => self.bit(7, Reg::HL),
            // BIT 7,A 2:8 Z 0 1 -
            0x7f => self.bit(7, Reg::A),
            // RES 0,B 2:8 - - - -
            0x80 => self.res(0, Reg::B),
            // RES 0,C 2:8 - - - -
            0x81 => self.res(0, Reg::C),
            // RES 0,D 2:8 - - - -
            0x82 => self.res(0, Reg::D),
            // RES 0,E 2:8 - - - -
            0x83 => self.res(0, Reg::E),
            // RES 0,H 2:8 - - - -
            0x84 => self.res(0, Reg::H),
            // RES 0,L 2:8 - - - -
            0x85 => self.res(0, Reg::L),
            // RES 0,(HL) 2:16 - - - -
            0x86 => self.res(0, Reg::HL),
            // RES 0,A 2:8 - - - -
            0x87 => self.res(0, Reg::A),
            // RES 1,B 2:8 - - - -
            0x88 => self.res(1, Reg::B),
            // RES 1,C 2:8 - - - -
            0x89 => self.res(1, Reg::C),
            // RES 1,D 2:8 - - - -
            0x8a => self.res(1, Reg::D),
            // RES 1,E 2:8 - - - -
            0x8b => self.res(1, Reg::E),
            // RES 1,H 2:8 - - - -
            0x8c => self.res(1, Reg::H),
            // RES 1,L 2:8 - - - -
            0x8d => self.res(1, Reg::L),
            // RES 1,(HL) 2:16 - - - -
            0x8e => self.res(1, Reg::HL),
            // RES 1,A 2:8 - - - -
            0x8f => self.res(1, Reg::A),
            // RES 2,B 2:8 - - - -
            0x90 => self.res(2, Reg::B),
            // RES 2,C 2:8 - - - -
            0x91 => self.res(2, Reg::C),
            // RES 2,D 2:8 - - - -
            0x92 => self.res(2, Reg::D),
            // RES 2,E 2:8 - - - -
            0x93 => self.res(2, Reg::E),
            // RES 2,H 2:8 - - - -
            0x94 => self.res(2, Reg::H),
            // RES 2,L 2:8 - - - -
            0x95 => self.res(2, Reg::L),
            // RES 2,(HL) 2:16 - - - -
            0x96 => self.res(2, Reg::HL),
            // RES 2,A 2:8 - - - -
            0x97 => self.res(2, Reg::A),
            // RES 3,B 2:8 - - - -
            0x98 => self.res(3, Reg::B),
            // RES 3,C 2:8 - - - -
            0x99 => self.res(3, Reg::C),
            // RES 3,D 2:8 - - - -
            0x9a => self.res(3, Reg::D),
            // RES 3,E 2:8 - - - -
            0x9b => self.res(3, Reg::E),
            // RES 3,H 2:8 - - - -
            0x9c => self.res(3, Reg::H),
            // RES 3,L 2:8 - - - -
            0x9d => self.res(3, Reg::L),
            // RES 3,(HL) 2:16 - - - -
            0x9e => self.res(3, Reg::HL),
            // RES 3,A 2:8 - - - - Ax
            0x9f => self.res(3, Reg::A),
            // RES 4,B 2:8 - - - -
            0xa0 => self.res(4, Reg::B),
            // RES 4,C 2:8 - - - -
            0xa1 => self.res(4, Reg::C),
            // RES 4,D 2:8 - - - -
            0xa2 => self.res(4, Reg::D),
            // RES 4,E 2:8 - - - -
            0xa3 => self.res(4, Reg::E),
            // RES 4,H 2:8 - - - -
            0xa4 => self.res(4, Reg::H),
            // RES 4,L 2:8 - - - -
            0xa5 => self.res(4, Reg::L),
            // RES 4,(HL) 2:16 - - - -
            0xa6 => self.res(4, Reg::HL),
            // RES 4,A 2:8 - - - -
            0xa7 => self.res(4, Reg::A),
            // RES 5,B 2:8 - - - -
            0xa8 => self.res(5, Reg::B),
            // RES 5,C 2:8 - - - -
            0xa9 => self.res(5, Reg::C),
            // RES 5,D 2:8 - - - -
            0xaa => self.res(5, Reg::D),
            // RES 5,E 2:8 - - - -
            0xab => self.res(5, Reg::E),
            // RES 5,H 2:8 - - - -
            0xac => self.res(5, Reg::H),
            // RES 5,L 2:8 - - - -
            0xad => self.res(5, Reg::L),
            // RES 5,(HL) 2:16 - - - -
            0xae => self.res(5, Reg::HL),
            // RES 5,A 2:8 - - - - Bx
            0xaf => self.res(5, Reg::A),
            // RES 6,B 2:8 - - - -
            0xb0 => self.res(6, Reg::B),
            // RES 6,C 2:8 - - - -
            0xb1 => self.res(6, Reg::C),
            // RES 6,D 2:8 - - - -
            0xb2 => self.res(6, Reg::D),
            // RES 6,E 2:8 - - - -
            0xb3 => self.res(6, Reg::E),
            // RES 6,H 2:8 - - - -
            0xb4 => self.res(6, Reg::H),
            // RES 6,L 2:8 - - - -
            0xb5 => self.res(6, Reg::L),
            // RES 6,(HL) 2:16 - - - -
            0xb6 => self.res(6, Reg::HL),
            // RES 6,A 2:8 - - - -
            0xb7 => self.res(6, Reg::A),
            // RES 7,B 2:8 - - - -
            0xb8 => self.res(7, Reg::B),
            // RES 7,C 2:8 - - - -
            0xb9 => self.res(7, Reg::C),
            // RES 7,D 2:8 - - - -
            0xba => self.res(7, Reg::D),
            // RES 7,E 2:8 - - - -
            0xbb => self.res(7, Reg::E),
            // RES 7,H 2:8 - - - -
            0xbc => self.res(7, Reg::H),
            // RES 7,L 2:8 - - - -
            0xbd => self.res(7, Reg::L),
            // RES 7,(HL) 2:16 - - - -
            0xbe => self.res(7, Reg::HL),
            // RES 7,A 2:8 - - - - Cx
            0xbf => self.res(7, Reg::A),
            // SET 0,B 2:8 - - - -
            0xc0 => self.set(0, Reg::B),
            // SET 0,C 2:8 - - - -
            0xc1 => self.set(0, Reg::C),
            // SET 0,D 2:8 - - - -
            0xc2 => self.set(0, Reg::D),
            // SET 0,E 2:8 - - - -
            0xc3 => self.set(0, Reg::E),
            // SET 0,H 2:8 - - - -
            0xc4 => self.set(0, Reg::H),
            // SET 0,L 2:8 - - - -
            0xc5 => self.set(0, Reg::L),
            // SET 0,(HL) 2:16 - - - -
            0xc6 => self.set(0, Reg::HL),
            // SET 0,A 2:8 - - - -
            0xc7 => self.set(0, Reg::A),
            // SET 1,B 2:8 - - - -
            0xc8 => self.set(1, Reg::B),
            // SET 1,C 2:8 - - - -
            0xc9 => self.set(1, Reg::C),
            // SET 1,D 2:8 - - - -
            0xca => self.set(1, Reg::D),
            // SET 1,E 2:8 - - - -
            0xcb => self.set(1, Reg::E),
            // SET 1,H 2:8 - - - -
            0xcc => self.set(1, Reg::H),
            // SET 1,L 2:8 - - - -
            0xcd => self.set(1, Reg::L),
            // SET 1,(HL) 2:16 - - - -
            0xce => self.set(1, Reg::HL),
            // SET 1,A 2:8 - - - - Dx
            0xcf => self.set(1, Reg::A),
            // SET 2,B 2:8 - - - -
            0xd0 => self.set(2, Reg::B),
            // SET 2,C 2:8 - - - -
            0xd1 => self.set(2, Reg::C),
            // SET 2,D 2:8 - - - -
            0xd2 => self.set(2, Reg::D),
            // SET 2,E 2:8 - - - -
            0xd3 => self.set(2, Reg::E),
            // SET 2,H 2:8 - - - -
            0xd4 => self.set(2, Reg::H),
            // SET 2,L 2:8 - - - -
            0xd5 => self.set(2, Reg::L),
            // SET 2,(HL) 2:16 - - - -
            0xd6 => self.set(2, Reg::HL),
            // SET 2,A 2:8 - - - -
            0xd7 => self.set(2, Reg::A),
            // SET 3,B 2:8 - - - -
            0xd8 => self.set(3, Reg::B),
            // SET 3,C 2:8 - - - -
            0xd9 => self.set(3, Reg::C),
            // SET 3,D 2:8 - - - -
            0xda => self.set(3, Reg::D),
            // SET 3,E 2:8 - - - -
            0xdb => self.set(3, Reg::E),
            // SET 3,H 2:8 - - - -
            0xdc => self.set(3, Reg::H),
            // SET 3,L 2:8 - - - -
            0xdd => self.set(3, Reg::L),
            // SET 3,(HL) 2:16 - - - -
            0xde => self.set(3, Reg::HL),
            // SET 3,A 2:8 - - - - Ex
            0xdf => self.set(3, Reg::A),
            // SET 4,B 2:8 - - - -
            0xe0 => self.set(4, Reg::B),
            // SET 4,C 2:8 - - - -
            0xe1 => self.set(4, Reg::C),
            // SET 4,D 2:8 - - - -
            0xe2 => self.set(4, Reg::D),
            // SET 4,E 2:8 - - - -
            0xe3 => self.set(4, Reg::E),
            // SET 4,H 2:8 - - - -
            0xe4 => self.set(4, Reg::H),
            // SET 4,L 2:8 - - - -
            0xe5 => self.set(4, Reg::L),
            // SET 4,(HL) 2:16 - - - -
            0xe6 => self.set(4, Reg::HL),
            // SET 4,A 2:8 - - - -
            0xe7 => self.set(4, Reg::A),
            // SET 5,B 2:8 - - - -
            0xe8 => self.set(5, Reg::B),
            // SET 5,C 2:8 - - - -
            0xe9 => self.set(5, Reg::C),
            // SET 5,D 2:8 - - - -
            0xea => self.set(5, Reg::D),
            // SET 5,E 2:8 - - - -
            0xeb => self.set(5, Reg::E),
            // SET 5,H 2:8 - - - -
            0xec => self.set(5, Reg::H),
            // SET 5,L 2:8 - - - -
            0xed => self.set(5, Reg::L),
            // SET 5,(HL) 2:16 - - - -
            0xee => self.set(5, Reg::HL),
            // SET 5,A 2:8 - - - - Fx
            0xef => self.set(5, Reg::A),
            // SET 6,B 2:8 - - - -
            0xf0 => self.set(6, Reg::B),
            // SET 6,C 2:8 - - - -
            0xf1 => self.set(6, Reg::C),
            // SET 6,D 2:8 - - - -
            0xf2 => self.set(6, Reg::D),
            // SET 6,E 2:8 - - - -
            0xf3 => self.set(6, Reg::E),
            // SET 6,H 2:8 - - - -
            0xf4 => self.set(6, Reg::H),
            // SET 6,L 2:8 - - - -
            0xf5 => self.set(6, Reg::L),
            // SET 6,(HL) 2:16 - - - -
            0xf6 => self.set(6, Reg::HL),
            // SET 6,A 2:8 - - - -
            0xf7 => self.set(6, Reg::A),
            // SET 7,B 2:8 - - - -
            0xf8 => self.set(7, Reg::B),
            // SET 7,C 2:8 - - - -
            0xf9 => self.set(7, Reg::C),
            // SET 7,D 2:8 - - - -
            0xfa => self.set(7, Reg::D),
            // SET 7,E 2:8 - - - -
            0xfb => self.set(7, Reg::E),
            // SET 7,H 2:8 - - - -
            0xfc => self.set(7, Reg::H),
            // SET 7,L 2:8 - - - -
            0xfd => self.set(7, Reg::L),
            // SET 7,(HL) 2:16 - - - -
            0xfe => self.set(7, Reg::HL),
            // SET 7,A 2:8 - - - -
            0xff => self.set(7, Reg::A),
        }
    }

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
            .print_around(self.0.cartridge.curr_bank(), pc, self.0, &mut string)
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
                let clocks = match input[1].parse::<u64>() {
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

    fn gb_read(&self, address: u16) -> u8 {
        #[allow(clippy::let_and_return)] // being useful for debugging
        let value = self.0.read(address);
        #[cfg(feature = "io_trace")]
        self.0.io_trace.borrow_mut().push((
            0 | ((self.0.clock_count & !3) as u8 >> 1),
            address,
            value,
        ));
        value
    }

    fn gb_write(&mut self, address: u16, value: u8) {
        #[cfg(feature = "io_trace")]
        self.0.io_trace.borrow_mut().push((
            1 | ((self.0.clock_count & !3) as u8 >> 1),
            address,
            value,
        ));
        self.0.write(address, value);
    }

    fn gb_write16(&mut self, address: u16, value: u16) {
        let [a, b] = value.to_le_bytes();
        self.gb_write(address, a);
        self.0.tick(4);
        self.gb_write(address.wrapping_add(1), b);
        self.0.tick(4);
    }

    /// Read from PC, tick 4 cycles, and increase it by 1
    pub fn read_next_pc(&mut self) -> u8 {
        let v = self.0.read(self.0.cpu.pc);
        self.0.tick(4);
        self.0.cpu.pc = add16(self.0.cpu.pc, 1);
        v
    }

    /// Read from next PC, two times, and form a u16
    fn read_next_pc16(&mut self) -> u16 {
        let lsb = self.read_next_pc();
        let msb = self.read_next_pc();
        u16::from_le_bytes([lsb, msb])
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
            Reg::Im8 => self.read_next_pc(),
            Reg::Im16 => {
                let address = self.read_next_pc16();
                let v = self.gb_read(address);
                self.0.tick(4);
                v
            }
            Reg::BC => {
                let v = self.gb_read(self.0.cpu.bc());
                self.0.tick(4);
                v
            }
            Reg::DE => {
                let v = self.gb_read(self.0.cpu.de());
                self.0.tick(4);
                v
            }
            Reg::HL => {
                let v = self.gb_read(self.0.cpu.hl());
                self.0.tick(4);
                v
            }
            Reg::HLI => {
                let v = self.gb_read(self.0.cpu.hl());
                self.0.tick(4);
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
                v
            }
            Reg::HLD => {
                let v = self.gb_read(self.0.cpu.hl());
                self.0.tick(4);
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
            Reg::Im8 => {
                self.gb_write(add16(self.0.cpu.pc, 1), value);
                self.0.tick(4);
            }
            Reg::Im16 => {
                let adress = self.read_next_pc16();
                self.gb_write(adress, value);
                self.0.tick(4);
            }
            Reg::BC => {
                self.gb_write(self.0.cpu.bc(), value);
                self.0.tick(4);
            }
            Reg::DE => {
                self.gb_write(self.0.cpu.de(), value);
                self.0.tick(4);
            }
            Reg::HL => {
                self.gb_write(self.0.cpu.hl(), value);
                self.0.tick(4);
            }
            Reg::HLI => {
                self.gb_write(self.0.cpu.hl(), value);
                self.0.tick(4);
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
            }
            Reg::HLD => {
                self.gb_write(self.0.cpu.hl(), value);
                self.0.tick(4);
                self.0.cpu.set_hl(sub16(self.0.cpu.hl(), 1));
            }
            Reg::SP => unreachable!(),
        }
    }

    fn check_condition(&self, c: Condition) -> bool {
        use Condition::*;
        match c {
            None => true,
            Z => self.0.cpu.f.z(),
            NZ => !self.0.cpu.f.z(),
            C => self.0.cpu.f.c(),
            NC => !self.0.cpu.f.c(),
        }
    }

    /// Set the value of the cpu PC, but also update the disassembly tracing
    pub fn jump_to(&mut self, pc: u16) {
        self.0.cpu.pc = pc;

        // don't trace RAM
        if pc > 0x7FFF {
            return;
        }

        let bank = self.0.cartridge.curr_bank();
        let mut trace = self.0.trace.borrow_mut();

        let Some(address) = Address::from_pc(bank, pc) else {
            return;
        };

        // check early if this address is already traced, and return if it is
        if pc <= 0x7FFF && trace.is_already_traced(address) {
            return;
        }

        trace.trace_starting_at(
            self.0,
            bank,
            pc,
            Some(format!(
                "L{:02x}_{:04x}",
                if pc <= 0x3FFF { bank.0 } else { bank.1 },
                pc
            )),
        );
    }

    pub fn jump(&mut self, c: Condition) {
        // JP cc, nn
        let c = self.check_condition(c);
        let address = self.read_next_pc16();
        if c {
            self.jump_to(address);
            self.0.tick(4); // Extra 1 M-cycle for jump
        }
    }

    pub fn jump_rel(&mut self, c: Condition) {
        // JR cc, nn
        let c = self.check_condition(c);
        let r8 = self.read_next_pc() as i8;
        if c {
            let pc = self.0.cpu.pc.wrapping_add_signed(r8 as i16);
            self.jump_to(pc);
            self.0.tick(4); // Extra 1 M-cycle for jump
        }
    }

    pub fn jump_hl(&mut self) {
        self.jump_to(self.0.cpu.hl())
    }

    fn pushr(&mut self, value: u16) {
        let [lsb, msb] = value.to_le_bytes();
        self.0.tick(4); // 1 M-cycle with SP in address buss
        self.gb_write(sub16(self.0.cpu.sp, 1), msb);
        self.0.tick(4); // 1 M-cycle with SP-1 in address buss
        self.gb_write(sub16(self.0.cpu.sp, 2), lsb);
        self.0.tick(4); // 1 M-cycle with SP-2 in address buss
        self.0.cpu.sp = sub16(self.0.cpu.sp, 2);
    }

    fn popr(&mut self) -> u16 {
        let lsp = self.gb_read(self.0.cpu.sp);
        self.0.tick(4); // 1 M-cycle with SP in address buss
        let msp = self.gb_read(add16(self.0.cpu.sp, 1));
        self.0.tick(4); // 1 M-cycle with SP+1 in address buss
        self.0.cpu.sp = add16(self.0.cpu.sp, 2);
        u16::from_be_bytes([msp, lsp])
    }

    pub fn push(&mut self, reg: Reg16) {
        let r = match reg {
            Reg16::AF => self.0.cpu.af(),
            Reg16::BC => self.0.cpu.bc(),
            Reg16::DE => self.0.cpu.de(),
            Reg16::HL => self.0.cpu.hl(),
            _ => unreachable!(),
        };
        self.pushr(r);
    }

    pub fn pop(&mut self, reg: Reg16) {
        let r = self.popr();
        match reg {
            Reg16::AF => self.0.cpu.set_af(r),
            Reg16::BC => self.0.cpu.set_bc(r),
            Reg16::DE => self.0.cpu.set_de(r),
            Reg16::HL => self.0.cpu.set_hl(r),
            _ => unreachable!(),
        }
    }

    pub fn call(&mut self, c: Condition) {
        // CALL cc, nn
        let c = self.check_condition(c);
        let address = self.read_next_pc16();
        if c {
            self.pushr(self.0.cpu.pc);
            self.jump_to(address);
        }
    }

    pub fn ret(&mut self, cond: Condition) {
        // RET nn
        let c = self.check_condition(cond);
        if cond != Condition::None {
            self.0.tick(4); // 1 M-cycle for condition check (I think?)
        }
        if c {
            let address = self.popr();
            self.jump_to(address);
            self.0.tick(4); // more 1 M-cycle
        }
    }

    pub fn nop(&mut self) {
        // do nothing
    }

    pub fn load(&mut self, dst: Reg, src: Reg) {
        let v = self.read(src);
        self.write(dst, v);
    }

    pub fn loadh(&mut self, dst: Reg, src: Reg) {
        let src = match src {
            Reg::A => self.0.cpu.a,
            Reg::C => {
                let v = self.gb_read(0xFF00 | self.0.cpu.c as u16);
                self.0.tick(4);
                v
            }
            Reg::Im8 => {
                let r8 = self.read_next_pc();
                let v = self.gb_read(0xFF00 | r8 as u16);
                self.0.tick(4);
                v
            }
            _ => unreachable!(),
        };

        match dst {
            Reg::A => self.0.cpu.a = src,
            Reg::C => {
                self.gb_write(0xFF00 | self.0.cpu.c as u16, src);
                self.0.tick(4);
            }
            Reg::Im8 => {
                let r8 = self.read_next_pc();
                self.gb_write(0xFF00 | r8 as u16, src);
                self.0.tick(4);
            }
            _ => unreachable!(),
        }
    }

    pub fn load16(&mut self, dst: Reg16, src: Reg16) {
        let v = match src {
            Reg16::HL => self.0.cpu.hl(),
            Reg16::SP => self.0.cpu.sp,
            Reg16::Im16 => self.read_next_pc16(),
            _ => unreachable!(),
        };
        if dst == Reg16::SP && src == Reg16::HL {
            self.0.tick(4);
        }
        match dst {
            Reg16::BC => self.0.cpu.set_bc(v),
            Reg16::DE => self.0.cpu.set_de(v),
            Reg16::HL => self.0.cpu.set_hl(v),
            Reg16::SP => self.0.cpu.sp = v,
            Reg16::Im16 => {
                let adress = self.read_next_pc16();
                self.gb_write16(adress, v)
            }
            _ => unreachable!(),
        }
    }

    pub fn inc(&mut self, reg: Reg) {
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
                self.0.tick(4);
                return;
            }
            Reg::DE => {
                self.0.cpu.set_de(add16(self.0.cpu.de(), 1));
                self.0.tick(4);
                return;
            }
            Reg::HL => {
                self.0.cpu.set_hl(add16(self.0.cpu.hl(), 1));
                self.0.tick(4);
                return;
            }
            Reg::SP => {
                self.0.cpu.sp = add16(self.0.cpu.sp, 1);
                self.0.tick(4);
                return;
            }
            _ => unreachable!(),
        };
        *reg = add(*reg, 1);
        self.0.cpu.f.def_z(*reg == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(*reg & 0x0f == 0x0);
    }

    pub fn dec(&mut self, reg: Reg) {
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
                self.0.tick(4);
                return;
            }
            Reg::DE => {
                self.0.cpu.set_de(sub16(self.0.cpu.de(), 1));
                self.0.tick(4);
                return;
            }
            Reg::HL => {
                self.0.cpu.set_hl(sub16(self.0.cpu.hl(), 1));
                self.0.tick(4);
                return;
            }
            Reg::SP => {
                self.0.cpu.sp = sub16(self.0.cpu.sp, 1);
                self.0.tick(4);
                return;
            }
            _ => unreachable!(),
        };
        *reg = sub(*reg, 1);
        self.0.cpu.f.def_z(*reg == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h(*reg & 0x0f == 0xf);
    }

    pub fn add(&mut self, reg: Reg) {
        let v = self.read(reg);
        let (r, o) = self.0.cpu.a.overflowing_add(v);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h((self.0.cpu.a & 0xF) + (v & 0xF) > 0xF);
        self.0.cpu.f.def_c(o);
        self.0.cpu.a = r;
    }

    pub fn adc(&mut self, reg: Reg) {
        let a = self.0.cpu.a as u16;
        let c = self.0.cpu.f.c() as u16;
        let v = self.read(reg) as u16;
        let r = a + v + c;
        self.0.cpu.f.def_z(r & 0xFF == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h((a & 0xF) + (v & 0xF) + c > 0xF);
        self.0.cpu.f.def_c(r > 0xff);
        self.0.cpu.a = (r & 0xff) as u8;
    }

    pub fn sub(&mut self, reg: Reg) {
        let v = self.read(reg);
        let (r, o) = self.0.cpu.a.overflowing_sub(v);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h((self.0.cpu.a & 0xF) < (v & 0xF));
        self.0.cpu.f.def_c(o);
        self.0.cpu.a = r;
    }

    pub fn sbc(&mut self, reg: Reg) {
        let a = self.0.cpu.a as i16;
        let c = self.0.cpu.f.c() as i16;
        let v = self.read(reg) as i16;
        let r = a - v - c;
        self.0.cpu.f.def_z(r & 0xFF == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h((a & 0xF) < (v & 0xF) + c);
        self.0.cpu.f.def_c(r < 0x0);
        self.0.cpu.a = (r & 0xff) as u8;
    }

    pub fn and(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a &= v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.set_h();
        self.0.cpu.f.clr_c();
    }

    pub fn or(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a |= v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.clr_c();
    }

    pub fn xor(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.a ^= v;
        self.0.cpu.f.def_z(self.0.cpu.a == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.clr_c();
    }

    pub fn cp(&mut self, reg: Reg) {
        let v = self.read(reg);
        self.0.cpu.f.def_z(self.0.cpu.a == v);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h(self.0.cpu.a & 0xF < v & 0xF);
        self.0.cpu.f.def_c(self.0.cpu.a < v);
    }

    pub fn add16(&mut self, b: Reg16) {
        let b = match b {
            Reg16::BC => self.0.cpu.bc(),
            Reg16::DE => self.0.cpu.de(),
            Reg16::HL => self.0.cpu.hl(),
            Reg16::SP => self.0.cpu.sp,
            Reg16::Im16 | Reg16::AF => unreachable!(),
        };
        let (r, o) = self.0.cpu.hl().overflowing_add(b);
        let h = (self.0.cpu.hl() & 0x0FFF) + (b & 0x0FFF) > 0xFFF;

        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(h);
        self.0.cpu.f.def_c(o);

        self.0.cpu.set_hl(r);
        self.0.tick(4);
    }

    pub fn rst(&mut self, address: u8) {
        self.pushr(self.0.cpu.pc);
        self.jump_to(address as u16);
    }

    pub fn rlc(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = r.rotate_left(1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(r & 0x1 != 0);
    }

    pub fn rrc(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = r.rotate_right(1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(r & 0x80 != 0);
    }

    pub fn rl(&mut self, reg: Reg) {
        let c = self.0.cpu.f.c() as u8;
        let mut r = self.read(reg);
        self.0.cpu.f.def_c(r & 0x80 != 0);
        r = r << 1 | c;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
    }

    pub fn rr(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = self.0.cpu.f.c() as u8;
        self.0.cpu.f.def_c(r & 0x01 != 0);
        r = r >> 1 | c << 7;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
    }

    pub fn sla(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = r & 0x80 != 0;
        r <<= 1;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(c);
    }

    pub fn sra(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        self.0.cpu.f.def_c(r & 0x01 != 0);
        r = (r & 0x80) | (r >> 1);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
    }

    pub fn swap(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        r = ((r & 0x0F) << 4) | ((r & 0xF0) >> 4);
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.clr_c();
    }

    pub fn srl(&mut self, reg: Reg) {
        let mut r = self.read(reg);
        let c = r & 0x01 != 0;
        r >>= 1;
        self.write(reg, r);
        self.0.cpu.f.def_z(r == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(c);
    }

    pub fn add_sp(&mut self) {
        let (r, c, h);
        let r8 = self.read_next_pc() as i8;
        if (r8) >= 0 {
            c = ((self.0.cpu.sp & 0xFF) + r8 as u16) > 0xFF;
            h = ((self.0.cpu.sp & 0x0F) as u8 + (r8 as u8 & 0xF)) > 0x0F;
            r = add16(self.0.cpu.sp, r8 as u16);
        } else {
            r = self.0.cpu.sp.wrapping_add_signed(r8 as i16);
            c = (r & 0xFF) <= (self.0.cpu.sp & 0xFF);
            h = (r & 0x0F) <= (self.0.cpu.sp & 0x0F);
        }
        self.0.cpu.sp = r;
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(h);
        self.0.cpu.f.def_c(c);
        self.0.tick(8);
    }

    pub fn reti(&mut self) {
        self.ret(Condition::None);
        self.0.cpu.ime = ImeState::Enabled;
    }

    pub fn halt(&mut self) {
        self.0.cpu.state = CpuState::Halt;
    }

    pub fn ccf(&mut self) {
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(!self.0.cpu.f.c());
    }

    pub fn scf(&mut self) {
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.set_c();
    }

    pub fn dec16(&mut self, reg: Reg) {
        let mut reg = self.read(reg);
        reg = sub(reg, 1);
        self.write(Reg::HL, reg);
        self.0.cpu.f.def_z(reg == 0);
        self.0.cpu.f.set_n();
        self.0.cpu.f.def_h(reg & 0x0f == 0xf);
    }

    pub fn inc16(&mut self, reg: Reg) {
        let mut reg = self.read(reg);
        reg = add(reg, 1);
        self.write(Reg::HL, reg);
        self.0.cpu.f.def_z(reg == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(reg & 0x0f == 0);
    }

    pub fn cpl(&mut self) {
        self.0.cpu.a = !self.0.cpu.a;
        self.0.cpu.f.set_n();
        self.0.cpu.f.set_h();
    }

    pub fn daa(&mut self) {
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

    pub fn rra(&mut self) {
        let c = self.0.cpu.f.c() as u8;
        self.0.cpu.f.def_c(self.0.cpu.a & 0x01 != 0);
        self.0.cpu.a = self.0.cpu.a >> 1 | c << 7;
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
    }

    pub fn rla(&mut self) {
        let c = self.0.cpu.f.c() as u8;
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(self.0.cpu.a & 0x80 != 0);
        self.0.cpu.a = self.0.cpu.a << 1 | c;
    }

    pub fn stop(&mut self) {
        self.0.cpu.state = CpuState::Stopped;
        self.0.cpu.pc = add16(self.0.cpu.pc, 1);
    }

    pub fn rrca(&mut self) {
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(self.0.cpu.a & 0x01 != 0);
        self.0.cpu.a = self.0.cpu.a.rotate_right(1);
    }

    pub fn rlca(&mut self) {
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.clr_h();
        self.0.cpu.f.def_c(self.0.cpu.a & 0x80 != 0);
        self.0.cpu.a = self.0.cpu.a.rotate_left(1);
    }

    pub fn invalid_opcode(&mut self, opcode: u8) {
        println!("executed invalid instructions: {opcode:02x}");
        std::process::exit(1);
    }

    pub fn ei(&mut self) {
        if self.0.cpu.ime == ImeState::Disabled {
            self.0.cpu.ime = ImeState::ToBeEnable;
        }
    }

    pub fn di(&mut self) {
        self.0.cpu.ime = ImeState::Disabled
    }

    pub fn ldhl_sp(&mut self) {
        let r;
        let c;
        let h;
        let r8 = self.read_next_pc() as i8;
        if (r8) >= 0 {
            c = ((self.0.cpu.sp & 0xFF) + r8 as u16) > 0xFF;
            h = ((self.0.cpu.sp & 0x0F) as u8 + (r8 as u8 & 0xF)) > 0x0F;
            r = add16(self.0.cpu.sp, r8 as u16);
        } else {
            r = self.0.cpu.sp.wrapping_add_signed(r8 as i16);
            c = (r & 0xFF) <= (self.0.cpu.sp & 0xFF);
            h = (r & 0x0F) <= (self.0.cpu.sp & 0x0F);
        }
        self.0.cpu.set_hl(r);
        self.0.cpu.f.clr_z();
        self.0.cpu.f.clr_n();
        self.0.cpu.f.def_h(h);
        self.0.cpu.f.def_c(c);
        self.0.tick(4);
    }

    pub fn bit(&mut self, bit: u8, reg: Reg) {
        let r = self.read(reg);
        self.0.cpu.f.def_z((r & (1 << bit)) == 0);
        self.0.cpu.f.clr_n();
        self.0.cpu.f.set_h();
    }

    pub fn res(&mut self, bit: u8, reg: Reg) {
        let mut r = self.read(reg);
        r &= !(1 << bit);
        self.write(reg, r);
    }

    pub fn set(&mut self, bit: u8, reg: Reg) {
        let mut r = self.read(reg);
        r |= 1 << bit;
        self.write(reg, r);
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
                let adress = self.0.read16(add16(self.0.cpu.pc, 1));
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
            0xcb => match self.0.read(self.0.cpu.pc + 1) {
                0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x36 | 0x3e | 0x86 | 0x8e | 0x96
                | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe | 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee
                | 0xf6 | 0xfe => some(self.0.cpu.hl()),
                _ => none,
            },
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
                let cond = match (x >> 3) & 0b11 {
                    0 => Condition::NZ,
                    1 => Condition::Z,
                    2 => Condition::NC,
                    3 => Condition::C,
                    _ => unreachable!(),
                };
                if self.check_condition(cond) {
                    let dest = u16::from_le_bytes([op[1], op[2]]);
                    Some(dest)
                } else {
                    None
                }
            }
            0x18 => {
                // JR $rr
                let dest = ((pc + len as u16) as i16 + op[1] as i8 as i16) as u16;
                Some(dest)
            }
            x if x & 0b1110_0111 == 0b0010_0000 => {
                // JR cc, $rr
                let cond = match (x >> 3) & 0b11 {
                    0 => Condition::NZ,
                    1 => Condition::Z,
                    2 => Condition::NC,
                    3 => Condition::C,
                    _ => unreachable!(),
                };
                if self.check_condition(cond) {
                    let dest = ((pc + len as u16) as i16 + op[1] as i8 as i16) as u16;
                    Some(dest)
                } else {
                    None
                }
            }
            0xCD => {
                // CALL $aaaa
                let dest = u16::from_le_bytes([op[1], op[2]]);
                Some(dest)
            }
            x if x & 0b11100111 == 0b11000100 => {
                // CALL cc, $aaaa
                let cond = match (x >> 3) & 0b11 {
                    0 => Condition::NZ,
                    1 => Condition::Z,
                    2 => Condition::NC,
                    3 => Condition::C,
                    _ => unreachable!(),
                };
                if self.check_condition(cond) {
                    let dest = u16::from_le_bytes([op[1], op[2]]);
                    Some(dest)
                } else {
                    None
                }
            }
            x @ (0xC0 | 0xC8 | 0xD0 | 0xD8) => {
                // RET cc
                let cond = match (x >> 3) & 0b11 {
                    0 => Condition::NZ,
                    1 => Condition::Z,
                    2 => Condition::NC,
                    3 => Condition::C,
                    _ => unreachable!(),
                };
                if self.check_condition(cond) {
                    let dest = u16::from_le_bytes([
                        self.0.read(sub16(self.0.cpu.sp, 1)),
                        self.0.read(sub16(self.0.cpu.sp, 2)),
                    ]);
                    Some(dest)
                } else {
                    None
                }
            }
            0xC9 | 0xD9 => {
                // RET or RETI
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
}
