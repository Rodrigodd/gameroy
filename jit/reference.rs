#![allow(dead_code)]
#![no_std]

#[repr(C)]
pub struct Cpu {
    mem: [u8; 256],
    clock_count: u64,
    a: u8,   // 264
    f: u8,   // 265
    c: u8,   // 266
    b: u8,   // 267
    e: u8,   // 268
    d: u8,   // 269
    l: u8,   // 270
    h: u8,   // 271
    pc: u16, // 272
    sp: u16, // 274

    ime_state: ImeState,

    next_interrupt: core::cell::Cell<u64>,
}

#[repr(u8)]
#[derive(PartialEq, Eq)]
pub enum ImeState {
    Disabled = 0,
    Enabled = 1,
    ToBeEnable = 2,
}

impl Cpu {
    fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b, self.c])
    }

    fn hl(&self) -> u16 {
        u16::from_be_bytes([self.h, self.l])
    }

    fn set_af(&mut self, value: u16) {
        let [h, l] = value.to_be_bytes();
        self.a = h;
        self.f = l & 0xf0;
    }

    fn set_de(&mut self, value: u16) {
        [self.d, self.e] = value.to_be_bytes();
    }

    fn set_hl(&mut self, value: u16) {
        [self.h, self.l] = value.to_be_bytes();
    }
}

fn def_bit(v: &mut u8, i: u8, b: bool) {
    *v = (*v & !(1 << i)) | (b as u8) << i;
}

pub extern "sysv64" fn inc(cpu: &mut Cpu) {
    cpu.a = cpu.a.wrapping_add(1);
    cpu.f = (cpu.f & !(1 << 7)) | ((cpu.a == 0) as u8) << 7;
    cpu.f &= !(1 << 6);
    cpu.f = (cpu.f & !(1 << 5)) | ((cpu.a & 0x0f == 0x0) as u8) << 5;
}

#[inline(never)]
extern "sysv64" fn read(cpu: &Cpu, address: u16) -> u8 {
    cpu.mem[address as usize]
}

#[inline(never)]
extern "sysv64" fn write(cpu: &mut Cpu, address: u16, value: u8) {
    cpu.mem[address as usize] = value;
}

pub extern "sysv64" fn inc_mem(cpu: &mut Cpu) {
    let hl = cpu.hl();
    let mut reg = read(cpu, hl);
    reg = reg.wrapping_add(1);

    def_bit(&mut cpu.f, 7, reg == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, reg & 0x0f == 0); // H

    write(cpu, hl, reg);
    core::hint::black_box(()); // avoid tail-call optimization
}

pub extern "sysv64" fn dec(cpu: &mut Cpu) {
    cpu.f = (cpu.f & !(1 << 5)) | ((cpu.a & 0x0f == 0x0) as u8) << 5;
    cpu.a = cpu.a.wrapping_sub(1);
    cpu.f = (cpu.f & !(1 << 7)) | ((cpu.a == 0) as u8) << 7;
    cpu.f |= 1 << 6;
}

pub extern "sysv64" fn dec_mem(cpu: &mut Cpu) {
    let hl = cpu.hl();
    let mut reg = read(cpu, hl);
    reg = reg.wrapping_sub(1);

    def_bit(&mut cpu.f, 7, reg == 0); // Z
    def_bit(&mut cpu.f, 6, true); // N
    def_bit(&mut cpu.f, 5, reg & 0x0f == 0xf); // H

    write(cpu, hl, reg);
    core::hint::black_box(()); // avoid tail-call optimization
}

pub extern "sysv64" fn inc_no_flags(cpu: &mut Cpu) {
    cpu.a = cpu.a.wrapping_add(1);
}

pub extern "sysv64" fn fixup_clock_count(cpu: &mut Cpu) {
    cpu.clock_count += i32::MAX as u64 + 2;
}

pub extern "sysv64" fn fixup_pc(cpu: &mut Cpu) {
    cpu.pc = 0xf16b;
}

pub extern "sysv64" fn inc16(cpu: &mut Cpu) {
    let x = cpu.bc();
    let x = x.wrapping_add(1);
    [cpu.c, cpu.b] = x.to_le_bytes();
}

pub extern "sysv64" fn dec16(cpu: &mut Cpu) {
    let x = cpu.bc();
    let x = x.wrapping_sub(1);
    [cpu.c, cpu.b] = x.to_le_bytes();
}

pub extern "sysv64" fn load(cpu: &mut Cpu) {
    cpu.a = cpu.b;
}

pub extern "sysv64" fn load16(cpu: &mut Cpu) {
    cpu.set_de(cpu.bc());
}

pub extern "sysv64" fn load_im(cpu: &mut Cpu) {
    cpu.a = 0xf7;
}

pub extern "sysv64" fn load_mem(cpu: &mut Cpu) {
    let address = cpu.bc();
    let cell: &mut u8 = unsafe { cpu.mem.get_unchecked_mut(address as usize) };
    *cell = cpu.b;
}

pub extern "sysv64" fn load16_im(cpu: &mut Cpu) {
    let sp = cpu.sp;
    let address = 0x9a85;
    let [a, b] = sp.to_le_bytes();
    write(cpu, address, a);
    write(cpu, address.wrapping_add(1), b);
    core::hint::black_box(()); // avoid tail-call optimization
}

pub extern "sysv64" fn loadh_read_c(cpu: &mut Cpu) {
    let v = cpu.c;
    let address = 0xff00 | v as u16;
    let value = read(cpu, address);
    cpu.a = value;
}

pub extern "sysv64" fn loadh_write_c(cpu: &mut Cpu) {
    let a = cpu.a;
    let v = 0x7f;
    let address = 0xff00 | v as u16;
    write(cpu, address, a);
    core::hint::black_box(()); // avoid tail-call optimization
}

pub extern "sysv64" fn ldhl_sp(cpu: &mut Cpu, im: u8) {
    let (r, c, h);
    let r8 = im as i8;

    if r8 >= 0 {
        c = ((cpu.sp & 0xFF) + r8 as u16) > 0xFF;
        h = ((cpu.sp & 0x0F) as u8 + (r8 as u8 & 0xF)) > 0x0F;
        r = cpu.sp.wrapping_add(r8 as u16);
    } else {
        // r = cpu.sp.wrapping_sub(-r8 as u16);
        // c = (r & 0xFF) <= (cpu.sp & 0xFF);
        // h = (r & 0x0F) <= (cpu.sp & 0x0F);
        unsafe { core::hint::unreachable_unchecked() }
    }

    cpu.set_hl(r);

    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, h); // H
    def_bit(&mut cpu.f, 4, c); // C
}

pub extern "sysv64" fn add_sp(cpu: &mut Cpu, im: u8) {
    let (r, c, h);
    let r8 = im as i8;

    if r8 >= 0 {
        // c = ((cpu.sp & 0xFF) + r8 as u16) > 0xFF;
        // h = ((cpu.sp & 0x0F) as u8 + (r8 as u8 & 0xF)) > 0x0F;
        // r = cpu.sp.wrapping_add(r8 as u16);
        unsafe { core::hint::unreachable_unchecked() }
    } else {
        r = cpu.sp.wrapping_sub(-r8 as u16);
        c = (r & 0xFF) <= (cpu.sp & 0xFF);
        h = (r & 0x0F) <= (cpu.sp & 0x0F);
        // unsafe { core::hint::unreachable_unchecked() }
    }

    cpu.sp = r;

    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, h); // H
    def_bit(&mut cpu.f, 4, c); // C
}

pub extern "sysv64" fn add(cpu: &mut Cpu) {
    let v = cpu.b;
    let a = cpu.a;
    let (r, o) = cpu.a.overflowing_add(v);

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, (a & 0xF) + (v & 0xF) > 0xF); // H
    def_bit(&mut cpu.f, 4, o); // C

    cpu.a = r;
}

pub extern "sysv64" fn add16(cpu: &mut Cpu) {
    let v = cpu.bc();
    let (r, o) = cpu.hl().overflowing_add(v);
    let hl = cpu.hl();
    cpu.set_hl(r);

    // def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, (hl & 0xFFF) + (v & 0xFFF) > 0xFFF); // H
    def_bit(&mut cpu.f, 4, o); // C
}

pub extern "sysv64" fn sub(cpu: &mut Cpu) {
    let v = cpu.b;
    let (r, o) = cpu.a.overflowing_sub(v);

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, true); // N
    def_bit(&mut cpu.f, 5, (cpu.a & 0xF) < (v & 0xF)); // H
    def_bit(&mut cpu.f, 4, o); // C

    cpu.a = r;
}

pub extern "sysv64" fn adc(cpu: &mut Cpu) {
    let a = cpu.a as u16;
    let c = ((cpu.f >> 4) & 1) as u16;
    let v = cpu.b as u16;
    let r = a + v + c;

    def_bit(&mut cpu.f, 7, r & 0xff == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, (a & 0xF) + (v & 0xF) + c > 0xF); // H
    def_bit(&mut cpu.f, 4, r > 0xff); // C

    cpu.a = (r & 0xff) as u8;
}

pub extern "sysv64" fn adc_mem(cpu: &mut Cpu) {
    let v = read(cpu, cpu.hl()) as u16;
    let a = cpu.a as u16;
    let c = ((cpu.f >> 4) & 1) as u16;
    let r = a + v + c;

    def_bit(&mut cpu.f, 7, r & 0xff == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, (a & 0xF) + (v & 0xF) + c > 0xF); // H
    def_bit(&mut cpu.f, 4, r > 0xff); // C

    cpu.a = (r & 0xff) as u8;
}

pub extern "sysv64" fn sbc(cpu: &mut Cpu) {
    let v = cpu.b as i16;
    let a = cpu.a as i16;
    let c = ((cpu.f >> 4) & 1) as i16;
    let r = a - v - c;

    def_bit(&mut cpu.f, 7, r & 0xff == 0); // Z
    def_bit(&mut cpu.f, 6, true); // N
    def_bit(&mut cpu.f, 5, (a & 0xF) < (v & 0xF) + c); // H
    def_bit(&mut cpu.f, 4, r < 0x00); // C

    cpu.a = (r & 0xff) as u8;
}

pub extern "sysv64" fn and(cpu: &mut Cpu) {
    let v = cpu.b;
    cpu.a &= v;

    def_bit(&mut cpu.f, 7, cpu.a == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, true); // H
    def_bit(&mut cpu.f, 4, false); // C
}

pub extern "sysv64" fn xor(cpu: &mut Cpu) {
    let v = cpu.b;
    cpu.a ^= v;

    def_bit(&mut cpu.f, 7, cpu.a == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, false); // C
}

pub extern "sysv64" fn or(cpu: &mut Cpu) {
    let v = cpu.b;
    cpu.a |= v;

    def_bit(&mut cpu.f, 7, cpu.a == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, false); // C
}

pub extern "sysv64" fn cp(cpu: &mut Cpu) {
    let v = cpu.b;

    def_bit(&mut cpu.f, 7, cpu.a == v); // Z
    def_bit(&mut cpu.f, 6, true); // N
    def_bit(&mut cpu.f, 5, cpu.a & 0xF < v & 0xF); // H
    def_bit(&mut cpu.f, 4, cpu.a < v); // C
}

pub extern "sysv64" fn rlca(cpu: &mut Cpu) {
    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, cpu.a & 0x80 != 0); // C

    cpu.a = cpu.a.rotate_left(1);
}

pub extern "sysv64" fn rrca(cpu: &mut Cpu) {
    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, cpu.a & 0x01 != 0); // C

    cpu.a = cpu.a.rotate_right(1);
}

pub extern "sysv64" fn rla(cpu: &mut Cpu) {
    let c = ((cpu.f >> 4) & 1) as u8;

    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, cpu.a & 0x80 != 0); // C

    cpu.a = cpu.a << 1 | c;
}

pub extern "sysv64" fn rra(cpu: &mut Cpu) {
    let c = ((cpu.f >> 4) & 1) as u8;

    def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, cpu.a & 0x01 != 0); // C

    cpu.a = cpu.a >> 1 | c << 7;
}

pub extern "sysv64" fn dda(cpu: &mut Cpu) {
    let n = ((cpu.f >> 6) & 1) != 0;
    let h = ((cpu.f >> 5) & 1) != 0;
    let c = ((cpu.f >> 4) & 1) != 0;

    if !n {
        if c || cpu.a > 0x99 {
            cpu.a = cpu.a.wrapping_add(0x60);
            def_bit(&mut cpu.f, 4, true); // C
        }
        if h || (cpu.a & 0x0F) > 0x09 {
            cpu.a = cpu.a.wrapping_add(0x6);
        }
    } else {
        if c {
            cpu.a = cpu.a.wrapping_sub(0x60);
        }
        if h {
            cpu.a = cpu.a.wrapping_sub(0x6);
        }
    }

    def_bit(&mut cpu.f, 7, cpu.a == 0); // Z
                                        // def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
                                   // def_bit(&mut cpu.f, 4, cpu.a & 0x01 != 0); // C
}

pub extern "sysv64" fn cpl(cpu: &mut Cpu) {
    cpu.a = !cpu.a;

    // def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, true); // N
    def_bit(&mut cpu.f, 5, true); // H

    // def_bit(&mut cpu.f, 4, cpu.a & 0x01 != 0); // C
}

pub extern "sysv64" fn ccf(cpu: &mut Cpu) {
    let c = ((cpu.f >> 4) & 1) != 0;

    // def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, !c); // C
}

pub extern "sysv64" fn scf(cpu: &mut Cpu) {
    // def_bit(&mut cpu.f, 7, false); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, true); // C
}

pub extern "sysv64" fn pop(cpu: &mut Cpu) {
    let lsp = read(cpu, cpu.sp);
    let msp = read(cpu, cpu.sp.wrapping_add(1));
    cpu.sp = cpu.sp.wrapping_add(2);
    let v = u16::from_be_bytes([msp, lsp]);
    cpu.set_de(v);
}

pub extern "sysv64" fn pop_af(cpu: &mut Cpu) {
    let lsp = read(cpu, cpu.sp);
    let msp = read(cpu, cpu.sp.wrapping_add(1));
    cpu.sp = cpu.sp.wrapping_add(2);
    let v = u16::from_be_bytes([msp, lsp & 0xf0]);
    cpu.set_af(v);
}

pub extern "sysv64" fn push(cpu: &mut Cpu) {
    let value = cpu.hl();
    let [lsb, msb] = value.to_le_bytes();

    write(cpu, cpu.sp.wrapping_sub(1), msb);
    write(cpu, cpu.sp.wrapping_sub(2), lsb);
    cpu.sp = cpu.sp.wrapping_sub(2);
}

pub extern "sysv64" fn rlc(cpu: &mut Cpu) {
    let mut r = cpu.a;
    r = r.rotate_left(1);
    cpu.a = r;

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, r & 0x1 != 0); // C
}

pub extern "sysv64" fn rlc_mem(cpu: &mut Cpu) {
    let mut r = read(cpu, cpu.hl());
    r = r.rotate_left(1);

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, r & 0x1 != 0); // C
                                          //
    write(cpu, cpu.hl(), r);
    core::hint::black_box(()); // avoid tail-call optimization
}

pub extern "sysv64" fn rrc(cpu: &mut Cpu) {
    let mut r = cpu.a;
    r = r.rotate_right(1);
    cpu.a = r;

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, r & 0x80 != 0); // C
}

pub extern "sysv64" fn rl(cpu: &mut Cpu) {
    let c = ((cpu.f >> 4) & 1) as u8;

    let mut r = cpu.a;

    def_bit(&mut cpu.f, 4, r & 0x80 != 0); // C

    r = r << 1 | c;

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H

    cpu.a = r;
}

pub extern "sysv64" fn sla(cpu: &mut Cpu) {
    let mut r = cpu.a;

    def_bit(&mut cpu.f, 4, r & 0x80 != 0); // C

    r <<= 1;

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H

    cpu.a = r;
}

pub extern "sysv64" fn sra(cpu: &mut Cpu) {
    let mut r = cpu.a;

    def_bit(&mut cpu.f, 4, r & 0x01 != 0); // C

    r = (r & 0x80) | (r >> 1);

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H

    cpu.a = r;
}

pub extern "sysv64" fn swap(cpu: &mut Cpu) {
    let mut r = cpu.a;
    r = ((r & 0x0F) << 4) | ((r & 0xF0) >> 4);

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, false); // C
    cpu.a = r;
}

pub extern "sysv64" fn srl(cpu: &mut Cpu) {
    let mut r = cpu.a;
    let c = r & 0x01 != 0;
    r >>= 1;

    def_bit(&mut cpu.f, 7, r == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, false); // H
    def_bit(&mut cpu.f, 4, c); // C
    cpu.a = r;
}

pub extern "sysv64" fn bit(cpu: &mut Cpu) {
    let r = cpu.a;
    let bit = 5;
    def_bit(&mut cpu.f, 7, (r & (1 << bit)) == 0); // Z
    def_bit(&mut cpu.f, 6, false); // N
    def_bit(&mut cpu.f, 5, true); // H
                                  // def_bit(&mut cpu.f, 4, c); // C
}

pub extern "sysv64" fn res(cpu: &mut Cpu) {
    let mut r = cpu.a;
    let bit = 4;
    r &= !(1 << bit);
    cpu.a = r;
}

pub extern "sysv64" fn set(cpu: &mut Cpu) {
    let mut r = read(cpu, cpu.hl());
    let bit = 7;
    r |= 1 << bit;
    write(cpu, cpu.hl(), r);
}

pub extern "sysv64" fn jump_rel(cpu: &mut Cpu) {
    let r8 = -0x30i8;
    let condition = 1;
    let c = [
        true,
        cpu.f & (1 << 7) != 0, // Z is set?
        cpu.f & (1 << 7) == 0, // Z is unset?
        cpu.f & (1 << 4) != 0, // C is set?
        cpu.f & (1 << 4) == 0, // C is unset?
    ][condition];
    if c {
        cpu.pc = (cpu.pc as i16 + r8 as i16) as u16;
        cpu.clock_count += 4;
    }
}

pub extern "sysv64" fn jump_hl(cpu: &mut Cpu) {
    cpu.pc = cpu.hl();
}

pub extern "sysv64" fn exit_if_interrupt(cpu: &mut Cpu) {
    let block_length = 120;
    let curr_clock_count = 68;
    // self.block_length - self.curr_clock_count < gb.next_interrupt - gb.clock_count
    if block_length as i64 - curr_clock_count as i64
        > cpu.next_interrupt.get() as i64 - cpu.clock_count as i64
    {
        core::hint::black_box(());
        return;
    }
}

pub extern "sysv64" fn update_ime_state(cpu: &mut Cpu) {
    if cpu.ime_state == ImeState::Disabled {
        cpu.ime_state = ImeState::ToBeEnable;
    }
}
