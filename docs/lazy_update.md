# Lazily updating Game Boy components

A Game Boy emulator must not only emulate the execution of the CPU, but also
each of the remaining components that run in parallel to it. These components
are:

- The CPU
- The Picture Processing Unit (PPU)
- The Sound Controller
- The Timer
- The Serial Transfer

The emulator cannot emulate each component individually (like running each one
in its own thread), because the components communicate with each other, and they
can be very time sensitive (may depend on the ROM).

The simplest way to solve this, is to interleave the emulation of each
component. Ideally you would emulate each component one cycle, in an interleaved
manner:

```rust
fn run_one_cycle() {
    cpu.run_one_cycle();
    ppu.run_one_cycle();
    sound.run_one_cycle();
    timer.run_one_cycle();
    serial.run_one_cycle();
}
```

But this not very efficient, and updating the CPU state one cycle at time is
complicated. A more common approach is to emulate one instruction at time:

```rust
fn run_one_instruction() {
    let cycles = cpu.run_one_instruction();
    ppu.run_n_cycles(cycles);
    sound.run_n_cycles(cycles);
    timer.run_n_cycles(cycles);
    serial.run_n_cycles(cycles);
}
```

But that has no timing accuracy. The CPU could, for example, read the
current value of the Timer's DIV registers (that increments every cycle), but
the read may happen some cycles after the instruction started, so the value read
would be outdated.

To solve this, you need to update the components before each read/write of the
CPU following the timing of each instruction:

```rust
fn run_n_cycles(cycles: u64) {
    ppu.run_n_cycles(cycles);
    sound.run_n_cycles(cycles);
    timer.run_n_cycles(cycles);
    serial.run_n_cycles(cycles);
}

fn run_one_instruction() {
    check_for_interrupts();

    let opcode = read_byte(cpu.pc);
    run_n_cycles(4);

    match opcode {
        ...
        0xe5 => { // PUSH HL, for example
            self.0.cpu.sp -= 2;
            run_n_cycles(4);

            write_byte(self.0.cpu.sp + 1, cpu.l);
            run_n_cycles(4);

            write_byte(self.0.cpu.sp, cpu.h);
            run_n_cycles(4);
        }
        ...
    }
}
```

With this approach we can have almost perfect timing (PPU timing is a little
more complicated).

Notice that we are also running 4 consecutive cycles of each component. We can
do that because these components don't communicate with each other (if you
ignore that the Sound Controller uses the Timer DIV, and the emulation of bus
conflicts, as I did in my emulator).

This is good because this allows us to optimize the emulation of each component
(the Timer could increment the DIV register by 4 instead of incrementing by 1
four times for example).

But we can do better! The Sound Controller, for example, only need to be updated
when its registers need to be read or written, or when its sound output must be
played. This means that we can lazy updated this component:

```rust
fn run_n_cycles(cycles: u64) {
    ppu.run_n_cycles(cycles);
    sound.run_n_cycles(cycles);
    // timer.run_n_cycles(cycles);
    serial.run_n_cycles(cycles);
}

...

fn read_byte(address: u16) -> u8 {
    match address {
        ...
        0x10..=0x14 | ... | 0x30..=0x3f => { // addresses of the Sound Controller registers
            sound.run_n_cycles(current_cycle - sound.last_updated);
            sound.read(address)
        }
        ...
    }
}
```

Depending on how often the Sound Controller is accessed, we can have a huge
amount of consecutive cycles, with a lot of room for optimization.

But unfortunately, the same straightforward approach don't work for the
remaining components. This is because not only the CPU can interact with them
through read/writes, but they can also interact with the CPU by triggering
interrupts.

This means that even if we implement lazy updating, we still need to update them
before checking for updates:


```rust
fn update_all(cycles: u64) {
    ppu.run_n_cycles(current_cycles - ppu.last_updated);
    timer.run_n_cycles(current_cycles - timer.last_updated);
    serial.run_n_cycles(current_cycles - serial.last_updated);
}

fn run_n_cycles(cycles: u64) {
    current_cyles += cycles;
}

fn run_one_instruction() {
    update_all();
    check_for_interrupts();

    let opcode = read_byte(cpu.pc);
    run_n_cycles(4);

    match opcode {
        ...
    }
}
```

We only accomplish a maximum of 24 cycles of lazy updating (but it gives a
little more of flexibility that allow us to implement a more accurate PPU
timing).

This is really unfortunately. Mainly because if we could lazy update the PPU for
an entire scan line, we could achieve a massive performance boost (emulating the
[PPU's Mode 3 pixel FIFO] timing is very heavy).

[PPU's Mode 3 pixel FIFO]: https://gbdev.io/pandocs/pixel_fifo.html

But we can solve this problem by predicting when the next interrupt would
happen, or at least by estimating how many cycles are guaranteed to have no
interrupt.

For example, if the PPU's STAT interrupts are disabled and only the VBLANK
interrupt is enabled, we can say that the next interrupt will only happen once
per frame, giving us maximum of 70224 cycles of lazy updating! (if no write or
read at the PPU is done by CPU, of course).

Now the implementation would be something like:

```rust
fn update_for_interrupt(cycles: u64) {
    if ppu.next_interrupt <= current_cycles {
        ppu.run_n_cycles(current_cycles - ppu.last_updated);
    }
    if timer.next_interrupt <= current_cycles {
        timer.run_n_cycles(current_cycles - timer.last_updated);
    }
    if serial.next_interrupt <= current_cycles {
        serial.run_n_cycles(current_cycles - serial.last_updated);
    }
}

fn run_n_cycles(cycles: u64) {
    current_cyles += cycles;
}

fn run_one_instruction() {
    update_for_interrupt();
    check_for_interrupts();
    ...
}

fn write_byte(address: u16, value: u8) -> u8 {
    match address {
        ...
        0x04..=0x07 => { // addresses of the Timer registers
            timer.run_n_cycles(current_cycle - timer.last_updated);
            timer.write(address, value);
            timer.next_interrupt = timer.estimate_next_interrupt();
        }
        // Something similiar for each component
        ...
    }
}
```

Of course the implementation of `estimate_next_interrupt` is a lot more
complicated than what I described above. It is also very fragile, meaning that
any misprediction on the estimating (that could happen in a lot of very edgy
cases) would lead to timing inaccuracies. But nothing that an infinity amount of
fuzzing could not resolve.

The work also unblocks the use of JIT compilation to optimize the CPU.
