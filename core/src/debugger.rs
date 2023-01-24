use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::{gameboy::GameBoy, interpreter::Interpreter, save_state::SaveState};

pub mod break_flags {
    pub const WRITE: u8 = 1 << 0;
    pub const READ: u8 = 1 << 1;
    pub const EXECUTE: u8 = 1 << 2;
    pub const JUMP: u8 = 1 << 3;
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum RunResult {
    ReachBreakpoint,
    ReachTargetAddress,
    ReachTargetClock,
    TimeOut,
}

pub enum DebuggerEvent {
    Step,
    StepBack,
    Reset,
    Run,
    BreakpointsUpdate,
    WatchsUpdate,
}

#[cfg(not(target_arch = "wasm32"))]
type DebuggerCallback = Box<dyn FnMut(&Debugger, DebuggerEvent) + Send>;
#[cfg(target_arch = "wasm32")]
type DebuggerCallback = Box<dyn FnMut(&Debugger, DebuggerEvent)>;

#[derive(Default)]
pub struct Debugger {
    write_breakpoints: HashSet<u16>,
    read_breakpoints: HashSet<u16>,
    jump_breakpoints: HashSet<u16>,
    execute_breakpoints: HashSet<u16>,
    /// Break if a interrupt is flagged and enabled.
    interrupt_breakpoint: bool,
    breakpoints: BTreeMap<u16, u8>,
    watchs: BTreeSet<u16>,
    /// Address to stop at
    pub target_address: Option<u16>,
    /// Clock to stop at
    pub target_clock: Option<u64>,
    /// The clock_count in the previous instruction, used for stepback.
    pub last_op_clock: Option<u64>,
    /// Callback called when self is mutated
    pub callback: Option<DebuggerCallback>,
}
impl Debugger {
    pub fn execute_command<'a>(&mut self, gb: &GameBoy, args: &[&'a str]) -> Result<(), String> {
        use DebuggerEvent::*;
        let callback = |a: &mut Debugger, b| {
            let mut callback = a.callback.take();
            if let Some(callback) = &mut callback {
                callback(a, b);
            }
            a.callback = callback;
        };
        self.target_address = None;
        self.target_clock = None;
        match args[0] {
            "step" | "" => callback(self, Step),
            "stepback" => callback(self, StepBack),

            "reset" => callback(self, Reset),
            "runto" => {
                if args.len() != 2 {
                    return Err(format!(
                        "'runto' expect 1 argument, receive {}",
                        args.len() - 1
                    ));
                }
                let address = match u16::from_str_radix(args[1], 16) {
                    Ok(x) => x,
                    Err(_) => {
                        return Err(format!(
                            "'runto' expected a address, '{}' is not a valid one",
                            args[1]
                        ))
                    }
                };
                self.target_address = Some(address);
                callback(self, Run);
            }
            "run" => {
                if args.len() == 1 {
                    callback(self, Run);
                } else if args.len() == 3 {
                    let clocks = match args[2].parse::<u64>() {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(format!(
                            "'run's subcommand' expected a clock number, '{}' is not a valid one",
                            args[2]
                        ))
                        }
                    };
                    match args[1] {
                        "for" => {
                            self.target_clock = Some(gb.clock_count + clocks);
                            callback(self, Run);
                        }
                        "until" => {
                            self.target_clock = Some(clocks);
                            callback(self, Run);
                        }
                        _ => {
                            return Err(format!(
                                "'{}' is not a valid subcommand for 'run'",
                                args[1]
                            ))
                        }
                    }
                } else {
                    return Err(format!(
                        "'run' expect 0 or 2 arguments, receive {}",
                        args.len() - 1
                    ));
                }
            }
            "break" => {
                if args.len() == 2 {
                    if let "interrupt" = args[1] {
                        self.interrupt_breakpoint = true;
                        return Ok(());
                    }
                }
                if args.len() != 3 {
                    return Err(format!(
                        "'break' expect 3 arguments, receive {}",
                        args.len() - 1
                    ));
                }

                let flags = args[1].as_bytes();
                if let Some(x) = flags.iter().find(|x| !b"wrxj".contains(x)) {
                    return Err(format!(
                        "'{}' is not a valid break flag. Valid ones are 'r', 'w', 'x' and 'j'.",
                        *x as char
                    ));
                }

                let write = args[1].contains('w') as u8;
                let read = args[1].contains('r') as u8;
                let execute = args[1].contains('x') as u8;
                let jump = args[1].contains('j') as u8;

                use break_flags::*;
                let flags = (write * WRITE) | (read * READ) | (execute * EXECUTE) | (jump * JUMP);

                let address = match u16::from_str_radix(args[2], 16) {
                    Ok(x) => x,
                    Err(_) => {
                        return Err(format!(
                            "'break' expected a address, '{}' is not a valid one",
                            args[2]
                        ))
                    }
                };

                self.add_break(flags, address);
            }
            "watch" => {
                if args.len() != 2 {
                    return Err(format!(
                        "'watch' expect 1 argument, receive {}",
                        args.len() - 1
                    ));
                }

                let address = match u16::from_str_radix(args[1], 16) {
                    Ok(x) => x,
                    Err(_) => {
                        return Err(format!(
                            "'watch' expected a address, '{}' is not a valid one",
                            args[1]
                        ))
                    }
                };

                self.add_watch(address);
            }
            // write the currently dissasembly to a file
            "dump" => {
                if args.len() != 2 {
                    return Err(format!(
                        "'dump' expect 1 argument, receive {}",
                        args.len() - 1
                    ));
                }
                let file = args[1];
                let trace = gb.trace.borrow();
                let mut string = String::new();
                trace.fmt(gb, &mut string).map_err(|x| x.to_string())?;
                std::fs::write(file, string).map_err(|x| x.to_string())?;
            }
            // save some state to a file (for dev purposes)
            "save" => {
                if args.len() != 2 {
                    return Err(format!(
                        "'save' expect 1 argument, receive {}",
                        args.len() - 1
                    ));
                }
                let dir = args[1];

                // save to file
                let stf = |name: &str| {
                    let path = dir.to_string() + "/" + name;
                    std::fs::File::create(path).unwrap()
                };

                // gb.trace.save_state(output)?;
                gb.cpu
                    .save_state(&mut stf("cpu.sav"))
                    .map_err(|x| x.to_string())?;
                // gb.cartridge.save_state(&mut stf("cpu.sav")).map_err(|x| x.to_string())?;
                gb.wram
                    .save_state(&mut stf("wram.sav"))
                    .map_err(|x| x.to_string())?;
                gb.hram
                    .save_state(&mut stf("hram.sav"))
                    .map_err(|x| x.to_string())?;
                // gb.boot_rom.save_state(output).map_err(|x| x.to_string())?;
                [&gb.boot_rom_active]
                    .save_state(&mut stf("boot_rom_active.sav"))
                    .map_err(|x| x.to_string())?;
                gb.clock_count
                    .save_state(&mut stf("clock_count.sav"))
                    .map_err(|x| x.to_string())?;
                gb.timer
                    .save_state(&mut stf("timer.sav"))
                    .map_err(|x| x.to_string())?;
                {
                    let mut sound = gb.sound.borrow_mut();
                    sound.update(gb.clock_count);
                    sound
                        .save_state(&mut stf("sound.sav"))
                        .map_err(|x| x.to_string())?;
                }
                gb.ppu
                    .borrow()
                    .save_state(&mut stf("ppu.sav"))
                    .map_err(|x| x.to_string())?;
                gb.joypad
                    .save_state(&mut stf("joypad.sav"))
                    .map_err(|x| x.to_string())?;
                // gb.serial_transfer.save_state(data)?;
                // gb.v_blank.save_state(data)
            }
            x => return Err(format!("'{}' is not a valid command", x)),
        }
        Ok(())
    }

    pub fn breakpoints(&self) -> &BTreeMap<u16, u8> {
        &self.breakpoints
    }

    pub fn remove_break(&mut self, address: u16) {
        let address = &address;
        self.breakpoints.remove(address);
        self.read_breakpoints.remove(address);
        self.jump_breakpoints.remove(address);
        self.write_breakpoints.remove(address);
        self.execute_breakpoints.remove(address);

        let mut take = self.callback.take();
        if let Some(x) = take.as_mut() {
            x(self, DebuggerEvent::BreakpointsUpdate)
        }
        self.callback = take;
    }

    pub fn add_break(&mut self, flags: u8, address: u16) {
        debug_assert!(flags & 0xF0 == 0);
        *self.breakpoints.entry(address).or_default() |= flags;
        if (flags & break_flags::WRITE) != 0 {
            self.write_breakpoints.insert(address);
        }
        if (flags & break_flags::READ) != 0 {
            self.read_breakpoints.insert(address);
        }
        if (flags & break_flags::EXECUTE) != 0 {
            self.execute_breakpoints.insert(address);
        }
        if (flags & break_flags::JUMP) != 0 {
            self.jump_breakpoints.insert(address);
        }
        let mut take = self.callback.take();
        if let Some(x) = take.as_mut() {
            x(self, DebuggerEvent::BreakpointsUpdate)
        }
        self.callback = take;
    }

    pub fn watchs(&self) -> &BTreeSet<u16> {
        &self.watchs
    }

    pub fn remove_watch(&mut self, address: u16) {
        self.watchs.remove(&address);
        let mut take = self.callback.take();
        if let Some(x) = take.as_mut() {
            x(self, DebuggerEvent::WatchsUpdate)
        }
        self.callback = take;
    }

    pub fn add_watch(&mut self, address: u16) {
        self.watchs.insert(address);
        let mut take = self.callback.take();
        if let Some(x) = take.as_mut() {
            x(self, DebuggerEvent::WatchsUpdate)
        }
        self.callback = take;
    }

    pub fn check_break(&self, inter: &mut Interpreter) -> bool {
        let writes = inter.will_write_to();
        for w in &writes.1[..writes.0 as usize] {
            if self.write_breakpoints.contains(w) {
                return true;
            }
        }
        let reads = inter.will_read_from();
        for r in &reads.1[..reads.0 as usize] {
            if self.read_breakpoints.contains(r) {
                return true;
            }
        }
        if let Some(jump) = inter.will_jump_to() {
            if self.jump_breakpoints.contains(&jump) {
                return true;
            }
        }
        if self.execute_breakpoints.contains(&inter.0.cpu.pc) {
            return true;
        }
        false
    }

    pub fn step(&mut self, gb: &mut GameBoy) -> RunResult {
        self.run_until(gb, gb.clock_count)
    }

    pub fn run_for(&mut self, gb: &mut GameBoy, clocks: u64) -> RunResult {
        self.run_until(gb, gb.clock_count + clocks)
    }

    /// Run the gameboy emulator until it trigger a breakpoint, or the clock count surpess
    /// `timeout_clock`. It will always run at least one step.
    pub fn run_until(&mut self, gb: &mut GameBoy, timeout_clock: u64) -> RunResult {
        let mut inter = Interpreter(gb);

        let timeout_clock = if let Some(clock) = self.target_clock {
            timeout_clock.min(clock)
        } else {
            timeout_clock
        };

        let result = loop {
            self.last_op_clock = Some(inter.0.clock_count);
            inter.interpret_op();

            if Some(inter.0.cpu.pc) == self.target_address {
                self.target_address = None;
                break RunResult::ReachTargetAddress;
            } else if inter.0.clock_count >= timeout_clock {
                if Some(inter.0.clock_count) == self.target_clock {
                    self.target_clock = None;
                    break RunResult::ReachTargetClock;
                } else {
                    break RunResult::TimeOut;
                };
            }

            if self.check_break(&mut inter) {
                break RunResult::ReachBreakpoint;
            }
            if self.interrupt_breakpoint {
                let interrupts: u8 = inter.0.interrupt_flag & inter.0.interrupt_enabled;
                if interrupts != 0 && inter.0.cpu.ime == crate::gameboy::cpu::ImeState::Enabled {
                    break RunResult::ReachBreakpoint;
                }
            }
        };

        // clear the audio output
        let clock_count = inter.0.clock_count;
        let _ = inter.0.sound.borrow_mut().get_output(clock_count);

        result
    }

    // pub fn set_target_address(&mut self, address: Option<u16>) {
    //     self.target_address = address;
    // }

    // pub fn set_target_clock(&mut self, clock: Option<u64>) {
    //     self.target_clock = clock;
    // }
}
