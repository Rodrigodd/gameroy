use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::gameboy::GameBoy;
use crate::interpreter::Interpreter;

pub mod break_flags {
    pub const WRITE: u8 = 1 << 0;
    pub const READ: u8 = 1 << 1;
    pub const EXECUTE: u8 = 1 << 2;
    pub const JUMP: u8 = 1 << 3;
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RunResult {
    ReachBreakpoint,
    ReachTargetAddress,
    ReachTargetClock,
    TimeOut,
}

pub enum DebuggerEvent {
    Step,
    Reset,
    Run,
    BreakpointsUpdate,
    WatchsUpdate,
}

#[derive(Default)]
pub struct Debugger {
    write_breakpoints: HashSet<u16>,
    read_breakpoints: HashSet<u16>,
    jump_breakpoints: HashSet<u16>,
    execute_breakpoints: HashSet<u16>,
    breakpoints: BTreeMap<u16, u8>,
    watchs: BTreeSet<u16>,
    /// Address to stop at
    target_address: Option<u16>,
    /// Clock to stop at
    target_clock: Option<u64>,
    /// Callback called when self is mutated
    pub callback: Option<Box<dyn FnMut(&Self, DebuggerEvent) + Send>>,
}
impl Debugger {
    pub fn execute_command(&mut self, gb: &GameBoy, args: &[&str]) {
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
            "reset" => callback(self, Reset),
            "runto" => {
                if args.len() != 2 {
                    // report a error!!
                    return;
                }
                let address = match u16::from_str_radix(args[1], 16) {
                    Ok(x) => x,
                    Err(_) => return,
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
                        Err(_) => return,
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
                        _ => return,
                    }
                } else {
                    // report a error!!
                    return;
                }
            }
            "break" => {
                if args.len() != 3 {
                    // report a error!!
                    return;
                }

                let address = match u16::from_str_radix(args[2], 16) {
                    Ok(x) => x,
                    Err(_) => return,
                };

                let write = args[1].contains('w') as u8;
                let read = args[1].contains('r') as u8;
                let execute = args[1].contains('x') as u8;
                let jump = args[1].contains('j') as u8;

                use break_flags::*;
                let flags = (write * WRITE) | (read * READ) | (execute * EXECUTE) | (jump * JUMP);

                self.add_break(flags, address);
            }
            "watch" => {
                if args.len() != 2 {
                    // report a error!!
                    return;
                }

                let address = match u16::from_str_radix(args[1], 16) {
                    Ok(x) => x,
                    Err(_) => return,
                };

                self.add_watch(address);
            }
            // write the currently dissasembly to a file
            "dump" => {
                if args.len() != 2 {
                    // report a error!!
                    return;
                }
                let file = args[1];
                let trace = gb.trace.borrow();
                let mut string = String::new();
                trace.fmt(gb, &mut string).unwrap();
                std::fs::write(file, string).unwrap();
            }
            _ => return,
        }
    }

    pub fn breakpoints(&self) -> &BTreeMap<u16, u8> {
        &self.breakpoints
    }

    pub fn remove_break(&mut self, address: u16) {
        println!("remove break {:02}", address);
        let address = &address;
        self.breakpoints.remove(address);
        self.read_breakpoints.remove(address);
        self.jump_breakpoints.remove(address);
        self.write_breakpoints.remove(address);
        self.execute_breakpoints.remove(address);

        let mut take = self.callback.take();
        take.as_mut()
            .map(|x| x(self, DebuggerEvent::BreakpointsUpdate));
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
        take.as_mut()
            .map(|x| x(self, DebuggerEvent::BreakpointsUpdate));
        self.callback = take;
    }

    pub fn watchs(&self) -> &BTreeSet<u16> {
        &self.watchs
    }

    pub fn remove_watch(&mut self, address: u16) {
        self.watchs.remove(&address);
        let mut take = self.callback.take();
        take.as_mut().map(|x| x(self, DebuggerEvent::WatchsUpdate));
        self.callback = take;
    }

    pub fn add_watch(&mut self, address: u16) {
        self.watchs.insert(address);
        let mut take = self.callback.take();
        take.as_mut().map(|x| x(self, DebuggerEvent::WatchsUpdate));
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

    pub fn run_for(&mut self, gb: &mut GameBoy, clocks: u64) -> RunResult {
        self.run_until(gb, gb.clock_count + clocks)
    }

    pub fn run_until(&mut self, gb: &mut GameBoy, target_clock: u64) -> RunResult {
        let mut inter = Interpreter(gb);
        let target_clock = if let Some(clock) = self.target_clock {
            target_clock.min(clock)
        } else {
            target_clock
        };
        while inter.0.clock_count < target_clock {
            if self.check_break(&mut inter) {
                return RunResult::ReachBreakpoint;
            }
            inter.interpret_op();
            if Some(inter.0.cpu.pc) == self.target_address {
                self.target_address = None;
                return RunResult::ReachTargetAddress;
            }
        }
        if let Some(clock) = self.target_clock {
            if gb.clock_count >= clock {
                self.target_clock = None;
                RunResult::ReachTargetClock
            } else {
                RunResult::TimeOut
            }
        } else {
            RunResult::TimeOut
        }
    }

    // pub fn set_target_address(&mut self, address: Option<u16>) {
    //     self.target_address = address;
    // }

    // pub fn set_target_clock(&mut self, clock: Option<u64>) {
    //     self.target_clock = clock;
    // }
}
