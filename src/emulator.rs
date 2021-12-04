use gameroy::interpreter::Interpreter;
use parking_lot::Mutex;
use std::{
    collections::HashSet,
    sync::{
        mpsc::{Receiver, TryRecvError},
        Arc,
    },
    time::{Duration, Instant},
};
use winit::event_loop::EventLoopProxy;

use super::UserEvent;

pub mod break_flags {
    pub const WRITE: u8 = 1 << 0;
    pub const EXECUTE: u8 = 1 << 1;
    pub const JUMP: u8 = 1 << 2;
}

pub enum EmulatorEvent {
    RunFrame,
    FrameLimit(bool),
    SetKeys(u8),
    Debug(bool),
    Step,
    RunTo(u16),
    Run,
    // flags, address
    AddBreakpoint(u8, u16),
}

#[derive(PartialEq, Eq)]
enum EmulatorState {
    Idle,
    Run,
    RunTo(u16),
    RunNoBreak,
}

// The number of clocks the gameboy runs per second.
const CLOCK_SPEED: u64 = 4_194_304;

struct Breakpoints {
    write_breakpoints: HashSet<u16>,
    jump_breakpoints: HashSet<u16>,
    execute_breakpoints: HashSet<u16>,
}
impl Breakpoints {
    fn check(&mut self, inter: &mut Interpreter) -> bool {
        let writes = inter.will_write_to();
        for w in &writes.1[..writes.0 as usize] {
            if self.write_breakpoints.contains(w) {
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
}

pub struct Emulator {
    inter: Arc<Mutex<Interpreter>>,
    recv: Receiver<EmulatorEvent>,
    proxy: EventLoopProxy<UserEvent>,

    debug: bool,
    state: EmulatorState,
    // When true, the program will sync the time that passed, and the time that is emulated.
    frame_limit: bool,
    // The instant in time that the gameboy supposedly was turned on.
    // Change when frame_limit is disabled.
    start_time: Instant,

    breakpoints: Breakpoints,
}
impl Emulator {
    pub fn run(
        inter: Arc<Mutex<Interpreter>>,
        recv: Receiver<EmulatorEvent>,
        proxy: EventLoopProxy<UserEvent>,
    ) {
        Self {
            inter,
            recv,
            proxy,
            debug: false,
            state: EmulatorState::Idle,
            frame_limit: true,
            start_time: Instant::now(),
            breakpoints: Breakpoints {
                write_breakpoints: HashSet::new(),
                execute_breakpoints: HashSet::new(),
                jump_breakpoints: HashSet::new(),
            },
        }
        .event_loop()
    }

    fn set_state(&mut self, new_state: EmulatorState) {
        if self.state == EmulatorState::Idle {
            self.proxy.send_event(UserEvent::EmulatorStarted).unwrap();
        }
        if new_state == EmulatorState::Idle {
            self.proxy.send_event(UserEvent::EmulatorPaused).unwrap();
        }
        self.state = new_state;
    }

    fn event_loop(&mut self) {
        while let Ok(mut event) = self.recv.recv() {
            'handle_event: loop {
                use EmulatorEvent::*;
                match event {
                    RunFrame => {
                        if self.frame_limit && !self.debug {
                            {
                                let mut inter = self.inter.lock();
                                let elapsed = self.start_time.elapsed();
                                let mut target_clock = CLOCK_SPEED * elapsed.as_secs()
                                    + (CLOCK_SPEED as f64 * (elapsed.subsec_nanos() as f64 * 1e-9))
                                        as u64;
                                // make sure that the target_clock don't increase indefinitely if the program can't keep up.
                                if target_clock > inter.0.clock_count + CLOCK_SPEED / 30 {
                                    let expected_target = target_clock;
                                    target_clock = inter.0.clock_count + CLOCK_SPEED / 30;
                                    self.start_time += Duration::from_secs_f64(
                                        (expected_target - target_clock) as f64
                                            / CLOCK_SPEED as f64,
                                    );
                                }
                                while inter.0.clock_count < target_clock {
                                    inter.interpret_op();
                                }
                            }
                        }
                    }
                    FrameLimit(value) => {
                        self.frame_limit = value;
                        if self.frame_limit {
                            let inter = self.inter.lock();
                            let secs = inter.0.clock_count / CLOCK_SPEED;
                            let nanos =
                                (inter.0.clock_count % CLOCK_SPEED) * 1_000_000_000 / CLOCK_SPEED;
                            self.start_time = Instant::now() - Duration::new(secs, nanos as u32);
                        }
                    }
                    SetKeys(keys) => self.inter.lock().0.joypad = keys,
                    Debug(value) => {
                        self.debug = value;
                        if self.debug {
                            self.set_state(EmulatorState::Idle);
                        }
                    }
                    Step => {
                        if self.debug {
                            self.inter.lock().interpret_op();
                            self.set_state(EmulatorState::Idle);
                        }
                    }
                    RunTo(address) => {
                        if self.debug {
                            self.set_state(EmulatorState::RunTo(address));
                        }
                    }
                    Run => {
                        if self.debug {
                            self.set_state(EmulatorState::Run);
                            // Run a single step, to ignore the current breakpoint
                            self.inter.lock().interpret_op();
                        }
                    }
                    AddBreakpoint(flags, address) => {
                        if (flags & break_flags::WRITE) != 0 {
                            self.breakpoints.write_breakpoints.insert(address);
                        }
                        if (flags & break_flags::EXECUTE) != 0 {
                            self.breakpoints.execute_breakpoints.insert(address);
                        }
                        if (flags & break_flags::JUMP) != 0 {
                            self.breakpoints.jump_breakpoints.insert(address);
                        }
                    }
                }

                if !self.debug {
                    if self.frame_limit {
                        break;
                    } else {
                        self.state = EmulatorState::RunNoBreak;
                    }
                }

                match self.state {
                    EmulatorState::Idle => {}
                    EmulatorState::Run => 'run: loop {
                        // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                        {
                            let mut inter = self.inter.lock();
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                if self.breakpoints.check(&mut inter) {
                                    drop(inter);
                                    self.set_state(EmulatorState::Idle);
                                    break 'run;
                                }
                                inter.interpret_op();
                            }
                        }
                        match self.recv.try_recv() {
                            Ok(next_event) => {
                                event = next_event;
                                continue 'handle_event;
                            }
                            Err(TryRecvError::Disconnected) => return,
                            _ => {}
                        }
                    },
                    EmulatorState::RunTo(target_address) => 'runto: loop {
                        {
                            let mut inter = self.inter.lock();
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                if self.breakpoints.check(&mut inter) {
                                    drop(inter);
                                    self.set_state(EmulatorState::Idle);
                                    break 'runto;
                                }
                                inter.interpret_op();
                                if inter.0.cpu.pc == target_address {
                                    drop(inter);
                                    self.set_state(EmulatorState::Idle);
                                    break 'runto;
                                }
                            }
                        }
                        match self.recv.try_recv() {
                            Ok(next_event) => {
                                event = next_event;
                                continue 'handle_event;
                            }
                            Err(TryRecvError::Disconnected) => return,
                            _ => {}
                        }
                    },
                    EmulatorState::RunNoBreak => loop {
                        // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                        {
                            let mut inter = self.inter.lock();
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                inter.interpret_op();
                            }
                        }
                        match self.recv.try_recv() {
                            Ok(next_event) => {
                                event = next_event;
                                continue 'handle_event;
                            }
                            Err(TryRecvError::Disconnected) => return,
                            _ => {}
                        }
                    },
                }

                break;
            }
        }
    }
}
