use audio_engine::{AudioEngine, SoundSource};
use gameroy::{
    consts::CLOCK_SPEED, gameboy::GameBoy, interpreter::Interpreter, parser::Vbm,
    save_state::SaveState,
};
use parking_lot::Mutex as ParkMutex;
use std::{
    collections::{BTreeMap, HashSet, VecDeque, BTreeSet},
    path::PathBuf,
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
    pub const READ: u8 = 1 << 1;
    pub const EXECUTE: u8 = 1 << 2;
    pub const JUMP: u8 = 1 << 3;
}

#[derive(Debug)]
pub enum EmulatorEvent {
    Kill,
    RunFrame,
    FrameLimit(bool),
    SetJoypad(u8),
    Debug(bool),
    Step,
    Run,
    RunTo(u16),
    RunFor(u64),
    RunUntil(u64),
    Reset,
    AddBreakpoint { flags: u8, address: u16 },
    RemoveBreakpoint(u16),
    AddWatch(u16),
    RemoveWatch(u16),
    SaveState,
    LoadState,
}

#[derive(PartialEq, Eq, Debug)]
enum EmulatorState {
    Idle,
    Run,
    /// Runs until it reachs to given program counter
    RunTo(u16),
    /// Runs until it reachs to given clock count
    RunUntil(u64),
    RunNoBreak,
}

#[derive(Default)]
pub struct Breakpoints {
    write_breakpoints: HashSet<u16>,
    read_breakpoints: HashSet<u16>,
    jump_breakpoints: HashSet<u16>,
    execute_breakpoints: HashSet<u16>,
    breakpoints: BTreeMap<u16, u8>,
    watchs: BTreeSet<u16>,
}
impl Breakpoints {
    pub fn breakpoints(&self) -> &BTreeMap<u16, u8> {
        &self.breakpoints
    }

    fn remove_break(&mut self, address: u16) {
        let address = &address;
        self.breakpoints.remove(address);
        self.read_breakpoints.remove(address);
        self.jump_breakpoints.remove(address);
        self.execute_breakpoints.remove(address);
    }

    fn add_break(&mut self, flags: u8, address: u16) {
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
    }

    pub fn watchs(&self) -> &BTreeSet<u16> {
        &self.watchs

    }
    fn remove_watch(&mut self, address: u16) {
        self.watchs.remove(&address);
    }

    fn add_watch(&mut self, address: u16) {
        self.watchs.insert(address);
    }

    fn check_break(&mut self, inter: &mut Interpreter) -> bool {
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
}

struct Buffer {
    buffer: Arc<ParkMutex<VecDeque<i16>>>,
    sample_rate: u32,
}
impl SoundSource for Buffer {
    fn channels(&self) -> u16 {
        2
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn reset(&mut self) {
        self.buffer.lock().clear()
    }

    fn write_samples(&mut self, buffer: &mut [i16]) -> usize {
        let mut lock = self.buffer.lock();
        let len = lock.len().min(buffer.len());
        for (a, b) in buffer[0..len].iter_mut().zip(lock.iter()) {
            *a = *b;
        }
        lock.drain(0..len);

        // even if the writed length is smaller than the buffer, return the entire length to avoid
        // the audio source to be reseted
        buffer.len()
    }
}

struct Joypad {
    /// Current pressed keys by the user
    current: u8,
    movie: Option<Vbm>,
    frame: usize,
}
impl Joypad {
    fn get_next_joypad(&mut self) -> u8 {
        if let Some(movie) = &self.movie {
            self.frame += 1;
            let skip = 333;

            if self.frame >= skip {
                let joy = !(movie.controller_data[self.frame - skip] as u8);
                ((joy & 0x0F) << 4) | (joy >> 4)
            } else {
                0xff
            }
        } else {
            self.current
        }
    }
}

pub struct Emulator {
    gb: Arc<ParkMutex<GameBoy>>,
    recv: Receiver<EmulatorEvent>,
    proxy: EventLoopProxy<UserEvent>,

    joypad: Arc<ParkMutex<Joypad>>,

    rom_path: PathBuf,
    save_path: PathBuf,

    debug: bool,
    state: EmulatorState,
    // When true, the program will sync the time that passed, and the time that is emulated.
    frame_limit: bool,
    // The instant in time that the gameboy supposedly was turned on.
    // Change when frame_limit is disabled.
    start_time: Instant,

    breakpoints: Arc<ParkMutex<Breakpoints>>,

    _audio: AudioEngine,
    audio_buffer: Arc<ParkMutex<VecDeque<i16>>>,
    last_buffer_len: usize,
}
impl Emulator {
    pub fn run(
        gb: Arc<ParkMutex<GameBoy>>,
        breakpoints: Arc<ParkMutex<Breakpoints>>,
        recv: Receiver<EmulatorEvent>,
        proxy: EventLoopProxy<UserEvent>,
        movie: Option<Vbm>,
        rom_path: PathBuf,
        save_path: PathBuf,
    ) {
        let audio = AudioEngine::new().unwrap();
        let audio_buffer = Arc::new(ParkMutex::new(VecDeque::<i16>::new()));
        let buffer = Buffer {
            buffer: audio_buffer.clone(),
            sample_rate: audio.sample_rate(),
        };
        // println!("{}", buffer.sample_rate);
        // std::process::exit(0);

        gb.lock().sound.borrow_mut().sample_frequency = audio.sample_rate() as u64;

        let mut sound = audio.new_sound(buffer).unwrap();
        sound.set_loop(true);
        sound.play();
        std::mem::forget(sound);

        let joypad = Arc::new(ParkMutex::new(Joypad {
            current: 0xff,
            movie,
            frame: 0,
        }));
        {
            let game_boy = &mut gb.lock();
            let mut old = game_boy.v_blank.take();
            let joypad = joypad.clone();
            game_boy.v_blank = Some(Box::new(move |gb| {
                gb.joypad = joypad.lock().get_next_joypad();
                old.as_mut().map(|x| x(gb));
            }));
        }

        Self {
            gb,
            recv,
            proxy,
            joypad,
            rom_path,
            save_path,
            debug: false,
            state: EmulatorState::Idle,
            frame_limit: true,
            start_time: Instant::now(),
            breakpoints,
            _audio: audio,
            audio_buffer,
            last_buffer_len: 0,
        }
        .event_loop();
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
        'event_loop: while let Ok(mut event) = self.recv.recv() {
            'handle_event: loop {
                use EmulatorEvent::*;
                match event {
                    SaveState => {
                        let mut save_state = std::fs::OpenOptions::new()
                            .create(true)
                            .write(true)
                            .open(self.rom_path.with_extension("save_state"))
                            .unwrap();
                        self.gb.lock().save_state(&mut save_state).unwrap();
                    }
                    LoadState => {
                        let mut save_state =
                            std::fs::File::open(self.rom_path.with_extension("save_state"))
                                .unwrap();
                        self.gb.lock().load_state(&mut save_state).unwrap();
                    }
                    Kill => break 'event_loop,
                    RunFrame => {
                        if !self.debug {
                            self.set_state(EmulatorState::RunNoBreak);
                        }
                    }
                    FrameLimit(value) => {
                        self.frame_limit = value;
                        if self.frame_limit {
                            let gb = self.gb.lock();
                            let secs = gb.clock_count / CLOCK_SPEED;
                            let nanos =
                                (gb.clock_count % CLOCK_SPEED) * 1_000_000_000 / CLOCK_SPEED;
                            self.start_time = Instant::now() - Duration::new(secs, nanos as u32);
                        }
                    }
                    SetJoypad(joypad) => {
                        self.joypad.lock().current = joypad;
                    }
                    Debug(value) => {
                        self.debug = value;
                        if self.debug {
                            self.set_state(EmulatorState::Idle);
                        } else {
                            self.set_state(EmulatorState::RunNoBreak);
                        }
                    }
                    Step => {
                        if self.debug {
                            Interpreter(&mut *self.gb.lock()).interpret_op();
                            self.set_state(EmulatorState::Idle);
                        }
                    }
                    Run => {
                        if self.debug {
                            self.set_state(EmulatorState::Run);
                            // Run a single step, to ignore the current breakpoint
                            Interpreter(&mut *self.gb.lock()).interpret_op();
                        }
                    }
                    RunTo(address) => {
                        if self.debug {
                            self.set_state(EmulatorState::RunTo(address));
                        }
                    }
                    RunFor(clocks) => {
                        if self.debug {
                            let clock_count = self.gb.lock().clock_count;
                            self.set_state(EmulatorState::RunUntil(clock_count + clocks));
                        }
                    }
                    RunUntil(clock) => {
                        if self.debug {
                            self.set_state(EmulatorState::RunUntil(clock));
                        }
                    }
                    Reset => self.gb.lock().reset(),
                    AddBreakpoint { flags, address } => {
                        let mut breaks = self.breakpoints.lock();
                        breaks.add_break(flags, address);
                        self.proxy
                            .send_event(UserEvent::BreakpointsUpdated)
                            .unwrap();
                    }
                    RemoveBreakpoint(address) => {
                        let mut breaks = self.breakpoints.lock();
                        breaks.remove_break(address);
                        self.proxy
                            .send_event(UserEvent::BreakpointsUpdated)
                            .unwrap();
                    }
                    AddWatch(address) => {
                        let mut breaks = self.breakpoints.lock();
                        breaks.add_watch(address);
                        self.proxy
                            .send_event(UserEvent::WatchsUpdated)
                            .unwrap();
                    }
                    RemoveWatch(address) => {
                        let mut breaks = self.breakpoints.lock();
                        breaks.remove_watch(address);
                        self.proxy
                            .send_event(UserEvent::WatchsUpdated)
                            .unwrap();
                    }
                }

                match self.state {
                    EmulatorState::Idle => {}
                    EmulatorState::Run => 'run: loop {
                        // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                        {
                            let mut gb = self.gb.lock();
                            let mut inter = Interpreter(&mut *gb);
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                if self.breakpoints.lock().check_break(&mut inter) {
                                    drop(gb);
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
                            Err(TryRecvError::Disconnected) => break 'event_loop,
                            _ => {}
                        }
                    },
                    EmulatorState::RunTo(target_address) => 'runto: loop {
                        {
                            let mut gb = self.gb.lock();
                            let mut inter = Interpreter(&mut *gb);
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                if self.breakpoints.lock().check_break(&mut inter) {
                                    drop(gb);
                                    self.set_state(EmulatorState::Idle);
                                    break 'runto;
                                }
                                inter.interpret_op();
                                if inter.0.cpu.pc == target_address {
                                    drop(gb);
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
                            Err(TryRecvError::Disconnected) => break 'event_loop,
                            _ => {}
                        }
                    },
                    EmulatorState::RunUntil(final_clock) => 'rununtil: loop {
                        {
                            let mut gb = self.gb.lock();
                            let mut inter = Interpreter(&mut *gb);
                            let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;
                            while inter.0.clock_count < target_clock {
                                if self.breakpoints.lock().check_break(&mut inter) {
                                    drop(gb);
                                    self.set_state(EmulatorState::Idle);
                                    break 'rununtil;
                                }
                                inter.interpret_op();
                                if inter.0.clock_count >= final_clock {
                                    drop(gb);
                                    self.set_state(EmulatorState::Idle);
                                    break 'rununtil;
                                }
                            }
                        }
                        match self.recv.try_recv() {
                            Ok(next_event) => {
                                event = next_event;
                                continue 'handle_event;
                            }
                            Err(TryRecvError::Disconnected) => break 'event_loop,
                            _ => {}
                        }
                    },
                    EmulatorState::RunNoBreak => {
                        let mut gb = self.gb.lock();
                        let mut inter = Interpreter(&mut *gb);
                        if self.frame_limit {
                            let elapsed = self.start_time.elapsed();
                            let mut target_clock = CLOCK_SPEED * elapsed.as_secs()
                                + (CLOCK_SPEED as f64 * (elapsed.subsec_nanos() as f64 * 1e-9))
                                    as u64;

                            // make sure that the target_clock don't increase indefinitely if the program can't keep up.
                            if target_clock > inter.0.clock_count + CLOCK_SPEED / 30 {
                                let expected_target = target_clock;
                                target_clock = inter.0.clock_count + CLOCK_SPEED / 30;
                                self.start_time += Duration::from_secs_f64(
                                    (expected_target - target_clock) as f64 / CLOCK_SPEED as f64,
                                );
                            }

                            while inter.0.clock_count < target_clock {
                                inter.interpret_op();
                            }

                            drop(gb);
                            self.update_audio();

                            // change state to Idle, and wait for the next RunFrame

                            // I don't call self.set_state, because i don't want to send the
                            // EmulatorPaused event
                            self.state = EmulatorState::Idle;
                        } else {
                            // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                            loop {
                                let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;

                                while inter.0.clock_count < target_clock {
                                    inter.interpret_op();
                                }
                                // clear the audio output
                                let clock_count = inter.0.clock_count;
                                let _ = inter.0.sound.borrow_mut().get_output(clock_count);

                                match self.recv.try_recv() {
                                    Ok(next_event) => {
                                        event = next_event;
                                        continue 'handle_event;
                                    }
                                    Err(TryRecvError::Disconnected) => break 'event_loop,
                                    _ => {}
                                }
                            }
                        }
                    }
                }

                break;
            }
        }

        println!("exiting emulator thread");

        print!("saving game data to {}... ", self.save_path.display());
        match std::fs::write(&self.save_path, self.gb.lock().cartridge.ram_mut()) {
            Ok(_) => println!("success"),
            Err(x) => println!("error: {}", x),
        }
    }

    fn update_audio(&mut self) {
        let gb = self.gb.lock();
        let clock_count = gb.clock_count;
        let buffer = gb.sound.borrow_mut().get_output(clock_count);
        let mut lock = self.audio_buffer.lock();
        if lock.len() == 0 {
            // if the buffer is empty, add zeros to increase it
            lock.extend((0..1600 * 5).map(|_| 0));
        }
        lock.extend(buffer.iter().map(|&x| (x as i16 - 128) * 20));

        self.last_buffer_len = lock.len();
    }
}
