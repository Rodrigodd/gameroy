use audio_engine::{AudioEngine, SoundSource};
use gameroy::{
    consts::CLOCK_SPEED,
    debugger::{Debugger, RunResult},
    gameboy::GameBoy,
    interpreter::Interpreter,
    parser::Vbm,
    save_state::SaveState,
};
use parking_lot::Mutex as ParkMutex;
use std::{
    collections::VecDeque,
    path::PathBuf,
    sync::{
        mpsc::{Receiver, TryRecvError},
        Arc,
    },
    time::{Duration, Instant},
};
use winit::event_loop::EventLoopProxy;

use super::UserEvent;

#[derive(Debug)]
pub enum EmulatorEvent {
    Kill,
    RunFrame,
    FrameLimit(bool),
    SetJoypad(u8),
    Debug(bool),
    Step,
    Run,
    Reset,
    SaveState,
    LoadState,
}

#[derive(PartialEq, Eq, Debug)]
enum EmulatorState {
    Idle,
    Run,
    RunNoBreak,
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

    debugger: Arc<ParkMutex<Debugger>>,

    _audio: AudioEngine,
    audio_buffer: Arc<ParkMutex<VecDeque<i16>>>,
    last_buffer_len: usize,
}
impl Emulator {
    pub fn run(
        gb: Arc<ParkMutex<GameBoy>>,
        debugger: Arc<ParkMutex<Debugger>>,
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

        let start_time = {
            let gb = gb.lock();
            let secs = gb.clock_count / CLOCK_SPEED;
            let nanos = (gb.clock_count % CLOCK_SPEED) * 1_000_000_000 / CLOCK_SPEED;
            Instant::now() - Duration::new(secs, nanos as u32)
        };

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
            start_time,
            debugger,
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
                    Reset => self.gb.lock().reset(),
                }

                match self.state {
                    EmulatorState::Idle => {}
                    EmulatorState::Run => 'run: loop {
                        // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                        {
                            let mut debugger = self.debugger.lock();
                            let mut gb = self.gb.lock();
                            use RunResult::*;
                            match debugger.run_for(&mut *gb, CLOCK_SPEED / 600) {
                                ReachBreakpoint | ReachTargetAddress | ReachTargetClock => {
                                    drop(gb);
                                    drop(debugger);
                                    self.set_state(EmulatorState::Idle);
                                    break 'run;
                                }
                                TimeOut => {}
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
