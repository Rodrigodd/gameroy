use std::{
    collections::VecDeque,
    io::Write,
    path::PathBuf,
    sync::{
        mpsc::{Receiver, TryRecvError},
        Arc,
    },
    time::{Duration, Instant},
};

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
use winit::event_loop::EventLoopProxy;

use super::UserEvent;

#[derive(Debug)]
pub enum EmulatorEvent {
    Kill,
    RunFrame,
    FrameLimit(bool),
    Rewind(bool),
    SetJoypad(u8),
    Debug(bool),
    Step,
    StepBack,
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

#[cfg(test)]
mod test {
    use std::io::Write;

    use super::CircularBuffer;
    #[test]
    fn circular_buffer() {
        let mut buffer = CircularBuffer {
            buffer: vec![0; 9].into(),
            head: 0,
            tail: 0,
        };
        eprintln!("{:?}", buffer);
        assert_eq!(buffer.free_len(), 8);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[0, 1, 2, 3]).unwrap();
        assert_eq!(w, 4);
        assert_eq!(buffer.free_len(), 4);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[4, 5]).unwrap();
        assert_eq!(w, 2);
        assert_eq!(buffer.free_len(), 2);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[6, 7, 8, 9]).unwrap();
        assert_eq!(w, 2);
        assert_eq!(buffer.free_len(), 0);

        eprintln!("{:?}", buffer);
        buffer.tail = 2;
        assert_eq!(buffer.free_len(), 2);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[10, 11, 12, 13]).unwrap();
        assert_eq!(w, 2);
        assert_eq!(buffer.free_len(), 0);

        eprintln!("{:?}", buffer);
        buffer.tail += 4;
        assert_eq!(buffer.free_len(), 4);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[14, 15]).unwrap();
        assert_eq!(w, 2);
        assert_eq!(buffer.free_len(), 2);

        eprintln!("{:?}", buffer);
        let w = buffer.write(&[16, 17, 18, 19]).unwrap();
        assert_eq!(w, 2);
        assert_eq!(buffer.free_len(), 0);

        eprintln!("{:?}", buffer);
        assert_eq!(&*buffer.buffer, &[11, 14, 15, 16, 17, 5, 6, 7, 10])
    }
}

#[derive(Debug)]
// this buffer is empty if head == tail, and full if head == tail - 1.
struct CircularBuffer {
    /// The underline buffer.The
    buffer: Box<[u8]>,
    /// Next byte to write to. This is index of the first byte of the free area.
    head: usize,
    /// Next byte to read from. First byte of the used area.
    tail: usize,
}
impl CircularBuffer {
    fn new(capacity: usize) -> Self {
        Self {
            buffer: vec![0; capacity].into(),
            head: 0,
            tail: 0,
        }
    }

    /// Return true if there is no more free space.
    fn is_full(&self) -> bool {
        (self.head + 1) % self.buffer.len() == self.tail
    }

    /// Return true if the used space is empty.
    fn is_empty(&self) -> bool {
        self.head == self.tail
    }
    /// The length of free space in the buffer.
    fn free_len(&self) -> usize {
        let len = self.buffer.len();
        let l = self.tail + len - self.head;
        if l == len {
            len - 1
        } else {
            l % len - 1
        }
    }

    /// Copy the slice in the given circular range, to the given vector.
    fn copy_slice(&self, (start, end): (usize, usize), out: &mut Vec<u8>) {
        if start <= end {
            out.extend_from_slice(&self.buffer[start..end]);
        } else {
            out.extend_from_slice(&self.buffer[start..]);
            out.extend_from_slice(&self.buffer[0..end]);
        }
    }
}
impl std::io::Write for CircularBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.is_full() {
            return Ok(0);
        }
        let len = self.buffer.len();
        // end of the free area is tail - 1, wrapping around
        let end = (self.tail + len - 1) % len;
        if self.head < end {
            let w = self.buffer[self.head..end].as_mut().write(buf)?;
            self.head += w;
            Ok(w)
        } else {
            let mut write = self.buffer[self.head..].as_mut().write(buf)?;
            if write == self.buffer.len() - self.head {
                let w = self.buffer[0..end].as_mut().write(&buf[write..])?;
                write += w;
                self.head = w;
            } else {
                self.head += write;
            }
            Ok(write)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

struct Timeline {
    /// a buffer for transiente use.
    buffer: Vec<u8>,
    /// Stores multiples savestates, for the savestate timeline.
    savestate_buffer: CircularBuffer,
    /// a list of pairs (frame, clock_count, circular_range), listing the game state for each
    /// frame. The data is stored in the savestate_buffer.
    savestate_timeline: VecDeque<(usize, u64, (usize, usize))>,
    /// Current pressed keys by the user
    current_joypad: u8,
    /// Current frame being emulated
    current_frame: usize,
    /// The state of the joypad for each frame
    joypad_timeline: Vec<u8>,

    /// If the emulator is currently rewinding.
    rewinding: bool,
}
impl Timeline {
    fn new(current_frame: usize, joypad_timeline: Vec<u8>) -> Self {
        let kib = 2usize.pow(10);
        let mib = 2usize.pow(20);
        Self {
            buffer: Vec::with_capacity(64 * kib),
            current_frame,
            joypad_timeline,
            savestate_buffer: CircularBuffer::new(32 * mib),
            savestate_timeline: VecDeque::new(),
            current_joypad: 0xff,
            rewinding: false,
        }
    }

    fn save_state(&mut self, gb: &GameBoy) {
        self.buffer.clear();
        gb.save_state(&mut self.buffer).unwrap();
        // println!("{:.3} KiB", (self.buffer.len() as f32) * 2f32.powi(-10));
        while self.savestate_buffer.free_len() < self.buffer.len() {
            let (_, _, (_, end)) = self
                .savestate_timeline
                .pop_front()
                .expect("not enough size to save state");
            self.savestate_buffer.tail = end;
        }
        let start = self.savestate_buffer.head;
        self.savestate_buffer.write_all(&self.buffer).unwrap();
        let end = self.savestate_buffer.head;
        self.savestate_timeline
            .push_back((self.current_frame, gb.clock_count, (start, end)));
    }

    fn last_frame_clock_count(&self) -> Option<u64> {
        self.savestate_timeline.back().map(|&(_, x, _)| x)
    }

    /// Load the save sate of the last frame in the given GameBoy.
    fn load_last_frame(&mut self, gb: &mut GameBoy) -> bool {
        let &(_last_frame, clock_count, range) = if let Some(x) = self.savestate_timeline.back() {
            x
        } else {
            debug_assert!(self.savestate_buffer.is_empty());
            return false;
        };

        self.buffer.clear();
        debug_assert!(self.savestate_buffer.head == range.1);
        self.savestate_buffer.copy_slice(range, &mut self.buffer);
        gb.load_state(&mut std::io::Cursor::new(&self.buffer))
            .unwrap();
        debug_assert_eq!(gb.clock_count, clock_count);
        true
    }

    /// Remove the save state of the last frame
    fn pop_last_frame(&mut self) -> bool {
        let (last_frame, _clock_count, range) = if let Some(x) = self.savestate_timeline.pop_back()
        {
            x
        } else {
            debug_assert!(self.savestate_buffer.is_empty());
            return false;
        };

        self.savestate_buffer.head = range.0;
        self.current_frame = last_frame;
        true
    }

    /// Get next joypad and increase the current frame.
    fn next_frame(&mut self, gb: &GameBoy) -> u8 {
        let joy = if self.current_frame < self.joypad_timeline.len() {
            self.joypad_timeline[self.current_frame]
        } else {
            // if we are several frames behind, fill then with zeros
            let diff = self.current_frame - self.joypad_timeline.len();
            self.joypad_timeline.extend((0..diff).map(|_| 0xff));

            self.joypad_timeline.push(self.current_joypad);
            self.current_joypad
        };
        self.save_state(gb);
        self.current_frame += 1;
        if self.current_frame % 30 == 0 {
            log::trace!(
                "saved frames: {:3}, free_len: {:4}",
                self.savestate_timeline.len(),
                self.savestate_buffer.free_len()
            );
        }
        joy
    }
}

pub struct Emulator {
    gb: Arc<ParkMutex<GameBoy>>,
    recv: Receiver<EmulatorEvent>,
    proxy: EventLoopProxy<UserEvent>,

    joypad: Arc<ParkMutex<Timeline>>,

    rom_path: PathBuf,
    save_path: PathBuf,

    debug: bool,
    state: EmulatorState,
    // When true, the program will sync the time that passed, and the time that is emulated.
    frame_limit: bool,
    rewind: bool,
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

        let mut sound = audio.new_sound(buffer).unwrap();
        sound.set_loop(true);
        sound.play();
        std::mem::forget(sound);

        let clock_count = {
            let gb = gb.lock();
            gb.sound.borrow_mut().sample_frequency = audio.sample_rate() as u64;
            gb.clock_count
        };

        let frame_clock_count = 154 * 456;
        let current_frame = (clock_count / frame_clock_count) as usize;
        const BOOT_FRAMES: u64 = 23_384_580 / (154 * 456);
        let joypad_timeline = movie.map_or(Vec::new(), |m| {
            (0..BOOT_FRAMES)
                .map(|_| 0)
                .chain(m.controller_data.into_iter())
                .map(|x| {
                    let joy = !(x as u8);
                    ((joy & 0x0F) << 4) | (joy >> 4)
                })
                .collect()
        });
        let joypad = Arc::new(ParkMutex::new(Timeline::new(
            current_frame,
            joypad_timeline,
        )));
        {
            let game_boy = &mut gb.lock();
            let mut old = game_boy.v_blank.take();
            let joypad = joypad.clone();
            game_boy.v_blank = Some(Box::new(move |gb| {
                old.as_mut().map(|x| x(gb));
                let joypad = &mut *joypad.lock();
                if !joypad.rewinding {
                    gb.joypad = joypad.next_frame(gb);
                }
            }));
        }

        let start_time = Instant::now() - clock_to_duration(gb.lock().clock_count);

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
            rewind: false,
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
                        log::info!("save state");
                        self.gb.lock().save_state(&mut save_state).unwrap();
                    }
                    LoadState => {
                        let mut save_state =
                            std::fs::File::open(self.rom_path.with_extension("save_state"))
                                .unwrap();
                        self.gb.lock().load_state(&mut save_state).unwrap();
                        log::info!("load state");
                        self.proxy.send_event(UserEvent::EmulatorPaused).unwrap();
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
                            self.start_time =
                                Instant::now() - clock_to_duration(self.gb.lock().clock_count);
                        }
                    }
                    Rewind(value) => {
                        self.rewind = value;
                        let joypad = &mut *self.joypad.lock();
                        joypad.rewinding = value;
                        joypad.joypad_timeline.clear();
                    }
                    SetJoypad(joypad) => {
                        self.joypad.lock().current_joypad = joypad;
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
                    StepBack => {
                        if self.debug {
                            let mut gb = self.gb.lock();
                            let mut joypad = self.joypad.lock();
                            if let Some(last_clock_count) = joypad.last_frame_clock_count() {
                                let clock_count = gb.clock_count;
                                // currently, the maximum number of clocks that interpret_op
                                // elapses is 24, so if the last_frame_clock_count is recent than
                                // that, this could be after the last instruction. So pop it.
                                if last_clock_count > clock_count - 24 {
                                    joypad.pop_last_frame();
                                }
                                if joypad.load_last_frame(&mut *gb) {
                                    drop(joypad);
                                    drop(gb);
                                    self.debugger.lock().target_clock = Some(clock_count - 1);
                                    self.set_state(EmulatorState::Run);
                                }
                            } else {
                                log::warn!("there is no last frame");
                            }
                        }
                    }
                    Run => {
                        if self.debug {
                            self.set_state(EmulatorState::Run);
                            // Run a single step, to ignore the current breakpoint
                            Interpreter(&mut *self.gb.lock()).interpret_op();
                        }
                    }
                    Reset => {
                        self.gb.lock().reset();
                        self.state = EmulatorState::Idle;
                        // This will send EmulatorUpdated to the gui
                        self.proxy.send_event(UserEvent::EmulatorPaused).unwrap();
                    }
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
                        if self.rewind {
                            let gb = &mut *self.gb.lock();
                            {
                                let joypad = &mut *self.joypad.lock();
                                if joypad.load_last_frame(gb) {
                                    joypad.pop_last_frame();
                                }
                            }
                            {
                                let mut c = gb.v_blank.take();
                                c.as_mut().map(|c| c(gb));
                                gb.v_blank = c;
                            }
                            self.state = EmulatorState::Idle;
                        } else if self.frame_limit {
                            let mut gb = self.gb.lock();
                            let mut inter = Interpreter(&mut *gb);
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
                                let mut gb = self.gb.lock();
                                let mut inter = Interpreter(&mut *gb);
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

        log::info!("exiting emulator thread");

        log::info!("saving game data to {}... ", self.save_path.display());
        match std::fs::write(&self.save_path, &mut self.gb.lock().cartridge.ram) {
            Ok(_) => log::info!("save success"),
            Err(x) => log::error!("saving failed: {}", x),
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

fn clock_to_duration(clock_count: u64) -> Duration {
    let secs = clock_count / CLOCK_SPEED;
    let nanos = (clock_count % CLOCK_SPEED) * 1_000_000_000 / CLOCK_SPEED;
    Duration::new(secs, nanos as u32)
}
