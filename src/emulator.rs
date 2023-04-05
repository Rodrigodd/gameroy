use std::{collections::VecDeque, io::Write, sync::Arc, time::Duration};

#[cfg(feature = "audio-engine")]
use audio_engine::{AudioEngine, SoundSource};
use gameroy::{
    consts::CLOCK_SPEED,
    debugger::{Debugger, RunResult},
    diff_stack::DiffStack,
    gameboy::GameBoy,
    interpreter::Interpreter,
    parser::Vbm,
};
use instant::{Instant, SystemTime};
use parking_lot::Mutex as ParkMutex;
use winit::event_loop::EventLoopProxy;

use super::UserEvent;
use crate::rom_loading::RomFile;

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
    SaveRam,
    Pause,
    Resume,
}

#[derive(PartialEq, Eq, Debug)]
enum EmulatorState {
    /// Do nothing.
    Idle,
    /// Same as Idle, but transitioning from this state does not trigger a UserEvent::EmulatorStarted.
    WaitNextFrame,
    /// Run the emulator, while checking for breakpoints.
    Run,
    /// Run the emulator, without checking for breakpoints.
    RunNoBreak,
}

#[cfg(feature = "audio-engine")]
struct Buffer {
    buffer: Arc<ParkMutex<VecDeque<i16>>>,
    sample_rate: u32,
}
#[cfg(feature = "audio-engine")]
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

// This code is not being used anymore, but may be useful when implementing a circular buffer in
// DiffStack.
#[allow(dead_code)]
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
    /// a buffer for transient use.
    buffer: Vec<u8>,

    /// A stack of save-states delta-compressed.
    save_states: DiffStack,
    /// A second stack of save-states delta-compressed. Used to clear half of the save-states when
    /// the stacks become full.
    /// TODO: replace this by implementing a CircularBuffer in the DiffStack.
    save_states2: DiffStack,

    /// Current pressed keys by the user
    current_joypad: u8,
    /// Current frame being emulated
    current_frame: u32,
    /// The state of the joypad for each frame
    joypad_timeline: Vec<u8>,

    /// If the emulator is currently rewinding.
    rewinding: bool,
}
impl Timeline {
    fn new(current_frame: u32, joypad_timeline: Vec<u8>) -> Self {
        let kib = 2usize.pow(10);
        let mib = 2usize.pow(20);
        Self {
            buffer: Vec::with_capacity(64 * kib),
            current_frame,
            joypad_timeline,
            save_states: DiffStack::new(16 * mib),
            save_states2: DiffStack::new(16 * mib),
            current_joypad: 0xff,
            rewinding: false,
        }
    }

    fn save_state(&mut self, gb: &GameBoy) {
        self.buffer.clear();
        {
            self.buffer
                .write_all(&gb.clock_count.to_le_bytes())
                .unwrap();
            self.buffer
                .write_all(&self.current_frame.to_le_bytes())
                .unwrap();
            gb.save_state(timestamp(), &mut self.buffer).unwrap();
        }
        // println!("{:.3} KiB", (self.buffer.len() as f32) * 2f32.powi(-10));
        if !self.save_states.push(&self.buffer) {
            std::mem::swap(&mut self.save_states, &mut self.save_states2);

            println!("cleared after {} frames", self.save_states.count());
            self.save_states.clear();
            self.save_states.push(&self.buffer);
        }
    }

    fn last_frame_clock_count(&self) -> Option<u64> {
        Some(u64::from_le_bytes(
            self.save_states.top()?[0..8].try_into().unwrap(),
        ))
    }

    /// Load the save sate of the last frame in the given `GameBoy`. Returns false if there is no
    /// last frame.
    fn load_last_frame(&mut self, gb: &mut GameBoy) -> bool {
        let Some(last_frame) = self.save_states.top() else {
            debug_assert!(self.save_states.is_empty());
            return false;
        };

        gb.load_state(&mut &last_frame[12..]).unwrap();
        true
    }

    /// Remove the save state of the last frame
    fn pop_last_frame(&mut self) -> bool {
        let Some(last_frame) = self.save_states.top() else {
            debug_assert!(self.save_states.is_empty());
            return false;
        };

        self.current_frame = u32::from_le_bytes(last_frame[8..12].try_into().unwrap());

        self.save_states.pop();
        if self.save_states.is_empty() {
            std::mem::swap(&mut self.save_states, &mut self.save_states2);
        }

        true
    }

    /// Get next joypad and increase the current frame.
    fn next_frame(&mut self, gb: &GameBoy) -> u8 {
        let joy = if (self.current_frame as usize) < self.joypad_timeline.len() {
            self.joypad_timeline[self.current_frame as usize]
        } else {
            // if we are several frames behind, fill then with zeros
            let diff = self.current_frame as usize - self.joypad_timeline.len();
            self.joypad_timeline.extend((0..diff).map(|_| 0xff));

            self.joypad_timeline.push(self.current_joypad);
            self.current_joypad
        };
        self.save_state(gb);
        self.current_frame += 1;
        if self.current_frame % 30 == 0 {
            log::trace!(
                "saved frames: {:3}",
                self.save_states.count(),
                // self.save_states.free_len()
            );
        }
        joy
    }
}

#[cfg(feature = "audio-engine")]
struct SoundBackend {
    _audio_engine: AudioEngine,
    audio_buffer: Arc<ParkMutex<VecDeque<i16>>>,
    last_buffer_len: usize,
}

pub struct Emulator {
    gb: Arc<ParkMutex<GameBoy>>,
    proxy: EventLoopProxy<UserEvent>,

    #[cfg(target_arch = "x86_64")]
    jit_compiler: gameroy_jit::JitCompiler,

    joypad: Arc<ParkMutex<Timeline>>,

    rom: RomFile,

    debug: bool,
    state: EmulatorState,
    // When true, the program will sync the time that passed, and the time that is emulated.
    frame_limit: bool,
    rewind: bool,
    // The instant in time that the gameboy supposedly was turned on.
    // Change when frame_limit is disabled.
    start_time: Instant,

    debugger: Arc<ParkMutex<Debugger>>,

    #[cfg(feature = "audio-engine")]
    /// The sound backend.
    sound: Option<SoundBackend>,
}

pub enum Control {
    /// Continue polling
    Poll,
    /// Wait for the next event.
    Wait,
}

impl Emulator {
    pub fn new(
        gb: Arc<ParkMutex<GameBoy>>,
        debugger: Arc<ParkMutex<Debugger>>,
        proxy: EventLoopProxy<UserEvent>,
        movie: Option<Vbm>,
        rom: RomFile,
    ) -> Self {
        #[cfg(feature = "audio-engine")]
        let sound = match AudioEngine::new() {
            Ok(audio_engine) => {
                let audio_buffer = Arc::new(ParkMutex::new(VecDeque::<i16>::new()));
                let buffer = Buffer {
                    buffer: audio_buffer.clone(),
                    sample_rate: audio_engine.sample_rate(),
                };

                let mut sound = audio_engine.new_sound(buffer).unwrap();
                sound.set_loop(true);
                sound.play();
                std::mem::forget(sound);

                let mut gb = gb.lock();
                gb.sound.get_mut().sample_frequency = audio_engine.sample_rate() as u64;

                Some(SoundBackend {
                    _audio_engine: audio_engine,
                    audio_buffer,
                    last_buffer_len: 0,
                })
            }
            Err(e) => {
                log::error!("error at initiating AudioEngine: {}", e);
                None
            }
        };
        let clock_count = {
            let gb = gb.lock();
            gb.clock_count
        };
        let frame_clock_count = 154 * 456;
        let current_frame = (clock_count / frame_clock_count) as u32;
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
                if let Some(x) = old.as_mut() {
                    x(gb)
                }
                let joypad = &mut *joypad.lock();
                if !joypad.rewinding {
                    gb.joypad = joypad.next_frame(gb);
                }
            }));
        }
        let start_time = Instant::now() - clock_to_duration(gb.lock().clock_count);
        Self {
            gb,
            proxy,
            #[cfg(target_arch = "x86_64")]
            jit_compiler: gameroy_jit::JitCompiler::new(),
            joypad,
            rom,
            debug: false,
            state: EmulatorState::Idle,
            frame_limit: true,
            rewind: false,
            start_time,
            debugger,
            #[cfg(feature = "audio-engine")]
            sound,
        }
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

    #[cfg(feature = "threads")]
    pub fn event_loop(&mut self, recv: flume::Receiver<EmulatorEvent>) {
        'event_loop: while let Ok(mut event) = recv.recv() {
            'handle_event: loop {
                if self.handle_event(event) {
                    break 'event_loop;
                }
                'pool: loop {
                    match self.poll() {
                        Control::Poll => match recv.try_recv() {
                            Ok(next_event) => {
                                event = next_event;
                                continue 'handle_event;
                            }
                            Err(flume::TryRecvError::Disconnected) => break 'event_loop,
                            _ => {
                                continue 'pool;
                            }
                        },
                        Control::Wait => continue 'event_loop,
                    }
                }
            }
        }

        log::info!("exiting emulator thread");

        log::info!("saving game ram data... ");
        match self.rom.save_ram_data(&self.gb.lock().cartridge.ram) {
            Ok(_) => log::info!("save success"),
            Err(x) => log::error!("saving failed: {}", x),
        }
    }

    /// Return true if should terminate event_loop.
    pub fn handle_event(&mut self, event: EmulatorEvent) -> bool {
        use EmulatorEvent::*;
        match event {
            SaveRam => {
                log::info!("saving game ram data... ");
                match self.rom.save_ram_data(&self.gb.lock().cartridge.ram) {
                    Ok(_) => log::info!("save success"),
                    Err(x) => log::error!("saving failed: {}", x),
                }
            }
            SaveState => {
                log::info!("save state");
                let mut state = Vec::new();
                self.gb.lock().save_state(timestamp(), &mut state).unwrap();
                match self.rom.save_state(&state) {
                    Ok(_) => {}
                    Err(e) => log::error!("error saving state: {}", e),
                }
            }
            LoadState => {
                match self.rom.load_state() {
                    Ok(state) => {
                        let mut gb = self.gb.lock();

                        let mut old_state = Vec::new();
                        gb.save_state(timestamp(), &mut old_state).unwrap();

                        match gb.load_state(&mut state.as_slice()) {
                            Ok(_) => {
                                log::info!("load state")
                            }
                            Err(_) => {
                                log::error!("error loading save state: save state is malformatted");
                                // restore current state
                                gb.load_state(&mut old_state.as_slice()).unwrap();
                            }
                        }
                        self.start_time = recompute_start_time(gb.clock_count);
                        drop(gb);
                        // send EmulatorPaused to trigger the EmulatorUpdated event.
                        self.proxy.send_event(UserEvent::EmulatorPaused).unwrap();
                        // and send Started again, because the emulation is not paused.
                        self.proxy.send_event(UserEvent::EmulatorStarted).unwrap();
                    }
                    Err(e) => log::error!("error loading saved state: {}", e),
                };
            }
            Kill => return true,
            RunFrame => {
                if !self.debug {
                    self.set_state(EmulatorState::RunNoBreak);
                }
            }
            FrameLimit(value) => {
                if self.frame_limit == value {
                    return false;
                }
                self.frame_limit = value;
                if self.frame_limit {
                    self.start_time = recompute_start_time(self.gb.lock().clock_count);
                }
            }
            Rewind(value) => {
                if self.rewind == value {
                    return false;
                }
                self.rewind = value;
                {
                    let joypad = &mut *self.joypad.lock();
                    joypad.rewinding = self.rewind;
                    joypad.joypad_timeline.clear();
                }
                if !self.rewind {
                    self.start_time = recompute_start_time(self.gb.lock().clock_count);
                }
            }
            SetJoypad(joypad) => {
                self.joypad.lock().current_joypad = joypad;
            }
            Debug(value) => {
                if self.debug == value {
                    return false;
                }
                self.debug = value;
                if self.debug {
                    self.debugger.lock().last_op_clock = None;
                    self.set_state(EmulatorState::Idle);
                } else {
                    self.set_state(EmulatorState::RunNoBreak);
                }
            }
            Step => {
                if self.debug {
                    {
                        let gb = &mut &mut *self.gb.lock();
                        self.debugger.lock().step(gb);
                    }
                    self.set_state(EmulatorState::Idle);
                }
            }
            StepBack => {
                if self.debug {
                    let mut gb = self.gb.lock();
                    let mut joypad = self.joypad.lock();
                    loop {
                        let Some(last_clock_count) = joypad.last_frame_clock_count()  else {
                            log::warn!("there is no last frame");
                            break;
                        };

                        let clock_count = gb.clock_count;
                        // currently, the maximum number of clocks that interpret_op
                        // elapses is 24, so if the last_frame_clock_count is recent than
                        // that, this could be after the last instruction. So pop it.
                        if last_clock_count > clock_count - 24 {
                            joypad.pop_last_frame();
                            continue;
                        }
                        assert!(joypad.load_last_frame(&mut gb));
                        drop(joypad);
                        drop(gb);
                        {
                            let debugger = &mut *self.debugger.lock();
                            let Some(last_op_clock) = debugger.last_op_clock else {
                                log::warn!("debugger don't have last op clock");
                                break;
                            };
                            debugger.target_clock = Some(last_op_clock);
                            debugger.skip_breakpoints_until_target_clock = true;
                        }
                        self.set_state(EmulatorState::Run);
                        break;
                    }
                }
            }
            Run => {
                if self.debug {
                    self.set_state(EmulatorState::Run);
                    // Run a single step, to ignore the current breakpoint
                    let gb = &mut *self.gb.lock();
                    self.debugger.lock().step(gb);
                }
            }
            Reset => {
                self.gb.lock().reset();
                log::info!("reset");
                self.set_state(EmulatorState::Idle);
            }
            Pause => {
                self.debug = true;
            }
            Resume => {
                self.debug = false;
            }
        }
        false
    }

    pub fn poll(&mut self) -> Control {
        match self.state {
            EmulatorState::Idle | EmulatorState::WaitNextFrame => {}
            EmulatorState::Run => {
                // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                {
                    let mut gb = self.gb.lock();
                    let mut debugger = self.debugger.lock();
                    use RunResult::*;
                    match debugger.run_for(&mut gb, CLOCK_SPEED / 600) {
                        ReachBreakpoint | ReachTargetAddress | ReachTargetClock => {
                            drop(gb);
                            drop(debugger);
                            self.set_state(EmulatorState::Idle);
                            return Control::Wait;
                        }
                        TimeOut => {}
                    }
                }
                return Control::Poll;
            }
            EmulatorState::RunNoBreak => {
                if self.rewind {
                    {
                        let gb = &mut *self.gb.lock();
                        {
                            let joypad = &mut *self.joypad.lock();
                            if joypad.load_last_frame(gb) {
                                joypad.pop_last_frame();
                            }
                        }
                        {
                            let mut c = gb.v_blank.take();
                            if let Some(c) = c.as_mut() {
                                c(gb)
                            }
                            gb.v_blank = c;
                        }
                    }
                    self.set_state(EmulatorState::WaitNextFrame);
                } else if self.frame_limit {
                    let mut gb = self.gb.lock();
                    let elapsed = self.start_time.elapsed();
                    let mut target_clock = CLOCK_SPEED * elapsed.as_secs()
                        + (CLOCK_SPEED as f64 * (elapsed.subsec_nanos() as f64 * 1e-9)) as u64;

                    // make sure that the target_clock don't increase indefinitely if the program can't keep up.
                    if target_clock > gb.clock_count + CLOCK_SPEED / 30 {
                        target_clock = gb.clock_count + CLOCK_SPEED / 30;
                        self.start_time = recompute_start_time(gb.clock_count);
                    }

                    while gb.clock_count < target_clock {
                        #[cfg(target_arch = "x86_64")]
                        self.jit_compiler.interpret_block(&mut gb);
                        #[cfg(not(target_arch = "x86_64"))]
                        Interpreter(&mut gb).interpret_op();
                    }

                    drop(gb);
                    self.update_audio();

                    self.set_state(EmulatorState::WaitNextFrame);
                } else {
                    // run 1.6ms worth of emulation, and check for events in the channel, in a loop
                    let mut gb = self.gb.lock();
                    let mut inter = Interpreter(&mut gb);
                    let target_clock = inter.0.clock_count + CLOCK_SPEED / 600;

                    while inter.0.clock_count < target_clock {
                        inter.interpret_op();
                    }
                    // clear the audio output
                    let clock_count = inter.0.clock_count;
                    let _ = inter.0.sound.get_mut().get_output(clock_count);

                    return Control::Poll;
                }
            }
        }

        Control::Wait
    }

    fn update_audio(&mut self) {
        let mut gb = self.gb.lock();
        let clock_count = gb.clock_count;
        let buffer = gb.sound.get_mut().get_output(clock_count);
        #[cfg(feature = "audio-engine")]
        if let Some(SoundBackend {
            audio_buffer,
            last_buffer_len,
            ..
        }) = &mut self.sound
        {
            let mut lock = audio_buffer.lock();
            if lock.len() == 0 {
                // if the buffer is empty, add zeros to increase it
                lock.extend((0..1600 * 5).map(|_| 0));
            }
            lock.extend(buffer.iter().map(|&x| (x as i16 - 128) * 30));

            *last_buffer_len = lock.len();
        }
    }
}

fn clock_to_duration(clock_count: u64) -> Duration {
    let secs = clock_count / CLOCK_SPEED;
    let nanos = (clock_count % CLOCK_SPEED) * 1_000_000_000 / CLOCK_SPEED;
    Duration::new(secs, nanos as u32)
}

fn recompute_start_time(clock_count: u64) -> Instant {
    Instant::now() - clock_to_duration(clock_count)
}

/// The number of milliseconds since UNIX_EPOCH.
fn timestamp() -> Option<u64> {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        // cannot handle times before year 1970.
        .ok()?
        .as_millis()
        .try_into()
        // cannot handle times after year 584556501
        .ok()
}
