use std::{
    ffi::{c_char, c_void},
    io::Write,
    mem::MaybeUninit,
};

use gameroy::{
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};
use rust_libretro_sys::{
    retro_audio_sample_batch_t, retro_audio_sample_t, retro_environment_t, retro_game_geometry,
    retro_game_info, retro_input_poll_t, retro_input_state_t, retro_log_callback, retro_log_level,
    retro_system_av_info, retro_system_info, retro_system_timing, retro_video_refresh_t,
    RETRO_API_VERSION, RETRO_DEVICE_ID_JOYPAD_A, RETRO_DEVICE_ID_JOYPAD_B,
    RETRO_DEVICE_ID_JOYPAD_DOWN, RETRO_DEVICE_ID_JOYPAD_LEFT, RETRO_DEVICE_ID_JOYPAD_RIGHT,
    RETRO_DEVICE_ID_JOYPAD_SELECT, RETRO_DEVICE_ID_JOYPAD_START, RETRO_DEVICE_ID_JOYPAD_UP,
    RETRO_DEVICE_JOYPAD, RETRO_ENVIRONMENT_GET_LOG_INTERFACE,
};

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;
const SAMPLE_RATE: u64 = 44100;

#[macro_export]
macro_rules! concat {
    ($($s:expr),+) => {{
        const LEN: usize = $( $s.len() + )* 0;
        const ARR: [u8; LEN] = {
            let mut arr = [0; LEN];
            let mut base: usize = 0;
            $({
                let mut i = 0;
                let s = $s.as_bytes();
                while i < s.len() {
                    arr[base + i] = s[i];
                    i += 1;
                }
                base += s.len();
            })*
            if base != LEN { panic!("invalid length"); }
            arr
        };
        std::str::from_utf8(&ARR).unwrap()
    }}
}

macro_rules! c_str {
    ($x:expr) => {
        $crate::concat!($x, "\0").as_ptr() as *const c_char
    };
}

/// The Core global state.
struct Core {
    env_callback: retro_environment_t,
    video_callback: retro_video_refresh_t,
    audio_callback: retro_audio_sample_batch_t,
    input_poll_callback: retro_input_poll_t,
    input_state_callback: retro_input_state_t,
    state: Option<GameBoy>,

    // A double buffer of the lcd screen pixels. Updated on vblank.
    screen_buffer: [u8; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize],
}
impl Default for Core {
    fn default() -> Self {
        Self {
            env_callback: Default::default(),
            video_callback: Default::default(),
            audio_callback: Default::default(),
            input_poll_callback: Default::default(),
            input_state_callback: Default::default(),
            state: Default::default(),
            screen_buffer: [0; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize],
        }
    }
}

static mut CORE: Option<Core> = None;

fn core() -> &'static mut Core {
    // RetroLib promises that it will not call the API from multiple threads, so this is safe, right?
    // Actually this is unsafe because the caller could leak the mutable reference. This should be a
    // RefCell or something.
    // In fact, I am already doing UB, because I am calling core() in retro_run and in the
    // v_blank_callback, creating two mutable references to CORE at the same time!
    unsafe {
        if CORE.is_none() {
            CORE = Some(Core::default());
        }
        CORE.as_mut().unwrap()
    }
}

#[no_mangle]
pub extern "C" fn retro_api_version() -> u32 {
    RETRO_API_VERSION
}

#[no_mangle]
pub extern "C" fn retro_set_environment(callback: retro_environment_t) {
    core().env_callback = callback;
    let mut log_callback: retro_log_callback = retro_log_callback { log: None };

    if let Some(callback) = callback {
        let ok = unsafe {
            callback(
                RETRO_ENVIRONMENT_GET_LOG_INTERFACE,
                &mut log_callback as *mut _ as *mut c_void,
            )
        };
        if !ok {
            log_callback.log = None;
        }
    }

    init_log(log_callback);
    log::trace!("set environment");
}

#[no_mangle]
pub extern "C" fn retro_init() {
    log::trace!("retro init");
}

#[no_mangle]
pub extern "C" fn retro_deinit() {
    log::trace!("retro deinit");
}

#[no_mangle]
pub extern "C" fn retro_get_system_info(info: &mut MaybeUninit<retro_system_info>) {
    log::trace!("get system info");

    let info = unsafe {
        *info = MaybeUninit::zeroed();
        info.assume_init_mut()
    };

    info.library_name = c_str!("GameRoy");
    info.library_version = c_str!(gameroy::consts::VERSION);
    info.valid_extensions = c_str!("gb");
    info.need_fullpath = false;
    info.block_extract = false;
}

#[no_mangle]
pub extern "C" fn retro_get_system_av_info(info: &mut MaybeUninit<retro_system_av_info>) {
    log::trace!("get system av info");

    let info = unsafe {
        *info = MaybeUninit::zeroed();
        info.assume_init_mut()
    };

    *info = retro_system_av_info {
        geometry: retro_game_geometry {
            base_width: SCREEN_WIDTH,
            base_height: SCREEN_HEIGHT,
            max_width: SCREEN_WIDTH,
            max_height: SCREEN_HEIGHT,
            aspect_ratio: SCREEN_WIDTH as f32 / SCREEN_HEIGHT as f32,
        },
        timing: retro_system_timing {
            fps: 60.0,
            sample_rate: SAMPLE_RATE as f64,
        },
    };
}

#[no_mangle]
extern "C" fn retro_load_game(info: Option<&retro_game_info>) -> bool {
    log::info!("retro load game");

    // load rom data into core
    let Some(info) = info else { return false } ;

    let data = unsafe { std::slice::from_raw_parts(info.data as *const u8, info.size as usize) };
    let cartridge = match Cartridge::new(data.to_vec()) {
        Ok(x) => x,
        Err(err) => {
            log::error!("Error loading rom: {}", err);
            return false;
        }
    };

    let mut gb = GameBoy::new(None, cartridge);
    gb.sound.get_mut().sample_frequency = SAMPLE_RATE;
    gb.v_blank = Some(Box::new(|gb| {
        core().screen_buffer = gb.ppu.get_mut().screen.packed();
    }));

    core().state = Some(gb);

    true
}

#[no_mangle]
pub extern "C" fn retro_run() {
    let core = core();
    let Some(state) = &mut core.state else {
        log::error!("GameBoy is not loaded?");
        return;
    };

    state.joypad = 0xff;
    if let (Some(input_poll), Some(input_state)) =
        (core.input_poll_callback, core.input_state_callback)
    {
        let key_map = [
            RETRO_DEVICE_ID_JOYPAD_RIGHT,
            RETRO_DEVICE_ID_JOYPAD_LEFT,
            RETRO_DEVICE_ID_JOYPAD_UP,
            RETRO_DEVICE_ID_JOYPAD_DOWN,
            RETRO_DEVICE_ID_JOYPAD_A,
            RETRO_DEVICE_ID_JOYPAD_B,
            RETRO_DEVICE_ID_JOYPAD_SELECT,
            RETRO_DEVICE_ID_JOYPAD_START,
        ];
        unsafe {
            input_poll();
        }

        for (i, id) in key_map.iter().copied().enumerate() {
            let value = unsafe { input_state(0, RETRO_DEVICE_JOYPAD, 0, id) };
            if value != 0 {
                state.joypad &= !(1 << i);
            }
        }
    }

    let target = state.clock_count + gameroy::consts::FRAME_CYCLES;
    while state.clock_count < target {
        Interpreter(state).interpret_op();
    }

    if let Some(callback) = core.video_callback {
        let frame = core.screen_buffer.map(|c| {
            /// 0RGB1555 format
            /// FIXME: this format is deprecated
            #[allow(clippy::unusual_byte_groupings)]
            const COLOR: [u16; 4] = [
                0b0_11111_11111_11111, //
                0b0_10101_10101_10101,
                0b0_01011_01011_01011,
                0b0_00000_00000_00000,
            ];
            COLOR[c as usize]
        });
        unsafe {
            (callback)(
                frame.as_ptr() as *const c_void,
                SCREEN_WIDTH,
                SCREEN_HEIGHT,
                SCREEN_WIDTH as u64 * std::mem::size_of::<u16>() as u64,
            )
        }
    }

    if let Some(callback) = core.audio_callback {
        unsafe {
            let buffer = state.sound.get_mut().get_output(state.clock_count);
            let buffer: Vec<i16> = buffer.into_iter().map(|x| (x as i16 - 128) * 30).collect();
            (callback)(buffer.as_ptr(), buffer.len() as u64 / 2);
        }
    }
}

#[no_mangle]
pub extern "C" fn retro_set_video_refresh(callback: retro_video_refresh_t) {
    log::info!("set video refresh");
    core().video_callback = callback;
}

#[no_mangle]
pub extern "C" fn retro_set_audio_sample_batch(callback: retro_audio_sample_batch_t) {
    log::info!("set audio sample batch");
    core().audio_callback = callback;
}

#[no_mangle]
pub extern "C" fn retro_set_input_poll(callback: retro_input_poll_t) {
    log::info!("set input poll");
    core().input_poll_callback = callback;
}

#[no_mangle]
pub extern "C" fn retro_set_input_state(callback: retro_input_state_t) {
    log::info!("set input state");
    core().input_state_callback = callback;
}

#[no_mangle]
pub extern "C" fn retro_set_audio_sample(_: retro_audio_sample_t) {}
#[no_mangle]
pub extern "C" fn retro_set_controller_port_device(_port: u32, _device: u32) {}
#[no_mangle]
pub extern "C" fn retro_reset() {}
#[no_mangle]
pub extern "C" fn retro_serialize_size() -> usize {
    0
}
#[no_mangle]
pub extern "C" fn retro_serialize(_data: *const c_void, _size: usize) -> bool {
    false
}
#[no_mangle]
pub extern "C" fn retro_unserialize(_data: *const c_void, _size: usize) -> bool {
    false
}
#[no_mangle]
pub extern "C" fn retro_cheat_reset() {}
#[no_mangle]
pub extern "C" fn retro_cheat_set(_index: u32, _enabled: bool, _code: *const c_char) {}
#[no_mangle]
pub extern "C" fn retro_load_game_special(
    _game_type: u32,
    _info: *const retro_game_info,
    _num_info: usize,
) -> bool {
    false
}
#[no_mangle]
pub extern "C" fn retro_unload_game() {}
#[no_mangle]
pub extern "C" fn retro_get_region() -> u32 {
    0
}
#[no_mangle]
pub extern "C" fn retro_get_memory_data(_id: u32) -> *mut c_void {
    std::ptr::null_mut()
}
#[no_mangle]
pub extern "C" fn retro_get_memory_size(_id: u32) -> usize {
    0
}

struct RetroLogger {
    callback: retro_log_callback,
}
impl log::Log for RetroLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() < log::max_level()
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let target = if record.target().is_empty() {
            record.target()
        } else {
            record.module_path().unwrap_or_default()
        };

        if let Some(log) = self.callback.log {
            let mut message = Vec::new();
            let level = match record.level() {
                log::Level::Error => retro_log_level::RETRO_LOG_ERROR,
                log::Level::Warn => retro_log_level::RETRO_LOG_WARN,
                log::Level::Info => retro_log_level::RETRO_LOG_INFO,
                log::Level::Debug => retro_log_level::RETRO_LOG_DEBUG,
                log::Level::Trace => retro_log_level::RETRO_LOG_DEBUG,
            };
            write!(&mut message, "[{}] {}\0", target, record.args()).unwrap();
            unsafe { (log)(level, c_str!("%s\n"), message.as_ptr() as *const c_char) }
        } else {
            let path = concat!(env!("CARGO_WORKSPACE_DIR"), "libretro/log.txt");
            let Ok(mut file) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path) else { return };

            let _ = writeln!(
                &mut file,
                "[{}] [{}] {}",
                record.metadata().level(),
                target,
                record.args()
            );
        }
    }

    fn flush(&self) {}
}

fn init_log(callback: retro_log_callback) {
    use std::sync::atomic::{AtomicBool, Ordering};
    static IS_INIT: AtomicBool = AtomicBool::new(false);

    if IS_INIT.load(Ordering::Relaxed) {
        return;
    } else {
        IS_INIT.store(true, Ordering::Relaxed);
    }

    let retro_logger = RetroLogger { callback };

    log::set_max_level(log::LevelFilter::Trace);
    let _ = log::set_logger(Box::leak(Box::new(retro_logger)));
}
