use std::path::{Path, PathBuf};

use cfg_if::cfg_if;
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};
use winit::event::VirtualKeyCode;

cfg_if! {
    if #[cfg(target_arch = "wasm32")] {
        pub fn load_config() -> Result<Config, String> {
            Err("unimplemented".to_string())
        }

        pub fn save_config(_config: &Config) -> Result<(), String> {
            Err("unimplemented".to_string())
        }
    } else if #[cfg(target_os = "android")] {
        use jni::objects::JValue;
        pub fn load_config() -> Result<Config, String> {
            let android_context = ndk_context::android_context();
            let vm =
                std::sync::Arc::new(unsafe { jni::JavaVM::from_raw(android_context.vm().cast()).unwrap() });

            let saved = jni::Executor::new(vm)
                .with_attached(|env| {
                    let retrn = env.call_method(
                        android_context.context() as jni::sys::jobject,
                        "loadPreferences",
                        "()Ljava/lang/String;",
                        &[],
                    )?;

                    let obj = if let JValue::Object(x) = retrn { x } else {
                        panic!("expected object");
                    };

                    if obj.is_null() {
                        return Ok(None);
                    }

                    let string = env.get_string(obj.into())?.to_string_lossy().into_owned();

                    Ok(Some(string))
                })
            .unwrap();

            if let Some(saved) = saved {
                log::info!("load config: {:?}", saved);
                let conf = toml::from_str(&saved).map_err(|e| e.to_string())?;
                Ok(conf)
            } else {
                Err("there is no saved config".to_string())
            }
        }

        pub fn save_config(conf: &Config) -> Result<(), String> {
            let android_context = ndk_context::android_context();
            let vm =
                std::sync::Arc::new(unsafe { jni::JavaVM::from_raw(android_context.vm().cast()).unwrap() });

            jni::Executor::new(vm)
                .with_attached(|env| {
                    let serialized = toml::to_string(&conf).unwrap();
                    log::info!("saving config: {:?}", serialized );
                    let conf= env.new_string(&serialized)?;

                    env.call_method(
                        android_context.context() as jni::sys::jobject,
                        "savePreferences",
                        "(Ljava/lang/String;)V",
                        &[conf.into()],
                    )?;

                    Ok(())
                })
            .unwrap();

            Ok(())
        }
    } else {
        pub fn load_config() -> Result<Config, String> {
            let config_path = normalize_config_path("gameroy.toml");
            log::info!("using '{}' as config path", config_path.display());
            let config = std::fs::read_to_string(config_path).map_err(|e| e.to_string())?;
            let config: Config = toml::from_str(&config).map_err(|e| e.to_string())?;
            Ok(config)
        }

        pub fn save_config(_config: &Config) -> Result<(), String> {
            Err("unimplemented".to_string())
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct Config {
    pub start_in_debug: bool,
    pub rom_folder: Option<String>,
    pub boot_rom: Option<String>,
    pub sort_list: Option<String>,
    pub keymap: KeyMap,
}

impl Config {
    pub fn load() -> Result<Self, String> {
        load_config()
    }

    pub fn save(&self) -> Result<(), String> {
        save_config(self)
    }
}

/// Transform a path relative to the executable folder to a absolute path.
pub fn normalize_config_path(path: impl AsRef<Path>) -> PathBuf {
    let path: &Path = path.as_ref();
    if path.has_root() {
        path.to_path_buf()
    } else if let Some(mut base) = base_folder() {
        base.push(path);
        base
    } else {
        path.to_path_buf()
    }
}

pub fn base_folder() -> Option<PathBuf> {
    static BASE_FOLDER: OnceCell<Option<PathBuf>> = OnceCell::new();
    BASE_FOLDER
        .get_or_init(|| {
            #[cfg(not(target_os = "android"))]
            let base_folder =
                if let Some(path) = std::env::var("CARGO_WORKSPACE_DIR").ok().map(PathBuf::from) {
                    path
                } else {
                    std::env::current_exe()
                        .map_err(|e| log::error!("Could not get base folder: {}", e))
                        .ok()?
                        .parent()
                        .ok_or_else(|| log::error!("Could not get base folder"))
                        .ok()?
                        .to_path_buf()
                };

            #[cfg(target_os = "android")]
            let base_folder = ndk_glue::native_activity()
                .internal_data_path()
                .to_path_buf();

            Some(base_folder)
        })
        .clone()
}

impl Default for Config {
    fn default() -> Self {
        DEFAULT_CONFIG
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct KeyMap {
    pub left: VirtualKeyCode,
    pub right: VirtualKeyCode,
    pub up: VirtualKeyCode,
    pub down: VirtualKeyCode,
    pub a: VirtualKeyCode,
    pub b: VirtualKeyCode,
    pub select: VirtualKeyCode,
    pub start: VirtualKeyCode,

    pub speed: VirtualKeyCode,
    pub rewind: VirtualKeyCode,
    pub save_state: VirtualKeyCode,
    pub load_state: VirtualKeyCode,

    pub open_debugger: VirtualKeyCode,
    pub debug_step: VirtualKeyCode,
    pub debug_stepback: VirtualKeyCode,
    pub debug_run: VirtualKeyCode,
}

impl Default for KeyMap {
    fn default() -> Self {
        DEFAULT_KEYMAP
    }
}

const DEFAULT_KEYMAP: KeyMap = {
    use VirtualKeyCode::*;
    KeyMap {
        left: Left,
        right: Right,
        up: Up,
        down: Down,
        a: A,
        b: S,
        select: Back,
        start: Return,

        speed: LShift,
        rewind: R,
        save_state: F5,
        load_state: F6,

        open_debugger: F12,
        debug_stepback: F7,
        debug_step: F8,
        debug_run: F9,
    }
};

const DEFAULT_CONFIG: Config = Config {
    start_in_debug: false,
    rom_folder: None,
    boot_rom: None,
    sort_list: None,
    keymap: DEFAULT_KEYMAP,
};

static CONFIG: Mutex<Config> = parking_lot::const_mutex(DEFAULT_CONFIG);

pub fn config() -> MutexGuard<'static, Config> {
    CONFIG.lock()
}

pub fn init_config(config: Config) {
    *CONFIG.lock() = config
}
