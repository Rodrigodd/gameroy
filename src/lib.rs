#[cfg(target_arch = "wasm32")]
pub static RESIZE: parking_lot::Mutex<Option<(u32, u32)>> = parking_lot::const_mutex(None);

mod waker_fn;

mod emulator;
mod event_table;
pub mod executor;
pub mod rom_loading;
mod style;
mod ui;
mod widget {
    pub mod fold_view;
    pub mod menu;
    mod pixel_perfect_layout;
    mod split_view;
    pub mod table_item;

    pub use pixel_perfect_layout::ScreenLayout;
    pub use split_view::SplitView;
}
pub mod config;

use std::{any::Any, rc::Rc, sync::Arc, thread};

use emulator::{Emulator, EmulatorEvent};
pub use gameroy;
use gameroy::{
    debugger::{Debugger, DebuggerEvent},
    gameboy::GameBoy,
    parser::Vbm,
};
use parking_lot::Mutex;
#[cfg(feature = "rfd")]
pub use rfd;
pub use rom_loading::RomFile;
use winit::{
    dpi::PhysicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopBuilder, EventLoopProxy},
    window::{Icon, Window, WindowBuilder},
};

use self::ui::RomEntries;

#[macro_use]
extern crate giui;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;
pub const VERSION: &str = "0.2.0";

pub fn log_panic() {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let current = thread::current();
        let thread = current.name().unwrap_or("unnamed");

        let msg = if let Some(s) = info.payload().downcast_ref::<&'static str>() {
            *s
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            &**s
        } else {
            "Box<Any>"
        };

        match info.location() {
            Some(location) => {
                log::error!(
                    "thread '{}' panicked at '{}': {}:{}",
                    thread,
                    msg,
                    location.file(),
                    location.line(),
                );
            }
            None => {
                log::error!("thread '{}' panicked at '{}'", thread, msg,)
            }
        }

        default_hook(info);
    }));
}

pub fn main(gb: Option<(RomFile, Box<GameBoy>)>, movie: Option<Vbm>) {
    log::info!("GameRoy {}", VERSION);

    #[allow(unused_assignments, unused_mut)]
    let mut icon: Option<Icon> = None;
    #[cfg(target_os = "windows")]
    {
        use winit::platform::windows::IconExtWindows;
        icon = Icon::from_resource(101, None)
            .map_err(|e| log::error!("{}", e))
            .ok();
    };

    // create winit's window and event_loop
    let event_loop = EventLoopBuilder::with_user_event().build();
    let wb = WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(768u32, 400))
        // wait every thing to be loaded before showing the window
        .with_visible(false)
        .with_window_icon(icon)
        .with_title("gameroy");

    #[cfg(target_arch = "wasm32")]
    let wb = {
        use wasm_bindgen::JsCast;
        use winit::platform::web::WindowBuilderExtWebSys;

        let document = web_sys::window().unwrap().document().unwrap();
        let canvas = document.get_element_by_id("main_canvas").unwrap();
        let canvas: web_sys::HtmlCanvasElement = canvas
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .map_err(|_| ())
            .unwrap();

        wb.with_canvas(Some(canvas))
    };

    let window = wb.build(&event_loop).unwrap();

    let proxy = event_loop.create_proxy();
    let mut ui = ui::Ui::new(&window, proxy);

    let window = Rc::new(window);
    ui.gui.set(window.clone());

    // initiate in the apropriated screen
    match gb {
        Some((file, gb)) => {
            window.set_title(&format!(
                "{} - gameroy",
                gb.cartridge.header.title_as_string()
            ));
            let proxy = event_loop.create_proxy();
            let emu = EmulatorApp::new(
                gb,
                proxy,
                config::config().start_in_debug,
                &mut ui,
                movie,
                file,
            );
            start_event_loop(event_loop, window, ui, Box::new(emu));
        }
        _ => {
            let rom_loading = RomLoadingApp::new(&mut ui.gui, event_loop.create_proxy());
            start_event_loop(event_loop, window, ui, Box::new(rom_loading))
        }
    };
}

pub struct AppState {
    /// The current state of the joypad. It is a bitmask, where 0 means pressed, and 1 released.
    pub joypad: u8,
    /// If the emulation is in debug mode.
    pub debug: bool,
}
impl AppState {
    fn new(debug: bool) -> Self {
        Self {
            debug,
            joypad: 0xFF,
        }
    }
}

fn start_event_loop(
    event_loop: EventLoop<UserEvent>,
    window: Rc<Window>,
    mut ui: ui::Ui,
    app: Box<dyn App>,
) -> ! {
    let mut app = vec![app];
    #[rustfmt::skip]
    fn fn_<F: for<'a> Fn(&'a mut Vec<Box<dyn App>>) -> &'a mut Box<dyn App>>(x: F) -> F { x }
    let last = fn_(|app: &mut Vec<_>| app.last_mut().unwrap());

    window.set_visible(true);
    last(&mut app).build_ui(&mut ui);
    let proxy = event_loop.create_proxy();
    // winit event loop
    event_loop.run(move |event, _, control| {
        let app = &mut app;
        match event {
            Event::Resumed if cfg!(target_os = "android") => {
                log::info!("reloading graphics");
                ui.reload_graphics(&window);
            }
            Event::Suspended if cfg!(target_os = "android") => {
                log::info!("destroying graphics");
                ui.destroy_graphics();
            }
            Event::NewEvents(_) => {
                ui.new_events(control, &window);
            }
            Event::WindowEvent { ref event, .. } => {
                ui.window_event(event, &window);
                match event {
                    WindowEvent::CloseRequested => {
                        *control = ControlFlow::Exit;
                    }
                    WindowEvent::Resized(size) => {
                        ui.resize(*size, &window);
                    }
                    // Rebuild the UI
                    #[cfg(not(feature = "static"))]
                    WindowEvent::KeyboardInput {
                        input:
                            winit::event::KeyboardInput {
                                virtual_keycode: Some(winit::event::VirtualKeyCode::Escape),
                                state: winit::event::ElementState::Pressed,
                                ..
                            },
                        ..
                    } => {
                        log::debug!("clear ui");
                        ui.clear();
                        ui.reload_graphics(&*window);
                        log::debug!("build ui");
                        last(app).build_ui(&mut ui);
                    }
                    // Load Dropped File
                    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
                    WindowEvent::DroppedFile(path) => {
                        let proxy = proxy.clone();
                        let path = path.clone();
                        let task = async move {
                            log::info!("The file {:?} was dropped", path);
                            let file = RomFile::from_path(path);
                            let rom = file.read().await.unwrap();
                            let ram = match file.load_ram_data().await {
                                Ok(x) => Some(x),
                                Err(err) => {
                                    log::error!("{}", err);
                                    None
                                }
                            };
                            proxy
                                .send_event(UserEvent::LoadRom {
                                    file,
                                    game_boy: rom_loading::load_gameboy(rom, ram).unwrap(),
                                })
                                .unwrap();
                        };
                        executor::Executor::spawn_task(task, &mut ui.gui.get_context());
                    }
                    _ => {}
                }
            }
            Event::MainEventsCleared => {}
            Event::RedrawRequested(window_id) => {
                #[cfg(target_arch = "wasm32")]
                {
                    if let Some((width, height)) = RESIZE.lock().take() {
                        let size = winit::dpi::PhysicalSize { width, height };
                        ui.resize(size.clone(), &window);
                        window.set_inner_size(size);
                    }
                }
                // render the gui
                ui.render(window_id);

                if ui.is_animating {
                    *control = ControlFlow::Poll;
                }
            }
            Event::UserEvent(UserEvent::PopApp) => {
                log::trace!("popping app");
                app.pop();
                ui.clear();
                last(app).build_ui(&mut ui);
            }
            Event::UserEvent(UserEvent::LoadRom { file, game_boy }) => {
                let gb = game_boy;
                window.set_title(&format!("{} - gameroy", file.file_name()));
                log::trace!("create emu!!");
                let emu = EmulatorApp::new(
                    gb,
                    proxy.clone(),
                    config::config().start_in_debug,
                    &mut ui,
                    None,
                    file,
                );
                app.push(Box::new(emu));
                log::trace!("rebuilding ui for emulator");
                ui.clear();
                last(app).build_ui(&mut ui);
                return;
            }
            Event::UserEvent(UserEvent::SpawnTask(task_id)) => {
                use std::future::Future;
                let p = Arc::new(Mutex::new(proxy.clone()));
                let waker = waker_fn::waker_fn(move || {
                    p.lock().send_event(UserEvent::SpawnTask(task_id)).unwrap()
                });
                let executor = ui.get::<executor::Executor>();
                let task = executor.map.get_mut(&task_id).unwrap();
                let mut cx = std::task::Context::from_waker(&waker);
                match Future::poll(task.as_mut(), &mut cx) {
                    std::task::Poll::Ready(_) => {
                        executor.map.remove(&task_id);
                    }
                    std::task::Poll::Pending => log::info!("wait..."),
                }
            }
            _ => {}
        }
        last(app).handle_event(event, &mut ui, &window, control, &proxy);
    })
}

trait App: Any {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        control: &mut ControlFlow,
        proxy: &EventLoopProxy<UserEvent>,
    );

    fn build_ui(&self, ui: &mut ui::Ui);

    fn as_any(&mut self) -> &mut dyn Any;
}

struct RomLoadingApp;
impl RomLoadingApp {
    fn new(gui: &mut giui::Gui, proxy: EventLoopProxy<UserEvent>) -> Self {
        gui.set(RomEntries::new(proxy));
        Self
    }
}

impl App for RomLoadingApp {
    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        _window: &winit::window::Window,
        _control: &mut ControlFlow,
        proxy: &EventLoopProxy<UserEvent>,
    ) {
        match event {
            Event::UserEvent(UserEvent::UpdateRomList) => {
                ui.gui.get::<ui::RomEntries>().start_loading(proxy.clone());
            }
            Event::UserEvent(UserEvent::UpdatedRomList) => {
                let rom_entries = &mut ui.gui.get_mut::<ui::RomEntries>();
                let observers = std::mem::take(&mut rom_entries.observers);
                log::trace!("send events to {:?}", observers);
                for id in observers.iter() {
                    ui.gui
                        .send_event_to(*id, Box::new(event_table::UpdatedRomList));
                }
                ui.gui.get_mut::<ui::RomEntries>().observers = observers;
            }
            _ => {}
        }
    }

    fn build_ui(&self, ui: &mut ui::Ui) {
        let gui = &mut ui.gui;
        let style = &gui.get::<style::Style>().clone();
        ui::create_rom_loading_ui(gui, style, ui.event_table.clone());
    }

    fn as_any(&mut self) -> &mut dyn Any {
        self
    }
}

struct EmulatorApp {
    lcd_screen: Arc<
        parking_lot::lock_api::Mutex<parking_lot::RawMutex, [u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
    >,
    emu_channel: flume::Sender<EmulatorEvent>,
    #[cfg(feature = "threads")]
    emu_thread: Option<thread::JoinHandle<()>>,
    #[cfg(not(feature = "threads"))]
    emulator: Emulator,
    #[cfg(not(feature = "threads"))]
    recv: flume::Receiver<emulator::EmulatorEvent>,
}
impl EmulatorApp {
    fn new(
        mut gb: Box<GameBoy>,
        proxy: winit::event_loop::EventLoopProxy<UserEvent>,
        debug: bool,
        ui: &mut ui::Ui,
        movie: Option<Vbm>,
        rom: RomFile,
    ) -> EmulatorApp {
        let lcd_screen: Arc<Mutex<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>> =
            Arc::new(Mutex::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]));
        gb.v_blank = Some(Box::new({
            let lcd_screen = lcd_screen.clone();
            let proxy = proxy.clone();
            move |gb| {
                {
                    let img_data = &mut lcd_screen.lock();
                    img_data.copy_from_slice(&gb.ppu.borrow().screen);
                }
                let _ = proxy.send_event(UserEvent::FrameUpdated);
            }
        }));
        let gb = Arc::new(Mutex::new(*gb));
        let (emu_channel, recv) = flume::bounded(8);
        if debug {
            proxy.send_event(UserEvent::Debug(debug)).unwrap();
        } else {
            emu_channel.send(EmulatorEvent::RunFrame).unwrap();
        }
        let debugger = Arc::new(Mutex::new(Debugger::default()));
        {
            let proxy = proxy.clone();
            let emu_channel = emu_channel.clone();
            debugger.lock().callback = Some(Box::new(move |_, event| {
                use DebuggerEvent::*;
                match event {
                    Step => emu_channel.send(EmulatorEvent::Step).unwrap(),
                    StepBack => emu_channel.send(EmulatorEvent::StepBack).unwrap(),
                    Reset => emu_channel.send(EmulatorEvent::Reset).unwrap(),
                    Run => emu_channel.send(EmulatorEvent::Run).unwrap(),
                    BreakpointsUpdate => proxy.send_event(UserEvent::BreakpointsUpdated).unwrap(),
                    WatchsUpdate => proxy.send_event(UserEvent::WatchsUpdated).unwrap(),
                }
            }));
        }
        ui.gui.set::<Arc<Mutex<GameBoy>>>(gb.clone());
        ui.gui.set::<Arc<Mutex<Debugger>>>(debugger.clone());
        ui.gui.set(emu_channel.clone());
        ui.gui.set(AppState::new(debug));

        #[cfg(feature = "threads")]
        let emu_thread = {
            let join_handle = thread::Builder::new()
                .name("emulator".to_string())
                .spawn(move || {
                    Emulator::new(gb, debugger, proxy, movie, rom).event_loop(recv);
                })
                .unwrap();
            Some(join_handle)
        };

        EmulatorApp {
            lcd_screen,
            emu_channel,
            #[cfg(feature = "threads")]
            emu_thread,
            #[cfg(not(feature = "threads"))]
            emulator: Emulator::new(gb, debugger, proxy, movie, rom),
            #[cfg(not(feature = "threads"))]
            recv,
        }
    }

    fn kill_emulator(&mut self) {
        self.emu_channel.send(EmulatorEvent::Kill).unwrap();
        #[cfg(feature = "threads")]
        self.emu_thread.take().unwrap().join().unwrap();
    }
}
impl Drop for EmulatorApp {
    fn drop(&mut self) {
        self.kill_emulator();
    }
}
impl App for EmulatorApp {
    fn build_ui(&self, ui: &mut ui::Ui) {
        let debug = ui.get::<AppState>().debug;
        ui::create_emulator_ui(ui, debug);
    }

    fn handle_event(
        &mut self,
        event: Event<UserEvent>,
        ui: &mut ui::Ui,
        window: &winit::window::Window,
        _control: &mut ControlFlow,
        _proxy: &EventLoopProxy<UserEvent>,
    ) {
        match event {
            Event::RedrawRequested(_) => {
                let joypad = ui.get::<AppState>().joypad;
                self.emu_channel
                    .send(EmulatorEvent::SetJoypad(joypad))
                    .unwrap();
                self.emu_channel.send(EmulatorEvent::RunFrame).unwrap();
            }
            Event::Suspended => {
                self.emu_channel.send(EmulatorEvent::SaveRam).unwrap();
            }
            #[cfg(not(feature = "threads"))]
            Event::MainEventsCleared => {
                let mut poll = true;
                if let Ok(mut event) = self.recv.try_recv() {
                    loop {
                        if self.emulator.handle_event(event) {
                            // break 'event_loop;
                        }
                        match self.emulator.poll() {
                            emulator::Control::Poll => {
                                poll = true;
                                *_control = ControlFlow::Poll
                            }
                            emulator::Control::Wait => {
                                poll = false;
                                *_control = ControlFlow::Wait
                            }
                        }
                        match self.recv.try_recv() {
                            Ok(x) => event = x,
                            _ => break,
                        }
                    }
                }

                if poll {
                    use instant::{Duration, Instant};

                    // This assumes that every frame has 10 ms of slack time. Should estimate this
                    // somehow.
                    let next_frame = Instant::now() + Duration::from_micros(10_000);

                    'polling: while Instant::now() < next_frame {
                        match self.emulator.poll() {
                            emulator::Control::Poll => {
                                *_control = ControlFlow::Poll;
                                continue 'polling;
                            }
                            emulator::Control::Wait => {
                                *_control = ControlFlow::Wait;
                                break 'polling;
                            }
                        }
                    }
                }
            }
            Event::UserEvent(event) => {
                use UserEvent::*;
                match event {
                    FrameUpdated => {
                        let screen: &[u8] = &{
                            let lock = self.lcd_screen.lock();
                            *lock
                        };
                        const COLOR: [[u8; 3]; 4] =
                            [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                        let mut img_data = vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4];
                        for y in 0..SCREEN_HEIGHT {
                            for x in 0..SCREEN_WIDTH {
                                let i = (x + y * SCREEN_WIDTH) * 4;
                                let c = screen[i / 4];
                                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
                            }
                        }
                        ui.update_screen_texture(&img_data);

                        ui.notify(event_table::FrameUpdated);
                        window.request_redraw();
                    }
                    EmulatorStarted => {
                        ui.force_render = true;
                        window.request_redraw();
                    }
                    EmulatorPaused => {
                        ui.notify(event_table::EmulatorUpdated);
                        ui.force_render = false;
                    }
                    BreakpointsUpdated => ui.notify(event_table::BreakpointsUpdated),
                    WatchsUpdated => ui.notify(event_table::WatchsUpdated),
                    Debug(value) => {
                        ui.get::<AppState>().debug = value;
                        ui.notify(event_table::Debug(value));
                        self.emu_channel.send(EmulatorEvent::Debug(value)).unwrap();
                    }
                    UpdateTexture(texture, data) => ui.update_texture(texture, &data),
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn as_any(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Debug)]
pub enum UserEvent {
    FrameUpdated,
    EmulatorPaused,
    EmulatorStarted,
    BreakpointsUpdated,
    WatchsUpdated,
    Debug(bool),
    UpdateTexture(u32, Box<[u8]>),
    PopApp,
    LoadRom {
        file: RomFile,
        game_boy: Box<GameBoy>,
    },
    SpawnTask(u32),
    UpdateRomList,
    UpdatedRomList,
}
