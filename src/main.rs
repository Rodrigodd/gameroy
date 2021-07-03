use std::{
    fs::File,
    sync::{
        mpsc::{sync_channel, Receiver, TryRecvError},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use gameroy::{
    cartridge::Cartridge,
    gameboy,
    interpreter::{self, Interpreter},
};

mod layout;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

fn main() {
    let mut diss = false;
    let mut debug = false;
    let mut rom_path = "roms/test.gb".to_string();

    for arg in std::env::args() {
        match arg.as_str() {
            "-d" | "--dissasembly" => diss = true,
            "-b" | "--debug" => debug = true,
            _ if arg.starts_with("-") => {
                eprintln!("unknown argument {}", arg);
                return;
            }
            _ => {
                rom_path = arg;
            }
        }
    }

    let rom_file = File::open(rom_path).unwrap();
    let boot_rom_file = File::open("bootrom/dmg_boot.bin").unwrap();

    let cartridge = Cartridge::new(rom_file).unwrap();

    let game_boy = gameboy::GameBoy::new(boot_rom_file, cartridge).unwrap();

    let inter = interpreter::Interpreter(game_boy);

    if diss {
        let mut string = String::new();
        let mut trace = inter.0.trace.borrow_mut();
        trace.trace_starting_at(&inter.0, 0x100);
        trace.fmt(&inter.0, &mut string).unwrap();
        println!("{}", string);
        return;
    }

    create_window(inter, debug);
}

use crui::{
    font::{Font, Fonts},
    graphics::Texture,
    render::{GuiRender, GuiRenderer},
    Gui,
};
use sprite_render::{Camera, GLSpriteRender, SpriteInstance, SpriteRender};
use winit::{
    dpi::PhysicalSize,
    event::{ElementState, Event, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{WindowBuilder, WindowId},
};

use self::layout::PixelPerfectLayout;

fn resize(
    gui: &mut Gui,
    render: &mut GLSpriteRender,
    camera: &mut Camera,
    size: PhysicalSize<u32>,
    window: WindowId,
) {
    render.resize(window, size.width, size.height);
    camera.resize(size.width, size.height);
    let width = size.width as f32;
    let height = size.height as f32;
    gui.resize(width, height);
    camera.set_width(width);
    camera.set_height(height);
    camera.set_position(width / 2.0, height / 2.0);
}

fn create_window(mut inter: Interpreter, debug: bool) {
    // create winit's window and event_loop
    let event_loop = EventLoop::with_user_event();
    let window = WindowBuilder::new().with_inner_size(PhysicalSize::new(600, 400));

    // create the render and camera, and a texture for the glyphs rendering
    let (window, mut render) = GLSpriteRender::new(window, &event_loop, true);
    let mut camera = {
        let size = window.inner_size();
        let width = size.width;
        let height = size.height;
        Camera::new(width, height, height as f32)
    };
    let font_texture = render.new_texture(128, 128, &[], false);

    let screen_texture = render.new_texture(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32, &[], false);

    // load a font
    let mut fonts = Fonts::new();
    let _my_font = {
        fonts.add(Font::new(include_bytes!(
            "../../crui/examples/NotoSans-Regular.ttf"
        )))
    };

    // create the gui, and the gui_render
    let mut gui = Gui::new(0.0, 0.0, fonts);
    let mut gui_render = GuiRender::new(font_texture, [128, 128]);

    // populate the gui with controls. In this case a green 'Hello Word' text covering the entire of the screen.
    let _text = gui
        .create_control()
        .layout(PixelPerfectLayout::new((160, 144), (0, 0)))
        .child(|cb| cb.graphic(Texture::new(screen_texture, [0.0, 0.0, 1.0, 1.0]).into()))
        .build();

    // resize everthing to the screen size
    resize(
        &mut gui,
        &mut render,
        &mut camera,
        window.inner_size(),
        window.id(),
    );

    let img_data: Arc<Mutex<Vec<u8>>> =
        Arc::new(Mutex::new(vec![255; SCREEN_WIDTH * SCREEN_HEIGHT * 4]));
    let img_data_clone = img_data.clone();
    let proxy = event_loop.create_proxy();
    inter.0.v_blank = Box::new(move |ppu| {
        let img_data: &mut [u8] = &mut img_data_clone.lock().unwrap();
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let i = (x + y * SCREEN_WIDTH) as usize * 4;
                let c = ppu.screen[i / 4];
                const COLOR: [[u8; 3]; 4] =
                    [[255, 255, 255], [170, 170, 170], [85, 85, 85], [0, 0, 0]];
                img_data[i..i + 3].copy_from_slice(&COLOR[c as usize]);
            }
        }
        let _ = proxy.send_event(UserEvent::FrameUpdated);
    });

    let mut is_animating = false;

    let (sender, recv) = sync_channel(3);
    if debug {
        sender.send(EmulatorEvent::Debug).unwrap();
    }
    sender.send(EmulatorEvent::Run).unwrap();

    thread::Builder::new()
        .name("emulator".to_string())
        .spawn(move || emulator_thread(inter, recv))
        .unwrap();

    let mut joypad = 0xFF;

    // winit event loop
    event_loop.run(move |event, _, control| {
        match event {
            Event::NewEvents(_) => {
                *control = match gui.handle_scheduled_event() {
                    Some(time) => ControlFlow::WaitUntil(time),
                    None => ControlFlow::Wait,
                };
                if gui.render_is_dirty() {
                    window.request_redraw();
                }
                if let Some(cursor) = gui.cursor_change() {
                    window.set_cursor_icon(cursor);
                }
                if is_animating {
                    window.request_redraw();
                }
            }
            Event::WindowEvent {
                event, window_id, ..
            } => {
                // gui receive events
                gui.handle_event(&event);
                if gui.render_is_dirty() {
                    window.request_redraw();
                }
                if let Some(cursor) = gui.cursor_change() {
                    window.set_cursor_icon(cursor);
                }
                match event {
                    WindowEvent::CloseRequested => {
                        *control = ControlFlow::Exit;
                    }
                    WindowEvent::Resized(size) => {
                        resize(&mut gui, &mut render, &mut camera, size, window_id);
                    }
                    WindowEvent::KeyboardInput { input, .. } => {
                        let mut set_key = |key: u8| {
                            joypad = (joypad & !(1 << key))
                                | (((input.state == ElementState::Released) as u8) << key)
                        };
                        match input.virtual_keycode {
                            Some(VirtualKeyCode::Right) => set_key(0),
                            Some(VirtualKeyCode::Left) => set_key(1),
                            Some(VirtualKeyCode::Up) => set_key(2),
                            Some(VirtualKeyCode::Down) => set_key(3),
                            Some(VirtualKeyCode::Z) => set_key(4),
                            Some(VirtualKeyCode::X) => set_key(5),
                            Some(VirtualKeyCode::Return) => set_key(6),
                            Some(VirtualKeyCode::S) => set_key(7),
                            Some(VirtualKeyCode::D) if input.state == ElementState::Pressed => {
                                sender.send(EmulatorEvent::Debug).unwrap();
                            }
                            Some(VirtualKeyCode::LShift) => sender
                                .send(EmulatorEvent::FrameLimit(
                                    input.state != ElementState::Pressed,
                                ))
                                .unwrap(),

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            Event::UserEvent(UserEvent::FrameUpdated) => {
                let img_data: &mut [u8] = &mut img_data.lock().unwrap();
                render.update_texture(screen_texture, &img_data, None);
                window.request_redraw();
            }
            Event::MainEventsCleared => {}
            Event::RedrawRequested(window_id) => {
                // render the gui
                struct Render<'a>(&'a mut GLSpriteRender);
                impl<'a> GuiRenderer for Render<'a> {
                    fn update_font_texure(
                        &mut self,
                        font_texture: u32,
                        rect: [u32; 4],
                        data_tex: &[u8],
                    ) {
                        let mut data = Vec::with_capacity(data_tex.len() * 4);
                        for byte in data_tex.iter() {
                            data.extend([0xff, 0xff, 0xff, *byte].iter());
                        }
                        self.0.update_texture(
                            font_texture,
                            &data,
                            Some([rect[0], rect[1], rect[2] - rect[0], rect[3] - rect[1]]),
                        );
                    }
                    fn resize_font_texture(&mut self, font_texture: u32, new_size: [u32; 2]) {
                        self.0
                            .resize_texture(font_texture, new_size[0], new_size[1], &[]);
                    }
                }
                let mut ctx = gui.get_render_context();
                let (sprites, is_anim) = gui_render.render(&mut ctx, Render(&mut render));
                is_animating = is_anim || true;
                let mut renderer = render.render(window_id);
                renderer.clear_screen(&[0.0, 0.0, 0.0, 1.0]);
                renderer.draw_sprites(
                    &mut camera,
                    &sprites
                        .iter()
                        .map(|x| {
                            let width = x.rect[2] - x.rect[0];
                            let height = x.rect[3] - x.rect[1];
                            SpriteInstance {
                                scale: [width, height],
                                angle: 0.0,
                                uv_rect: x.uv_rect,
                                color: x.color,
                                pos: [x.rect[0] + width / 2.0, x.rect[1] + height / 2.0],
                                texture: x.texture,
                            }
                        })
                        .collect::<Vec<_>>(),
                );

                if is_animating {
                    *control = ControlFlow::Poll;
                }

                renderer.finish();

                sender.send(EmulatorEvent::SetKeys(joypad)).unwrap();
                sender.send(EmulatorEvent::Run).unwrap();
            }
            _ => {}
        }
    });
}

enum UserEvent {
    FrameUpdated,
}

enum EmulatorEvent {
    Run,
    FrameLimit(bool),
    Debug,
    SetKeys(u8),
}

fn emulator_thread(mut inter: Interpreter, recv: Receiver<EmulatorEvent>) {
    use EmulatorEvent::*;
    let mut debug = false;
    let mut frame_limit = true;
    let mut start_time = Instant::now();
    let clock_speed = 4_194_304;
    while let Ok(mut event) = recv.recv() {
        'handle_event: loop {
            match event {
                Run if frame_limit && !debug => {
                    let elapsed = start_time.elapsed();
                    let mut target_clock = clock_speed * elapsed.as_secs()
                        + (clock_speed as f64 * (elapsed.subsec_nanos() as f64 * 1e-9)) as u64;
                    // make sure that the target_clock don't increase exponentially if the program can't keep up.
                    if target_clock > inter.0.clock_count + clock_speed / 30 {
                        let expected_target = target_clock;
                        target_clock = inter.0.clock_count + clock_speed / 30;
                        start_time += Duration::from_secs_f64(
                            (expected_target - target_clock) as f64 / clock_speed as f64,
                        );
                    }
                    while inter.0.clock_count < target_clock {
                        inter.interpret_op();
                    }
                }
                Run => {}
                FrameLimit(value) => frame_limit = value,
                SetKeys(keys) => inter.0.joypad = keys,
                Debug => debug = !debug,
            }

            if debug {
                inter.debug();
            } else if !frame_limit {
                loop {
                    start_time -= Duration::from_millis(5);
                    let elapsed = start_time.elapsed();
                    let target_clock = clock_speed * elapsed.as_secs()
                        + (clock_speed as f64 * (elapsed.subsec_nanos() as f64 * 1e-9)) as u64;
                    while inter.0.clock_count < target_clock {
                        inter.interpret_op();
                    }
                    match recv.try_recv() {
                        Ok(next_event) => {
                            event = next_event;
                            continue 'handle_event;
                        }
                        Err(TryRecvError::Disconnected) => return,
                        _ => {}
                    }
                }
            }

            break;
        }
    }
}
