use std::collections::HashMap;

use giui::{Context, Id};

pub struct GamePad {
    buttons: [Id; 9],
    pressed: HashMap<giui::MouseId, u8>,
}

impl GamePad {
    pub fn new(buttons: [Id; 9]) -> Self {
        Self {
            buttons,
            pressed: HashMap::default(),
        }
    }

    fn on_change(pressed: bool, b: u8, ctx: &mut Context) {
        let app_state = ctx.get_mut::<crate::AppState>();
        app_state.joypad = (app_state.joypad & !(1 << b)) | ((!pressed as u8) << b);
    }
}

impl giui::Behaviour for GamePad {
    fn on_active(&mut self, _this: Id, ctx: &mut Context) {
        for id in self.buttons {
            ctx.get_graphic_mut(id)
                .set_color([255, 255, 255, 128].into());
        }
        self.pressed.clear();
    }

    fn input_flags(&self) -> giui::InputFlags {
        giui::InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: giui::MouseInfo, _this: Id, ctx: &mut Context) {
        let pressed = *self.pressed.get(&mouse.id).unwrap_or(&u8::MAX);

        if mouse.buttons.left.pressed() {
            const MAX_DIST: f32 = 35.0;
            // find closest
            let (b, dist) = self.buttons[0..8]
                .iter()
                .enumerate()
                .map(|(i, &x)| {
                    let x = ctx.get_rect(x);
                    let center = [(x[0] + x[2]) / 2.0, (x[1] + x[3]) / 2.0];
                    let dx = mouse.pos[0] - center[0];
                    let dy = mouse.pos[1] - center[1];
                    // distance
                    (i, dx * dx + dy * dy)
                })
                .min_by_key(|x| x.1.min((MAX_DIST + 1.0).powi(2)) as u32)
                .unwrap();

            let b = if dist > MAX_DIST.powi(2) {
                u8::MAX
            } else {
                b as u8
            };

            // unpress the previous button
            if pressed != b && pressed != u8::MAX {
                self.pressed.remove(&mouse.id);
                let id = if pressed < 4 {
                    self.buttons[8]
                } else {
                    self.buttons[pressed as usize]
                };
                ctx.get_graphic_mut(id)
                    .set_color([255, 255, 255, 128].into());
                Self::on_change(false, pressed, ctx);
            }

            // press the current on
            if b != u8::MAX {
                self.pressed.insert(mouse.id, b);
                let id = if b < 4 {
                    self.buttons[8]
                } else {
                    self.buttons[b as usize]
                };
                ctx.get_graphic_mut(id)
                    .set_color([255, 255, 255, 255].into());
                Self::on_change(true, b, ctx);
            }
        } else {
            let b = u8::MAX;
            // unpress the previous button
            if pressed != b && pressed != u8::MAX {
                self.pressed.remove(&mouse.id);
                let id = if pressed < 4 {
                    self.buttons[8]
                } else {
                    self.buttons[pressed as usize]
                };
                ctx.get_graphic_mut(id)
                    .set_color([255, 255, 255, 128].into());
                Self::on_change(false, pressed, ctx);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::AppState;

    fn ctx_and_game_pad() -> giui::Gui {
        let size = 900.0;
        let mut gui = giui::Gui::new(size, size, 1.0, giui::font::Fonts::new());
        gui.set(AppState {
            joypad: 0xFF,
            debug: false,
        });
        {
            let ctx = &mut gui.get_context();

            let mut create_button = |[x, y]: [f32; 2]| -> Id {
                let w = 20.0;
                let h = 20.0;
                log::info!("(x,y): {}, {}", x, y);
                ctx.create_control()
                    .anchors([0.0, 0.0, 0.0, 0.0])
                    .margins([-w + x, -h + y, w + x, h + y])
                    .build(ctx)
            };

            // grid of buttons
            const W: usize = 3;
            let mut i = 0;
            let buttons = [(); W * W].map(|_| {
                let w = size / W as f32;
                let x = w * ((i % W) as f32 + 0.5);
                let y = w * ((i / W) as f32 + 0.5);
                i += 1;
                create_button([x, y])
            });
            ctx.create_control()
                .behaviour(GamePad::new(buttons))
                .build(ctx);
        };
        gui
    }

    #[test]
    fn test() {
        let _logger = flexi_logger::Logger::try_with_env_or_str("gameroy=trace,giui=trace")
            .unwrap()
            .log_to_stdout()
            .start()
            .unwrap();

        log::trace!("starting test");
        let mut gui = ctx_and_game_pad();

        // 0

        gui.mouse_enter(0);
        gui.mouse_moved(0, 150.0, 150.0);
        gui.mouse_down(0, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);
        gui.mouse_exit(0);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);

        // 1

        gui.mouse_enter(1);
        gui.mouse_moved(1, 450.0, 750.0); // 1, 2 => 7
        gui.mouse_down(1, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x80);
        gui.mouse_exit(1);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);

        // 0 & 2

        gui.mouse_enter(0);
        gui.mouse_moved(0, 150.0, 150.0);
        gui.mouse_down(0, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);

        gui.mouse_enter(2);
        gui.mouse_moved(2, 450.0, 750.0); // 1, 2 => 7
        gui.mouse_down(2, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x81);

        gui.mouse_exit(0);
        assert_eq!(gui.get::<AppState>().joypad, !0x80);

        gui.mouse_exit(2);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);
    }
}
