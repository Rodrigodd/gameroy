use std::collections::HashMap;

use giui::{Context, Id};

pub struct GamePad {
    /// The Id of the control that represent the button position, and a button bit flag that can
    /// represent multiple buttons.
    buttons: Vec<(Id, u8)>,
    /// The sprites of each button, plus the cross center. Gamepad is responsible to change they
    /// opacity.
    sprites: [Id; 9],
    pressed: HashMap<giui::MouseId, u8>,
}

impl GamePad {
    pub const MAX_DIST: f32 = 35.0;
    pub fn new(buttons: Vec<(Id, u8)>, sprites: [Id; 9]) -> Self {
        Self {
            buttons,
            sprites,
            pressed: HashMap::default(),
        }
    }

    fn on_change(&self, ctx: &mut Context) {
        let mut joypad = 0;
        for (_, p) in self.pressed.iter() {
            joypad |= self.buttons[*p as usize].1;
        }

        for i in 0..8 {
            let alpha = if (joypad >> i) & 1 != 0 { 255 } else { 128 };
            ctx.get_graphic_mut(self.sprites[i as usize])
                .set_alpha(alpha);
        }
        ctx.get_graphic_mut(self.sprites[8])
            .set_alpha([128, 255][((joypad & 0xf) != 0) as usize]);

        let app_state = ctx.get_mut::<crate::AppState>();
        app_state.joypad = !joypad;
    }
}

impl giui::Behaviour for GamePad {
    fn on_active(&mut self, _this: Id, ctx: &mut Context) {
        self.pressed.clear();
        self.on_change(ctx);
    }

    fn input_flags(&self) -> giui::InputFlags {
        giui::InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: giui::MouseInfo, _this: Id, ctx: &mut Context) {
        let pressed = *self.pressed.get(&mouse.id).unwrap_or(&u8::MAX);

        if mouse.buttons.left.pressed() {
            // find closest
            let (b, dist) = self
                .buttons
                .iter()
                .enumerate()
                .map(|(i, &(id, _))| {
                    let x = ctx.get_rect(id);
                    let center = [(x[0] + x[2]) / 2.0, (x[1] + x[3]) / 2.0];
                    let dx = mouse.pos[0] - center[0];
                    let dy = mouse.pos[1] - center[1];
                    // distance
                    (i, dx * dx + dy * dy)
                })
                .min_by_key(|x| x.1.min((Self::MAX_DIST + 1.0).powi(2)) as u32)
                .unwrap();

            let b = if dist > Self::MAX_DIST.powi(2) {
                u8::MAX
            } else {
                b as u8
            };

            // unpress the previous button
            if pressed != b && pressed != u8::MAX {
                self.pressed.remove(&mouse.id);
            }

            // press the current on
            if b != u8::MAX {
                self.pressed.insert(mouse.id, b);
            }
            self.on_change(ctx);
        } else {
            let b = u8::MAX;
            // unpress the previous button
            if pressed != b && pressed != u8::MAX {
                self.pressed.remove(&mouse.id);
                self.on_change(ctx);
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
                (
                    create_button([x, y]),
                    1_u8.checked_shl(i as u32 - 1).unwrap_or(0),
                )
            });
            ctx.create_control()
                .behaviour(GamePad::new(buttons[0..8].to_vec(), buttons.map(|x| x.0)))
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
        log::info!("test 0");

        gui.mouse_moved(0, 150.0, 150.0);
        gui.mouse_down(0, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);
        gui.mouse_exit(0);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);

        // 1
        log::info!("test 1");

        gui.mouse_moved(1, 450.0, 750.0); // 1, 2 => 7
        gui.mouse_down(1, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x80);
        gui.mouse_exit(1);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);

        // 0 & 2
        log::info!("test 0 & 2");

        gui.mouse_moved(0, 150.0, 150.0);
        gui.mouse_down(0, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);

        gui.mouse_moved(2, 450.0, 750.0); // 1, 2 => 7
        gui.mouse_down(2, giui::MouseButton::Left);
        gui.mouse_moved(0, 150.0, 150.0);
        gui.mouse_moved(2, 450.0, 750.0); // 1, 2 => 7
        assert_eq!(gui.get::<AppState>().joypad, !0x81);

        gui.mouse_exit(0);
        assert_eq!(gui.get::<AppState>().joypad, !0x80);
        gui.mouse_moved(2, 450.0, 750.0); // 1, 2 => 7
        assert_eq!(gui.get::<AppState>().joypad, !0x80);

        gui.mouse_exit(2);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);

        // 1 & 2
        log::info!("test 1 & 2");

        gui.mouse_moved(1, 150.0, 150.0);
        gui.mouse_down(1, giui::MouseButton::Left);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);

        gui.mouse_moved(2, 450.0, 750.0); // 1, 2 => 7
        gui.mouse_down(2, giui::MouseButton::Left);
        gui.mouse_moved(1, 150.0, 150.0);
        assert_eq!(gui.get::<AppState>().joypad, !0x81);

        gui.mouse_exit(2);
        gui.mouse_moved(1, 150.0, 150.0);
        assert_eq!(gui.get::<AppState>().joypad, !0x01);

        gui.mouse_exit(1);
        assert_eq!(gui.get::<AppState>().joypad, !0x00);
    }
}
