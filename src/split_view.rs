use crui::{Behaviour, InputFlags, Layout, MouseEvent};
use crui::{Id, LayoutContext, MinSizeContext};
use winit::window::CursorIcon;

/// Layout two controls side by side, allowing to resize the size of each control by dragging the
/// division beetween the two controls.
pub struct SplitView {
    spacing: f32,
    margins: [f32; 4],
    vertical: bool,
    pub split: f32,
    left_min_size: f32,
    right_min_size: f32,
    dragging: bool,
    dragging_delta: f32,
}
impl SplitView {
    pub fn new(spacing: f32, margins: [f32; 4], vertical: bool) -> Self {
        Self {
            spacing,
            margins,
            split: 0.5,
            vertical,
            left_min_size: 0.0,
            right_min_size: 0.0,
            dragging: false,
            dragging_delta: 0.0,
        }
    }

    fn free_space(&self, rect: [f32; 4]) -> f32 {
        if self.vertical {
            rect[3] - rect[1] - self.left_min_size - self.right_min_size - self.spacing
        } else {
            rect[2] - rect[0] - self.left_min_size - self.right_min_size - self.spacing
        }
    }

    fn is_on_split(&self, mouse_pos: [f32; 2], rect: [f32; 4]) -> bool {
        let free_space = self.free_space(rect);

        let split_pos = self.left_min_size + self.split * free_space;
        let mouse_pos = mouse_pos[self.vertical as usize];

        (split_pos - mouse_pos).abs() < 5.0
    }

    fn mouse_to_split_pos(&self, mouse_pos: [f32; 2], rect: [f32; 4]) -> f32 {
        let free_space = self.free_space(rect);
        let mouse_pos = mouse_pos[self.vertical as usize] - rect[self.vertical as usize];

        (mouse_pos - (self.left_min_size + self.spacing)) / free_space
    }
}
impl Layout for SplitView {
    fn compute_min_size(&mut self, this: Id, ctx: &mut MinSizeContext) -> [f32; 2] {
        let children = ctx.get_active_children(this);
        if children.is_empty() {
            return [
                self.margins[0] + self.margins[2],
                self.margins[1] + self.margins[3],
            ];
        }

        let left = ctx
            .get_active_children(this)
            .get(0)
            .map(|&id| ctx.get_min_size(id))
            .unwrap_or([0.0, 0.0]);
        let right = ctx
            .get_active_children(this)
            .get(1)
            .map(|&id| ctx.get_min_size(id))
            .unwrap_or([0.0, 0.0]);

        let min_width;
        let min_height;

        if self.vertical {
            self.left_min_size = left[1];
            self.right_min_size = right[1];
            min_width = left[0].max(right[0]);
            min_height = self.left_min_size + self.right_min_size;
        } else {
            self.left_min_size = left[0];
            self.right_min_size = right[0];
            min_width = self.left_min_size + self.right_min_size;
            min_height = left[1].max(right[1]);
        }

        [min_width + self.margins[0] + self.margins[2], min_height]
    }

    fn update_layouts(&mut self, this: Id, ctx: &mut LayoutContext) {
        let children = ctx.get_active_children(this);
        if children.is_empty() {
            return;
        }

        let left_child = ctx.get_active_children(this).get(0).cloned();
        let right_child = ctx.get_active_children(this).get(1).cloned();

        let rect = ctx.get_layouting(this);
        let height = rect.get_height() - self.margins[1] - self.margins[3];
        let width = rect.get_width() - self.margins[0] - self.margins[2];

        let rect = *rect.get_rect();
        let left = rect[0] + self.margins[0];
        let right = rect[2] - self.margins[2];
        let top = rect[1] + self.margins[1];
        let bottom = rect[3] - self.margins[3];

        if self.vertical {
            let reserved_height = self.left_min_size + self.right_min_size;
            let free_height = (height - reserved_height - self.spacing).max(0.0);
            let left_bottom = top + self.left_min_size + free_height * self.split;

            if let Some(left_child) = left_child {
                ctx.set_designed_rect(left_child, [left, top, right, left_bottom]);
            }
            if let Some(right_child) = right_child {
                let top = left_bottom;
                let right_bottom = top + self.right_min_size + free_height * (1.0 - self.split);
                ctx.set_designed_rect(right_child, [left, top, right, right_bottom]);
            }
        } else {
            let reserved_width = self.left_min_size + self.right_min_size;
            let free_width = (width - reserved_width - self.spacing).max(0.0);
            let left_right = top + self.left_min_size + free_width * self.split;
            if let Some(left_child) = left_child {
                ctx.set_designed_rect(left_child, [left, top, left_right, bottom]);
            }
            if let Some(right_child) = right_child {
                let left = left_right + self.spacing;
                let right_right = left + self.right_min_size + free_width * (1.0 - self.split);
                ctx.set_designed_rect(right_child, [left, top, right_right, bottom]);
            }
        }
    }
}

impl Behaviour for SplitView {
    fn input_flags(&self) -> InputFlags {
        InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: crui::MouseInfo, this: Id, ctx: &mut crui::Context) {
        let rect = ctx.get_rect(this);
        match mouse.event {
            MouseEvent::Down(crui::MouseButton::Left) => {
                if self.is_on_split(mouse.pos, rect) {
                    self.dragging = true;
                    let split = self.mouse_to_split_pos(mouse.pos, rect);
                    self.dragging_delta = self.split - split;
                    ctx.lock_cursor(true);
                }
            }
            MouseEvent::Up(crui::MouseButton::Left) => {
                self.dragging = false;
                ctx.lock_cursor(false);
            }
            MouseEvent::Exit => ctx.set_cursor(CursorIcon::Default),
            MouseEvent::Moved => {
                if self.dragging || self.is_on_split(mouse.pos, rect) {
                    ctx.set_cursor(CursorIcon::ColResize);
                } else {
                    ctx.set_cursor(CursorIcon::Default);
                }

                if self.dragging {
                    let split = self.dragging_delta + self.mouse_to_split_pos(mouse.pos, rect);

                    self.split = split.clamp(0.0, 1.0);
                    ctx.dirty_layout(this);
                }
            }
            _ => {}
        }
    }
}
