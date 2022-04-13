use giui::{Behaviour, Id, InputFlags, Layout, MouseEvent};
use std::cell::RefCell;
use std::rc::Rc;
use winit::window::CursorIcon;

type SplitIndex = u16;

/// Contains the layout data that is common to all TableItems.
pub struct TableGroup {
    pub collumns: Vec<Collumn>,
    pub h_spacing: f32,
    pub v_spacing: f32,
    pub h_margins: [f32; 2],
}

/// Layout data of each colloumn in a table.
pub struct Collumn {
    pub width: f32,
    // pub min_width: f32,
    pub expand: bool,
}

/// Layout and Behaviour for table's items.
pub struct TableItem {
    /// This table group this item belongs to.
    group: Rc<RefCell<TableGroup>>,
    /// The current split being dragged
    dragging: Option<SplitIndex>,
    /// The width of the current dragging collumn is computed by `mouse_pos[0] - dragging_anchor`.
    dragging_anchor: f32,
}
impl TableItem {
    pub fn new(group: Rc<RefCell<TableGroup>>) -> Self {
        Self {
            group,
            dragging: None,
            dragging_anchor: 0.0,
        }
    }

    /// Return the index of the currently hovering split, if any.
    fn is_on_split(&self, mouse_pos: [f32; 2], rect: [f32; 4]) -> Option<SplitIndex> {
        let mouse_pos = mouse_pos[0] - rect[0];

        let mut x = 0.0;
        let mut split = None;
        let g = &self.group.borrow_mut();
        for (i, &Collumn { width, .. }) in g.collumns.iter().enumerate() {
            x += width;
            if mouse_pos > x && mouse_pos <= x + g.h_spacing {
                split = Some(i as _);
            }
            x += g.h_spacing;
        }

        split
    }
}
impl Behaviour for TableItem {
    fn input_flags(&self) -> InputFlags {
        InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: giui::MouseInfo, this: Id, ctx: &mut giui::Context) {
        let rect = ctx.get_rect(this);
        match mouse.event {
            MouseEvent::Down(giui::MouseButton::Left) => {
                if let Some(split) = self.is_on_split(mouse.pos, rect) {
                    self.dragging = Some(split);
                    let g = &mut self.group.borrow_mut();
                    self.dragging_anchor = mouse.pos[0] - g.collumns[split as usize].width;
                    ctx.lock_cursor(true);
                }
            }
            MouseEvent::Up(giui::MouseButton::Left) => {
                self.dragging = None;
                ctx.lock_cursor(false);
            }
            MouseEvent::Exit => ctx.set_cursor(CursorIcon::Default),
            MouseEvent::Moved => {
                if self.dragging.is_some() || self.is_on_split(mouse.pos, rect).is_some() {
                    ctx.set_cursor(CursorIcon::ColResize);
                } else {
                    ctx.set_cursor(CursorIcon::Default);
                }

                if let Some(split) = self.dragging {
                    let width = mouse.pos[0] - self.dragging_anchor;
                    let g = &mut self.group.borrow_mut();
                    g.collumns[split as usize].width = width.max(0.0);
                    ctx.dirty_layout(this);
                }
            }
            _ => {}
        }
    }
}
impl Layout for TableItem {
    fn compute_min_size(&mut self, this: giui::Id, ctx: &mut giui::MinSizeContext) -> [f32; 2] {
        let children = ctx.get_active_children(this);
        let g = self.group.borrow_mut();
        if children.is_empty() {
            [g.h_margins[0] + g.h_margins[1], 0.0]
        } else {
            let mut min_width: f32 =
                g.h_margins[0] + g.h_margins[1] + (children.len() - 1) as f32 * g.h_spacing;
            let mut min_height: f32 = 0.0;
            for child in children {
                let [width, height] = ctx.get_layouting(child).unwrap().get_min_size();
                min_width += width;
                min_height = min_height.max(height);
            }
            [min_width, min_height + g.v_spacing]
        }
    }

    fn update_layouts(&mut self, this: giui::Id, ctx: &mut giui::LayoutContext) {
        let children = ctx.get_active_children(this);
        let g = self.group.borrow_mut();
        if children.is_empty() {
            return;
        }

        let rect = ctx.get_layouting(this);

        let width = rect.get_width() - g.h_margins[0] - g.h_margins[1];
        let reserved_width = g.h_spacing * (children.len() - 1) as f32
            + g.collumns.iter().map(|x| x.width).sum::<f32>();
        let free_width = width - reserved_width;
        let total_weigth: f32 = g
            .collumns
            .iter()
            .filter_map(|x| x.expand.then(|| 1.0))
            .sum();

        let rect = *rect.get_rect();
        let top = rect[1];
        let bottom = rect[3] - g.v_spacing;
        let mut x = rect[0] + g.h_margins[0] - g.h_spacing;

        for (child, &Collumn { width, expand }) in ctx
            .get_active_children(this)
            .into_iter()
            .zip(g.collumns.iter())
        {
            let width = if expand && free_width > 0.0 {
                width + free_width / total_weigth
            } else {
                width
            };
            ctx.set_rect(child, [x, top, x + width, bottom]);
            x += g.h_spacing + width;
        }
    }
}
