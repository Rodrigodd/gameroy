use giui::{Behaviour, Id, InputFlags, Layout, MouseEvent};
use std::cell::RefCell;
use std::rc::Rc;
use winit::window::CursorIcon;

type ColumnIndex = u16;

/// Contains the layout data that is common to all TableItems.
#[derive(Default)]
pub struct TableGroup {
    pub columns: Vec<Column>,
    pub h_spacing: f32,
    pub v_spacing: f32,
    pub h_margins: [f32; 2],
    /// The current split being dragged, and if it is in reverse.
    dragging: Option<(ColumnIndex, bool)>,
    /// The width of the current dragging column is computed by `mouse_pos[0] - dragging_anchor`.
    dragging_anchor: f32,
}
impl TableGroup {
    pub fn new(h_spacing: f32, v_spacing: f32, h_margins: [f32; 2]) -> Self {
        Self {
            h_spacing,
            v_spacing,
            h_margins,
            ..Self::default()
        }
    }

    pub fn column(mut self, width: f32, expand: bool) -> Self {
        self.columns.push(Column {
            width,
            expand,
            ..Default::default()
        });
        self
    }
}

/// Layout data of each colloumn in a table.
#[derive(Default)]
pub struct Column {
    pub width: f32,
    // pub min_width: f32,
    pub expand: bool,
    /// The actual width of the column after maybe being expanded.
    curr_width: f32,
}

/// Layout and Behaviour for table's items.
pub struct TableItem {
    /// This table group this item belongs to.
    group: Rc<RefCell<TableGroup>>,
    pub resizable: bool,
}
impl TableItem {
    pub fn new(group: Rc<RefCell<TableGroup>>) -> Self {
        Self {
            group,
            resizable: false,
        }
    }

    pub fn with_resizable(mut self, resizable: bool) -> Self {
        self.resizable = resizable;
        self
    }

    /// If hovering a split, return the index of the column to be resized, and if it is in
    /// reverse.
    fn to_be_dragged(
        &self,
        mouse_pos: [f32; 2],
        rect: [f32; 4],
        g: &TableGroup,
    ) -> Option<(ColumnIndex, bool)> {
        const DRAG_MARG: f32 = 5.0;
        let mouse_pos = mouse_pos[0] - rect[0];

        let mut x = 0.0;
        // If there was any expand column to the left, resize the column at the rigth of the
        // split, in reverse.
        let mut reverse = false;
        for (i, c) in g.columns.iter().enumerate() {
            reverse |= c.expand;
            if reverse && i + 1 == g.columns.len() {
                break;
            }
            x += c.curr_width;
            if mouse_pos <= x + g.h_spacing + DRAG_MARG {
                if mouse_pos > x - DRAG_MARG {
                    return Some((i as ColumnIndex + reverse as ColumnIndex, reverse));
                }
                break;
            }
            x += g.h_spacing;
        }

        None
    }
}
impl Behaviour for TableItem {
    fn input_flags(&self) -> InputFlags {
        if self.resizable {
            InputFlags::MOUSE
        } else {
            InputFlags::empty()
        }
    }

    fn on_mouse_event(&mut self, mouse: giui::MouseInfo, this: Id, ctx: &mut giui::Context) {
        let rect = ctx.get_rect(this);
        match mouse.event {
            MouseEvent::Down(giui::MouseButton::Left) => {
                let g = &mut self.group.borrow_mut();
                if let Some(d) = self.to_be_dragged(mouse.pos, rect, g) {
                    g.dragging = Some(d);
                    let reverse = if d.1 { -1.0 } else { 1.0 };
                    g.dragging_anchor = mouse.pos[0] - g.columns[d.0 as usize].curr_width * reverse;
                    ctx.lock_cursor(true);
                }
            }
            MouseEvent::Up(giui::MouseButton::Left) => {
                let g = &mut self.group.borrow_mut();
                g.dragging = None;
                ctx.lock_cursor(false);
            }
            MouseEvent::Exit => ctx.set_cursor(CursorIcon::Default),
            MouseEvent::Moved => {
                let g = &mut self.group.borrow_mut();
                if g.dragging.is_some() || self.to_be_dragged(mouse.pos, rect, g).is_some() {
                    ctx.set_cursor(CursorIcon::ColResize);
                } else {
                    ctx.set_cursor(CursorIcon::Default);
                }

                if let Some(d) = g.dragging {
                    let reverse = if d.1 { -1.0 } else { 1.0 };
                    let width = (mouse.pos[0] - g.dragging_anchor) * reverse;
                    g.columns[d.0 as usize].width = width.max(0.0);
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
        let g = &mut *self.group.borrow_mut();
        if children.is_empty() {
            return;
        }

        let rect = ctx.get_layouting(this);

        let width = rect.get_width() - g.h_margins[0] - g.h_margins[1];
        let reserved_width = g.h_spacing * (children.len() - 1) as f32
            + g.columns.iter().map(|x| x.width).sum::<f32>();
        let free_width = width - reserved_width;
        let total_weigth: f32 = g.columns.iter().filter_map(|x| x.expand.then(|| 1.0)).sum();

        let rect = *rect.get_rect();
        let top = rect[1];
        let bottom = rect[3] - g.v_spacing;
        let mut x = rect[0] + g.h_margins[0];

        for (
            child,
            &mut Column {
                width,
                expand,
                ref mut curr_width,
            },
        ) in ctx
            .get_active_children(this)
            .into_iter()
            .zip(g.columns.iter_mut())
        {
            *curr_width = if expand && free_width > 0.0 {
                width + free_width / total_weigth
            } else {
                width
            };
            ctx.set_rect(child, [x, top, x + *curr_width, bottom]);
            x += g.h_spacing + *curr_width;
        }
    }
}
