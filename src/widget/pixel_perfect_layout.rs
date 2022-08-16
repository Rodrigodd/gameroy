use giui::{Id, Layout, LayoutContext, MinSizeContext};

pub struct ScreenLayout {
    size: (u32, u32),
}
impl ScreenLayout {
    pub fn new(size: (u32, u32)) -> Self {
        Self { size }
    }
}
impl Layout for ScreenLayout {
    fn compute_min_size(&mut self, this: Id, ctx: &mut MinSizeContext) -> [f32; 2] {
        let mut min_size = [self.size.0 as f32, self.size.1 as f32];
        let ratio = min_size[0] / min_size[1];
        for child in ctx.get_active_children(this) {
            let c_min_size = ctx.get_layouting(child).unwrap().get_min_size();
            min_size[0] = min_size[0].max(c_min_size[0]);
            min_size[1] = min_size[1].max(c_min_size[1]);
        }
        if min_size[0] > min_size[1] * ratio {
            [min_size[0], min_size[0] / ratio]
        } else {
            [min_size[1] * ratio, min_size[1]]
        }
    }

    fn update_layouts(&mut self, this: Id, ctx: &mut LayoutContext) {
        let rect = ctx.get_layouting(this);
        let mut x = rect.get_rect()[0];
        let mut y = rect.get_rect()[1];

        let width = rect.get_width();
        let height = rect.get_height();

        let des_width;
        let des_height;
        let ratio = self.size.0 as f32 / self.size.1 as f32;

        if width / height < ratio {
            des_width = self.size.0 as f32 * (width / self.size.0 as f32).floor();
            des_height = (des_width / ratio).round();
        } else {
            des_height = self.size.1 as f32 * (height / self.size.1 as f32).floor();
            des_width = des_height * ratio;
        }

        x += (width - des_width) / 2.0;

        if width / height >= ratio {
            y += (height - des_height) / 2.0;
        } else {
            y += (width - des_width) / 2.0;
        }

        let des_rect = [x.floor(), y.floor(), x + des_width, y + des_height];
        for child in ctx.get_active_children(this) {
            ctx.set_designed_rect(child, des_rect);
        }
    }
}
