use crui::graphics::Texture;
use crui::layouts::VBoxLayout;
use crui::{BuilderContext, Id};

use crate::style::Style;

use crate::event_table::EventTable;

pub fn build(
    parent: Id,
    ctx: &mut dyn BuilderContext,
    _event_table: &mut EventTable,
    _style: &Style,
    tilemap_texture: u32,
) {
    ctx.create_control()
        .parent(parent)
        .layout(VBoxLayout::default())
        .build(ctx);

    ctx.create_control()
        .parent(parent)
        .graphic(Texture::new(tilemap_texture, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([128.0, 196.0])
        .fill_x(crui::RectFill::ShrinkCenter)
        .fill_y(crui::RectFill::ShrinkCenter)
        .build(ctx);
}
