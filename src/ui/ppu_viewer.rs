use crui::graphics::Texture;
use crui::layouts::VBoxLayout;
use crui::text::Text;
use crui::{BuilderContext, Id};

use crate::style::Style;

use crate::event_table::{EventTable};

pub fn build(
    parent: Id,
    ctx: &mut dyn BuilderContext,
    event_table: &mut EventTable,
    style: &Style,
    tilemap_texture: u32,
) {
    ctx.create_control()
        .parent(parent)
        .layout(VBoxLayout::default())
        .build(ctx);

    ctx.create_control()
        .parent(parent)
        .graphic(Texture::new(tilemap_texture, [0.0, 0.0, 1.0, 1.0]).into())
        .min_size([100.0, 100.0])
        .build(ctx);
}
