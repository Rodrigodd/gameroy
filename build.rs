use resvg::usvg::TreeParsing;

fn main() {
    render_svg_assets();
}

fn cargo_rerun(path: &str) {
    println!("cargo:rerun-if-changed={}", path);
}

fn render_svg_assets() {
    let icons_svg = "assets/icons.svg";
    cargo_rerun(icons_svg);

    let dir = std::env::var("CARGO_MANIFEST_DIR").expect("set by cargo");

    let icons = include_bytes!("assets/icons.svg");

    let opts = resvg::usvg::Options {
        resources_dir: std::fs::canonicalize(dir).ok(),
        ..Default::default()
    };

    let tree = resvg::usvg::Tree::from_data(icons, &opts).unwrap();

    let tree = resvg::Tree::from_usvg(&tree);

    let size = tree.size.to_int_size();
    let zoom_size = size.scale_by(2.0).unwrap();
    to_png(&tree, size, "assets/icons.png");
    to_png(&tree, zoom_size, "assets/icons2x.png");
}

fn to_png(tree: &resvg::Tree, size: resvg::tiny_skia::IntSize, png_file_name: &str) {
    let mut pixmap = resvg::tiny_skia::Pixmap::new(size.width(), size.height()).unwrap();

    let size = size.to_size();
    let transform = resvg::tiny_skia::Transform::from_scale(
        size.width() / tree.size.width(),
        size.height() / tree.size.height(),
    );
    tree.render(transform, &mut pixmap.as_mut());

    pixmap.save_png(png_file_name).unwrap();
}
