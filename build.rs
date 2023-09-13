use resvg::usvg::TreeParsing;

fn main() {
    render_svg_assets();
}

fn cargo_rerun(path: &str) {
    println!("cargo:rerun-if-changed={}", path);
}

/// Avaliable scales for bitmaps.
///
/// These comes from Android's documentation: https://developer.android.com/training/multiscreen/screendensities#TaskProvideAltBmp
const SCALES: &[f32] = &[0.75, 1.0, 1.5, 2.0, 3.0, 4.0];

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

    for &scale in SCALES {
        to_png(&tree, scale, &format!("assets/icons{}x.png", scale));
    }
}

fn to_png(tree: &resvg::Tree, scale: f32, png_file_name: &str) {
    let size = tree.size.to_int_size().scale_by(scale).unwrap();
    let mut pixmap = resvg::tiny_skia::Pixmap::new(size.width(), size.height()).unwrap();

    let transform = resvg::tiny_skia::Transform::from_scale(scale, scale);
    tree.render(transform, &mut pixmap.as_mut());

    pixmap.save_png(png_file_name).unwrap();
}
