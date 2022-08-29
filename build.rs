use std::process::Command;

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

    let out = Command::new("resvg")
        .args([icons_svg, "assets/icons.png", "--zoom=0.5"])
        .current_dir(&dir)
        .output()
        .expect("'resvg' should be installed");

    if !out.status.success() {
        panic!("resvg didnt successed: {:#?}", out);
    }

    let out = Command::new("resvg")
        .args([icons_svg, "assets/icons2x.png"])
        .current_dir(&dir)
        .output()
        .expect("'resvg' should be installed");

    if !out.status.success() {
        panic!("resvg didnt successed: {:#?}", out);
    }
}
