extern crate embed_resource;

fn main() {
    println!("cargo:rerun-if-changed=../assets/resources.rc");
    let target = std::env::var("TARGET").unwrap();
    if target.contains("windows") {
        embed_resource::compile("../assets/resources.rc", embed_resource::NONE)
    }
}
