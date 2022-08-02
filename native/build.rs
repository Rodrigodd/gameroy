extern crate embed_resource;

fn main() {
    let target = std::env::var("TARGET").unwrap();
    if target.contains("windows") {
        println!("cargo:rerun-if-changed=assets/resources.rc");
        embed_resource::compile("../assets/resources.rc")
    }
}
