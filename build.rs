extern crate embed_resource;

fn main() {
    println!("cargo:rerun-if-changed=assets/resources.rc");
    embed_resource::compile("assets/resources.rc")
}
