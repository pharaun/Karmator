use std::env;
use std::path::Path;

fn main() {
    let src = env::var("CARGO_MANIFEST_DIR").unwrap();
    let dst = Path::new(&env::var("OUT_DIR").unwrap()).join("built.rs");
    built::write_built_file_with_opts(Some(src.as_ref()), &dst).expect("Failed to build time info");
}
