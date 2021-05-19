use std::path::Path;
use std::env;

fn main() {
    let src = env::var("CARGO_MANIFEST_DIR").unwrap();
    let dst = Path::new(&env::var("OUT_DIR").unwrap()).join("built.rs");
    let mut opts = built::Options::default();
    opts.set_env(true);
    opts.set_git(true);
    opts.set_time(true);

    built::write_built_file_with_opts(&opts, src.as_ref(), &dst).expect("Failed to build time info");
}
