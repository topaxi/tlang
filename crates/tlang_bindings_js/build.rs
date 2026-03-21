use std::env;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let pkg_dir = Path::new(&manifest_dir).join("pkg");

    std::fs::create_dir_all(&pkg_dir).expect("failed to create pkg directory");

    let bundle = format!(
        "// Auto-generated. Do not edit directly.\n\
         // Rebuild the tlang_bindings_js crate or run `cargo xtask gen-stdlib` to regenerate.\n\
         \n\
         {}",
        tlang_codegen_js::generator::CodegenJS::compile_stdlib_module()
    );

    std::fs::write(pkg_dir.join("stdlib.js"), bundle).expect("failed to write stdlib.js");

    // Re-run whenever stdlib source files change.
    println!("cargo:rerun-if-changed=../tlang_codegen_js/std/");
}
