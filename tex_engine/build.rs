// https://github.com/bblanchon/pdfium-binaries/releases
#[cfg(feature="pdfium-static")]
use std::path::Path;
#[cfg(feature="pdfium-static")]
use std::env;

fn main() {
    #[cfg(feature="pdfium-static")]
    {
        let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        println!("cargo:rustc-link-search=native={}", Path::new(&dir).join("lib").display());
        println!("cargo:rustc-link-lib=static=pdfium");
    }
}