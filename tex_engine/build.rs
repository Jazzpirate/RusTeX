// https://github.com/bblanchon/pdfium-binaries/releases

fn main() {
    /*
    #[cfg(feature = "pdfium-static")]
    {
        use flate2::read::GzDecoder;
        use reqwest::blocking::get;
        use std::env;
        use std::path::PathBuf;
        use tar::Archive;

        const PDFIUM_VERSION: &str = "6721";
        const BASE_URL: &str = "https://github.com/bblanchon/pdfium-binaries/releases/download";

        let target_os = env::var("CARGO_CFG_TARGET_OS").expect("Could not get target OS");
        let target_arch = env::var("CARGO_CFG_TARGET_ARCH").expect("Could not get target arch");
        let out_dir = PathBuf::from(env::var("OUT_DIR").expect("Could not get OUT_DIR"));

        let (platform, lib_name) = match (target_os.as_str(), target_arch.as_str()) {
            ("windows", "x86_64") => ("win-x64", "pdfium.dll.lib"),
            ("windows", "x86") => ("win-x86", "pdfium.dll.lib"),
            ("linux", "x86_64") => ("linux-x64", "libpdfium.a"),
            ("linux", "aarch64") => ("linux-arm64", "libpdfium.a"),
            ("macos", "x86_64") => ("mac-x64", "libpdfium.a"),
            ("macos", "aarch64") => ("mac-arm64", "libpdfium.a"),
            _ => panic!("Unsupported platform: {}-{}", target_os, target_arch),
        };

        let lib_dir = out_dir.join("lib");
        let lib_path = lib_dir.join(lib_name);

        if !lib_path.exists() {
            std::fs::create_dir_all(&lib_dir).expect("Could not create lib dir");
            let download_url = format!("{BASE_URL}/chromium/{PDFIUM_VERSION}/pdfium-{platform}.tgz");
            let archive_path = out_dir.join("pdfium.tgz");
            let mut response = get(download_url).expect("Could not download pdfium");
            let mut dest =
                std::fs::File::create(&archive_path).expect("Could not create pdfium archive");
            std::io::copy(&mut response, &mut dest).expect("Could not copy pdfium archive");
            let tar_gz = std::fs::File::open(&archive_path).expect("Could not open pdfium archive");
            let tar = GzDecoder::new(tar_gz);
            let mut archive = Archive::new(tar);
            archive
                .unpack(&out_dir)
                .expect("Could not unpack pdfium archive");
            std::fs::remove_file(archive_path).expect("Could not remove pdfium archive");
            assert!(lib_path.exists(),"File does not exist: {}",lib_path.display());
        }
        println!("cargo:rustc-link-lib=static=pdfium");
        println!("cargo:rustc-link-search=native={}", lib_dir.display());


        //let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        //println!("cargo:rustc-link-search=native={}", Path::new(&dir).join("lib").display());
        //println!("cargo:rustc-link-lib=static=pdfium");
    }
     */
}
