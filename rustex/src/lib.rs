pub mod engine;
pub(crate) mod shipout;

pub mod utils;
pub use shipout::html::ImageOptions;

const RUSTEX_CSS_URL:&str = "https://raw.githack.com/Jazzpirate/RusTeX/main/rustex/src/resources/rustex.css";

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use tex_engine::utils::PWD;
    use crate::engine::RusTeXEngine;
    use crate::engine::RusTeXEngineT;
    use path_dedot::*;


    #[test]
    fn initialize() {
        RusTeXEngine::initialize(true);
    }
    #[test]
    fn test_tex() {
        let testpath:PathBuf = PWD.join("../test/test.tex").parse_dot().unwrap().to_path_buf();
        let ret = RusTeXEngine::do_file(testpath.to_str().unwrap(),false,true,true);
        let out = testpath.with_extension("html");
        std::fs::write(out.to_str().unwrap(), &ret.out).unwrap();
    }
}