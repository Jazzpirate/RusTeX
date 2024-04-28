pub mod engine;
pub mod fonts;
pub mod files;
pub(crate) mod shipout;
pub(crate) mod extension;
pub(crate) mod html;
pub(crate) mod nodes;
pub mod commands;
pub mod state;
pub mod stomach;
pub(crate) mod pgf;
pub mod output;

pub mod utils;

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