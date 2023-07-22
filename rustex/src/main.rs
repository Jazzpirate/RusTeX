use tex_engine::engine::filesystem::{FileSystem,KpseVirtualFileSystem};
use tex_engine::engine::gullet::TeXGullet;
use tex_engine::engine::mouth::TracingMouth;
use tex_engine::engine::state::TeXState;
use tex_engine::engine::stomach::NoShipoutDefaultStomach;
use tex_engine::tex::boxes::StandardTeXBox;
use tex_engine::tex::token::TokenWithSourceref;

fn main() {
    /*use log::{error,warn,info,debug,trace};
    env_logger::builder().filter_level(log::LevelFilter::Info).init();
    warn!("Hello, world!");
    info!("File System");
    let mut fs = KpseVirtualFileSystem::new("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/");
    info!("TeX State");
    let mut state: TeXState<TokenWithSourceref<u8>,_> = TeXState::new(fs);
    info!("TeX Gullet");
    let mut gullet = TeXGullet::new(TracingMouth::<u8>::new());
    info!("TeX Stomach");
    let mut stomach = NoShipoutDefaultStomach::<TokenWithSourceref<u8>,StandardTeXBox>::new();
    warn!("Done.")*/
}