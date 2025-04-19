/*! This crate provides a way to access glyphs from TeX fonts. It is intended to be used by
crates using [`tex_engine`](https://crates.io/crates/tex_engine).

TeX deals with fonts by parsing *font metric files* (`.tfm` files), which contain information
about the dimensions of each glyph in the font. So from the point of view of (the core of) TeX,
a *glyph* is just an index $0 \leq i \leq 255$ into the font metric file.

In order to find out what the glyph actually looks like, we want to ideally know the corresponding
unicode codepoint. This crate attempts to do exactly that.

# Usage

This crate attempts to associate a tex font (identified by the file name stem of its `.tfm` file) with:
1. A list of [`FontModifier`](fontstyles::FontModifier)s (e.g. bold, italic, sans-serif, etc.)
2. A [`GlyphList`], being an array `[`[`Glyph`]`;256]`

A [`Glyph`] then is either undefined (i.e. the glyph is not present in the font, or the crate couldn't
figure out what exactly it is) or presentable as a string.

Consider e.g. `\mathbf{\mathit{\Gamma^\kappa_\ell}}` (i.e. $\mathbf{\mathit{\Gamma^\kappa_\ell}}$).
From the point of view of TeX, this is a sequence of 3 glyphs, represented as indices into the font
`cmmib10`, namely 0, 20, and 96.

Here's how to use this crate to obtain the corresponding unicode characters, i.e. `𝜞`, `𝜿` and `ℓ`:

### Instantiation

First, we instantiate a [`FontInfoStore`](encodings::FontInfoStore) with a function that
allows it to find files. This function should take a string (e.g. `cmmib10.tfm`) and return a string
(e.g. `/usr/share/texmf-dist/fonts/tfm/public/cm/cmmib10.tfm`). This could be done by calling `kpsewhich`
for example, but repeated and frequent calls to `kpsewhich` are slow, so more efficient alternatives
are recommended.

```no_run
use tex_glyphs::encodings::FontInfoStore;
let mut store = FontInfoStore::new(|s| {
    std::str::from_utf8(std::process::Command::new("kpsewhich")
        .args(vec!(s)).output().expect("kpsewhich not found!")
        .stdout.as_slice()).unwrap().trim().to_string()
});
```
This store will now use the provided function to find your `pdftex.map` file, which lists
all the fonts that are available to TeX and associates them with `.enc`, `.pfa` and `.pfb` files.

### Obtaining Glyphs

If we now query the store for the [`GlyphList`] of some font, e.g. `cmmib10`, like so:
```no_run
# use tex_glyphs::encodings::FontInfoStore;
# let mut store = FontInfoStore::new(|s| {
#     std::str::from_utf8(std::process::Command::new("kpsewhich")
#         .args(vec!(s)).output().expect("kpsewhich not found!")
#         .stdout.as_slice()).unwrap().trim().to_string()
# });
let ls = store.get_glyphlist("cmmib10");
```
...it will attempt to parse the `.enc` file associated with `cmmib10`, if existent. If not, or if this
fails, it will try to parse the `.pfa` or `.pfb` file. If neither works, it will search for a `.vf` file
and try to parse that. If that too fails, it will return an empty [`GlyphList`].

From either of those three sources, it will then attempt to associate each byte index with a
[`Glyph`]:
```no_run
# use tex_glyphs::encodings::FontInfoStore;
# let mut store = FontInfoStore::new(|s| {
#     std::str::from_utf8(std::process::Command::new("kpsewhich")
#         .args(vec!(s)).output().expect("kpsewhich not found!")
#         .stdout.as_slice()).unwrap().trim().to_string()
# });
# let ls = store.get_glyphlist("cmmib10");
let zero = ls.get(0);
let twenty = ls.get(20);
let ninety_six = ls.get(96);
println!("0={}={}, 20={}={}, and 96={}={}",
    zero.name(),zero,
    twenty.name(),twenty,
    ninety_six.name(),ninety_six
);
```
```text
0=Gamma=Γ, 20=kappa=κ, and 96=lscript=ℓ
```

### Font Modifiers

So far, so good - but the glyphs are not bold or italic, but in `cmmib10`, they are.
So let's check out what properties `cmmib10` has:
```
# use tex_glyphs::encodings::FontInfoStore;
# let mut store = FontInfoStore::new(|s| {
#     std::str::from_utf8(std::process::Command::new("kpsewhich")
#         .args(vec!(s)).output().expect("kpsewhich not found!")
#         .stdout.as_slice()).unwrap().trim().to_string()
# });
let font_info = store.get_info("cmmib10").unwrap();
println!("{:?}",font_info.styles);
println!("{:?}",font_info.weblink);
```
```text
ModifierSeq { blackboard: false, fraktur: false, script: false, bold: true, capitals: false, monospaced: false, italic: true, oblique: false, sans_serif: false }
Some(("Latin Modern Math", "https://fonts.cdnfonts.com/css/latin-modern-math"))
```
...so this tells us that the font is bold and italic, but not sans-serif, monospaced, etc.
Also, it tells us that the publically available web-compatible quivalent
of this font is called "Latin Modern Math" and that we can find it at the provided
URL, if we want to use it in e.g. HTML :)

Now we only need to apply the modifiers to the glyphs:
```
# use tex_glyphs::encodings::FontInfoStore;
# let mut store = FontInfoStore::new(|s| {
#     std::str::from_utf8(std::process::Command::new("kpsewhich")
#         .args(vec!(s)).output().expect("kpsewhich not found!")
#         .stdout.as_slice()).unwrap().trim().to_string()
# });
# let ls = store.get_glyphlist("cmmib10");
# let zero = ls.get(0);
# let twenty = ls.get(20);
# let ninety_six = ls.get(96);
# let font_info = store.get_info("cmmib10").unwrap();
use tex_glyphs::fontstyles::FontModifiable;
println!("{}, {}, and {}",
    zero.to_string().apply(font_info.styles),
    twenty.to_string().apply(font_info.styles),
    ninety_six.to_string().apply(font_info.styles)
);
```
```text
𝜞, 𝜿, and ℓ
```

The [`apply`](fontstyles::FontModifiable::apply)-method stems
from the trait [`FontModifiable`](fontstyles::FontModifiable), which is implemented
for any type that implements `AsRef<str>`, including `&str` and `String`.
It also provides more direct methods, e.g. [`make_bold`](fontstyles::FontModifiable::make_bold),
[`make_italic`](fontstyles::FontModifiable::make_italic), [`make_sans`](fontstyles::FontModifiable::make_sans), etc.

# Fixing Mistakes
The procedure above for determining glyphs and font modifiers is certainly not perfect; not just
because `enc` and `pfa`/`pfb` files might contain wrong or unknown glyph names, but also because
font modifiers are determined heuristically. For that reason, we provide a way to fix mistakes:
1. The map from glyphnames to unicode is stored in the file [glyphs.map](https://github.com/Jazzpirate/RusTeX/blob/main/tex-glyphs/src/resources/glyphs.map)
2. Font modifiers, web font names and links, or even full glyph lists can be added
   to the markdown file [patches.md](https://github.com/Jazzpirate/RusTeX/blob/main/tex-glyphs/src/resources/patches.md),
   which additionally serves as a how-to guide for patching any mistakes you might find.

Both files are parsed *during compilation*.

If you notice any mistakes, feel free to open a pull request for these files.
*/
#![allow(text_direction_codepoint_in_literal)]
#![warn(missing_docs)]

pub mod encodings;
pub mod fontstyles;
pub mod glyphs;
mod parsing;

pub use crate::glyphs::{Combinator, Glyph, GlyphList};
pub use encodings::FontInfoStore;

include!(concat!(env!("OUT_DIR"), "/codegen.rs"));

#[cfg(test)]
mod tests {
    use super::fontstyles::{FontModifiable, FontModifier};
    use super::*;
    use crate::encodings::FontInfoStore;
    #[test]
    fn test_glyphmap() {
        assert_eq!(Glyph::get("AEacute").to_string(), "Ǽ");
        assert_eq!(Glyph::get("contourintegral").to_string(), "∮");
        assert_eq!(Glyph::get("bulletinverse").to_string(), "◘");
        assert_eq!(Glyph::get("Gangiacoptic").to_string(), "Ϫ");
        assert_eq!(Glyph::get("zukatakana").to_string(), "ズ");
        assert_eq!("test".make_bold().to_string(), "𝐭𝐞𝐬𝐭");
        assert_eq!("test".make_bold().make_sans().to_string(), "𝘁𝗲𝘀𝘁");
        assert_eq!(
            "test"
                .apply_modifiers(&[FontModifier::SansSerif, FontModifier::Bold])
                .to_string(),
            "𝘁𝗲𝘀𝘁"
        );
    }
    fn get_store() -> FontInfoStore<String, fn(&str) -> String> {
        FontInfoStore::new(|s| {
            std::str::from_utf8(
                std::process::Command::new("kpsewhich")
                    .args(vec![s])
                    .output()
                    .expect("kpsewhich not found!")
                    .stdout
                    .as_slice(),
            )
            .expect("unexpected kpsewhich output")
            .trim()
            .to_string()
        })
    }

    #[test]
    fn test_encodings() {
        let mut es = get_store();
        let names = es
            .all_encs()
            .take(50)
            .map(|e| e.tfm_name.clone())
            .collect::<Vec<_>>();
        for n in names {
            es.get_glyphlist(n);
        }
    }
    #[test]
    fn print_table() {
        env_logger::builder()
            .filter_level(log::LevelFilter::Debug)
            .try_init()
            .expect("failed to initialize tests");
        let mut es = get_store();
        log::info!(
            "cmr10:\n{}",
            es.display_encoding("cmr10").expect("cmr10 not found")
        );
        log::info!(
            "cmbx10:\n{}",
            es.display_encoding("cmbx10").expect("cmbx not found")
        );
        log::info!(
            "wasy10:\n{}",
            es.display_encoding("wasy10").expect("cmbx not found")
        );
        /*
        log::info!("ptmr7t:\n{}",es.display_encoding("ptmr7t").unwrap());
        log::info!("ecrm1095:\n{}",es.display_encoding("ecrm1095").unwrap());
        log::info!("ec-lmr10:\n{}",es.display_encoding("ec-lmr10").unwrap());
        log::info!("jkpbitc:\n{}",es.display_encoding("jkpbitc").unwrap());
        log::info!("ot1-stix2textsc:\n{}",es.display_encoding("ot1-stix2textsc").unwrap());
        log::info!("stix-mathbbit-bold:\n{}",es.display_encoding("stix-mathbbit-bold").unwrap());
        log::info!("MnSymbolE10:\n{}",es.display_encoding("MnSymbolE10").unwrap());
         */
    }
    /*
       #[test]
       fn vfs() {
           env_logger::builder().filter_level(log::LevelFilter::Debug).try_init().unwrap();
           use tex_engine::engine::filesystem::kpathsea::*;
           let mut store = encodings::EncodingStore::new(|s| {
               match KPATHSEA.which(s).map(|s| s.to_str().map(|s| s.to_string())).flatten() {
                   Some(s) => s,
                   _ => "".into()
               }
           });
           let vfs = &KPATHSEA.post.clone();
           for v in vfs.values() {
               match v.extension() {
                   Some(e) if e == "vf" => {
                       let name = v.file_stem().unwrap().to_str().unwrap();
                       log::info!("{}",v.display());
                       match store.display_encoding(name) {
                           Some(s) => log::info!("{}",s),
                           None => log::info!("Failed!")
                       }
                       print!("");
                   }
                   _ => ()
               }
           }
       }

    */
}
