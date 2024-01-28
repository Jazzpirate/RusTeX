This crate provides the necessary data structures and algorithms for
processing TeX documents fully. Since there are many different TeX engines
(e.g. pdfTeX, XeTeX, LuaTeX, etc.), this crate largely follows an object-oriented design 
for modularity and adaptability, with functionality largely implemented in generic traits. 
Ideally, this lets the compiler optimize the code for the specific types used, 
while still allowing for easy customization.

**Currently requires nightly branch** due to a compiler error in the stable branch.

See [documentation](https://docs.rs/tex_engine/latest/tex_engine/) for details