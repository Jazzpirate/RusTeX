An implementation of a generic TeX engine, the core of the TeX typesetting system.
This crate largely follows an object-oriented design for modularity and adaptability,
with functionality largely implemented in generic traits. Ideally, this lets the compiler
optimize the code for the specific types used, while still allowing for easy customization.

**Currently requires nightly branch** due to a compiler error in the stable branch.

See [documentation](https://docs.rs/tex_engine/latest/tex_engine/) for details