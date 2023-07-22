***Work In Progress***

An implementation of a generic TeX engine, the core of the TeX typesetting system.
This crate largely follows an object-oriented design for modularity and adaptability,
with functionality largely implemented in generic traits. This lets the compiler
optimize the code for the specific types used, while still allowing for easy customization.