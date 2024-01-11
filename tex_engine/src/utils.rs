/*! Utility methods and data structures.*/

use lazy_static::lazy_static;
use std::path::PathBuf;
use std::rc::Rc;

pub mod errors;

/// A [`HashMap`](std::collections::HashMap) with [`ahash::RandomState`] as hasher.
pub type HMap<A,B> = ahash::HashMap<A,B>;
pub type HSet<A> = ahash::HashSet<A>;
/// The reference counting pointer type used throughout the engine.
pub type Ptr<A> = Rc<A>;

lazy_static! {
    /// The current working directory.
    pub static ref PWD : PathBuf = std::env::current_dir().expect("No current directory!")
        .as_path().to_path_buf();
}