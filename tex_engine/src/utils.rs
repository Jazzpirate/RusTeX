/*! Utility methods and data structures.*/

use lazy_static::lazy_static;
use std::path::PathBuf;

pub mod errors;

/// A [`HashMap`](std::collections::HashMap) with [`ahash::RandomState`] as hasher.
pub type HMap<A,B> = ahash::HashMap<A,B>;
pub type HSet<A> = ahash::HashSet<A>;
/// The reference counting pointer type used throughout the engine.

#[cfg(feature = "multithreaded")]
pub type Ptr<A> = std::sync::Arc<A>;
#[cfg(not(feature = "multithreaded"))]
pub type Ptr<A> = std::rc::Rc<A>;

lazy_static! {
    /// The current working directory.
    pub static ref PWD : PathBuf = std::env::current_dir().expect("No current directory!")
        .as_path().to_path_buf();
}