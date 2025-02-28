/*! Utility methods and data structures.*/

use lazy_static::lazy_static;
use std::path::PathBuf;

pub mod errors;

/// A [`HashMap`](std::collections::HashMap) with [`rustc_hash`] as hasher.
pub type HMap<A, B> = rustc_hash::FxHashMap<A, B>; //ahash::HashMap<A,B>;
pub type HSet<A> = rustc_hash::FxHashSet<A>; //ahash::HashSet<A>;

#[cfg(feature = "multithreaded")]
/// The reference counting pointer type used throughout the engine.
pub type Ptr<A> = std::sync::Arc<A>;
#[cfg(not(feature = "multithreaded"))]
/// The reference counting pointer type used throughout the engine.
pub type Ptr<A> = std::rc::Rc<A>;

lazy_static! {
    /// The current working directory.
    pub static ref PWD : PathBuf = std::env::current_dir().expect("No current directory!");
}
