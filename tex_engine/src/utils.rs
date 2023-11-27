/*! Utility methods and data structures.*/

use lazy_static::lazy_static;
use std::path::PathBuf;
use std::rc::Rc;

pub mod errors;

/// A [`HashMap`](std::collections::HashMap) with [`ahash::RandomState`] as hasher.
pub type HMap<A,B> = ahash::HashMap<A,B>;
/// The reference counting pointer type used throughout the engine.
pub type Ptr<A> = Rc<A>;

lazy_static! {
    /// The current working directory.
    pub static ref PWD : PathBuf = std::env::current_dir().expect("No current directory!")
        .as_path().to_path_buf();
}

/// Provides [`Vec`]s of some type for reuse, avoiding unnecessary allocations.
/// Primarily used for reusing [`Vec`]`<`[`Token`](crate::tex::token::Token)`>`s.
pub struct ReusableVectorFactory<T> {
    size:usize,
    vecs:Vec<Vec<T>>
}
impl<T> ReusableVectorFactory<T> {
    /// Initializes a new [`ReusableVectorFactory`] with `initial_vecs` [`Vec`]s of size `size`.
    pub fn new(initial_vecs:usize,size:usize) -> Self {
        let mut vecs = Vec::with_capacity(initial_vecs);
        for _ in 0..initial_vecs {
            vecs.push(Vec::with_capacity(size));
        }
        ReusableVectorFactory { vecs,size }
    }
    pub const fn constant() -> Self {
        ReusableVectorFactory { vecs:Vec::new(),size:0 }
    }
    /// Returns an empty [`Vec`] with capacity `≥ size` for reuse.
    pub fn get(&mut self) -> Vec<T> {
        if let Some(vec) = self.vecs.pop() {
            vec
        } else {
            Vec::with_capacity(self.size)
        }
    }
    /// Give a [`Vec`] of back for later reuse, clearing it first.
    pub fn give_back(&mut self,mut vec:Vec<T>) {
        vec.clear();
        self.vecs.push(vec);
    }
}