use std::cell::RefCell;
use std::rc::Rc;
use std::thread;
use lazy_static::lazy_static;
use std::path::PathBuf;
use crate::tex;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;

pub mod strings;
pub mod errors;
pub mod map;

/// A pointer type for use in TeX - this is just an alias for `Rc`, but may by replaced by `Arc` in the future.
pub type Ptr<A> = Rc<A>;
pub type Mut<A> = RefCell<A>;

lazy_static! {
    /// The current working directory.
    pub static ref PWD : PathBuf = std::env::current_dir().expect("No current directory!")
        .as_path().to_path_buf();
}

//pub const STACK_SIZE : usize = 16 * 1024 * 1024;
/// run a function in a new thread with a given stack size. Useful to increase the stack size.
pub fn with_stack_size<A : 'static,B : 'static>(size:usize,f : B) -> A  where B: FnOnce() -> A + Send, A : Send {
    let child = thread::Builder::new()
        .stack_size(size)
        .spawn(f)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap()
}

thread_local! {
    pub static BUMP : bumpalo::Bump = bumpalo::Bump::new().into();
}

pub struct MVec<T>(Vec<T>);//(bumpalo::collections::Vec<'a,T>);
impl<T> MVec<T> {
    pub fn new(bump:&bumpalo::Bump) -> Self {
        MVec(Vec::with_capacity(512))//bumpalo::collections::Vec::<'a>::new_in(bump))
    }
    pub fn ends_with(&self, needle: &[T]) -> bool
        where
            T: PartialEq {
        self.0.ends_with(needle)
    }
    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }
    pub fn push(&mut self, t:T) {
        self.0.push(t);
    }
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn remove(&mut self, index:usize) -> T {
        self.0.remove(index)
    }
    pub fn as_vec(&self) -> Vec<T> where T:Clone {
        self.0.iter().cloned().collect()
    }
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }
}