use std::rc::Rc;
use std::thread;
use lazy_static::lazy_static;
use std::path::PathBuf;

pub mod strings;
pub mod errors;
pub mod map;

/// A pointer type for use in TeX - this is just an alias for `Rc`, but may by replaced by `Arc` in the future.
pub type Ptr<A> = Rc<A>;

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