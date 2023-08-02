use std::rc::Rc;
use std::thread;
use lazy_static::lazy_static;
use std::path::PathBuf;
use crate::tex::token::Token;

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

pub struct Pool<T:Token>{
    args:(Vec<T>,Vec<T>,Vec<T>,Vec<T>,Vec<T>,Vec<T>,Vec<T>,Vec<T>,Vec<T>)
}
impl<T:Token> Pool<T> {
    pub fn new() -> Self { Self {
        args: (Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
               Vec::with_capacity(512),
        )
    }}
    pub fn clear_args(&mut self) {
        self.args.0.clear();
        self.args.1.clear();
        self.args.2.clear();
        self.args.3.clear();
        self.args.4.clear();
        self.args.5.clear();
        self.args.6.clear();
        self.args.7.clear();
        self.args.8.clear();
    }
    pub fn iter_arg(&self,i:usize) -> std::slice::Iter<T> {
        match i {
            0 => self.args.0.iter(),
            1 => self.args.1.iter(),
            2 => self.args.2.iter(),
            3 => self.args.3.iter(),
            4 => self.args.4.iter(),
            5 => self.args.5.iter(),
            6 => self.args.6.iter(),
            7 => self.args.7.iter(),
            8 => self.args.8.iter(),
            _ => panic!("Invalid argument index {}",i)
        }
    }
    pub fn arg(&mut self,i:usize) -> &mut Vec<T> {
        match i {
            0 => &mut self.args.0,
            1 => &mut self.args.1,
            2 => &mut self.args.2,
            3 => &mut self.args.3,
            4 => &mut self.args.4,
            5 => &mut self.args.5,
            6 => &mut self.args.6,
            7 => &mut self.args.7,
            8 => &mut self.args.8,
            _ => panic!("Invalid argument index {}",i)
        }
    }
}
impl<T:Token> Clone for Pool<T> {
    fn clone(&self) -> Self { Self::new() }
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