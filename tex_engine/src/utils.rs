use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
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

/*
pub trait Poolable:Hash+PartialEq+Clone+'static {
    fn with_pool<F,R>(f:F) -> R where F:FnMut(&mut StringPool<Self>) -> R;
}

#[derive(Clone,Copy,Hash)]
pub struct PRef<A: Poolable>(usize, PhantomData<A>);

impl<A: Poolable + Debug> Debug for PRef<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(|v| write!(f,"{:?}",v))
    }
}
impl<A: Poolable + Display> Display for PRef<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(|v| write!(f,"{}",v))
    }
}
impl<A: Poolable> PartialEq for PRef<A> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<A: Poolable +Eq> Eq for PRef<A> {}

impl<A: Poolable +PartialOrd+Eq+PartialEq> PartialOrd for PRef<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        A::with_pool(|p| p.get_value(self.0).partial_cmp(p.get_value(other.0)))
    }
}
impl<A: Poolable +Ord+Eq> Ord for PRef<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        A::with_pool(|p| p.get_value(self.0).cmp(p.get_value(other.0)))
    }
}

impl<K: Poolable> PRef<K> {
    fn with<F,R>(&self,mut f:F) -> R where F:FnMut(&K) -> R {
        K::with_pool(|p| f(p.get_value(self.0)))
    }
    fn copy_value(&self) -> K {
        self.with(|v| v.clone())
    }
}

pub struct StringPool<K: Poolable> {
    table:hashbrown::raw::RawTable<(K,usize)>,
    values:Vec<K>
}
impl<K: Poolable> StringPool<K> {
    fn get_value(&self,i:usize) -> &K {
        &self.values[i]
    }
    pub fn len(&self) -> usize {
        self.table.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn clear(&mut self) {
        self.table.clear();
    }
    pub const fn new() -> Self {
        Self { table: hashbrown::raw::RawTable::new(),values:Vec::new() }
    }

    pub fn get(&mut self,k:&K) -> PRef<K> {
        use std::hash::Hasher;
        let mut hasher = ahash::AHasher::default();
        let hash = {k.hash(&mut hasher);hasher.finish()};
        let i = match self.table.find_or_find_insert_slot(hash,|p| p.0 == *k,|p| {
            let mut mhash = ahash::AHasher::default();
            p.0.hash(&mut mhash);
            mhash.finish()
        }) {
            Ok(bucket) => unsafe{bucket.as_ref().1},
            Err(slot) => {
                let ret = self.values.len();
                self.values.push(k.clone());
                unsafe{self.table.insert_in_slot(hash,slot,(k.clone(),ret));}
                ret
            }
        };
        PRef(i, PhantomData)
    }
}
/*
thread_local! {
    pub(crate) static u8vecpool: Mut<StringPool<Vec<u8>>> = Mut::new(StringPool::new());
    pub(crate) static charvecpool: Mut<StringPool<Vec<char>>> = Mut::new(StringPool::new());
}
*/
*/