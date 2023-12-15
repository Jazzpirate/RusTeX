use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use tex_engine::utils::Mut;

pub trait Poolable:Hash+PartialEq+Clone+'static {
    fn with_pool<F,R>(f:F) -> R where F:FnMut(&mut StringPool<Self>) -> R;
}

#[derive(Clone,Copy)]
pub struct PRef<A: Poolable>(usize, PhantomData<A>);

impl<A: Poolable +Debug> Debug for PRef<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(|v| write!(f,"{:?}",v))
    }
}
impl<A: Poolable +Display> Display for PRef<A> {
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
        self.with(|v| other.with(|w| v.partial_cmp(w)))
    }
}
impl<A: Poolable +Ord+Eq> Ord for PRef<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.with(|v| other.with(|w| v.cmp(w)))
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

struct StringPool<K: Poolable> {
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
    pub pub const fn new() -> Self {
        Self { table: hashbrown::raw::RawTable::new(),values:Vec::new() }
    }

    pub fn get(&mut self,k:&K) -> PRef<K> {
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
thread_local! {
    static u8vecpool: Mut<StringPool<Vec<u8>>> = Mut::new(StringPool::new());
}

impl Poolable for Vec<u8> {
    fn with_pool<F, R>(mut f: F) -> R where F: FnMut(&mut StringPool<Self>) -> R {
        u8vecpool.with(|p| f(&mut *p.borrow_mut()))
    }
    //fn MAP() -> &'static TestPool<Self> { &*TESTMAP.borrow() }
}
impl From<&str> for PRef<Vec<u8>> {
    fn from(s:&str) -> Self {
        Vec::with_pool(|p| p.get(&s.as_bytes().iter().map(|c|*c).collect()))
    }
}

pub fn test() {
    let astr = "aaaaah";
    let bstr = "bbbbbb";
    let a : PRef<Vec<u8>> = astr.into();
    let b : PRef<Vec<u8>> = bstr.into();
    let c = a.copy_value();
    let d = b.copy_value();
    assert_eq!(a.copy_value().as_slice(),astr.as_bytes());
    assert_eq!(b.copy_value().as_slice(),bstr.as_bytes());
    let mut str = String::new();
    str.push(0 as char);
}