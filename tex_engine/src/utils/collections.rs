use crate::engine::EngineType;
use crate::tex::token::Token;

pub type HMap<A,B> = ahash::HashMap<A,B>;
pub type HSet<A> = ahash::HashSet<A>;

/*
use std::alloc::{alloc,dealloc,Layout};
use std::ptr;

struct AllocationPool<A>{free:*mut LinkedNode<A>,layout:Layout}
impl<A> AllocationPool<A> {
    fn new() -> Self {
        AllocationPool{free:ptr::null_mut(),layout:Layout::new::<LinkedNode<A>>()}
    }
}
impl<A> Drop for AllocationPool<A> {
    fn drop(&mut self) {
        unsafe {
            let mut next = self.free;
            while !next.is_null() {
                let current = next;
                next = (*next).next;
                dealloc(current as *mut u8,self.layout);
            }
        }
    }
}

struct LinkedNode<A> {
    value: A,
    next: *mut LinkedNode<A>
}
struct LinkedArray<A> {
    head:*mut LinkedNode<A>,
    pools:Vec<AllocationPool<A>>
}
impl<A> LinkedArray<A> {
    fn new() -> Self {
        LinkedArray {head:ptr::null_mut(),pools:vec!(AllocationPool::new()) }
    }
    fn next(&mut self) -> Option<A> {
        if self.head.is_null() {
            None
        } else { unsafe {
            if self.pools.is_empty() { self.pools.push(AllocationPool::new()); }
            let mut pool = unsafe{ self.pools.last_mut().unwrap_unchecked()};
            let value = ptr::read(&(*self.head).value);
            let next = (*self.head).next;
            (*self.head).next = pool.free;
            pool.free = self.head;
            self.head = next;
            Some(value)
        } }
    }
}

impl<A> Drop for LinkedArray<A> {
    fn drop(&mut self) {
        unsafe {
            let mut next = self.head;
            let layout = Layout::new::<LinkedNode<A>>();
            while !next.is_null() {
                let current = next;
                next = (*next).next;
                ptr::drop_in_place(&mut (*current).value);
                dealloc(current as *mut u8,layout);
            }
        }
    }
}

struct LinkedArrayPrepender<A> {
    head:*mut LinkedNode<A>,
    last:*mut LinkedNode<A>,
    pool:AllocationPool<A>,
}
impl<A> LinkedArrayPrepender<A> {
    fn new(pool:AllocationPool<A>) -> Self {
        LinkedArrayPrepender {head:ptr::null_mut(),last:ptr::null_mut(),pool}
    }
    fn push(&mut self, a: A) {
        let node = if self.pool.free.is_null() {
            unsafe {alloc(self.pool.layout) as *mut LinkedNode<A>}
        } else {
            let n = self.pool.free;
            self.pool.free = unsafe{ (*n).next };
            n
        };
        unsafe {
            if self.head.is_null() {
                self.head = node;
                self.last = node;
            } else {
                (*self.last).next = node;
                self.last = node;
            }
            ptr::write(&mut (*node).value, a);
            (*node).next = ptr::null_mut();
        }
    }
    fn close(mut self,arr: &mut LinkedArray<A>) {
        if self.head.is_null() {return ()}
        unsafe {
            (*self.last).next = arr.head;
            arr.head = self.head;
            self.head = ptr::null_mut();
            self.last = ptr::null_mut();
            arr.pools.push(self.pool)
        }
    }
}

 */