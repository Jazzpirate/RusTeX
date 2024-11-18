/*! Memory manangement and string interning. */

use crate::tex::tokens::control_sequences::CSName;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::tokens::Token;

/// Utility struct for managing memory allocation and string interning. In particular, it
/// provides a [`CSHandler`](crate::tex::tokens::control_sequences::CSHandler) implementation for interning control
/// sequence names (if applicable).
#[derive(Clone)]
pub struct MemoryManager<T: Token> {
    cs_interner: <T::CS as CSName<T::Char>>::Handler,
    strings: Vec<String>,
    bytes: Vec<Vec<u8>>,
    token_vecs: Vec<Vec<T>>,
    empty: TokenList<T>,
}
impl<T: Token> Default for MemoryManager<T> {
    fn default() -> Self {
        MemoryManager {
            cs_interner: <T::CS as CSName<T::Char>>::Handler::default(),
            strings: Vec::new(),
            bytes: Vec::new(),
            token_vecs: Vec::new(),
            #[cfg(feature = "multithreaded")]
            empty: TokenList(shared_vector::AtomicSharedVector::new()),
            #[cfg(not(feature = "multithreaded"))]
            empty: TokenList(shared_vector::SharedVector::new()),
        }
    }
}
impl<T: Token> MemoryManager<T> {
    /// Provides an empty `Vec<u8>` that can be returned using [`return_bytes`](Self::return_bytes), avoiding
    /// the need to allocate a new `Vec<u8>` every time.
    pub fn get_bytes(&mut self) -> Vec<u8> {
        self.bytes.pop().unwrap_or_default()
    }
    /// Returns a `Vec<u8>` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_bytes(&mut self, mut b: Vec<u8>) {
        b.clear();
        self.bytes.push(b)
    }
    /// Provides an empty `String` that can be returned using [`return_string`](Self::return_string), avoiding
    /// the need to allocate a new `String` every time.
    pub fn get_string(&mut self) -> String {
        self.strings.pop().unwrap_or_default()
    }
    /// Returns a `String` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_string(&mut self, mut s: String) {
        s.clear();
        self.strings.push(s)
    }
    /// Provides an empty `Vec<T>` that can be returned using [`return_token_vec`](Self::return_token_vec), avoiding
    /// the need to allocate a new `Vec<T>` every time.
    pub fn get_token_vec(&mut self) -> Vec<T> {
        self.token_vecs.pop().unwrap_or_default()
    }
    /// Returns a `Vec<T>` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_token_vec(&mut self, mut v: Vec<T>) {
        v.clear();
        self.token_vecs.push(v)
    }
    /// Returns a new memory manager with the given control sequence interner.
    pub fn new_with_cs_interner(cs_interner: <T::CS as CSName<T::Char>>::Handler) -> Self {
        MemoryManager {
            cs_interner,
            strings: Vec::new(),
            bytes: Vec::new(),
            token_vecs: Vec::new(),
            #[cfg(feature = "multithreaded")]
            empty: TokenList(shared_vector::AtomicSharedVector::new()),
            #[cfg(not(feature = "multithreaded"))]
            empty: TokenList(shared_vector::SharedVector::new()),
        }
    }
    /// Sets the control sequence interner.
    pub fn set_cs_interner(&mut self, cs_interner: <T::CS as CSName<T::Char>>::Handler) {
        self.cs_interner = cs_interner;
    }

    /// Returns the control sequence interner mutably.
    pub fn cs_interner_mut(&mut self) -> &mut <T::CS as CSName<T::Char>>::Handler {
        &mut self.cs_interner
    }

    /// Returns the control sequence interner.
    pub fn cs_interner(&self) -> &<T::CS as CSName<T::Char>>::Handler {
        &self.cs_interner
    }

    /// Returns an empty token list.
    pub fn empty_list(&self) -> TokenList<T> {
        self.empty.clone()
    }
}
