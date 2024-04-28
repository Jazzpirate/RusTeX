use std::fmt::Debug;

#[derive(Clone)]
pub struct VecMap<K, V> {
    pub inner: Vec<(K, V)>,
}
impl<K: Debug, V: Debug> Debug for VecMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.inner.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}
impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self { inner: Vec::new() }
    }
}
impl<K: PartialEq, V> VecMap<K, V> {
    pub fn get<E:?Sized>(&self, key: &E) -> Option<&V> where for <'a> &'a E: PartialEq<&'a K> {
        self.inner.iter().find(|(k, _)| key == k).map(|(_, v)| v)
    }
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner
            .iter_mut()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v)
    }
    pub fn insert(&mut self, key: K, value: V) {
        match self.inner.iter_mut().find(|(k, _)| k == &key) {
            Some((_, v)) => *v = value,
            None => self.inner.push((key, value)),
        };
    }
    #[inline]
    pub fn is_empty(&self) -> bool { self.inner.is_empty() }
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner.iter().map(|(k, v)| (k, v))
    }
    pub fn contains_key<E:?Sized>(&self, key: &E) -> bool  where for <'a> &'a E: PartialEq<&'a K>{
        self.inner.iter().any(|(k, _)| key == k)
    }
}
impl<K,V> IntoIterator for VecMap<K,V> {
    type Item = (K,V);
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}