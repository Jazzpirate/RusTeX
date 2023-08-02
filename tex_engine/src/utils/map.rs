pub type HMap<A,B> = ahash::HashMap<A,B>;
pub type HSet<A> = ahash::HashSet<A>;

#[derive(Clone)]
pub struct Map<V>(Vec<(&'static str,V)>);
impl<V> Default for Map<V> {
    fn default() -> Self {
        Self(Vec::new())
    }
}
impl<V> Map<V> {
    pub fn index_of(&self, key:&'static str) -> Option<usize> {
        for (idx,(mk,_)) in self.0.iter().enumerate() {
            if *mk == key {
                return Some(idx);
            }
        }
        None
    }
    pub fn insert(&mut self, key:&'static str, value:V) -> usize {
        let ret = self.0.len();
        for (idx,(mk,mv)) in &mut self.0.iter_mut().enumerate() {
            if *mk == key {
                *mv = value;
                return idx;
            }
        }
        self.0.push((key, value));
        ret
    }
    pub fn get(&self, idx:usize) -> Option<&V> {
        self.0.get(idx).map(|(_,v)|v)
    }
    pub fn get_mut(&mut self, idx:usize) -> Option<&mut V> {
        self.0.get_mut(idx).map(|(_,v)|v)
    }
    pub fn get_from_name(&self, k:&'static str) -> Option<&V> {
        for (key,value) in &self.0 {
            if *key == k {
                return Some(value);
            }
        }
        None
    }
    pub fn get_from_name_mut(&mut self, k:&'static str) -> Option<&mut V> {
        for (key,value) in &mut self.0 {
            if *key == k {
                return Some(value);
            }
        }
        None
    }
}