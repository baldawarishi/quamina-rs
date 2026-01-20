//! Sparse set data structure for O(1) clear operations.
//!
//! Based on: https://research.swtch.com/sparse
//! Adapted from regex-automata/src/util/sparse_set.rs
//!
//! Properties:
//! - Preserves insertion order
//! - O(1) membership testing
//! - O(1) insertion
//! - O(1) clearing (just reset length counter)
//!
//! Tradeoff: Requires knowing capacity upfront and uses 2*capacity memory.

/// A sparse set for storing indices up to a known capacity.
///
/// This is useful when you need to frequently clear and reuse a set,
/// as clearing is O(1) (just reset the length counter).
#[derive(Clone, Debug)]
pub struct SparseSet {
    /// Number of elements currently in the set.
    len: usize,
    /// Dense array containing the IDs in insertion order.
    dense: Vec<usize>,
    /// Sparse array mapping ID -> position in dense.
    /// An ID is in the set iff sparse[id] < len && dense[sparse[id]] == id.
    sparse: Vec<usize>,
}

impl SparseSet {
    /// Create a new sparse set with the given capacity.
    ///
    /// Elements must be in range [0, capacity).
    #[inline]
    pub fn new(capacity: usize) -> Self {
        SparseSet {
            len: 0,
            dense: vec![0; capacity],
            sparse: vec![0; capacity],
        }
    }

    /// Resize the set to a new capacity.
    ///
    /// The set is automatically cleared.
    #[inline]
    pub fn resize(&mut self, new_capacity: usize) {
        self.clear();
        self.dense.resize(new_capacity, 0);
        self.sparse.resize(new_capacity, 0);
    }

    /// Returns the capacity of this set.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.dense.len()
    }

    /// Returns the number of elements in this set.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the set is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Insert an ID into the set. Returns true if the ID was not already present.
    ///
    /// Panics if id >= capacity.
    #[inline]
    pub fn insert(&mut self, id: usize) -> bool {
        if self.contains(id) {
            return false;
        }
        debug_assert!(
            self.len < self.capacity(),
            "SparseSet overflow: len={}, capacity={}",
            self.len,
            self.capacity()
        );
        self.dense[self.len] = id;
        self.sparse[id] = self.len;
        self.len += 1;
        true
    }

    /// Returns true if the set contains the given ID.
    ///
    /// Panics if id >= capacity.
    #[inline]
    pub fn contains(&self, id: usize) -> bool {
        let idx = self.sparse[id];
        idx < self.len && self.dense[idx] == id
    }

    /// Clear the set in O(1) time.
    #[inline]
    pub fn clear(&mut self) {
        self.len = 0;
    }

    /// Iterate over elements in insertion order.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.dense[..self.len].iter().copied()
    }

    /// Returns the memory usage in bytes.
    #[inline]
    pub fn memory_usage(&self) -> usize {
        self.dense.len() * std::mem::size_of::<usize>()
            + self.sparse.len() * std::mem::size_of::<usize>()
    }
}

/// A pair of sparse sets for NFA traversal.
///
/// Useful for epsilon closure computation where you alternate between
/// current states and next states.
#[derive(Clone, Debug)]
pub struct SparseSets {
    pub set1: SparseSet,
    pub set2: SparseSet,
}

impl SparseSets {
    /// Create a new pair of sparse sets with the given capacity.
    #[inline]
    pub fn new(capacity: usize) -> Self {
        SparseSets {
            set1: SparseSet::new(capacity),
            set2: SparseSet::new(capacity),
        }
    }

    /// Resize both sets to the new capacity.
    #[inline]
    pub fn resize(&mut self, new_capacity: usize) {
        self.set1.resize(new_capacity);
        self.set2.resize(new_capacity);
    }

    /// Clear both sets.
    #[inline]
    pub fn clear(&mut self) {
        self.set1.clear();
        self.set2.clear();
    }

    /// Swap set1 and set2.
    #[inline]
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.set1, &mut self.set2);
    }

    /// Returns the total memory usage in bytes.
    #[inline]
    pub fn memory_usage(&self) -> usize {
        self.set1.memory_usage() + self.set2.memory_usage()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sparse_set_basic() {
        let mut set = SparseSet::new(10);
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
        assert_eq!(set.capacity(), 10);

        assert!(set.insert(3));
        assert!(set.insert(7));
        assert!(set.insert(1));

        assert_eq!(set.len(), 3);
        assert!(set.contains(3));
        assert!(set.contains(7));
        assert!(set.contains(1));
        assert!(!set.contains(0));
        assert!(!set.contains(5));

        // Duplicate insert returns false
        assert!(!set.insert(3));
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn test_sparse_set_insertion_order() {
        let mut set = SparseSet::new(10);
        set.insert(5);
        set.insert(2);
        set.insert(8);
        set.insert(1);

        let items: Vec<_> = set.iter().collect();
        assert_eq!(items, vec![5, 2, 8, 1]);
    }

    #[test]
    fn test_sparse_set_clear() {
        let mut set = SparseSet::new(10);
        set.insert(1);
        set.insert(2);
        set.insert(3);
        assert_eq!(set.len(), 3);

        set.clear();
        assert!(set.is_empty());
        assert!(!set.contains(1));
        assert!(!set.contains(2));
        assert!(!set.contains(3));

        // Can reuse after clear
        set.insert(5);
        assert_eq!(set.len(), 1);
        assert!(set.contains(5));
    }

    #[test]
    fn test_sparse_sets_swap() {
        let mut sets = SparseSets::new(10);
        sets.set1.insert(1);
        sets.set1.insert(2);
        sets.set2.insert(3);

        assert_eq!(sets.set1.len(), 2);
        assert_eq!(sets.set2.len(), 1);

        sets.swap();

        assert_eq!(sets.set1.len(), 1);
        assert_eq!(sets.set2.len(), 2);
        assert!(sets.set1.contains(3));
        assert!(sets.set2.contains(1));
    }

    #[test]
    fn test_sparse_set_resize() {
        let mut set = SparseSet::new(5);
        set.insert(1);
        set.insert(2);

        set.resize(20);
        assert!(set.is_empty()); // resize clears
        assert_eq!(set.capacity(), 20);

        set.insert(15); // can now insert larger IDs
        assert!(set.contains(15));
    }
}
