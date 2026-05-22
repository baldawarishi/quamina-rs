//! Sparse set data structure for O(1) clear operations.
//!
//! Based on: <https://research.swtch.com/sparse>
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

impl Default for SparseSet {
    fn default() -> Self {
        Self::new(0)
    }
}

impl SparseSet {
    /// Create a new sparse set with the given capacity.
    ///
    /// Elements must be in range [0, capacity).
    #[inline]
    pub fn new(capacity: usize) -> Self {
        Self {
            len: 0,
            dense: vec![0; capacity],
            sparse: vec![0; capacity],
        }
    }

    /// Returns the capacity of this set.
    #[inline]
    pub const fn capacity(&self) -> usize {
        self.dense.len()
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
        let prev_len = self.len;
        self.len += 1;
        debug_assert!(self.len > prev_len, "SparseSet::insert must advance len");
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
    pub const fn clear(&mut self) {
        self.len = 0;
    }
}

// Test-only helpers: len, is_empty, iter, resize are not used in production.
#[cfg(test)]
impl SparseSet {
    fn len(&self) -> usize {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.dense[..self.len].iter().copied()
    }

    fn resize(&mut self, new_capacity: usize) {
        self.clear();
        self.dense.resize(new_capacity, 0);
        self.sparse.resize(new_capacity, 0);
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
