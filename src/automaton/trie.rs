//! Trie-based bulk construction for finite automata.
//!
//! This module provides an optimized approach to building automata from many string values.
//! Instead of creating individual FAs and merging them (O(n²) even with hierarchical merge),
//! we build a trie first and convert it to a SmallTable in a single pass (O(n)).
//!
//! Based on the approach from https://github.com/DigitalPath-Inc/quamina
//!
//! ## Key optimizations:
//! - Arena allocation: all trie nodes stored in a Vec, referenced by index
//! - Shared prefixes are naturally deduplicated by the trie structure
//! - Hash-based deduplication of end states avoids creating duplicate FaStates
//! - Single-pass conversion avoids the unpack/repack overhead of merge_fas

use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use std::sync::Arc;

use super::small_table::{FaState, FieldMatcher, SmallTable, VALUE_TERMINATOR};

/// Index into the trie arena.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
struct TrieIdx(u32);

impl TrieIdx {
    fn get(self) -> usize {
        self.0 as usize
    }
}

/// A node in the value trie, stored in an arena.
///
/// Uses SmallVec for children since most nodes have few children.
#[derive(Default)]
struct ArenaTrieNode {
    /// Children as (byte, index) pairs, kept sorted by byte for consistent hashing
    children: SmallVec<[(u8, TrieIdx); 4]>,
    /// If this is an end node, the field matchers to transition to
    field_transitions: SmallVec<[Arc<FieldMatcher>; 1]>,
    /// Cached hash for deduplication
    hash: u64,
}

/// Arena-based trie for bulk string construction.
///
/// All nodes are stored in a contiguous Vec, reducing heap allocations.
pub struct TrieNode {
    nodes: Vec<ArenaTrieNode>,
    root: TrieIdx,
}

impl Default for TrieNode {
    fn default() -> Self {
        Self::new()
    }
}

impl TrieNode {
    /// Create a new empty trie node.
    pub fn new() -> Self {
        let mut nodes = Vec::with_capacity(64);
        nodes.push(ArenaTrieNode::default());
        Self {
            nodes,
            root: TrieIdx(0),
        }
    }

    /// Allocate a new node in the arena.
    fn alloc(&mut self) -> TrieIdx {
        let idx = self.nodes.len();
        self.nodes.push(ArenaTrieNode::default());
        TrieIdx(idx as u32)
    }

    /// Find or create a child for the given byte.
    fn get_or_create_child(&mut self, parent: TrieIdx, byte: u8) -> TrieIdx {
        // Binary search for the byte
        let children = &self.nodes[parent.get()].children;
        match children.binary_search_by_key(&byte, |&(b, _)| b) {
            Ok(pos) => children[pos].1,
            Err(pos) => {
                let child = self.alloc();
                self.nodes[parent.get()].children.insert(pos, (byte, child));
                child
            }
        }
    }

    /// Insert a string value into the trie.
    ///
    /// # Arguments
    /// * `value` - The byte sequence to insert
    /// * `next_field` - The field matcher to transition to when this value matches
    pub fn insert(&mut self, value: &[u8], next_field: Arc<FieldMatcher>) {
        let mut node = self.root;

        // Walk/create path for each byte
        for &byte in value {
            node = self.get_or_create_child(node, byte);
        }

        // Mark end state - add transition on VALUE_TERMINATOR
        let end_node = self.get_or_create_child(node, VALUE_TERMINATOR);

        // Add field transition (may have multiple patterns ending here)
        self.nodes[end_node.get()].field_transitions.push(next_field);
    }

    /// Insert multiple string values that share the same field matcher.
    ///
    /// More efficient than calling insert() multiple times because it
    /// creates a single shared Arc<FieldMatcher> for all values.
    pub fn insert_all(&mut self, values: &[&[u8]], next_field: Arc<FieldMatcher>) {
        for value in values {
            self.insert(value, next_field.clone());
        }
    }

    /// Generate hashes for all nodes (for deduplication).
    ///
    /// Uses DJB2 hash algorithm iteratively using post-order traversal.
    fn generate_all_hashes(&mut self) {
        // Post-order traversal: process children before parents
        // Stack holds (node_idx, next_child_to_process)
        let mut stack: Vec<(TrieIdx, usize)> = vec![(self.root, 0)];

        while let Some((idx, child_pos)) = stack.pop() {
            let node = &self.nodes[idx.get()];

            // Already computed?
            if node.hash != 0 {
                continue;
            }

            // Check if all children have been processed
            if child_pos < node.children.len() {
                // Push self back with incremented position
                stack.push((idx, child_pos + 1));
                // Push child to be processed first
                let child_idx = node.children[child_pos].1;
                stack.push((child_idx, 0));
            } else {
                // All children processed, compute this node's hash
                let mut hash: u64 = 5381; // DJB2 initial value

                // Children are already sorted, hash them in order
                for &(byte, child_idx) in &self.nodes[idx.get()].children {
                    let child_hash = self.nodes[child_idx.get()].hash;
                    hash = hash.wrapping_mul(33).wrapping_add(byte as u64);
                    hash = hash.wrapping_mul(33).wrapping_add(child_hash);
                }

                // Hash field transitions by their pointer addresses
                for fm in &self.nodes[idx.get()].field_transitions {
                    hash = hash.wrapping_mul(33).wrapping_add(Arc::as_ptr(fm) as u64);
                }

                self.nodes[idx.get()].hash = hash;
            }
        }
    }

    /// Convert the trie to a SmallTable in a single pass.
    ///
    /// Uses hash-based deduplication to reuse identical FaStates.
    pub fn to_small_table(&mut self) -> SmallTable {
        // Generate hashes for deduplication
        self.generate_all_hashes();

        // Cache for deduplicated states
        let mut state_cache: FxHashMap<u64, Arc<FaState>> = FxHashMap::default();

        self.build_small_table(self.root, &mut state_cache)
    }

    /// Build SmallTable recursively with state caching.
    fn build_small_table(
        &self,
        idx: TrieIdx,
        cache: &mut FxHashMap<u64, Arc<FaState>>,
    ) -> SmallTable {
        let node = &self.nodes[idx.get()];
        if node.children.is_empty() {
            // Leaf node - create final state with field transitions
            return SmallTable::new();
        }

        // Build states for each child (children are already sorted)
        let mut indices = Vec::with_capacity(node.children.len());
        let mut steps = Vec::with_capacity(node.children.len());

        for &(byte, child_idx) in &node.children {
            let child = &self.nodes[child_idx.get()];

            // Check cache first
            let state = if child.hash != 0 {
                if let Some(cached) = cache.get(&child.hash) {
                    cached.clone()
                } else {
                    let state = Arc::new(self.build_child_state(child_idx, cache));
                    cache.insert(child.hash, state.clone());
                    state
                }
            } else {
                Arc::new(self.build_child_state(child_idx, cache))
            };

            indices.push(byte);
            steps.push(state);
        }

        SmallTable::with_mappings(None, &indices, &steps)
    }

    /// Build an FaState for a child node.
    fn build_child_state(
        &self,
        idx: TrieIdx,
        cache: &mut FxHashMap<u64, Arc<FaState>>,
    ) -> FaState {
        let child_table = self.build_small_table(idx, cache);
        let node = &self.nodes[idx.get()];

        FaState {
            table: child_table,
            field_transitions: node.field_transitions.to_vec(),
        }
    }
}

/// A trie for building value matchers from multiple string values.
///
/// This is a higher-level API that wraps TrieNode and provides
/// a convenient interface for the common case of building a value matcher
/// from multiple exact string values.
pub struct ValueTrie {
    root: TrieNode,
}

impl ValueTrie {
    /// Create a new empty value trie.
    pub fn new() -> Self {
        Self {
            root: TrieNode::new(),
        }
    }

    /// Insert a string value.
    pub fn insert(&mut self, value: &[u8], next_field: Arc<FieldMatcher>) {
        self.root.insert(value, next_field);
    }

    /// Insert multiple values sharing the same field matcher.
    pub fn insert_all(&mut self, values: &[&[u8]], next_field: Arc<FieldMatcher>) {
        self.root.insert_all(values, next_field);
    }

    /// Convert to a SmallTable.
    pub fn to_small_table(&mut self) -> SmallTable {
        self.root.to_small_table()
    }

    /// Check if the trie is empty.
    pub fn is_empty(&self) -> bool {
        self.root.nodes[0].children.is_empty()
    }
}

impl Default for ValueTrie {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_value() {
        let mut trie = ValueTrie::new();
        let fm = Arc::new(FieldMatcher::new());
        trie.insert(b"hello", fm.clone());

        let table = trie.to_small_table();

        // Should have transition on 'h'
        let (step, _) = table.step(b'h');
        assert!(step.is_some());
    }

    #[test]
    fn test_shared_prefix() {
        let mut trie = ValueTrie::new();
        let fm1 = Arc::new(FieldMatcher::new());
        let fm2 = Arc::new(FieldMatcher::new());

        trie.insert(b"hello", fm1);
        trie.insert(b"help", fm2);

        let table = trie.to_small_table();

        // Both share 'hel' prefix
        let (step, _) = table.step(b'h');
        assert!(step.is_some());

        let step = step.unwrap();
        let (step, _) = step.table.step(b'e');
        assert!(step.is_some());

        let step = step.unwrap();
        let (step, _) = step.table.step(b'l');
        assert!(step.is_some());

        // Then they diverge
        let step = step.unwrap();
        let (step_l, _) = step.table.step(b'l');
        let (step_p, _) = step.table.step(b'p');
        assert!(step_l.is_some());
        assert!(step_p.is_some());
    }

    #[test]
    fn test_bulk_insert() {
        let mut trie = ValueTrie::new();
        let fm = Arc::new(FieldMatcher::new());

        let values: Vec<&[u8]> = vec![b"apple", b"banana", b"cherry"];
        trie.insert_all(&values, fm);

        let table = trie.to_small_table();

        // Should have transitions for 'a', 'b', 'c'
        let (step_a, _) = table.step(b'a');
        let (step_b, _) = table.step(b'b');
        let (step_c, _) = table.step(b'c');

        assert!(step_a.is_some());
        assert!(step_b.is_some());
        assert!(step_c.is_some());
    }

    #[test]
    fn test_empty_trie() {
        let mut trie = ValueTrie::new();
        assert!(trie.is_empty());

        let table = trie.to_small_table();
        // Empty table should just have ceiling entry
        assert_eq!(table.ceilings.len(), 1);
    }

    #[test]
    fn test_same_value_different_matchers() {
        let mut trie = ValueTrie::new();
        let fm1 = Arc::new(FieldMatcher::new());
        let fm2 = Arc::new(FieldMatcher::new());

        trie.insert(b"test", fm1);
        trie.insert(b"test", fm2);

        let table = trie.to_small_table();

        // Walk to the end
        let (step, _) = table.step(b't');
        let step = step.unwrap();
        let (step, _) = step.table.step(b'e');
        let step = step.unwrap();
        let (step, _) = step.table.step(b's');
        let step = step.unwrap();
        let (step, _) = step.table.step(b't');
        let step = step.unwrap();
        let (step, _) = step.table.step(VALUE_TERMINATOR);
        let step = step.unwrap();

        // Should have both field transitions
        assert_eq!(step.field_transitions.len(), 2);
    }
}
