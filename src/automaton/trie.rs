//! Trie-based bulk construction for finite automata.
//!
//! This module provides an optimized approach to building automata from many string values.
//! Instead of creating individual FAs and merging them (O(n²) even with hierarchical merge),
//! we build a trie first and convert it to a SmallTable in a single pass (O(n)).
//!
//! Based on the approach from https://github.com/DigitalPath-Inc/quamina
//!
//! ## Key optimizations:
//! - Shared prefixes are naturally deduplicated by the trie structure
//! - Hash-based deduplication of end states avoids creating duplicate FaStates
//! - Single-pass conversion avoids the unpack/repack overhead of merge_fas

use std::collections::HashMap;
use std::sync::Arc;

use super::small_table::{FaState, FieldMatcher, SmallTable, VALUE_TERMINATOR};

/// A node in the value trie.
///
/// Each node has byte-indexed children and may mark the end of one or more values.
#[derive(Default)]
pub struct TrieNode {
    /// Children indexed by byte value
    children: HashMap<u8, Box<TrieNode>>,
    /// If this is an end node, the field matchers to transition to
    /// Multiple values can share the same end state
    field_transitions: Vec<Arc<FieldMatcher>>,
    /// Cached hash for deduplication
    hash: u64,
}

impl TrieNode {
    /// Create a new empty trie node.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a string value into the trie.
    ///
    /// # Arguments
    /// * `value` - The byte sequence to insert
    /// * `next_field` - The field matcher to transition to when this value matches
    pub fn insert(&mut self, value: &[u8], next_field: Arc<FieldMatcher>) {
        let mut node = self;

        // Walk/create path for each byte
        for &byte in value {
            node = node
                .children
                .entry(byte)
                .or_insert_with(|| Box::new(TrieNode::new()));
        }

        // Mark end state - add transition on VALUE_TERMINATOR
        let end_node = node
            .children
            .entry(VALUE_TERMINATOR)
            .or_insert_with(|| Box::new(TrieNode::new()));

        // Add field transition (may have multiple patterns ending here)
        end_node.field_transitions.push(next_field);
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

    /// Generate a hash for this node (for deduplication).
    ///
    /// Uses DJB2 hash algorithm, recursively hashing children and field transitions.
    fn generate_hash(&mut self) -> u64 {
        if self.hash != 0 {
            return self.hash;
        }

        let mut hash: u64 = 5381; // DJB2 initial value

        // Hash children in sorted order for consistency
        let mut child_keys: Vec<u8> = self.children.keys().copied().collect();
        child_keys.sort_unstable();

        for key in child_keys {
            let child = self.children.get_mut(&key).unwrap();
            let child_hash = child.generate_hash();
            hash = hash.wrapping_mul(33).wrapping_add(key as u64);
            hash = hash.wrapping_mul(33).wrapping_add(child_hash);
        }

        // Hash field transitions by their pointer addresses
        for fm in &self.field_transitions {
            hash = hash.wrapping_mul(33).wrapping_add(Arc::as_ptr(fm) as u64);
        }

        self.hash = hash;
        hash
    }

    /// Convert the trie to a SmallTable in a single pass.
    ///
    /// Uses hash-based deduplication to reuse identical FaStates.
    pub fn to_small_table(&mut self) -> SmallTable {
        // Generate hashes for deduplication
        self.generate_hash();

        // Cache for deduplicated states
        let mut state_cache: HashMap<u64, Arc<FaState>> = HashMap::new();

        self.build_small_table(&mut state_cache)
    }

    /// Build SmallTable recursively with state caching.
    fn build_small_table(&self, cache: &mut HashMap<u64, Arc<FaState>>) -> SmallTable {
        if self.children.is_empty() {
            // Leaf node - create final state with field transitions
            return SmallTable::new();
        }

        // Collect children sorted by byte
        let mut child_bytes: Vec<u8> = self.children.keys().copied().collect();
        child_bytes.sort_unstable();

        // Build states for each child
        let mut indices = Vec::with_capacity(child_bytes.len());
        let mut steps = Vec::with_capacity(child_bytes.len());

        for byte in child_bytes {
            let child = self.children.get(&byte).unwrap();

            // Check cache first
            let state = if child.hash != 0 {
                if let Some(cached) = cache.get(&child.hash) {
                    cached.clone()
                } else {
                    let state = Arc::new(self.build_child_state(child, cache));
                    cache.insert(child.hash, state.clone());
                    state
                }
            } else {
                Arc::new(self.build_child_state(child, cache))
            };

            indices.push(byte);
            steps.push(state);
        }

        SmallTable::with_mappings(None, &indices, &steps)
    }

    /// Build an FaState for a child node.
    fn build_child_state(
        &self,
        child: &TrieNode,
        cache: &mut HashMap<u64, Arc<FaState>>,
    ) -> FaState {
        let child_table = child.build_small_table(cache);

        FaState {
            table: child_table,
            field_transitions: child.field_transitions.clone(),
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
        self.root.children.is_empty()
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
