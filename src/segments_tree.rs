//! Segments tree for tracking which field paths appear in patterns.
//!
//! This enables the JSON flattener to skip fields that aren't used in any pattern,
//! providing a significant performance optimization.

use std::collections::HashMap;
use std::sync::Arc;

/// Separator used between path segments (e.g., "context\nuser\nid")
pub const SEGMENT_SEPARATOR: char = '\n';

/// A tree structure tracking which field paths appear in patterns.
///
/// Consider this JSON: `{"a": {"b": 1, "c": 2}}`
///
/// With pattern `{"a": {"b": [1]}}`, the tree looks like:
/// ```text
/// [root]
///    |
///  ["a"] -> node
///    |-> fields: ["b"]
/// ```
///
/// This allows the flattener to:
/// - Skip "c" since it's not used in any pattern
/// - Track how many fields/nodes remain to be processed
/// - Early-terminate when all needed fields are found
#[derive(Clone, Debug)]
pub struct SegmentsTree {
    /// Whether this is the root node
    is_root: bool,
    /// Child nodes (for nested objects)
    /// Maps segment name -> child tree
    nodes: HashMap<String, SegmentsTree>,
    /// Leaf fields at this level
    /// Maps segment name -> full path bytes (Arc for O(1) cloning)
    fields: HashMap<String, Arc<[u8]>>,
}

impl Default for SegmentsTree {
    fn default() -> Self {
        Self::new()
    }
}

impl SegmentsTree {
    /// Create a new root segments tree
    pub fn new() -> Self {
        Self {
            is_root: true,
            nodes: HashMap::new(),
            fields: HashMap::new(),
        }
    }

    /// Create a new non-root node
    fn new_node() -> Self {
        Self {
            is_root: false,
            nodes: HashMap::new(),
            fields: HashMap::new(),
        }
    }

    /// Add a path to the tree.
    /// Path uses SEGMENT_SEPARATOR between segments (e.g., "context\nuser\nid")
    pub fn add(&mut self, path: &str) {
        let segments: Vec<&str> = path.split(SEGMENT_SEPARATOR).collect();

        if segments.len() == 1 {
            // Direct field on this node
            self.fields
                .insert(path.to_string(), Arc::from(path.as_bytes()));
            return;
        }

        // Traverse/create nodes for all but last segment
        let mut node = self;
        for (i, segment) in segments.iter().enumerate() {
            if i == segments.len() - 1 {
                // Last segment is a field
                node.fields
                    .insert((*segment).to_string(), Arc::from(path.as_bytes()));
            } else {
                // Create node if it doesn't exist
                node = node
                    .nodes
                    .entry((*segment).to_string())
                    .or_insert_with(SegmentsTree::new_node);
            }
        }
    }

    /// Check if this is the root node
    #[inline]
    pub fn is_root(&self) -> bool {
        self.is_root
    }

    /// Check if a segment is used (either as a field or node)
    #[inline]
    pub fn is_segment_used(&self, segment: &[u8]) -> bool {
        // Fast path: check as bytes first if possible
        if let Ok(s) = std::str::from_utf8(segment) {
            self.fields.contains_key(s) || self.nodes.contains_key(s)
        } else {
            false
        }
    }

    /// Get a child node for a segment
    #[inline]
    pub fn get(&self, segment: &[u8]) -> Option<&SegmentsTree> {
        std::str::from_utf8(segment)
            .ok()
            .and_then(|s| self.nodes.get(s))
    }

    /// Get the full path for a leaf segment as a slice reference
    #[inline]
    pub fn path_for_segment(&self, segment: &[u8]) -> Option<&[u8]> {
        std::str::from_utf8(segment)
            .ok()
            .and_then(|s| self.fields.get(s))
            .map(|v| v.as_ref())
    }

    /// Get the full path for a leaf segment as an Arc (O(1) clone)
    #[inline]
    pub fn path_arc_for_segment(&self, segment: &[u8]) -> Option<Arc<[u8]>> {
        std::str::from_utf8(segment)
            .ok()
            .and_then(|s| self.fields.get(s))
            .cloned()
    }

    /// Number of child nodes (non-leaf)
    #[inline]
    pub fn nodes_count(&self) -> usize {
        self.nodes.len()
    }

    /// Number of leaf fields
    #[inline]
    pub fn fields_count(&self) -> usize {
        self.fields.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_field() {
        let mut tree = SegmentsTree::new();
        tree.add("status");

        assert!(tree.is_segment_used(b"status"));
        assert!(!tree.is_segment_used(b"other"));
        assert_eq!(tree.fields_count(), 1);
        assert_eq!(tree.nodes_count(), 0);
    }

    #[test]
    fn test_nested_path() {
        let mut tree = SegmentsTree::new();
        tree.add("context\nuser\nid");

        // "context" is a node
        assert!(tree.is_segment_used(b"context"));
        assert_eq!(tree.nodes_count(), 1);
        assert_eq!(tree.fields_count(), 0);

        // Get the "context" node
        let context = tree.get(b"context").unwrap();
        assert!(context.is_segment_used(b"user"));
        assert_eq!(context.nodes_count(), 1);
        assert_eq!(context.fields_count(), 0);

        // Get the "user" node
        let user = context.get(b"user").unwrap();
        assert!(user.is_segment_used(b"id"));
        assert_eq!(user.fields_count(), 1);
        assert_eq!(user.nodes_count(), 0);

        // Check path_for_segment returns the full path
        assert_eq!(
            user.path_for_segment(b"id"),
            Some(b"context\nuser\nid".as_slice())
        );
    }

    #[test]
    fn test_multiple_paths() {
        let mut tree = SegmentsTree::new();
        tree.add("a\nb");
        tree.add("a\nc");
        tree.add("d");

        assert!(tree.is_segment_used(b"a"));
        assert!(tree.is_segment_used(b"d"));
        assert!(!tree.is_segment_used(b"x"));

        let a = tree.get(b"a").unwrap();
        assert!(a.is_segment_used(b"b"));
        assert!(a.is_segment_used(b"c"));
        assert_eq!(a.fields_count(), 2);
    }

    #[test]
    fn test_is_root() {
        let tree = SegmentsTree::new();
        assert!(tree.is_root());

        let mut tree = SegmentsTree::new();
        tree.add("a\nb");
        let child = tree.get(b"a").unwrap();
        assert!(!child.is_root());
    }
}
