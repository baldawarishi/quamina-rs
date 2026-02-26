//! Segments tree for tracking which field paths appear in patterns.
//!
//! This enables the JSON flattener to skip fields that aren't used in any pattern,
//! providing a significant performance optimization.
//!
//! Uses a single fused map per tree level: each segment name maps to a `SegmentEntry`
//! that may carry a leaf path, a child tree, or both. This eliminates redundant
//! hash lookups in the hot flattening path (1 hash+probe instead of up to 3+3).
//!
//! This module uses unsafe for:
//! - `from_utf8_unchecked` on field name segments that are guaranteed valid UTF-8
//!   by the JSON parser and `read_member_name` validation.
#![allow(unsafe_code)]

use rustc_hash::FxHashMap;
use std::sync::Arc;

/// Separator used between path segments (e.g., "context\nuser\nid")
pub(crate) const SEGMENT_SEPARATOR: char = '\n';

/// A fused entry in the segments tree.
///
/// A single segment name can be a leaf field (with a full path Arc),
/// a node leading to deeper fields, or both simultaneously.
#[derive(Clone, Debug)]
pub(crate) enum SegmentEntry {
    /// Leaf field only — stores the full path as `Arc<[u8]>`
    Field(Arc<[u8]>),
    /// Interior node only — stores the child tree
    Node(SegmentsTree),
    /// Both a leaf field and an interior node
    Both(Arc<[u8]>, SegmentsTree),
}

impl SegmentEntry {
    /// Get the leaf path Arc, if this entry has a field component.
    #[inline]
    pub(crate) fn field(&self) -> Option<&Arc<[u8]>> {
        match self {
            Self::Field(f) | Self::Both(f, _) => Some(f),
            Self::Node(_) => None,
        }
    }

    /// Get the child tree, if this entry has a node component.
    #[inline]
    pub(crate) fn node(&self) -> Option<&SegmentsTree> {
        match self {
            Self::Node(n) | Self::Both(_, n) => Some(n),
            Self::Field(_) => None,
        }
    }

    /// Ensure this entry has a node component and return a mutable reference to it.
    fn ensure_node(&mut self) -> &mut SegmentsTree {
        if matches!(self, Self::Field(_)) {
            // Upgrade Field → Both
            let old = std::mem::replace(self, Self::Node(SegmentsTree::new_node()));
            let Self::Field(f) = old else { unreachable!() };
            *self = Self::Both(f, SegmentsTree::new_node());
        }
        match self {
            Self::Node(n) | Self::Both(_, n) => n,
            Self::Field(_) => unreachable!(),
        }
    }

    /// Set (or overwrite) the leaf field path.
    fn set_field(&mut self, path: Arc<[u8]>) {
        match self {
            Self::Field(f) | Self::Both(f, _) => *f = path,
            Self::Node(_) => {
                // Upgrade Node → Both
                let old = std::mem::replace(self, Self::Field(path.clone()));
                let Self::Node(n) = old else { unreachable!() };
                *self = Self::Both(path, n);
            }
        }
    }
}

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
    /// Fused map: segment name → entry (field, node, or both)
    entries: FxHashMap<String, SegmentEntry>,
    /// Number of entries that have a field component
    field_count: usize,
    /// Number of entries that have a node component
    node_count: usize,
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
            entries: FxHashMap::default(),
            field_count: 0,
            node_count: 0,
        }
    }

    /// Create a new non-root node
    fn new_node() -> Self {
        Self {
            is_root: false,
            entries: FxHashMap::default(),
            field_count: 0,
            node_count: 0,
        }
    }

    /// Add a path to the tree.
    /// Path uses SEGMENT_SEPARATOR between segments (e.g., "context\nuser\nid")
    pub fn add(&mut self, path: &str) {
        let segments: Vec<&str> = path.split(SEGMENT_SEPARATOR).collect();

        if segments.len() == 1 {
            // Direct field on this node
            let path_arc = Arc::from(path.as_bytes());
            self.insert_field(path.to_string(), path_arc);
            return;
        }

        // Traverse/create nodes for all but last segment
        let mut node = self;
        for (i, segment) in segments.iter().enumerate() {
            if i == segments.len() - 1 {
                // Last segment is a field
                let path_arc = Arc::from(path.as_bytes());
                node.insert_field((*segment).to_string(), path_arc);
            } else {
                // Create node if it doesn't exist, then descend
                node = node.ensure_child_node((*segment).to_string());
            }
        }
    }

    /// Insert or update a field entry, maintaining the field_count.
    fn insert_field(&mut self, key: String, path_arc: Arc<[u8]>) {
        if let Some(entry) = self.entries.get_mut(&key) {
            // Entry exists — only update the field path
            let had_field = entry.field().is_some();
            entry.set_field(path_arc);
            if !had_field {
                self.field_count += 1;
            }
        } else {
            // New entry
            self.entries.insert(key, SegmentEntry::Field(path_arc));
            self.field_count += 1;
        }
    }

    /// Ensure a child node exists for the given segment and return `&mut` to it.
    fn ensure_child_node(&mut self, key: String) -> &mut Self {
        let had_entry = self.entries.contains_key(&key);
        let entry = self
            .entries
            .entry(key)
            .or_insert_with(|| SegmentEntry::Node(Self::new_node()));
        let had_node = matches!(entry, SegmentEntry::Node(_) | SegmentEntry::Both(_, _));
        let child = entry.ensure_node();
        if !had_entry || !had_node {
            self.node_count += 1;
        }
        child
    }

    /// Check if this is the root node
    #[inline]
    pub fn is_root(&self) -> bool {
        self.is_root
    }

    /// Single fused lookup — returns the entry for a segment name.
    ///
    /// One hash + one probe replaces up to 3 separate lookups
    /// (`is_segment_used` + `get` + `path_arc_for_segment`).
    #[inline]
    pub(crate) fn lookup(&self, segment: &[u8]) -> Option<&SegmentEntry> {
        // SAFETY: Segments come from the JSON parser which guarantees valid UTF-8 field names
        // (RFC 8259). The flattener's read_member_name validates this. Same pattern as
        // flatten_json::Field::path_str().
        let s = unsafe { std::str::from_utf8_unchecked(segment) };
        self.entries.get(s)
    }

    /// Check if a segment is used (either as a field or node)
    #[inline]
    pub fn is_segment_used(&self, segment: &[u8]) -> bool {
        self.lookup(segment).is_some()
    }

    /// Get a child node for a segment
    #[inline]
    pub fn get(&self, segment: &[u8]) -> Option<&Self> {
        self.lookup(segment).and_then(|e| e.node())
    }

    /// Get the full path for a leaf segment as a slice reference
    #[inline]
    pub fn path_for_segment(&self, segment: &[u8]) -> Option<&[u8]> {
        self.lookup(segment)
            .and_then(|e| e.field())
            .map(|arc| arc.as_ref())
    }

    /// Get the full path for a leaf segment as an Arc (O(1) clone)
    #[inline]
    pub fn path_arc_for_segment(&self, segment: &[u8]) -> Option<Arc<[u8]>> {
        self.lookup(segment).and_then(|e| e.field()).cloned()
    }

    /// Number of child nodes (non-leaf)
    #[inline]
    pub fn nodes_count(&self) -> usize {
        self.node_count
    }

    /// Number of leaf fields
    #[inline]
    pub fn fields_count(&self) -> usize {
        self.field_count
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

    #[test]
    fn test_both_field_and_node() {
        // A segment can be both a field and a node simultaneously
        let mut tree = SegmentsTree::new();
        tree.add("a"); // "a" is a field
        tree.add("a\nb"); // "a" is also a node

        assert!(tree.is_segment_used(b"a"));
        assert_eq!(tree.fields_count(), 1);
        assert_eq!(tree.nodes_count(), 1);

        // Should have both field and node components
        let entry = tree.lookup(b"a").unwrap();
        assert!(entry.field().is_some());
        assert!(entry.node().is_some());
        assert_eq!(entry.field().unwrap().as_ref(), b"a");

        let child = entry.node().unwrap();
        assert!(child.is_segment_used(b"b"));
    }

    #[test]
    fn test_node_then_field() {
        // Add node first, then field
        let mut tree = SegmentsTree::new();
        tree.add("a\nb"); // "a" is a node
        tree.add("a"); // "a" is also a field

        assert_eq!(tree.fields_count(), 1);
        assert_eq!(tree.nodes_count(), 1);

        let entry = tree.lookup(b"a").unwrap();
        assert!(entry.field().is_some());
        assert!(entry.node().is_some());
    }

    #[test]
    fn test_duplicate_add() {
        let mut tree = SegmentsTree::new();
        tree.add("a");
        tree.add("a");

        assert_eq!(tree.fields_count(), 1);
        assert_eq!(tree.nodes_count(), 0);
    }
}
