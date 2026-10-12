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
#[derive(Debug)]
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
    pub(crate) const fn field(&self) -> Option<&Arc<[u8]>> {
        match self {
            Self::Field(f) | Self::Both(f, _) => Some(f),
            Self::Node(_) => None,
        }
    }

    /// Get the child tree, if this entry has a node component.
    #[inline]
    pub(crate) const fn node(&self) -> Option<&SegmentsTree> {
        match self {
            Self::Node(n) | Self::Both(_, n) => Some(n),
            Self::Field(_) => None,
        }
    }

    /// Get the child tree mutably, if this entry has a node component.
    const fn node_mut(&mut self) -> Option<&mut SegmentsTree> {
        match self {
            Self::Node(n) | Self::Both(_, n) => Some(n),
            Self::Field(_) => None,
        }
    }

    /// Copy this entry with its child tree, if any, left empty.
    fn clone_level(&self) -> Self {
        match self {
            Self::Field(f) => Self::Field(f.clone()),
            Self::Node(_) => Self::Node(SegmentsTree::new_node()),
            Self::Both(f, _) => Self::Both(f.clone(), SegmentsTree::new_node()),
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
///
/// Paths nest as deep as patterns do, so dropping and cloning a tree walk its
/// levels with a heap worklist rather than recursing once per level, which
/// would overflow the stack on a deep enough pattern.
#[derive(Debug)]
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

impl Drop for SegmentsTree {
    fn drop(&mut self) {
        // Detach every child tree before the node holding it drops, so each
        // drop only ever frees one level.
        let mut detached = Vec::new();
        self.detach_children(&mut detached);
        while let Some(mut node) = detached.pop() {
            node.detach_children(&mut detached);
        }
    }
}

impl Clone for SegmentsTree {
    fn clone(&self) -> Self {
        // Copy one level at a time, then queue each child copy to be filled
        // from the child it was copied from.
        let mut copy = self.clone_level();
        let mut pending = vec![(self, &mut copy)];
        while let Some((source, target)) = pending.pop() {
            for (key, entry) in &mut target.entries {
                if let Some(child) = entry.node_mut() {
                    let source_child = source.entries[key]
                        .node()
                        .expect("a copied entry keeps its child tree");
                    *child = source_child.clone_level();
                    pending.push((source_child, child));
                }
            }
        }
        copy
    }
}

impl Default for SegmentsTree {
    fn default() -> Self {
        Self::new()
    }
}

impl SegmentsTree {
    /// Create a new root segments tree
    #[must_use]
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

    /// Move this node's child trees into `detached`, leaving empty nodes.
    fn detach_children(&mut self, detached: &mut Vec<Self>) {
        detached.extend(
            self.entries
                .values_mut()
                .filter_map(SegmentEntry::node_mut)
                .map(std::mem::take),
        );
    }

    /// Copy this node with every child tree left empty.
    fn clone_level(&self) -> Self {
        Self {
            is_root: self.is_root,
            entries: self
                .entries
                .iter()
                .map(|(key, entry)| (key.clone(), entry.clone_level()))
                .collect(),
            field_count: self.field_count,
            node_count: self.node_count,
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
    #[must_use]
    pub const fn is_root(&self) -> bool {
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
    #[must_use]
    pub fn is_segment_used(&self, segment: &[u8]) -> bool {
        self.lookup(segment).is_some()
    }

    /// Get a child node for a segment
    #[inline]
    #[must_use]
    pub fn get(&self, segment: &[u8]) -> Option<&Self> {
        self.lookup(segment).and_then(|e| e.node())
    }

    /// Get the full path for a leaf segment as a slice reference
    #[inline]
    #[must_use]
    pub fn path_for_segment(&self, segment: &[u8]) -> Option<&[u8]> {
        self.lookup(segment)
            .and_then(|e| e.field())
            .map(std::convert::AsRef::as_ref)
    }

    /// Get the full path for a leaf segment as an Arc (O(1) clone)
    #[inline]
    #[must_use]
    pub fn path_arc_for_segment(&self, segment: &[u8]) -> Option<Arc<[u8]>> {
        self.lookup(segment).and_then(|e| e.field()).cloned()
    }

    /// Number of child nodes (non-leaf)
    #[inline]
    #[must_use]
    pub const fn nodes_count(&self) -> usize {
        self.node_count
    }

    /// Number of leaf fields
    #[inline]
    #[must_use]
    pub const fn fields_count(&self) -> usize {
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

    #[test]
    fn test_path_arc_for_segment() {
        let mut tree = SegmentsTree::new();
        tree.add("x\ny");

        let x_node = tree.get(b"x").unwrap();
        let arc = x_node.path_arc_for_segment(b"y");
        assert_eq!(arc.as_deref(), Some(b"x\ny".as_slice()));

        // Missing segment returns None
        assert!(x_node.path_arc_for_segment(b"z").is_none());
    }

    #[test]
    fn test_clone_copies_every_level() {
        let mut tree = SegmentsTree::new();
        tree.add("a\nb\nc");
        tree.add("a\nb");
        tree.add("a\nd");
        tree.add("e");
        let copy = tree.clone();
        drop(tree);

        assert!(copy.is_root());
        assert_eq!((copy.fields_count(), copy.nodes_count()), (1, 1));
        assert_eq!(copy.path_for_segment(b"e"), Some(b"e".as_slice()));
        let a = copy.get(b"a").unwrap();
        assert!(!a.is_root());
        assert_eq!((a.fields_count(), a.nodes_count()), (2, 1));
        assert_eq!(a.path_for_segment(b"b"), Some(b"a\nb".as_slice()));
        assert_eq!(a.path_for_segment(b"d"), Some(b"a\nd".as_slice()));
        let b = a.get(b"b").unwrap();
        assert_eq!(b.path_for_segment(b"c"), Some(b"a\nb\nc".as_slice()));
    }

    #[test]
    fn test_deep_tree_clones_and_drops() {
        let depth = if cfg!(miri) { 64 } else { 100_000 };
        let path = vec!["s"; depth].join("\n");
        let mut tree = SegmentsTree::new();
        tree.add(&path);
        let copy = tree.clone();
        drop(tree);

        let mut node = &copy;
        for _ in 1..depth {
            node = node.get(b"s").expect("every level is copied");
        }
        assert_eq!(node.path_for_segment(b"s"), Some(path.as_bytes()));
    }
}
