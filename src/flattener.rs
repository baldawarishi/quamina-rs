//! Flattener trait for pluggable event flattening.
//!
//! This module provides the `Flattener` trait that allows custom event parsers
//! to be used with Quamina. The default implementation handles JSON events,
//! but custom flatteners can be provided for other formats (CBOR, Protocol Buffers, etc.).
//!
//! # Example
//!
//! ```
//! use quamina::{Flattener, SegmentsTreeTracker, OwnedField, ArrayPos, QuaminaError};
//!
//! struct MyCustomFlattener;
//!
//! impl Flattener for MyCustomFlattener {
//!     fn flatten(
//!         &mut self,
//!         event: &[u8],
//!         tracker: &dyn SegmentsTreeTracker,
//!     ) -> Result<Vec<OwnedField>, QuaminaError> {
//!         // Custom parsing logic here
//!         Ok(vec![])
//!     }
//!
//!     fn copy(&self) -> Box<dyn Flattener> {
//!         Box::new(MyCustomFlattener)
//!     }
//! }
//! ```

use crate::flatten_json::{ArrayPos, FlattenJsonState};
use crate::segments_tree::SegmentsTree;
use crate::QuaminaError;
use std::any::Any;

/// A flattened field with owned data.
///
/// This is the public field type used by custom `Flattener` implementations.
/// Unlike the internal `Field<'a>` type, this owns its data and can be stored
/// and moved freely.
///
/// # Field Format
///
/// - `path`: The newline-separated path from the event root (e.g., `b"context\nuser\nid"`)
/// - `val`: The value bytes, with strings including their quotes (e.g., `b"\"active\""`)
/// - `array_trail`: Position tracking for array elements (prevents cross-element matching)
/// - `is_number`: True if the value is a JSON number
#[derive(Clone, Debug)]
pub struct OwnedField {
    /// Full path using '\n' separator (e.g., "context\nuser\nid")
    pub path: Vec<u8>,
    /// Value bytes - strings include quotes, numbers are raw
    pub val: Vec<u8>,
    /// Array position tracking - each entry identifies an array and position within it
    pub array_trail: Vec<ArrayPos>,
    /// True if the value is a numeric type
    pub is_number: bool,
}

/// Trait for tracking which field paths are used in patterns.
///
/// This interface allows a `Flattener` to optimize parsing by skipping fields
/// that aren't used in any pattern. The tracker maintains a tree structure
/// of all paths mentioned in patterns.
///
/// # Example Usage in a Flattener
///
/// ```ignore
/// fn flatten(&mut self, event: &[u8], tracker: &dyn SegmentsTreeTracker)
///     -> Result<Vec<OwnedField>, QuaminaError>
/// {
///     // Check if "status" field is needed
///     if tracker.is_segment_used(b"status") {
///         // Parse the status field
///     }
///
///     // For nested paths, traverse the tracker tree
///     if let Some(user_tracker) = tracker.get(b"user") {
///         if user_tracker.is_segment_used(b"id") {
///             // Parse user.id
///         }
///     }
///
///     Ok(fields)
/// }
/// ```
pub trait SegmentsTreeTracker: Send + Sync {
    /// Get the tracker for a child node.
    ///
    /// Returns `Some(tracker)` if the segment leads to nested fields that are used
    /// in patterns, or `None` if this segment has no children.
    fn get(&self, segment: &[u8]) -> Option<&dyn SegmentsTreeTracker>;

    /// Returns true if this is the root tracker.
    fn is_root(&self) -> bool;

    /// Check if a segment is used in any pattern.
    ///
    /// A segment is "used" if it appears as either a leaf field or a node
    /// leading to nested fields in any pattern.
    fn is_segment_used(&self, segment: &[u8]) -> bool;

    /// Get the full path bytes for a leaf segment.
    ///
    /// Returns the newline-separated full path for a field, or `None` if the
    /// segment is not a leaf field at this level.
    fn path_for_segment(&self, segment: &[u8]) -> Option<&[u8]>;

    /// Number of child nodes (non-leaf segments) at this level.
    fn nodes_count(&self) -> usize;

    /// Number of leaf fields at this level.
    fn fields_count(&self) -> usize;

    /// Returns self as Any for downcasting to concrete types.
    ///
    /// This is used internally by JsonFlattener to access SegmentsTree methods.
    fn as_any(&self) -> &dyn Any;
}

/// Trait for flattening events into field lists.
///
/// A `Flattener` transforms raw event bytes into a list of path/value pairs
/// that can be matched against patterns. The default JSON flattener handles
/// JSON events, but custom implementations can support other formats.
///
/// # Contract
///
/// Implementations must:
/// - Return fields with paths using '\n' as the segment separator
/// - Include quotes around string values (e.g., `b"\"value\""`)
/// - Use the tracker to determine which fields to include
/// - Set `is_number` correctly for numeric values
/// - Track array positions correctly to prevent cross-element matching
///
/// # Thread Safety
///
/// Flatteners are wrapped in a Mutex internally, so `flatten()` doesn't need
/// to be reentrant. However, the type must be `Send + Sync` to be stored in
/// shared structures.
pub trait Flattener: Send + Sync {
    /// Flatten an event into a list of fields.
    ///
    /// # Arguments
    ///
    /// * `event` - The raw event bytes to parse
    /// * `tracker` - Tracks which field paths are needed by patterns
    ///
    /// # Returns
    ///
    /// A vector of flattened fields, or an error if parsing fails.
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError>;

    /// Create a copy of this flattener for use in parallel contexts.
    ///
    /// When `Quamina::clone()` is called, the flattener is also copied.
    /// Each copy should be independent (no shared mutable state).
    fn copy(&self) -> Box<dyn Flattener>;
}

// =============================================================================
// SegmentsTreeTracker implementation for SegmentsTree
// =============================================================================

impl SegmentsTreeTracker for SegmentsTree {
    fn get(&self, segment: &[u8]) -> Option<&dyn SegmentsTreeTracker> {
        SegmentsTree::get(self, segment).map(|t| t as &dyn SegmentsTreeTracker)
    }

    fn is_root(&self) -> bool {
        SegmentsTree::is_root(self)
    }

    fn is_segment_used(&self, segment: &[u8]) -> bool {
        SegmentsTree::is_segment_used(self, segment)
    }

    fn path_for_segment(&self, segment: &[u8]) -> Option<&[u8]> {
        SegmentsTree::path_for_segment(self, segment)
    }

    fn nodes_count(&self) -> usize {
        SegmentsTree::nodes_count(self)
    }

    fn fields_count(&self) -> usize {
        SegmentsTree::fields_count(self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// =============================================================================
// JsonFlattener - Default JSON implementation
// =============================================================================

/// Default JSON flattener.
///
/// This wraps the internal `FlattenJsonState` and implements the `Flattener` trait.
/// It provides high-performance JSON parsing with field skipping optimization.
#[derive(Default)]
pub struct JsonFlattener {
    state: FlattenJsonState,
}

impl JsonFlattener {
    /// Create a new JSON flattener.
    pub fn new() -> Self {
        Self {
            state: FlattenJsonState::new(),
        }
    }
}

impl Flattener for JsonFlattener {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        // Downcast to SegmentsTree - JsonFlattener requires SegmentsTree for its
        // optimized internal implementation
        let segments_tree = tracker
            .as_any()
            .downcast_ref::<SegmentsTree>()
            .ok_or_else(|| {
                QuaminaError::InvalidJson(
                    "JsonFlattener requires SegmentsTree as tracker".into(),
                )
            })?;

        let fields = self.state.flatten(event, segments_tree)?;

        // Convert borrowed fields to owned
        Ok(fields
            .iter()
            .map(|f| OwnedField {
                path: f.path.to_vec(),
                val: f.val.as_bytes().to_vec(),
                array_trail: f.array_trail.to_vec(),
                is_number: f.is_number,
            })
            .collect())
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(JsonFlattener::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_segments_tree_tracker_impl() {
        let mut tree = SegmentsTree::new();
        tree.add("status");
        tree.add("context\nuser\nid");

        // Test as trait object
        let tracker: &dyn SegmentsTreeTracker = &tree;

        assert!(tracker.is_root());
        assert!(tracker.is_segment_used(b"status"));
        assert!(tracker.is_segment_used(b"context"));
        assert!(!tracker.is_segment_used(b"missing"));

        // Test nested traversal
        let context = tracker.get(b"context").unwrap();
        assert!(!context.is_root());
        assert!(context.is_segment_used(b"user"));

        let user = context.get(b"user").unwrap();
        assert!(user.is_segment_used(b"id"));
        assert_eq!(
            user.path_for_segment(b"id"),
            Some(b"context\nuser\nid".as_slice())
        );
    }

    #[test]
    fn test_owned_field() {
        let field = OwnedField {
            path: b"status".to_vec(),
            val: b"\"active\"".to_vec(),
            array_trail: vec![],
            is_number: false,
        };

        assert_eq!(field.path, b"status");
        assert_eq!(field.val, b"\"active\"");
        assert!(!field.is_number);
    }
}
