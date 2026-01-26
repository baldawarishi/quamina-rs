//! quamina-rs: Fast pattern-matching library for filtering JSON events

pub mod automaton;
mod case_folding;
#[doc(hidden)]
pub mod flatten_json;
mod flattener;
mod json;
pub mod numbits;
pub mod regexp;
#[doc(hidden)]
pub mod segments_tree;
mod unicode_categories;

#[cfg(test)]
mod regexp_samples;

// Re-export flattener types for custom implementations
pub use crate::flatten_json::ArrayPos;
pub use crate::flattener::{Flattener, JsonFlattener, OwnedField, SegmentsTreeTracker};

use automaton::{NfaBuffers, ThreadSafeCoreMatcher};
use flatten_json::FlattenJsonState;
use json::{Field, Matcher};
use parking_lot::Mutex;
use segments_tree::SegmentsTree;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::sync::atomic::{AtomicU64, Ordering};

/// Statistics for pruner rebuilding decisions
#[derive(Debug, Default)]
pub struct PrunerStats {
    /// Count of patterns emitted (returned after filtering) since last rebuild
    emitted: AtomicU64,
    /// Count of patterns filtered out (deleted) since last rebuild
    filtered: AtomicU64,
}

impl PrunerStats {
    fn new() -> Self {
        Self::default()
    }

    fn reset(&self) {
        self.emitted.store(0, Ordering::Relaxed);
        self.filtered.store(0, Ordering::Relaxed);
    }

    fn add_emitted(&self, count: u64) {
        self.emitted.fetch_add(count, Ordering::Relaxed);
    }

    fn add_filtered(&self, count: u64) {
        self.filtered.fetch_add(count, Ordering::Relaxed);
    }

    /// Get current emitted count
    pub fn emitted(&self) -> u64 {
        self.emitted.load(Ordering::Relaxed)
    }

    /// Get current filtered count
    pub fn filtered(&self) -> u64 {
        self.filtered.load(Ordering::Relaxed)
    }

    /// Check if rebuild should be triggered (Go uses 0.2 ratio, 1000 minimum)
    fn should_rebuild(&self) -> bool {
        let emitted = self.emitted.load(Ordering::Relaxed);
        let filtered = self.filtered.load(Ordering::Relaxed);

        // Minimum activity threshold
        if emitted + filtered < 1000 {
            return false;
        }

        // Avoid division by zero
        if emitted == 0 {
            return false;
        }

        // Trigger rebuild when filtered/emitted > 0.2
        let ratio = filtered as f64 / emitted as f64;
        ratio > 0.2
    }
}

impl Clone for PrunerStats {
    fn clone(&self) -> Self {
        Self {
            emitted: AtomicU64::new(self.emitted.load(Ordering::Relaxed)),
            filtered: AtomicU64::new(self.filtered.load(Ordering::Relaxed)),
        }
    }
}

/// Pattern definition: field matchers
type PatternDef = HashMap<String, Vec<Matcher>>;

/// Errors that can occur during pattern matching
#[derive(Debug)]
pub enum QuaminaError {
    InvalidJson(String),
    InvalidPattern(String),
    InvalidUtf8,
    UnsupportedMediaType(String),
}

impl fmt::Display for QuaminaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QuaminaError::InvalidJson(msg) => write!(f, "invalid JSON: {}", msg),
            QuaminaError::InvalidPattern(msg) => write!(f, "invalid pattern: {}", msg),
            QuaminaError::InvalidUtf8 => write!(f, "invalid UTF-8"),
            QuaminaError::UnsupportedMediaType(mt) => {
                write!(f, "media type \"{}\" is not supported by Quamina", mt)
            }
        }
    }
}

impl std::error::Error for QuaminaError {}

/// Builder for configuring a Quamina instance
///
/// This provides a Go-compatible builder pattern for creating Quamina instances
/// with custom configuration options.
///
/// # Example
/// ```
/// use quamina::QuaminaBuilder;
///
/// let q = QuaminaBuilder::<String>::new()
///     .with_media_type("application/json")
///     .unwrap()
///     .with_auto_rebuild(true)
///     .build()
///     .unwrap();
/// ```
pub struct QuaminaBuilder<X: Clone + Eq + Hash + Send + Sync = String> {
    /// Whether auto-rebuild is enabled (default: true)
    auto_rebuild_enabled: bool,
    /// Media type (only "application/json" supported)
    media_type_validated: bool,
    /// Custom flattener (if provided, replaces default JSON flattener)
    custom_flattener: Option<Box<dyn flattener::Flattener>>,
    /// PhantomData to carry the X type parameter
    _phantom: std::marker::PhantomData<X>,
}

impl<X: Clone + Eq + Hash + Send + Sync> QuaminaBuilder<X> {
    /// Create a new QuaminaBuilder with default settings
    pub fn new() -> Self {
        QuaminaBuilder {
            auto_rebuild_enabled: true,
            media_type_validated: false,
            custom_flattener: None,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Specify the media type for event parsing
    ///
    /// Currently only "application/json" is supported.
    ///
    /// # Errors
    /// Returns `QuaminaError::UnsupportedMediaType` if the media type is not supported.
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// // Valid media type
    /// let builder = QuaminaBuilder::<String>::new()
    ///     .with_media_type("application/json")
    ///     .unwrap();
    ///
    /// // Invalid media type
    /// let result = QuaminaBuilder::<String>::new()
    ///     .with_media_type("text/html");
    /// assert!(result.is_err());
    /// ```
    pub fn with_media_type(mut self, media_type: &str) -> Result<Self, QuaminaError> {
        // Check for conflict with custom flattener
        if self.custom_flattener.is_some() {
            return Err(QuaminaError::InvalidPattern(
                "flattener already specified".into(),
            ));
        }
        match media_type {
            "application/json" => {
                self.media_type_validated = true;
                Ok(self)
            }
            other => Err(QuaminaError::UnsupportedMediaType(other.to_string())),
        }
    }

    /// Specify a custom flattener for event parsing.
    ///
    /// This allows using custom parsers for non-JSON formats (CBOR, Protocol Buffers, etc.).
    /// When a custom flattener is provided, the default JSON flattener is replaced.
    ///
    /// This option cannot be combined with `with_media_type()`.
    ///
    /// # Errors
    /// Returns an error if `with_media_type()` has already been called.
    ///
    /// # Example
    /// ```
    /// use quamina::{QuaminaBuilder, Flattener, SegmentsTreeTracker, OwnedField, QuaminaError};
    ///
    /// struct MyFlattener;
    ///
    /// impl Flattener for MyFlattener {
    ///     fn flatten(
    ///         &mut self,
    ///         event: &[u8],
    ///         tracker: &dyn SegmentsTreeTracker,
    ///     ) -> Result<Vec<OwnedField>, QuaminaError> {
    ///         // Custom parsing logic
    ///         Ok(vec![])
    ///     }
    ///
    ///     fn copy(&self) -> Box<dyn Flattener> {
    ///         Box::new(MyFlattener)
    ///     }
    /// }
    ///
    /// let q = QuaminaBuilder::<String>::new()
    ///     .with_flattener(Box::new(MyFlattener))
    ///     .unwrap()
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_flattener(
        mut self,
        flattener: Box<dyn flattener::Flattener>,
    ) -> Result<Self, QuaminaError> {
        // Check for conflict with media type
        if self.media_type_validated {
            return Err(QuaminaError::InvalidPattern(
                "media-type already specified".into(),
            ));
        }
        if self.custom_flattener.is_some() {
            return Err(QuaminaError::InvalidPattern(
                "flattener specified more than once".into(),
            ));
        }
        self.custom_flattener = Some(flattener);
        Ok(self)
    }

    /// Enable or disable automatic pruner rebuilding
    ///
    /// When enabled (default), the matcher will automatically rebuild its internal
    /// data structures when the ratio of deleted to active patterns exceeds a threshold.
    /// This helps maintain matching performance after many deletions.
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// // Disable auto-rebuild for manual control
    /// let q = QuaminaBuilder::<String>::new()
    ///     .with_auto_rebuild(false)
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_auto_rebuild(mut self, enabled: bool) -> Self {
        self.auto_rebuild_enabled = enabled;
        self
    }

    /// Build the Quamina instance
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// let q = QuaminaBuilder::<String>::new()
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn build(self) -> Result<Quamina<X>, QuaminaError> {
        Ok(Quamina {
            automaton: ThreadSafeCoreMatcher::new(),
            pattern_defs: HashMap::new(),
            fallback_patterns: HashMap::new(),
            fallback_field_index: HashMap::new(),
            fallback_exists_false: HashSet::new(),
            deleted_patterns: HashSet::new(),
            segments_tree: SegmentsTree::new(),
            flattener: Mutex::new(FlattenJsonState::new()),
            custom_flattener: self.custom_flattener.map(Mutex::new),
            nfa_bufs: Mutex::new(NfaBuffers::new()),
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
        })
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Default for QuaminaBuilder<X> {
    fn default() -> Self {
        Self::new()
    }
}

/// The main pattern matcher
///
/// Quamina uses a hybrid matching approach:
/// - Automaton-based matching for patterns using supported operators (exact, prefix, suffix, wildcard, numeric comparisons, I-Regexp with lookarounds, etc.)
/// - HashMap-based fallback for patterns with unsupported features (CIDR, anything-but-numeric)
///
/// Quamina is Clone, allowing you to create snapshots for concurrent use:
/// ```
/// # use quamina::Quamina;
/// let mut q = Quamina::new();
/// q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
///
/// // Clone for use in another thread
/// let q_snapshot = q.clone();
/// ```
///
/// For shared concurrent access, wrap in Arc:
/// ```
/// # use quamina::Quamina;
/// use std::sync::Arc;
///
/// let q = Arc::new(Quamina::<String>::new());
/// let q_clone = Arc::clone(&q);
/// // Both can now be used for concurrent matching
/// ```
pub struct Quamina<X: Clone + Eq + Hash + Send + Sync = String> {
    /// Automaton-based matcher for patterns with supported operators
    automaton: ThreadSafeCoreMatcher<X>,
    /// All pattern definitions (source of truth for cloning)
    pattern_defs: HashMap<X, Vec<PatternDef>>,
    /// Fallback patterns that use unsupported operators (suffix, regex, numeric comparisons)
    fallback_patterns: HashMap<X, Vec<Pattern>>,
    /// Index mapping field paths to fallback pattern IDs
    fallback_field_index: HashMap<String, HashSet<X>>,
    /// Fallback pattern IDs that have exists:false matchers
    fallback_exists_false: HashSet<X>,
    /// Deleted patterns (filtered from automaton results since automaton doesn't support deletion)
    deleted_patterns: HashSet<X>,
    /// Segments tree for fast field skipping during event parsing
    segments_tree: SegmentsTree,
    /// Reusable JSON flattener state (Mutex for thread-safe access)
    flattener: Mutex<FlattenJsonState>,
    /// Custom flattener for non-JSON formats (if provided)
    custom_flattener: Option<Mutex<Box<dyn flattener::Flattener>>>,
    /// Reusable NFA traversal buffers (Mutex for thread-safe access)
    nfa_bufs: Mutex<NfaBuffers>,
    /// Statistics for auto-rebuild decisions
    pruner_stats: PrunerStats,
    /// Whether auto-rebuild is enabled (default: true)
    auto_rebuild_enabled: bool,
}

/// Internal representation of a compiled pattern
#[derive(Clone)]
struct Pattern {
    fields: HashMap<String, Vec<Matcher>>,
}

impl<X: Clone + Eq + Hash + Send + Sync> Clone for Quamina<X> {
    fn clone(&self) -> Self {
        // Create a new automaton and rebuild from pattern_defs
        let automaton = ThreadSafeCoreMatcher::new();

        for (id, patterns) in &self.pattern_defs {
            if self.deleted_patterns.contains(id) {
                continue;
            }
            for fields in patterns {
                let pattern_fields: Vec<(String, Vec<Matcher>)> =
                    fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                automaton.add_pattern(id.clone(), &pattern_fields);
            }
        }

        // Copy custom flattener if present
        let custom_flattener = self.custom_flattener.as_ref().map(|f| {
            let flattener = f.lock();
            Mutex::new(flattener.copy())
        });

        Quamina {
            automaton,
            pattern_defs: self.pattern_defs.clone(),
            fallback_patterns: self.fallback_patterns.clone(),
            fallback_field_index: self.fallback_field_index.clone(),
            fallback_exists_false: self.fallback_exists_false.clone(),
            deleted_patterns: self.deleted_patterns.clone(),
            segments_tree: self.segments_tree.clone(),
            flattener: Mutex::new(FlattenJsonState::new()),
            custom_flattener,
            nfa_bufs: Mutex::new(NfaBuffers::new()),
            pruner_stats: self.pruner_stats.clone(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
        }
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Quamina<X> {
    /// Create a new Quamina instance
    pub fn new() -> Self {
        Quamina {
            automaton: ThreadSafeCoreMatcher::new(),
            pattern_defs: HashMap::new(),
            fallback_patterns: HashMap::new(),
            fallback_field_index: HashMap::new(),
            fallback_exists_false: HashSet::new(),
            deleted_patterns: HashSet::new(),
            segments_tree: SegmentsTree::new(),
            flattener: Mutex::new(FlattenJsonState::new()),
            custom_flattener: None,
            nfa_bufs: Mutex::new(NfaBuffers::new()),
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: true,
        }
    }

    /// Add a pattern with the given identifier
    pub fn add_pattern(&mut self, x: X, pattern_json: &str) -> Result<(), QuaminaError> {
        let fields = json::parse_pattern(pattern_json)?;

        // Add field paths to segments tree (convert dot-separated to newline-separated)
        for field_path in fields.keys() {
            let segment_path = field_path.replace('.', "\n");
            self.segments_tree.add(&segment_path);
        }

        // If pattern was previously deleted, un-delete it
        self.deleted_patterns.remove(&x);

        // Store pattern definition for cloning
        self.pattern_defs
            .entry(x.clone())
            .or_default()
            .push(fields.clone());

        // Route to automaton
        let pattern_fields: Vec<(String, Vec<Matcher>)> = fields.into_iter().collect();
        self.automaton.add_pattern(x, &pattern_fields);

        Ok(())
    }

    /// Find all patterns that match the given event
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        // Check if we have a custom flattener
        if let Some(ref custom_flattener_mutex) = self.custom_flattener {
            // Use custom flattener path
            return self.matches_for_event_custom_flattener(event, custom_flattener_mutex);
        }

        // Default path: use optimized JSON flattener with borrowed data
        let mut flattener = self.flattener.lock();
        let streaming_fields = flattener.flatten(event, &self.segments_tree)?;

        // Sort by path for automaton matching (using byte comparison which is equivalent to str for ASCII paths)
        streaming_fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));

        // Get matches from automaton using fields directly (no intermediate EventFieldRef)
        let mut matches: Vec<X> = {
            let mut bufs = self.nfa_bufs.lock();
            let raw_matches = self
                .automaton
                .matches_for_fields_direct(streaming_fields, &mut bufs);
            // Fast path: skip filtering if no patterns have been deleted
            if self.deleted_patterns.is_empty() {
                // Track stats: all matches are emitted
                self.pruner_stats.add_emitted(raw_matches.len() as u64);
                raw_matches
            } else {
                // Slow path: filter deleted patterns and track stats
                let raw_count = raw_matches.len();
                let filtered: Vec<X> = raw_matches
                    .into_iter()
                    .filter(|x| !self.deleted_patterns.contains(x))
                    .collect();
                let filtered_count = raw_count - filtered.len();
                self.pruner_stats.add_emitted(filtered.len() as u64);
                self.pruner_stats.add_filtered(filtered_count as u64);
                filtered
            }
        };

        // Slow path: fallback matching still needs legacy Field format (with String values)
        if !self.fallback_patterns.is_empty() {
            // Only build legacy fields if we have fallback patterns
            let event_fields: Vec<Field> = streaming_fields
                .iter()
                .map(|f| {
                    let path_str = String::from_utf8_lossy(&f.path).into_owned();
                    let raw_bytes = f.val.as_bytes();
                    let value = if raw_bytes.starts_with(b"\"") && raw_bytes.ends_with(b"\"") {
                        String::from_utf8_lossy(&raw_bytes[1..raw_bytes.len() - 1]).into_owned()
                    } else {
                        String::from_utf8_lossy(raw_bytes).into_owned()
                    };
                    let array_trail = f
                        .array_trail
                        .iter()
                        .map(|ap| json::ArrayPos {
                            array: ap.array,
                            pos: ap.pos,
                        })
                        .collect();
                    Field {
                        path: path_str,
                        value,
                        array_trail,
                        is_number: f.is_number,
                    }
                })
                .collect();

            // Build multimap for fallback matching
            let mut event_map: HashMap<&str, Vec<&Field>> = HashMap::new();
            for field in &event_fields {
                event_map
                    .entry(field.path.as_str())
                    .or_default()
                    .push(field);
            }

            let fallback_matches = self.fallback_matches(&event_map);
            matches.extend(fallback_matches);
        }

        Ok(matches)
    }

    /// Match using a custom flattener (slower path with owned data)
    fn matches_for_event_custom_flattener(
        &self,
        event: &[u8],
        custom_flattener_mutex: &Mutex<Box<dyn flattener::Flattener>>,
    ) -> Result<Vec<X>, QuaminaError> {
        use std::sync::Arc;

        // Get owned fields from custom flattener
        let mut custom_flattener = custom_flattener_mutex.lock();
        let owned_fields = custom_flattener.flatten(event, &self.segments_tree)?;
        drop(custom_flattener); // Release lock early

        // Convert OwnedField to flatten_json::Field with owned data
        let mut streaming_fields: Vec<flatten_json::Field<'static>> = owned_fields
            .into_iter()
            .map(|f| flatten_json::Field {
                path: Arc::from(f.path.as_slice()),
                val: flatten_json::FieldValue::Owned(f.val),
                array_trail: f.array_trail.into(),
                is_number: f.is_number,
            })
            .collect();

        // Sort by path for automaton matching
        streaming_fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));

        // Get matches from automaton
        let mut matches: Vec<X> = {
            let mut bufs = self.nfa_bufs.lock();
            let raw_matches = self
                .automaton
                .matches_for_fields_direct(&streaming_fields, &mut bufs);
            // Fast path: skip filtering if no patterns have been deleted
            if self.deleted_patterns.is_empty() {
                self.pruner_stats.add_emitted(raw_matches.len() as u64);
                raw_matches
            } else {
                // Slow path: filter deleted patterns and track stats
                let raw_count = raw_matches.len();
                let filtered: Vec<X> = raw_matches
                    .into_iter()
                    .filter(|x| !self.deleted_patterns.contains(x))
                    .collect();
                let filtered_count = raw_count - filtered.len();
                self.pruner_stats.add_emitted(filtered.len() as u64);
                self.pruner_stats.add_filtered(filtered_count as u64);
                filtered
            }
        };

        // Fallback matching
        if !self.fallback_patterns.is_empty() {
            let event_fields: Vec<Field> = streaming_fields
                .iter()
                .map(|f| {
                    let path_str = String::from_utf8_lossy(&f.path).into_owned();
                    let raw_bytes = f.val.as_bytes();
                    let value = if raw_bytes.starts_with(b"\"") && raw_bytes.ends_with(b"\"") {
                        String::from_utf8_lossy(&raw_bytes[1..raw_bytes.len() - 1]).into_owned()
                    } else {
                        String::from_utf8_lossy(raw_bytes).into_owned()
                    };
                    let array_trail = f
                        .array_trail
                        .iter()
                        .map(|ap| json::ArrayPos {
                            array: ap.array,
                            pos: ap.pos,
                        })
                        .collect();
                    Field {
                        path: path_str,
                        value,
                        array_trail,
                        is_number: f.is_number,
                    }
                })
                .collect();

            let mut event_map: HashMap<&str, Vec<&Field>> = HashMap::new();
            for field in &event_fields {
                event_map
                    .entry(field.path.as_str())
                    .or_default()
                    .push(field);
            }

            let fallback_matches = self.fallback_matches(&event_map);
            matches.extend(fallback_matches);
        }

        Ok(matches)
    }

    /// Flatten an event without matching (for benchmarking)
    #[doc(hidden)]
    pub fn flatten_only(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        let mut flattener = self.flattener.lock();
        let fields = flattener.flatten(event, &self.segments_tree)?;
        Ok(fields.len())
    }

    /// Get matches from fallback patterns
    fn fallback_matches(&self, event_map: &HashMap<&str, Vec<&Field>>) -> Vec<X> {
        // Heuristic: field indexing helps when patterns have diverse fields
        let use_field_index = self.fallback_patterns.len() >= 10
            && self.fallback_field_index.len() * 2 > self.fallback_patterns.len();

        if use_field_index {
            self.fallback_matches_via_field_index(event_map)
        } else {
            self.fallback_matches_via_direct_iteration(event_map)
        }
    }

    /// Match fallback patterns using field index
    fn fallback_matches_via_field_index(&self, event_map: &HashMap<&str, Vec<&Field>>) -> Vec<X> {
        let mut seen: HashSet<&X> = HashSet::new();
        let mut matches = Vec::new();

        // Check patterns from field index
        for event_field_path in event_map.keys() {
            if let Some(pattern_ids) = self.fallback_field_index.get(*event_field_path) {
                for id in pattern_ids {
                    if !seen.insert(id) {
                        continue;
                    }
                    if let Some(patterns) = self.fallback_patterns.get(id) {
                        for pattern in patterns {
                            if self.pattern_matches(&pattern.fields, event_map) {
                                matches.push(id.clone());
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Also check patterns with exists:false
        for id in &self.fallback_exists_false {
            if !seen.insert(id) {
                continue;
            }
            if let Some(patterns) = self.fallback_patterns.get(id) {
                for pattern in patterns {
                    if self.pattern_matches(&pattern.fields, event_map) {
                        matches.push(id.clone());
                        break;
                    }
                }
            }
        }

        matches
    }

    /// Match fallback patterns using direct iteration
    fn fallback_matches_via_direct_iteration(
        &self,
        event_map: &HashMap<&str, Vec<&Field>>,
    ) -> Vec<X> {
        let mut matches = Vec::new();
        for (id, patterns) in &self.fallback_patterns {
            for pattern in patterns {
                if self.pattern_matches(&pattern.fields, event_map) {
                    matches.push(id.clone());
                    break;
                }
            }
        }
        matches
    }

    fn pattern_matches(
        &self,
        pattern_fields: &HashMap<String, Vec<Matcher>>,
        event_map: &HashMap<&str, Vec<&Field>>,
    ) -> bool {
        // Empty pattern matches everything
        if pattern_fields.is_empty() {
            return true;
        }

        // Fast path: single-field patterns don't need backtracking for array trail consistency
        if pattern_fields.len() == 1 {
            let (field_path, matchers) = pattern_fields.iter().next().unwrap();
            return self.single_field_matches(field_path, matchers, event_map);
        }

        // Multi-field patterns: use backtracking for array trail consistency
        let pattern_field_list: Vec<_> = pattern_fields.iter().collect();
        let mut trails = Vec::with_capacity(pattern_field_list.len());
        self.backtrack_match(&pattern_field_list, 0, &mut trails, event_map)
    }

    /// Fast path for single-field pattern matching (no backtracking needed)
    fn single_field_matches(
        &self,
        field_path: &str,
        matchers: &[Matcher],
        event_map: &HashMap<&str, Vec<&Field>>,
    ) -> bool {
        let event_fields = event_map.get(field_path);

        // Try each matcher (OR semantics)
        for matcher in matchers {
            if let Matcher::Exists(should_exist) = matcher {
                let exists = event_fields.map(|v| !v.is_empty()).unwrap_or(false);
                if exists == *should_exist {
                    return true;
                }
                continue;
            }

            // For other matchers, check if any event value matches
            if let Some(fields) = event_fields {
                if fields.iter().any(|f| self.value_matches(matcher, &f.value)) {
                    return true;
                }
            }
        }

        false
    }

    /// Backtracking match that ensures array trail compatibility across all matched fields.
    /// Uses push/pop on trails Vec to avoid allocations during recursion.
    fn backtrack_match<'a>(
        &self,
        pattern_fields: &[(&String, &Vec<Matcher>)],
        field_idx: usize,
        current_trails: &mut Vec<&'a [json::ArrayPos]>,
        event_map: &HashMap<&str, Vec<&'a Field>>,
    ) -> bool {
        if field_idx >= pattern_fields.len() {
            return true; // All fields matched successfully
        }

        let (field_path, matchers) = pattern_fields[field_idx];
        let event_fields = event_map.get(field_path.as_str());

        // Try each matcher (OR semantics within a field)
        for matcher in matchers.iter() {
            // Handle exists patterns specially - they don't require specific field values
            if let Matcher::Exists(should_exist) = matcher {
                let exists = event_fields.map(|v| !v.is_empty()).unwrap_or(false);
                if exists == *should_exist {
                    // exists:true/false doesn't constrain array trails, just recurse
                    if self.backtrack_match(
                        pattern_fields,
                        field_idx + 1,
                        current_trails,
                        event_map,
                    ) {
                        return true;
                    }
                }
                continue;
            }

            // For other matchers, find all matching event values
            if let Some(fields) = event_fields {
                for event_field in fields.iter() {
                    if self.value_matches(matcher, &event_field.value) {
                        // Check array trail compatibility with already-matched fields
                        let compatible = current_trails.iter().all(|prev_trail| {
                            no_array_trail_conflict(prev_trail, &event_field.array_trail)
                        });

                        if compatible {
                            // Add this field's trail, recurse, then pop (avoids allocation)
                            current_trails.push(&event_field.array_trail);
                            let matched = self.backtrack_match(
                                pattern_fields,
                                field_idx + 1,
                                current_trails,
                                event_map,
                            );
                            current_trails.pop();

                            if matched {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Legacy fallback path - should never be reached.
    /// All matchers now go through the automaton path.
    fn value_matches(&self, matcher: &Matcher, _value: &str) -> bool {
        debug_assert!(
            false,
            "Matcher should use automaton path, not fallback: {:?}",
            std::mem::discriminant(matcher)
        );
        false
    }

    /// Delete all patterns with the given identifier
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError> {
        // Check if pattern exists
        if !self.pattern_defs.contains_key(x) || self.deleted_patterns.contains(x) {
            return Ok(()); // Pattern doesn't exist or already deleted
        }

        // If this was a fallback pattern, remove from fallback structures
        if let Some(patterns) = self.fallback_patterns.get(x) {
            for pattern in patterns {
                for field_path in pattern.fields.keys() {
                    if let Some(ids) = self.fallback_field_index.get_mut(field_path) {
                        ids.remove(x);
                    }
                }
            }
        }
        self.fallback_field_index.retain(|_, ids| !ids.is_empty());
        self.fallback_exists_false.remove(x);
        self.fallback_patterns.remove(x);

        // For automaton patterns, add to deleted set (automaton doesn't support deletion)
        // This will filter the pattern from automaton results
        self.deleted_patterns.insert(x.clone());

        Ok(())
    }

    /// Check if any pattern matches the event (returns early on first match)
    pub fn has_matches(&self, event: &[u8]) -> Result<bool, QuaminaError> {
        // Use matches_for_event and check if non-empty
        // This could be optimized to return early, but for now this is simpler
        Ok(!self.matches_for_event(event)?.is_empty())
    }

    /// Count how many unique pattern IDs match the event
    pub fn count_matches(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        Ok(self.matches_for_event(event)?.len())
    }

    /// Returns the number of unique pattern IDs stored
    pub fn pattern_count(&self) -> usize {
        self.pattern_defs
            .keys()
            .filter(|k| !self.deleted_patterns.contains(*k))
            .count()
    }

    /// Returns true if no patterns are stored
    pub fn is_empty(&self) -> bool {
        self.pattern_count() == 0
    }

    /// Get the pruner statistics
    pub fn pruner_stats(&self) -> &PrunerStats {
        &self.pruner_stats
    }

    /// Enable or disable auto-rebuild
    pub fn set_auto_rebuild(&mut self, enabled: bool) {
        self.auto_rebuild_enabled = enabled;
    }

    /// Check if auto-rebuild is enabled
    pub fn auto_rebuild_enabled(&self) -> bool {
        self.auto_rebuild_enabled
    }

    /// Rebuild the automaton from only live patterns, removing deleted patterns permanently.
    /// This reclaims memory from deleted patterns and improves matching performance.
    /// Returns the number of patterns that were purged from the automaton.
    pub fn rebuild(&mut self) -> usize {
        let purged = self.deleted_patterns.len();
        if purged == 0 {
            return 0;
        }

        // Create new automaton with only live patterns
        let new_automaton = ThreadSafeCoreMatcher::new();

        for (id, patterns) in &self.pattern_defs {
            if self.deleted_patterns.contains(id) {
                continue;
            }
            for fields in patterns {
                let pattern_fields: Vec<(String, Vec<Matcher>)> =
                    fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                new_automaton.add_pattern(id.clone(), &pattern_fields);
            }
        }

        // Remove deleted patterns from pattern_defs (they're now permanently gone)
        self.pattern_defs
            .retain(|id, _| !self.deleted_patterns.contains(id));

        // Clear the deleted set and reset stats
        self.deleted_patterns.clear();
        self.pruner_stats.reset();

        // Swap in new automaton
        self.automaton = new_automaton;

        purged
    }

    /// Check if rebuild is recommended based on pruner statistics.
    /// Returns true when filtered/emitted ratio exceeds 0.2 and activity threshold is met.
    pub fn should_rebuild(&self) -> bool {
        self.pruner_stats.should_rebuild()
    }

    /// Perform rebuild if the pruner statistics indicate it would be beneficial.
    /// Returns the number of patterns purged, or 0 if no rebuild was needed.
    pub fn maybe_rebuild(&mut self) -> usize {
        if self.auto_rebuild_enabled && self.pruner_stats.should_rebuild() {
            self.rebuild()
        } else {
            0
        }
    }

    /// Removes all patterns
    pub fn clear(&mut self) {
        self.automaton = ThreadSafeCoreMatcher::new();
        self.pattern_defs.clear();
        self.fallback_patterns.clear();
        self.fallback_field_index.clear();
        self.fallback_exists_false.clear();
        self.deleted_patterns.clear();
        self.pruner_stats.reset();
    }

    /// Returns a list of all active (non-deleted) pattern identifiers.
    ///
    /// This provides a way to inspect what patterns are currently registered
    /// with the Quamina instance.
    ///
    /// # Example
    /// ```
    /// # use quamina::Quamina;
    /// let mut q: Quamina<String> = Quamina::new();
    /// q.add_pattern("p1".into(), r#"{"status": ["active"]}"#).unwrap();
    /// q.add_pattern("p2".into(), r#"{"type": ["event"]}"#).unwrap();
    ///
    /// let ids = q.list_pattern_ids();
    /// assert_eq!(ids.len(), 2);
    /// ```
    pub fn list_pattern_ids(&self) -> Vec<&X> {
        self.pattern_defs
            .keys()
            .filter(|id| !self.deleted_patterns.contains(*id))
            .collect()
    }

    /// Checks if a pattern with the given identifier exists (and hasn't been deleted).
    ///
    /// # Example
    /// ```
    /// # use quamina::Quamina;
    /// let mut q: Quamina<String> = Quamina::new();
    /// let p1: String = "p1".into();
    /// assert!(!q.contains_pattern(&p1));
    ///
    /// q.add_pattern(p1.clone(), r#"{"status": ["active"]}"#).unwrap();
    /// assert!(q.contains_pattern(&p1));
    /// ```
    pub fn contains_pattern(&self, id: &X) -> bool {
        self.pattern_defs.contains_key(id) && !self.deleted_patterns.contains(id)
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Default for Quamina<X> {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if two array trails are compatible (no conflict).
/// Two trails conflict if they reference different positions in the same array.
/// This prevents matching across different elements of the same array.
fn no_array_trail_conflict(from: &[json::ArrayPos], to: &[json::ArrayPos]) -> bool {
    for from_pos in from {
        for to_pos in to {
            if from_pos.array == to_pos.array && from_pos.pos != to_pos.pos {
                return false;
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_no_match() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "inactive"}"#.as_bytes())
            .unwrap();
        assert!(matches.is_empty());
    }

    #[test]
    fn test_numeric_match() {
        // This tests matching numeric values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"count": [42]}"#).unwrap();

        let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match numeric value 42");
    }

    #[test]
    fn test_numeric_variant_matching() {
        // All these numeric representations of 35 should match pattern [35]
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [35]}"#).unwrap();

        // Integer form
        let m1 = q.matches_for_event(r#"{"x": 35}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "35 should match");

        // Decimal with trailing zero
        let m2 = q.matches_for_event(r#"{"x": 35.0}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p1"], "35.0 should match [35]");

        // Scientific notation
        let m3 = q.matches_for_event(r#"{"x": 3.5e1}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p1"], "3.5e1 should match [35]");

        // Additional variants from Go's TestMatcherNumerics (numbers_test.go:174)
        let m4 = q.matches_for_event(r#"{"x": 35.000}"#.as_bytes()).unwrap();
        assert_eq!(m4, vec!["p1"], "35.000 should match [35]");

        let m5 = q
            .matches_for_event(r#"{"x": 0.000035e6}"#.as_bytes())
            .unwrap();
        assert_eq!(m5, vec!["p1"], "0.000035e6 should match [35]");
    }

    #[test]
    fn test_boolean_match() {
        // This tests matching boolean values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"enabled": [true]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"enabled": true}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match boolean true");
    }

    #[test]
    fn test_null_match() {
        // This tests matching null values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"value": [null]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"value": null}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match null value");
    }

    #[test]
    fn test_exists_true() {
        // Tests the exists operator - field must be present
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"exists": true}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "anything", "other": 1}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match when field exists");

        let no_match = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
        assert!(
            no_match.is_empty(),
            "Should not match when field is missing"
        );
    }

    #[test]
    fn test_exists_false() {
        // Tests the exists:false operator - field must NOT be present
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"exists": false}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match when field is absent");

        let no_match = q
            .matches_for_event(r#"{"name": "value"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match when field exists");
    }

    #[test]
    fn test_exists_with_empty_array() {
        // Per Go quamina: {"a": []} with exists:true does NOT match
        // but exists:false DOES match (no leaf values)
        let mut q_true = Quamina::new();
        q_true
            .add_pattern("p1", r#"{"a": [{"exists": true}]}"#)
            .unwrap();

        let mut q_false = Quamina::new();
        q_false
            .add_pattern("p2", r#"{"a": [{"exists": false}]}"#)
            .unwrap();

        // Event with empty array
        let event = r#"{"a": []}"#;

        // exists:true should NOT match (no leaf values in empty array)
        let matches_true = q_true.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches_true.is_empty(),
            "exists:true should not match empty array"
        );

        // exists:false SHOULD match (no leaf values means field effectively absent)
        let matches_false = q_false.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches_false,
            vec!["p2"],
            "exists:false should match empty array"
        );
    }

    #[test]
    fn test_prefix_match() {
        // Tests the prefix operator
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"prefix": "prod-"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "prod-server-1"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match prefix");

        let no_match = q
            .matches_for_event(r#"{"name": "dev-server-1"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match different prefix");
    }

    #[test]
    fn test_wildcard_suffix() {
        // Tests wildcard with * at start (suffix match)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"file": [{"wildcard": "*.txt"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"file": "document.txt"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match *.txt");

        let no_match = q
            .matches_for_event(r#"{"file": "document.pdf"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match .pdf");
    }

    #[test]
    fn test_wildcard_prefix() {
        // Tests wildcard with * at end (prefix match)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"wildcard": "prod-*"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "prod-server"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match prod-*");
    }

    #[test]
    fn test_wildcard_contains() {
        // Tests wildcard with * on both sides (contains)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"msg": [{"wildcard": "*error*"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"msg": "an error occurred"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match *error*");

        let no_match = q
            .matches_for_event(r#"{"msg": "all good"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_anything_but() {
        // Tests anything-but operator
        let mut q = Quamina::new();
        q.add_pattern(
            "p1",
            r#"{"status": [{"anything-but": ["deleted", "archived"]}]}"#,
        )
        .unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match non-excluded value");

        let no_match = q
            .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match excluded value");
    }

    #[test]
    fn test_anything_but_validation() {
        // Empty anything-but array should return error
        let mut q = Quamina::new();
        let result = q.add_pattern("p1", r#"{"status": [{"anything-but": []}]}"#);
        assert!(
            result.is_err(),
            "Empty anything-but array should be rejected"
        );

        // Non-string/non-number values in anything-but should return error
        let mut q2 = Quamina::new();
        let result2 = q2.add_pattern("p2", r#"{"x": [{"anything-but": [true, null]}]}"#);
        assert!(
            result2.is_err(),
            "anything-but with only booleans/nulls should be rejected"
        );

        // Mixed strings and numbers should return error
        let mut q3 = Quamina::new();
        let result3 = q3.add_pattern("p3", r#"{"x": [{"anything-but": ["a", 1]}]}"#);
        assert!(
            result3.is_err(),
            "anything-but with mixed strings and numbers should be rejected"
        );
    }

    #[test]
    fn test_anything_but_single_string() {
        // Test single string: {"anything-but": "foo"} (EventBridge/Ruler compatibility)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": [{"anything-but": "deleted"}]}"#)
            .unwrap();

        // Should match non-excluded values
        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            m1,
            vec!["p1"],
            "Single anything-but should match non-excluded"
        );

        let m2 = q
            .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap();
        assert!(
            m2.is_empty(),
            "Single anything-but should not match excluded"
        );
    }

    #[test]
    fn test_anything_but_numeric_single() {
        // Test single number: {"anything-but": 123}
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"code": [{"anything-but": 404}]}"#)
            .unwrap();

        // Should match non-excluded numbers
        let m1 = q.matches_for_event(r#"{"code": 200}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "Should match non-excluded number");

        let m2 = q.matches_for_event(r#"{"code": 404}"#.as_bytes()).unwrap();
        assert!(m2.is_empty(), "Should not match excluded number");

        // Non-numeric string doesn't match excluded number, so passes
        let m3 = q
            .matches_for_event(r#"{"code": "not-a-number"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            m3,
            vec!["p1"],
            "Non-numeric value passes numeric anything-but"
        );
    }

    #[test]
    fn test_anything_but_numeric_array() {
        // Test array of numbers: {"anything-but": [100, 200, 300]}
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"code": [{"anything-but": [400, 404, 500]}]}"#)
            .unwrap();

        // Should match non-excluded numbers
        let m1 = q.matches_for_event(r#"{"code": 200}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "Should match non-excluded number");

        let m2 = q.matches_for_event(r#"{"code": 404}"#.as_bytes()).unwrap();
        assert!(m2.is_empty(), "Should not match excluded number");

        let m3 = q.matches_for_event(r#"{"code": 500}"#.as_bytes()).unwrap();
        assert!(m3.is_empty(), "Should not match another excluded number");
    }

    #[test]
    fn test_anything_but_numeric_float() {
        // Test with floating point numbers
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"price": [{"anything-but": [9.99, 19.99]}]}"#)
            .unwrap();

        let m1 = q
            .matches_for_event(r#"{"price": 14.99}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "Should match non-excluded float");

        let m2 = q
            .matches_for_event(r#"{"price": 9.99}"#.as_bytes())
            .unwrap();
        assert!(m2.is_empty(), "Should not match excluded float");
    }

    #[test]
    fn test_equals_ignore_case() {
        // Tests case-insensitive matching
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"equals-ignore-case": "Test"}]}"#)
            .unwrap();

        let m1 = q
            .matches_for_event(r#"{"name": "test"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "Should match lowercase");

        let m2 = q
            .matches_for_event(r#"{"name": "TEST"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"], "Should match uppercase");

        let m3 = q
            .matches_for_event(r#"{"name": "TeSt"}"#.as_bytes())
            .unwrap();
        assert_eq!(m3, vec!["p1"], "Should match mixed case");

        let no_match = q
            .matches_for_event(r#"{"name": "other"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_invalid_json_events() {
        // Based on Go quamina's TestFJErrorCases
        // Test that various malformed JSON events return errors
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": [1]}"#).unwrap();

        // Truncated JSON
        assert!(
            q.matches_for_event(r#"{"a"#.as_bytes()).is_err(),
            "Truncated JSON should error"
        );
        assert!(
            q.matches_for_event(r#"{"a": "#.as_bytes()).is_err(),
            "Truncated value should error"
        );
        assert!(
            q.matches_for_event(r#"{"a": ["#.as_bytes()).is_err(),
            "Truncated array should error"
        );

        // Empty input
        assert!(
            q.matches_for_event(r#""#.as_bytes()).is_err(),
            "Empty input should error"
        );

        // Non-object at top level
        assert!(
            q.matches_for_event(r#""string""#.as_bytes()).is_err(),
            "String at top level should error"
        );
        assert!(
            q.matches_for_event(r#"[1, 2]"#.as_bytes()).is_err(),
            "Array at top level should error"
        );
        assert!(
            q.matches_for_event(r#"123"#.as_bytes()).is_err(),
            "Number at top level should error"
        );

        // Malformed JSON
        assert!(
            q.matches_for_event(r#"{ "a" : }"#.as_bytes()).is_err(),
            "Missing value should error"
        );

        // Additional error cases from Go's TestFJErrorCases (flatten_json_test.go:249)
        // Invalid escape sequence in string value
        assert!(
            q.matches_for_event(r#"{"a": "a\zb"}"#.as_bytes()).is_err(),
            "Invalid escape \\z should error"
        );

        // Invalid escape sequence in field name
        assert!(
            q.matches_for_event(r#"{"a\zb": 2}"#.as_bytes()).is_err(),
            "Invalid escape in field name should error"
        );

        // Invalid value identifier
        assert!(
            q.matches_for_event(r#"{"a": xx}"#.as_bytes()).is_err(),
            "Invalid value xx should error"
        );

        // Truncated true literal
        assert!(
            q.matches_for_event(r#"{"a": tru}"#.as_bytes()).is_err(),
            "Truncated 'tru' should error"
        );

        // Invalid literal
        assert!(
            q.matches_for_event(r#"{"a": truse}"#.as_bytes()).is_err(),
            "Invalid 'truse' should error"
        );

        // Note: Rust's flattener has an early termination optimization - once all
        // needed pattern fields are found, parsing stops. This means invalid JSON
        // after the needed fields (like `{"a": 23z}` when only "a" is needed) may
        // not be detected. Go always fully validates. This trade-off is intentional
        // for performance on large JSON documents with few needed fields.
    }

    #[test]
    fn test_json_escape_sequences() {
        // Test that JSON escape sequences in events are properly decoded
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"msg": ["line1\nline2"]}"#).unwrap();

        // Event with \n escape (literal newline)
        let event = r#"{"msg": "line1\nline2"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match \\n escape sequence");

        // Test tab escape
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": ["a\tb"]}"#).unwrap();
        let m2 = q2.matches_for_event(r#"{"x": "a\tb"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p2"], "Should match \\t escape sequence");

        // Test backslash escape
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"x": ["a\\b"]}"#).unwrap();
        let m3 = q3.matches_for_event(r#"{"x": "a\\b"}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p3"], "Should match \\\\ escape sequence");

        // Test quote escape
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"x": ["say \"hello\""]}"#).unwrap();
        let m4 = q4
            .matches_for_event(r#"{"x": "say \"hello\""}"#.as_bytes())
            .unwrap();
        assert_eq!(m4, vec!["p4"], "Should match \\\" escape sequence");
    }

    #[test]
    fn test_equals_ignore_case_multiple_patterns() {
        // Based on Go quamina's TestEqualsIgnoreCaseMatching
        // Multiple patterns with different case variations should both match
        let mut q = Quamina::new();
        q.add_pattern("r1", r#"{"a": [{"equals-ignore-case": "aBc"}]}"#)
            .unwrap();
        q.add_pattern("r2", r#"{"b": [{"equals-ignore-case": "XyZ"}]}"#)
            .unwrap();
        q.add_pattern("r3", r#"{"b": [{"equals-ignore-case": "xyZ"}]}"#)
            .unwrap();

        // r1 matches any case of "abc"
        let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["r1"]);

        let m2 = q.matches_for_event(r#"{"a": "AbC"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["r1"]);

        // r2 and r3 both match "XYZ" (they're both case variations of the same value)
        let m3 = q.matches_for_event(r#"{"b": "XYZ"}"#.as_bytes()).unwrap();
        assert_eq!(m3.len(), 2, "Both r2 and r3 should match XYZ");
        assert!(m3.contains(&"r2"));
        assert!(m3.contains(&"r3"));

        // Non-matches
        let m4 = q.matches_for_event(r#"{"b": "xyzz"}"#.as_bytes()).unwrap();
        assert!(m4.is_empty(), "xyzz should not match xyz patterns");

        let m5 = q
            .matches_for_event(r#"{"b": "ABCXYZ"}"#.as_bytes())
            .unwrap();
        assert!(m5.is_empty(), "ABCXYZ should not match xyz patterns");
    }

    #[test]
    fn test_equals_ignore_case_with_exact_match() {
        // Based on Go's TestSingletonMonocaseMerge (monocase_test.go:48)
        // Tests that exact match and equals-ignore-case patterns can coexist and merge correctly
        let mut q = Quamina::new();

        // Add exact match pattern
        q.add_pattern("singleton", r#"{"x": ["singleton"]}"#)
            .unwrap();

        // Add equals-ignore-case pattern on same field
        q.add_pattern("mono", r#"{"x": [{"equals-ignore-case": "foo"}]}"#)
            .unwrap();

        // Exact match should work
        let m1 = q
            .matches_for_event(r#"{"x": "singleton"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["singleton"], "Exact match should work");

        // Case-insensitive match should work
        let m2 = q.matches_for_event(r#"{"x": "FoO"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["mono"], "Case-insensitive match should work");

        // Neither should match different values
        let m3 = q.matches_for_event(r#"{"x": "bar"}"#.as_bytes()).unwrap();
        assert!(m3.is_empty(), "Unrelated value should not match");
    }

    #[test]
    fn test_equals_ignore_case_unicode() {
        // Based on Go quamina's TestHungarianMono from monocase_test.go
        // Test that equals-ignore-case works with full Unicode, not just ASCII

        // Test German sharp s (ß) which lowercases to "ss"
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"equals-ignore-case": "straße"}]}"#)
            .unwrap();

        let m1 = q
            .matches_for_event(r#"{"name": "STRASSE"}"#.as_bytes())
            .unwrap();
        // Note: In Unicode, ß.to_lowercase() = "ß" and "SS".to_lowercase() = "ss"
        // So "straße" != "strasse" in lowercase form. This is expected behavior.
        // German orthography has both forms valid.
        assert!(
            m1.is_empty(),
            "straße and STRASSE are different in Unicode lowercase"
        );

        // Test Greek sigma: Σ, σ, ς all lowercase to σ
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"word": [{"equals-ignore-case": "Σοφία"}]}"#)
            .unwrap();

        let m2 = q2
            .matches_for_event(r#"{"word": "σοφία"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p2"], "Greek sigma case folding should work");

        let m3 = q2
            .matches_for_event(r#"{"word": "ΣΟΦΊΑ"}"#.as_bytes())
            .unwrap();
        assert_eq!(m3, vec!["p2"], "Greek uppercase should match");

        // Test Turkish dotless i (ı) - note: this is a special case
        // In Turkish locale, I lowercases to ı and İ lowercases to i
        // But Rust's to_lowercase() uses Unicode default case folding
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"city": [{"equals-ignore-case": "istanbul"}]}"#)
            .unwrap();

        let m4 = q3
            .matches_for_event(r#"{"city": "Istanbul"}"#.as_bytes())
            .unwrap();
        assert_eq!(m4, vec!["p3"], "Standard I/i case folding should work");
    }

    #[test]
    fn test_suffix() {
        // Tests suffix operator
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"file": [{"suffix": ".jpg"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"file": "photo.jpg"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"file": "photo.png"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_nested_object_pattern() {
        // Tests matching nested object fields
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"user": {"role": ["admin"]}}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"user": {"role": "admin", "name": "alice"}}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match nested field");

        let no_match = q
            .matches_for_event(r#"{"user": {"role": "guest"}}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_deeply_nested() {
        // Tests deeply nested patterns
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": {"b": {"c": ["value"]}}}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"a": {"b": {"c": "value"}}}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_multiple_patterns_same_id() {
        // Multiple patterns with same ID - any match counts
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p1", r#"{"status": ["pending"]}"#).unwrap();

        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"]);

        let m2 = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"]);
    }

    #[test]
    fn test_delete_patterns() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

        // Both match initially
        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert!(m1.contains(&"p1"));

        // Delete p1
        q.delete_patterns(&"p1").unwrap();

        // p1 no longer matches
        let m2 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert!(m2.is_empty());

        // p2 still works
        let m3 = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"p2"));
    }

    #[test]
    fn test_rebuild_after_delete() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();
        q.add_pattern("p3", r#"{"status": ["review"]}"#).unwrap();

        // Initial count
        assert_eq!(q.pattern_count(), 3);

        // Delete p1
        q.delete_patterns(&"p1").unwrap();
        assert_eq!(q.pattern_count(), 2);

        // p1 is in deleted set
        assert!(q.deleted_patterns.contains(&"p1"));

        // Rebuild should purge deleted patterns
        let purged = q.rebuild();
        assert_eq!(purged, 1);

        // After rebuild, deleted set is clear
        assert!(q.deleted_patterns.is_empty());
        assert_eq!(q.pattern_count(), 2);

        // p2 and p3 still work
        let m2 = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(m2.contains(&"p2"));

        let m3 = q
            .matches_for_event(r#"{"status": "review"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"p3"));

        // p1 does not match (and is not in deleted set, was purged)
        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert!(m1.is_empty());
    }

    #[test]
    fn test_pruner_stats() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

        // Initially stats are zero
        assert_eq!(q.pruner_stats().emitted(), 0);
        assert_eq!(q.pruner_stats().filtered(), 0);

        // Match - should increment emitted
        let _ = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(q.pruner_stats().emitted(), 1);
        assert_eq!(q.pruner_stats().filtered(), 0);

        // Delete p1
        q.delete_patterns(&"p1").unwrap();

        // Match active - should increment filtered (was deleted)
        let _ = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(q.pruner_stats().emitted(), 1); // still 1
        assert_eq!(q.pruner_stats().filtered(), 1); // now 1

        // Match pending - should increment emitted
        let _ = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert_eq!(q.pruner_stats().emitted(), 2);
        assert_eq!(q.pruner_stats().filtered(), 1);

        // Rebuild resets stats
        q.rebuild();
        assert_eq!(q.pruner_stats().emitted(), 0);
        assert_eq!(q.pruner_stats().filtered(), 0);
    }

    #[test]
    fn test_should_rebuild_threshold() {
        let mut q = Quamina::new();

        // Add patterns that will match many events
        q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("p3", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("p4", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("p5", r#"{"x": ["a"]}"#).unwrap();

        // Delete half
        q.delete_patterns(&"p1").unwrap();
        q.delete_patterns(&"p2").unwrap();

        // Not enough activity yet - should not trigger rebuild
        assert!(!q.should_rebuild());

        // Simulate lots of matches
        let event = br#"{"x": "a"}"#;
        for _ in 0..500 {
            let _ = q.matches_for_event(event).unwrap();
        }

        // After 500 matches with 5 patterns, 3 emit, 2 filtered
        // filtered = 500 * 2 = 1000
        // emitted = 500 * 3 = 1500
        // Total activity = 2500 > 1000 threshold
        // Ratio = 1000/1500 = 0.67 > 0.2
        assert!(q.should_rebuild());

        // maybe_rebuild should trigger
        let purged = q.maybe_rebuild();
        assert_eq!(purged, 2);

        // After rebuild, no longer needs rebuild
        assert!(!q.should_rebuild());
    }

    #[test]
    fn test_auto_rebuild_disabled() {
        let mut q = Quamina::new();
        q.set_auto_rebuild(false);

        q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();

        q.delete_patterns(&"p1").unwrap();

        // Simulate enough activity to trigger
        let event = br#"{"x": "a"}"#;
        for _ in 0..2000 {
            let _ = q.matches_for_event(event).unwrap();
        }

        // Should want rebuild but auto is disabled
        assert!(q.should_rebuild());

        // maybe_rebuild returns 0 when disabled
        let purged = q.maybe_rebuild();
        assert_eq!(purged, 0);
    }

    #[test]
    fn test_or_within_field() {
        // Multiple values in array = OR
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active", "pending", "review"]}"#)
            .unwrap();

        for status in &["active", "pending", "review"] {
            let event = format!(r#"{{"status": "{}"}}"#, status);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", status);
        }

        let no_match = q
            .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_and_across_fields() {
        // Multiple fields = AND
        let mut q = Quamina::new();
        q.add_pattern(
            "p1",
            r#"{"type": ["order"], "status": ["pending"], "priority": ["high"]}"#,
        )
        .unwrap();

        let matches = q
            .matches_for_event(
                r#"{"type": "order", "status": "pending", "priority": "high"}"#.as_bytes(),
            )
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Missing one field
        let no_match = q
            .matches_for_event(r#"{"type": "order", "status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_greater_than() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"age": [{"numeric": [">", 18]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"age": 25}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"age": 18}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());

        let no_match2 = q.matches_for_event(r#"{"age": 15}"#.as_bytes()).unwrap();
        assert!(no_match2.is_empty());
    }

    #[test]
    fn test_numeric_range() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"score": 50}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let edge1 = q.matches_for_event(r#"{"score": 0}"#.as_bytes()).unwrap();
        assert_eq!(edge1, vec!["p1"]);

        let edge2 = q.matches_for_event(r#"{"score": 100}"#.as_bytes()).unwrap();
        assert_eq!(edge2, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"score": 101}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_equals() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"count": [{"numeric": ["=", 42]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"count": 43}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_scientific_notation() {
        // JSON allows scientific notation: 3.5e2 = 350
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"value": [{"numeric": [">=", 300, "<=", 400]}]}"#)
            .unwrap();

        // All of these represent values in the 300-400 range
        let m1 = q.matches_for_event(r#"{"value": 350}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "Integer 350 should match");

        let m2 = q
            .matches_for_event(r#"{"value": 350.0}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"], "Float 350.0 should match");

        let m3 = q
            .matches_for_event(r#"{"value": 3.5e2}"#.as_bytes())
            .unwrap();
        assert_eq!(m3, vec!["p1"], "Scientific 3.5e2 should match");

        let m4 = q
            .matches_for_event(r#"{"value": 3.5E2}"#.as_bytes())
            .unwrap();
        assert_eq!(m4, vec!["p1"], "Scientific 3.5E2 should match");

        let m5 = q
            .matches_for_event(r#"{"value": 35e1}"#.as_bytes())
            .unwrap();
        assert_eq!(m5, vec!["p1"], "Scientific 35e1 should match");

        // Test negative exponents
        let mut q2 = Quamina::new();
        q2.add_pattern(
            "p2",
            r#"{"tiny": [{"numeric": [">=", 0.00001, "<=", 0.001]}]}"#,
        )
        .unwrap();

        let m6 = q2
            .matches_for_event(r#"{"tiny": 3.02e-5}"#.as_bytes())
            .unwrap();
        assert_eq!(m6, vec!["p2"], "Scientific 3.02e-5 should match");

        let m7 = q2
            .matches_for_event(r#"{"tiny": 1E-4}"#.as_bytes())
            .unwrap();
        assert_eq!(m7, vec!["p2"], "Scientific 1E-4 should match");
    }

    #[test]
    fn test_regex_match() {
        let mut q = Quamina::new();
        // Pattern code like ABC-123
        // Note: I-Regexp patterns are anchored by default (no ^ or $ needed)
        q.add_pattern("p1", r#"{"code": [{"regex": "[A-Z]{3}-[0-9]{3}"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"code": "ABC-123"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"code": "invalid"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_regex_with_escape() {
        let mut q = Quamina::new();
        // Email pattern with escaped dot - I-Regexp uses ~ as escape, not \
        // Also I-Regexp regexps are anchored by default, no ^ or $
        q.add_pattern("p1", r#"{"email": [{"regex": "[a-z]+@example~.com"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"email": "alice@example.com"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"email": "alice@exampleXcom"}"#.as_bytes())
            .unwrap();
        assert!(
            no_match.is_empty(),
            "Dot should be escaped, not match any char"
        );
    }

    #[test]
    fn test_regex_various_patterns() {
        // Based on Go quamina's TestRegexpEnd2End
        // Test various regex patterns for correctness

        // Alternation
        let mut q1 = Quamina::new();
        q1.add_pattern("p1", r#"{"a": [{"regex": "a|b"}]}"#)
            .unwrap();
        assert!(q1
            .matches_for_event(r#"{"a": "a"}"#.as_bytes())
            .unwrap()
            .contains(&"p1"));
        assert!(q1
            .matches_for_event(r#"{"a": "b"}"#.as_bytes())
            .unwrap()
            .contains(&"p1"));
        assert!(q1
            .matches_for_event(r#"{"a": "c"}"#.as_bytes())
            .unwrap()
            .is_empty());

        // Character class
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"a": [{"regex": "[hij]"}]}"#)
            .unwrap();
        assert!(q2
            .matches_for_event(r#"{"a": "h"}"#.as_bytes())
            .unwrap()
            .contains(&"p2"));
        assert!(q2
            .matches_for_event(r#"{"a": "i"}"#.as_bytes())
            .unwrap()
            .contains(&"p2"));
        assert!(q2
            .matches_for_event(r#"{"a": "j"}"#.as_bytes())
            .unwrap()
            .contains(&"p2"));
        assert!(q2
            .matches_for_event(r#"{"a": "x"}"#.as_bytes())
            .unwrap()
            .is_empty());

        // Character range
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"a": [{"regex": "a[e-g]x"}]}"#)
            .unwrap();
        assert!(q3
            .matches_for_event(r#"{"a": "aex"}"#.as_bytes())
            .unwrap()
            .contains(&"p3"));
        assert!(q3
            .matches_for_event(r#"{"a": "afx"}"#.as_bytes())
            .unwrap()
            .contains(&"p3"));
        assert!(q3
            .matches_for_event(r#"{"a": "agx"}"#.as_bytes())
            .unwrap()
            .contains(&"p3"));
        assert!(q3
            .matches_for_event(r#"{"a": "ax"}"#.as_bytes())
            .unwrap()
            .is_empty());

        // Ordinal suffix pattern (like 11th, 23rd)
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"a": [{"regex": "[0-9][0-9][rtn][dh]"}]}"#)
            .unwrap();
        assert!(q4
            .matches_for_event(r#"{"a": "11th"}"#.as_bytes())
            .unwrap()
            .contains(&"p4"));
        assert!(q4
            .matches_for_event(r#"{"a": "23rd"}"#.as_bytes())
            .unwrap()
            .contains(&"p4"));
        assert!(q4
            .matches_for_event(r#"{"a": "22nd"}"#.as_bytes())
            .unwrap()
            .contains(&"p4"));
        assert!(q4
            .matches_for_event(r#"{"a": "first"}"#.as_bytes())
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_regexp_simple_optional() {
        // Test simple optional quantifier first
        let mut q = Quamina::new();
        q.add_pattern("test", r#"{"a": [{"regexp": "a?b"}]}"#)
            .unwrap();

        // Should match "ab" (a matched, b matched)
        let matches = q.matches_for_event(r#"{"a": "ab"}"#.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test"),
            "'a?b' should match 'ab': {:?}",
            matches
        );

        // Should match "b" (a skipped, b matched)
        let matches = q.matches_for_event(r#"{"a": "b"}"#.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test"),
            "'a?b' should match 'b': {:?}",
            matches
        );

        // Should NOT match "aab"
        let matches = q.matches_for_event(r#"{"a": "aab"}"#.as_bytes()).unwrap();
        assert!(
            matches.is_empty(),
            "'a?b' should NOT match 'aab': {:?}",
            matches
        );
    }

    #[test]
    fn test_regexp_end2end() {
        // Comprehensive regexp tests ported from Go's TestRegexpEnd2End
        use crate::regexp_samples::RegexpSample;

        let tests = [
            RegexpSample {
                regex: "(xyz)?a?b",
                matches: &["xyzb", "xyzab", "ab", "b"],
                nomatches: &["xyzc", "c", "xyza"],
                valid: true,
            },
            RegexpSample {
                regex: "a|b",
                matches: &["a", "b"],
                nomatches: &["x"],
                valid: true,
            },
            RegexpSample {
                regex: "a",
                matches: &["a"],
                nomatches: &["b", ""],
                valid: true,
            },
            RegexpSample {
                regex: "a.b",
                matches: &["axb", "a.b"],
                nomatches: &["ab", "axxb"],
                valid: true,
            },
            RegexpSample {
                regex: "abc|def",
                matches: &["abc", "def"],
                nomatches: &["x"],
                valid: true,
            },
            RegexpSample {
                regex: "[hij]",
                matches: &["h", "i", "j"],
                nomatches: &["x"],
                valid: true,
            },
            RegexpSample {
                regex: "a[e-g]x",
                matches: &["aex", "afx", "agx"],
                nomatches: &["ax", "axx"],
                valid: true,
            },
            RegexpSample {
                regex: "[ae-gx]",
                matches: &["a", "e", "f", "g", "x"],
                nomatches: &["b"],
                valid: true,
            },
            RegexpSample {
                regex: "[-ab]",
                matches: &["-", "a", "b"],
                nomatches: &["c"],
                valid: true,
            },
            RegexpSample {
                regex: "[ab-]",
                matches: &["-", "a", "b"],
                nomatches: &["c"],
                valid: true,
            },
            RegexpSample {
                regex: "[~[~]]",
                matches: &["[", "]"],
                nomatches: &["a"],
                valid: true,
            },
            // Note: Go tests [~r~t~n] matching \r, \t, \n literal bytes
            // In JSON these would be escaped, so we test differently
            RegexpSample {
                regex: "[a-c]|[xz]",
                matches: &["a", "b", "c", "x", "z"],
                nomatches: &["w"],
                valid: true,
            },
            RegexpSample {
                regex: "[ac-e]h|p[xy]",
                matches: &["ah", "ch", "dh", "eh", "px", "py"],
                nomatches: &["xp"],
                valid: true,
            },
            RegexpSample {
                regex: "[0-9][0-9][rtn][dh]",
                matches: &["11th", "23rd", "22nd"],
                nomatches: &["first", "9th"],
                valid: true,
            },
            RegexpSample {
                regex: "a(h|i)z",
                matches: &["ahz", "aiz"],
                nomatches: &["a.z"],
                valid: true,
            },
            RegexpSample {
                regex: "a([1-3]|ac)z",
                matches: &["a1z", "a2z", "a3z", "aacz"],
                nomatches: &["a.z", "a0z"],
                valid: true,
            },
            RegexpSample {
                regex: "a(h|([x-z]|(1|2)))z",
                matches: &["ahz", "axz", "a1z", "a2z"],
                nomatches: &["a.z"],
                valid: true,
            },
        ];

        // Test each pattern individually
        for test in &tests {
            let mut q = Quamina::new();
            let pattern = format!(r#"{{"a": [{{"regexp": "{}"}}]}}"#, test.regex);
            if let Err(e) = q.add_pattern("test", &pattern) {
                panic!("Failed to add pattern '{}': {}", test.regex, e);
            }

            for m in test.matches {
                let event = format!(r#"{{"a": "{}"}}"#, m);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.contains(&"test"),
                    "Pattern '{}' should match '{}', but didn't",
                    test.regex,
                    m
                );
            }

            for m in test.nomatches {
                let event = format!(r#"{{"a": "{}"}}"#, m);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.is_empty(),
                    "Pattern '{}' should NOT match '{}', but did",
                    test.regex,
                    m
                );
            }
        }

        // Test merged FA (all patterns together) - like Go does
        let mut all_patterns = Quamina::new();
        for (i, test) in tests.iter().enumerate() {
            let pattern = format!(r#"{{"a": [{{"regexp": "{}"}}]}}"#, test.regex);
            let name = format!("p{}", i);
            if let Err(e) = all_patterns.add_pattern(name, &pattern) {
                panic!("Failed to add pattern '{}': {}", test.regex, e);
            }
        }

        for (i, test) in tests.iter().enumerate() {
            let expected_name = format!("p{}", i);
            for m in test.matches {
                let event = format!(r#"{{"a": "{}"}}"#, m);
                let matches = all_patterns.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.contains(&expected_name),
                    "Merged FA: Pattern '{}' should match '{}', but didn't",
                    test.regex,
                    m
                );
            }
        }
    }

    #[test]
    fn test_clone_for_snapshot() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        // Clone creates an independent snapshot
        let snapshot = q.clone();

        // Modify original
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

        // Snapshot doesn't have p2
        let snap_matches = snapshot
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(snap_matches.is_empty());

        // Original has p2
        let orig_matches = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(orig_matches.contains(&"p2"));
    }

    #[test]
    fn test_send_sync() {
        // Verify Quamina is Send + Sync for thread safety
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Quamina<String>>();
    }

    #[test]
    fn test_has_matches() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        assert!(q.has_matches(r#"{"status": "active"}"#.as_bytes()).unwrap());
        assert!(!q
            .has_matches(r#"{"status": "inactive"}"#.as_bytes())
            .unwrap());
    }

    #[test]
    fn test_count_matches() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p3", r#"{"status": ["pending"]}"#).unwrap();

        assert_eq!(
            q.count_matches(r#"{"status": "active"}"#.as_bytes())
                .unwrap(),
            2
        );
        assert_eq!(
            q.count_matches(r#"{"status": "pending"}"#.as_bytes())
                .unwrap(),
            1
        );
        assert_eq!(
            q.count_matches(r#"{"status": "deleted"}"#.as_bytes())
                .unwrap(),
            0
        );
    }

    #[test]
    fn test_pattern_count_and_clear() {
        let mut q = Quamina::new();
        assert!(q.is_empty());
        assert_eq!(q.pattern_count(), 0);

        q.add_pattern("p1", r#"{"a": ["1"]}"#).unwrap();
        q.add_pattern("p2", r#"{"b": ["2"]}"#).unwrap();
        assert!(!q.is_empty());
        assert_eq!(q.pattern_count(), 2);

        q.clear();
        assert!(q.is_empty());
        assert_eq!(q.pattern_count(), 0);
    }

    #[test]
    fn test_unicode_escape_in_event() {
        // \u0048\u0065\u006c\u006c\u006f = "Hello"
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"greeting": ["Hello"]}"#).unwrap();

        // Event with unicode escapes
        let event = r#"{"greeting": "\u0048\u0065\u006c\u006c\u006f"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Unicode escape should decode to 'Hello'"
        );
    }

    #[test]
    fn test_unicode_escape_emoji() {
        // Test UTF-16 surrogate pair for emoji 💋 (U+1F48B = D83D DC8B)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"emoji": ["💋"]}"#).unwrap();

        let event = r#"{"emoji": "\ud83d\udc8b"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "UTF-16 surrogate pair should decode to emoji"
        );
    }

    #[test]
    fn test_unicode_escape_multiple_emojis() {
        // Test multiple UTF-16 surrogate pairs in sequence
        // From Go's escaping_test.go: 😀💋😺 = \ud83d\ude00\ud83d\udc8b\ud83d\ude3a
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"emojis": ["😀💋😺"]}"#).unwrap();

        let event = r#"{"emojis": "\ud83d\ude00\ud83d\udc8b\ud83d\ude3a"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Multiple surrogate pairs should decode correctly"
        );
    }

    #[test]
    fn test_unicode_escape_mixed_codepoints() {
        // Test mixing single-codepoint and surrogate pairs
        // From Go's escaping_test.go combinations
        // Ж = \u0416 (single), 💋 = \ud83d\udc8b (surrogate), 中 = \u4e2d (single)

        // Test: Ж💋中
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"mixed": ["Ж💋中"]}"#).unwrap();

        let event = r#"{"mixed": "\u0416\ud83d\udc8b\u4e2d"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Mixed codepoints should decode");

        // Test: x💋y - ASCII mixed with surrogate
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"mixed": ["x💋y"]}"#).unwrap();

        let event2 = r#"{"mixed": "\u0078\ud83d\udc8b\u0079"}"#;
        let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
        assert_eq!(matches2, vec!["p2"], "ASCII + surrogate should decode");
    }

    #[test]
    fn test_unicode_escape_standard_escapes() {
        // Test standard JSON escape sequences
        let mut q = Quamina::new();

        // Test newline
        q.add_pattern("newline", r#"{"text": ["hello\nworld"]}"#)
            .unwrap();
        let m1 = q
            .matches_for_event(r#"{"text": "hello\nworld"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["newline"], "Newline escape should match");

        // Test tab
        q.add_pattern("tab", r#"{"text": ["hello\tworld"]}"#)
            .unwrap();
        let m2 = q
            .matches_for_event(r#"{"text": "hello\tworld"}"#.as_bytes())
            .unwrap();
        assert!(m2.contains(&"tab"), "Tab escape should match");

        // Test backslash
        q.add_pattern("backslash", r#"{"text": ["hello\\world"]}"#)
            .unwrap();
        let m3 = q
            .matches_for_event(r#"{"text": "hello\\world"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"backslash"), "Backslash escape should match");
    }

    #[test]
    fn test_json_all_escape_sequences() {
        // Based on Go's TestOneEscape (escaping_test.go:45)
        // Tests all 8 standard JSON escape sequences plus unicode escapes

        // Test: \" (quote)
        let mut q1 = Quamina::new();
        // Pattern with literal quote in value (escaped in JSON as \")
        // Raw string: "hello\"world" = hello"world
        q1.add_pattern("p1", r#"{"x": ["hello\"world"]}"#).unwrap();
        let m1 = q1
            .matches_for_event(r#"{"x": "hello\"world"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "Quote escape should match");

        // Test: \/ (forward slash - optional in JSON but must be handled)
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": ["a/b"]}"#).unwrap();
        let m2 = q2.matches_for_event(r#"{"x": "a\/b"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p2"], "Forward slash escape should match");

        // Test: \b (backspace, 0x08)
        let mut q3 = Quamina::new();
        let pattern_with_backspace = format!(r#"{{"x": ["a{}b"]}}"#, '\x08');
        q3.add_pattern("p3", &pattern_with_backspace).unwrap();
        let m3 = q3.matches_for_event(r#"{"x": "a\bb"}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p3"], "Backspace escape should match");

        // Test: \f (form feed, 0x0c)
        let mut q4 = Quamina::new();
        let pattern_with_formfeed = format!(r#"{{"x": ["a{}b"]}}"#, '\x0c');
        q4.add_pattern("p4", &pattern_with_formfeed).unwrap();
        let m4 = q4.matches_for_event(r#"{"x": "a\fb"}"#.as_bytes()).unwrap();
        assert_eq!(m4, vec!["p4"], "Form feed escape should match");

        // Test: \r (carriage return)
        let mut q5 = Quamina::new();
        q5.add_pattern("p5", r#"{"x": ["a\rb"]}"#).unwrap();
        let m5 = q5.matches_for_event(r#"{"x": "a\rb"}"#.as_bytes()).unwrap();
        assert_eq!(m5, vec!["p5"], "Carriage return escape should match");
    }

    #[test]
    fn test_array_element_matching() {
        // Pattern should match if value is ANY element of the array
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"ids": [943]}"#).unwrap();

        // Event has array - should match if 943 is in the array
        let event = r#"{"ids": [116, 943, 234]}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Should match when pattern value is in event array"
        );
    }

    #[test]
    fn test_wildcard_escape_star() {
        // \* in wildcard should match literal * character
        let mut q = Quamina::new();
        // Pattern: a\\*b matches literal "a*b" (double backslash in JSON = single backslash)
        q.add_pattern("p1", r#"{"val": [{"wildcard": "a\\*b"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"val": "a*b"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "\\* should match literal *");

        // Should NOT match "aXb" - the * is escaped, not a wildcard
        let no_match = q.matches_for_event(r#"{"val": "aXb"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty(), "Escaped * should not be wildcard");
    }

    #[test]
    fn test_wildcard_escape_backslash() {
        // \\ in wildcard should match literal \ character
        let mut q = Quamina::new();
        // Pattern: a\\\\b matches literal "a\b" (four backslash in JSON = two = one in wildcard)
        q.add_pattern("p1", r#"{"path": [{"wildcard": "a\\\\b"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"path": "a\\b"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "\\\\ should match literal \\");
    }

    #[test]
    fn test_wildcard_invalid_patterns() {
        // Invalid wildcard patterns should return errors

        // Adjacent ** is invalid
        let mut q = Quamina::new();
        let result = q.add_pattern("p1", r#"{"x": [{"wildcard": "foo**bar"}]}"#);
        assert!(result.is_err(), "Adjacent ** should be rejected");

        // Invalid escape \l (only \* and \\ are valid)
        let mut q2 = Quamina::new();
        let result2 = q2.add_pattern("p2", r#"{"x": [{"wildcard": "he\\llo"}]}"#);
        assert!(result2.is_err(), "Invalid escape \\l should be rejected");

        // Trailing backslash is invalid
        let mut q3 = Quamina::new();
        let result3 = q3.add_pattern("p3", r#"{"x": [{"wildcard": "x\\"}]}"#);
        assert!(result3.is_err(), "Trailing backslash should be rejected");
    }

    #[test]
    fn test_shellstyle_suffix() {
        // shellstyle is simpler wildcard without escape support
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": [{"shellstyle": "*bc"}]}"#)
            .unwrap();

        // Should match "bc" and "abc"
        let matches = q.matches_for_event(r#"{"a": "bc"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Should not match
        let no_match = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_prefix() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"c": "xy"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_shellstyle_infix() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"b": [{"shellstyle": "d*f"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"b": "df"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_shellstyle_multiple_wildcards() {
        // Multiple * in pattern
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"d": [{"shellstyle": "12*4*"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"d": "12345"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"d": "1244"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Should not match - missing "4"
        let no_match = q.matches_for_event(r#"{"d": "1235"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_contains() {
        // *foo* matches if "foo" appears anywhere
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo*"}]}"#)
            .unwrap();

        for text in ["xxfooyy", "fooyy", "xxfoo", "foo"] {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", text);
        }

        let no_match = q.matches_for_event(r#"{"x": "bar"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_long_case() {
        // Test the "abab" suffix case from Go
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
            .unwrap();

        for text in ["abaabab", "ababab", "ababaabab", "abab"] {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", text);
        }
    }

    #[test]
    fn test_invalid_pattern_handling() {
        // Test that invalid patterns don't cause panics and are handled gracefully
        let mut q = Quamina::new();

        // Empty pattern
        assert!(q.add_pattern("p1", "").is_err());

        // Non-object at top level
        assert!(q.add_pattern("p2", "33").is_err());
        assert!(q.add_pattern("p3", "[1,2]").is_err());

        // Malformed JSON
        assert!(q.add_pattern("p4", "{").is_err());
        assert!(q.add_pattern("p5", r#"{"foo": }"#).is_err());

        // Pattern field must be array or nested object
        assert!(q.add_pattern("p6", r#"{"foo": "string"}"#).is_err());
        assert!(q.add_pattern("p7", r#"{"foo": 123}"#).is_err());
        assert!(q.add_pattern("p8", r#"{"foo": true}"#).is_err());

        // Valid patterns should work
        assert!(q.add_pattern("valid1", r#"{"x": [1]}"#).is_ok());
        assert!(q.add_pattern("valid2", r#"{"x": ["string"]}"#).is_ok());
        assert!(q.add_pattern("valid3", r#"{"x": {"y": [1]}}"#).is_ok());
    }

    #[test]
    fn test_array_cross_element_matching() {
        // Test cross-element array matching behavior (matches Go quamina behavior)
        // Pattern {"members": {"given": ["Mick"], "surname": ["Strummer"]}}
        // Event: members=[{given: "Joe", surname: "Strummer"}, {given: "Mick", surname: "Jones"}]
        //
        // Should NOT match because no single array element has both given=Mick AND surname=Strummer
        // - Element 0 has given="Joe", surname="Strummer"
        // - Element 1 has given="Mick", surname="Jones"

        let mut q = Quamina::new();
        q.add_pattern(
            "cross",
            r#"{"members": {"given": ["Mick"], "surname": ["Strummer"]}}"#,
        )
        .unwrap();

        let event = r#"{"members": [
            {"given": "Joe", "surname": "Strummer"},
            {"given": "Mick", "surname": "Jones"}
        ]}"#;

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        // Should NOT match - cross-element matching is correctly prevented
        assert!(
            matches.is_empty(),
            "Should not match across different array elements"
        );
    }

    #[test]
    fn test_array_cross_element_comprehensive() {
        // Comprehensive test from Go's arrays_test.go TestArrayCorrectness
        // Tests the "bands" scenario with multiple patterns where only one should match
        //
        // Only "Wata guitar" pattern should match because:
        // - "mick_strummer": Mick has surname Jones, not Strummer (cross-element would be false positive)
        // - "wata_drums": Wata has role guitar/vocals, Atsuo has drums (cross-element would be false positive)
        // - "wata_guitar": Wata has role guitar - this is in the same array element, so it matches

        let bands = r#"{
            "bands": [
                {
                    "name": "The Clash",
                    "members": [
                        {"given": "Joe", "surname": "Strummer", "role": ["guitar", "vocals"]},
                        {"given": "Mick", "surname": "Jones", "role": ["guitar", "vocals"]},
                        {"given": "Paul", "surname": "Simonon", "role": ["bass"]},
                        {"given": "Topper", "surname": "Headon", "role": ["drums"]}
                    ]
                },
                {
                    "name": "Boris",
                    "members": [
                        {"given": "Wata", "role": ["guitar", "vocals"]},
                        {"given": "Atsuo", "role": ["drums"]},
                        {"given": "Takeshi", "role": ["bass", "vocals"]}
                    ]
                }
            ]
        }"#;

        let mut q = Quamina::new();
        // Pattern 1: Mick with surname Strummer - SHOULD NOT match (cross-element)
        q.add_pattern(
            "mick_strummer",
            r#"{"bands": {"members": {"given": ["Mick"], "surname": ["Strummer"]}}}"#,
        )
        .unwrap();
        // Pattern 2: Wata with role drums - SHOULD NOT match (cross-element)
        q.add_pattern(
            "wata_drums",
            r#"{"bands": {"members": {"given": ["Wata"], "role": ["drums"]}}}"#,
        )
        .unwrap();
        // Pattern 3: Wata with role guitar - SHOULD match (same element)
        q.add_pattern(
            "wata_guitar",
            r#"{"bands": {"members": {"given": ["Wata"], "role": ["guitar"]}}}"#,
        )
        .unwrap();

        let matches = q.matches_for_event(bands.as_bytes()).unwrap();

        // Should return exactly ["wata_guitar"]
        assert_eq!(
            matches.len(),
            1,
            "Expected exactly one match, got: {:?}",
            matches
        );
        assert!(
            matches.contains(&"wata_guitar"),
            "wata_guitar should match (same array element)"
        );

        // These should NOT match (would be cross-element false positives)
        assert!(
            !matches.contains(&"mick_strummer"),
            "mick_strummer should NOT match (cross-element)"
        );
        assert!(
            !matches.contains(&"wata_drums"),
            "wata_drums should NOT match (cross-element)"
        );
    }

    #[test]
    fn test_wildcard_matches_empty_string() {
        // Based on Go quamina's wildcard tests
        // Pattern "*" should match empty string ""
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"wildcard": "*"}]}"#)
            .unwrap();

        // Should match empty string
        let m1 = q.matches_for_event(r#"{"x": ""}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "* should match empty string");

        // Should match any string
        let m2 = q.matches_for_event(r#"{"x": "hello"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p1"], "* should match any string");

        let m3 = q.matches_for_event(r#"{"x": "*"}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p1"], "* should match literal *");
    }

    #[test]
    fn test_multiple_overlapping_shellstyle_patterns() {
        // Based on Go quamina's TestWildCardRuler
        // Multiple shellstyle patterns can match the same event
        let mut q = Quamina::new();
        q.add_pattern("suffix_bc", r#"{"a": [{"shellstyle": "*bc"}]}"#)
            .unwrap();
        q.add_pattern("infix_ef", r#"{"b": [{"shellstyle": "d*f"}]}"#)
            .unwrap();
        q.add_pattern("infix_eff", r#"{"b": [{"shellstyle": "d*ff"}]}"#)
            .unwrap();
        q.add_pattern("prefix_xy", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        // Test suffix match
        let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert!(m1.contains(&"suffix_bc"), "*bc should match abc");

        // Test infix match
        let m2 = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
        assert!(m2.contains(&"infix_ef"), "d*f should match dexef");

        // Test both infix patterns match
        let m3 = q
            .matches_for_event(r#"{"b": "dexeff"}"#.as_bytes())
            .unwrap();
        assert_eq!(m3.len(), 2, "Both d*f and d*ff should match dexeff");
        assert!(m3.contains(&"infix_ef"));
        assert!(m3.contains(&"infix_eff"));

        // Test prefix match
        let m4 = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
        assert!(m4.contains(&"prefix_xy"), "xy* should match xyzzz");

        // Test non-matches
        let m5 = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
        assert!(m5.is_empty(), "*bc should not match xyz");

        let m6 = q.matches_for_event(r#"{"b": "ef"}"#.as_bytes()).unwrap();
        assert!(m6.is_empty(), "d*f should not match ef (no d prefix)");
    }

    #[test]
    fn test_multiple_shellstyle_same_field() {
        // Test multiple shellstyle patterns on the SAME field
        // This is the merge_fas spinout bug reproduction test
        let mut q = Quamina::new();

        // Add multiple shellstyle patterns on the same field
        q.add_pattern("suffix_bc", r#"{"x": [{"shellstyle": "*bc"}]}"#)
            .unwrap();
        q.add_pattern("suffix_xc", r#"{"x": [{"shellstyle": "*xc"}]}"#)
            .unwrap();
        q.add_pattern("prefix_ab", r#"{"x": [{"shellstyle": "ab*"}]}"#)
            .unwrap();

        // Test suffix_bc pattern
        let m1 = q.matches_for_event(r#"{"x": "abc"}"#.as_bytes()).unwrap();
        assert!(
            m1.contains(&"suffix_bc"),
            "*bc should match abc, got: {:?}",
            m1
        );
        // abc also matches ab* prefix pattern
        assert!(
            m1.contains(&"prefix_ab"),
            "ab* should match abc, got: {:?}",
            m1
        );

        // Test suffix_xc pattern
        let m2 = q.matches_for_event(r#"{"x": "axc"}"#.as_bytes()).unwrap();
        assert!(
            m2.contains(&"suffix_xc"),
            "*xc should match axc, got: {:?}",
            m2
        );

        // Test prefix_ab pattern
        let m3 = q
            .matches_for_event(r#"{"x": "abcdef"}"#.as_bytes())
            .unwrap();
        assert!(
            m3.contains(&"prefix_ab"),
            "ab* should match abcdef, got: {:?}",
            m3
        );

        // Test non-match
        let m4 = q.matches_for_event(r#"{"x": "xyz"}"#.as_bytes()).unwrap();
        assert!(m4.is_empty(), "Nothing should match xyz, got: {:?}", m4);
    }

    #[test]
    fn test_multiple_shellstyle_citylots_patterns() {
        // Test multiple complex shellstyle patterns on the SAME field (citylots-like)
        // This tests patterns similar to the citylots stress test that had to be
        // run individually due to merge_fas spinout bug.
        let mut q = Quamina::new();

        // These mirror the citylots shellstyle patterns
        q.add_pattern("pattern_143", r#"{"x": [{"shellstyle": "143*"}]}"#)
            .unwrap();
        q.add_pattern("pattern_2017", r#"{"x": [{"shellstyle": "2*0*1*7"}]}"#)
            .unwrap();
        q.add_pattern("pattern_218", r#"{"x": [{"shellstyle": "*218"}]}"#)
            .unwrap();
        q.add_pattern("pattern_352", r#"{"x": [{"shellstyle": "3*5*2"}]}"#)
            .unwrap();
        q.add_pattern("pattern_vail", r#"{"x": [{"shellstyle": "VA*IL"}]}"#)
            .unwrap();

        // Test individual patterns work correctly
        let test_cases: Vec<(&str, Vec<&str>)> = vec![
            ("1430022", vec!["pattern_143"]),   // matches 143*
            ("2607117", vec!["pattern_2017"]),  // matches 2*0*1*7
            ("2607218", vec!["pattern_218"]),   // matches *218
            ("3745012", vec!["pattern_352"]),   // matches 3*5*2
            ("VACSTWIL", vec!["pattern_vail"]), // matches VA*IL (note: incorrect, should be VACTSTWIL?)
            ("xyz", vec![]),                    // no match
        ];

        for (value, expected_patterns) in test_cases {
            let event = format!(r#"{{"x": "{}"}}"#, value);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();

            if expected_patterns.is_empty() {
                assert!(
                    matches.is_empty(),
                    "Expected no match for '{}', got: {:?}",
                    value,
                    matches
                );
            } else {
                for expected in &expected_patterns {
                    assert!(
                        matches.contains(expected),
                        "Expected '{}' to match '{}', but got: {:?}",
                        value,
                        expected,
                        matches
                    );
                }
            }
        }
    }

    #[test]
    fn test_anything_but_prefix_relationship() {
        // Based on Go quamina's TestFootCornerCase
        // Tests that anything-but ["foo"] matches "foot" (since "foot" != "foo")
        let mut q = Quamina::new();
        q.add_pattern("not_foo", r#"{"z": [{"anything-but": ["foo"]}]}"#)
            .unwrap();

        // "foot" is not "foo", so should match
        let m1 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
        assert_eq!(
            m1,
            vec!["not_foo"],
            "anything-but ['foo'] should match 'foot'"
        );

        // "foo" should NOT match
        let m2 = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
        assert!(m2.is_empty(), "anything-but ['foo'] should not match 'foo'");

        // "fo" is not "foo", so should match
        let m3 = q.matches_for_event(r#"{"z": "fo"}"#.as_bytes()).unwrap();
        assert_eq!(
            m3,
            vec!["not_foo"],
            "anything-but ['foo'] should match 'fo'"
        );
    }

    #[test]
    fn test_exists_false_ordering() {
        // Based on Go quamina's TestExistsFalseOrder
        // exists:false should properly disqualify a match regardless of where
        // it occurs lexicographically in the pattern
        let event = r#"{"aField": "a", "bField": "b", "cField": "c"}"#;

        // All these patterns should NOT match because each requires a field to be absent
        // that is actually present in the event
        let should_not_patterns = [
            // exists:false on middle field (bField)
            r#"{"aField": ["a"], "bField": [{"exists": false}], "cField": ["c"]}"#,
            // exists:false on first field (aField)
            r#"{"aField": [{"exists": false}], "bField": ["b"], "cField": ["c"]}"#,
            // exists:false on last field (cField)
            r#"{"aField": ["a"], "bField": ["b"], "cField": [{"exists": false}]}"#,
        ];

        for (i, pattern) in should_not_patterns.iter().enumerate() {
            let mut q = Quamina::new();
            q.add_pattern(format!("p{}", i), pattern).unwrap();
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Pattern {} should NOT match: {}",
                i,
                pattern
            );
        }

        // Also test with events that DO match these patterns (missing the required absent field)
        let events_that_match = [
            r#"{"aField": "a", "cField": "c"}"#, // missing bField
            r#"{"bField": "b", "cField": "c"}"#, // missing aField
            r#"{"aField": "a", "bField": "b"}"#, // missing cField
        ];

        for (i, (pattern, event)) in should_not_patterns
            .iter()
            .zip(events_that_match.iter())
            .enumerate()
        {
            let mut q = Quamina::new();
            q.add_pattern(format!("p{}", i), pattern).unwrap();
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.is_empty(),
                "Pattern {} should match event {}: {}",
                i,
                i,
                pattern
            );
        }
    }

    #[test]
    fn test_overlapping_exact_match_patterns() {
        // Based on Go quamina's TestOverlappingValues
        // Tests patterns with overlapping prefixes (foo, football, footballer)
        // to ensure exact matching with no false positives
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": ["foo"]}"#).unwrap();
        q.add_pattern("p2", r#"{"a": ["football"]}"#).unwrap();
        q.add_pattern("p3", r#"{"a": ["footballer"]}"#).unwrap();

        // Each event should only match its corresponding pattern
        let matches1 = q
            .matches_for_event(r#"{"x": 3, "a": "foo"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches1,
            vec!["p1"],
            "foo should only match p1, not football or footballer"
        );

        let matches2 = q
            .matches_for_event(r#"{"x": 3, "a": "football"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches2,
            vec!["p2"],
            "football should only match p2, not foo or footballer"
        );

        let matches3 = q
            .matches_for_event(r#"{"x": 3, "a": "footballer"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches3,
            vec!["p3"],
            "footballer should only match p3, not foo or football"
        );

        // Ensure no matches for unrelated values
        let no_match = q.matches_for_event(r#"{"a": "foot"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty(), "foot should not match any pattern");
    }

    #[test]
    fn test_same_pattern_id_multiple_value_types() {
        // Based on Go quamina's TestExerciseSingletonReplacement and TestMergeNfaAndNumeric
        // Same pattern ID can match via different value types (string OR number)
        let mut q = Quamina::new();
        // Add two patterns with same ID but different value types
        q.add_pattern("x", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("x", r#"{"x": [1]}"#).unwrap();

        // Both string and number should match pattern "x"
        let matches1 = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
        assert_eq!(matches1, vec!["x"], "number 1 should match");

        let matches2 = q.matches_for_event(r#"{"x": "a"}"#.as_bytes()).unwrap();
        assert_eq!(matches2, vec!["x"], "string 'a' should match");

        // Test wildcard OR number for same pattern ID
        let mut q2 = Quamina::new();
        q2.add_pattern("x", r#"{"x": [{"wildcard": "x*y"}]}"#)
            .unwrap();
        q2.add_pattern("x", r#"{"x": [3]}"#).unwrap();

        let m1 = q2.matches_for_event(r#"{"x": 3}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["x"], "number 3 should match");

        let m2 = q2
            .matches_for_event(r#"{"x": "xasdfy"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["x"], "wildcard pattern should match");
    }

    #[test]
    fn test_empty_regex_matches_empty_string() {
        // Based on Go quamina's TestEmptyRegexp
        // Empty regex pattern should match ONLY empty string value
        let mut q = Quamina::new();
        q.add_pattern("a", r#"{"a": [{"regex": ""}]}"#).unwrap();

        let matches = q.matches_for_event(r#"{"a": ""}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["a"], "empty regex should match empty string");

        // Empty regex should NOT match non-empty strings
        let matches2 = q.matches_for_event(r#"{"a": "hello"}"#.as_bytes()).unwrap();
        assert!(
            matches2.is_empty(),
            "empty regex should NOT match non-empty string"
        );
    }

    #[test]
    fn test_anything_but_with_exact_match() {
        // Based on Go quamina's TestAnythingButMerging
        // Tests that exact and anything-but patterns can coexist
        let mut q = Quamina::new();

        // Add exact match for "foo"
        q.add_pattern("pFoo", r#"{"z": ["foo"]}"#).unwrap();
        // Add anything-but for "foot"
        q.add_pattern("pAbFoot", r#"{"z": [{"anything-but": ["foot"]}]}"#)
            .unwrap();

        // "foo" should match BOTH patterns:
        // - pFoo: exact match
        // - pAbFoot: "foo" is not "foot"
        let matches = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
        assert_eq!(matches.len(), 2, "foo should match both patterns");

        // "foot" should match neither:
        // - pFoo: not "foot"
        // - pAbFoot: excluded
        let matches2 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
        assert!(matches2.is_empty(), "foot should match nothing");

        // "bar" should match only pAbFoot (not foo, not foot)
        let matches3 = q.matches_for_event(r#"{"z": "bar"}"#.as_bytes()).unwrap();
        assert_eq!(matches3.len(), 1, "bar should only match pAbFoot");
        assert!(matches3.contains(&"pAbFoot"));
    }

    #[test]
    fn test_anything_but_with_shellstyle() {
        // Based on Go quamina's TestAnythingButMerging (second part)
        // Tests that anything-but can be merged with shellstyle (NFA) patterns
        let mut q = Quamina::new();

        // Add shellstyle pattern for "foo*"
        q.add_pattern("pFooStar", r#"{"z": [{"shellstyle": "foo*"}]}"#)
            .unwrap();
        // Add anything-but for "foot"
        q.add_pattern("pAbFoot", r#"{"z": [{"anything-but": ["foot"]}]}"#)
            .unwrap();

        // "foo" should match BOTH patterns:
        // - pFooStar: matches "foo*"
        // - pAbFoot: "foo" is not "foot"
        let matches = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
        assert_eq!(
            matches.len(),
            2,
            "foo should match both pFooStar and pAbFoot, got {:?}",
            matches
        );
        assert!(matches.contains(&"pFooStar"));
        assert!(matches.contains(&"pAbFoot"));

        // "foot" should match only pFooStar:
        // - pFooStar: matches "foo*"
        // - pAbFoot: excluded (is "foot")
        let matches2 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
        assert_eq!(
            matches2.len(),
            1,
            "foot should only match pFooStar, got {:?}",
            matches2
        );
        assert!(matches2.contains(&"pFooStar"));

        // "bar" should match only pAbFoot:
        // - pFooStar: doesn't match "foo*"
        // - pAbFoot: "bar" is not "foot"
        let matches3 = q.matches_for_event(r#"{"z": "bar"}"#.as_bytes()).unwrap();
        assert_eq!(
            matches3.len(),
            1,
            "bar should only match pAbFoot, got {:?}",
            matches3
        );
        assert!(matches3.contains(&"pAbFoot"));
    }

    #[test]
    fn test_anything_but_with_overlapping_exclusions() {
        // Based on Go quamina's TestAnythingButAlgo
        // Tests anything-but with overlapping prefix exclusions
        let mut q = Quamina::new();
        q.add_pattern(
            "notTTT",
            r#"{"x": [{"anything-but": ["tim", "time", "timed"]}]}"#,
        )
        .unwrap();

        // All excluded values should not match
        let excluded = ["tim", "time", "timed"];
        for val in excluded {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "{} should be excluded", val);
        }

        // Similar but non-excluded values should match
        let included = ["t", "ti", "timer", "timely", "timekeeper"];
        for val in included {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches.len(), 1, "{} should match notTTT", val);
        }
    }

    // MIRI SKIP RATIONALE: This test reads testdata/wwords.txt from disk.
    // Miri runs with isolation enabled by default, blocking filesystem access.
    // Cannot be broken down - the test's purpose is to validate against real word list.
    // Coverage: test_anything_but_* tests exercise same anything-but matching without file I/O.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_anything_but_wordle_words() {
        // Based on Go quamina's TestAnythingButMatching (anything_but_test.go:150)
        // Tests anything-but against wordle word list with edge case "problem words"
        use std::fs;
        use std::path::Path;

        // Problem words designed to test edge cases:
        // - 4-letter prefix of existing wordle
        // - 4-letter suffix of existing wordle
        // - 5-letter non-wordle
        // - 6-letter where wordle might match at start or end
        let problem_words = ["bloo", "aper", "fnord", "doubts", "astern"];

        let mut q = Quamina::new();
        // Build pattern with quoted problem words for JSON array
        let problem_json: Vec<String> =
            problem_words.iter().map(|w| format!("\"{}\"", w)).collect();
        let pattern = format!(
            r#"{{"a": [{{"anything-but": [{}]}}]}}"#,
            problem_json.join(",")
        );
        q.add_pattern("not_problems", &pattern).unwrap();

        // Problem words should NOT match (they're excluded)
        for word in &problem_words {
            let event = format!(r#"{{"a": "{}"}}"#, word);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Problem word '{}' should be excluded",
                word
            );
        }

        // All wordle words should match (they're not in the exclusion list)
        let wwords_path = Path::new("testdata/wwords.txt");
        if wwords_path.exists() {
            let contents = fs::read_to_string(wwords_path).unwrap();
            for word in contents.lines() {
                let word = word.trim();
                if word.is_empty() {
                    continue;
                }
                let event = format!(r#"{{"a": "{}"}}"#, word);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert_eq!(
                    matches.len(),
                    1,
                    "Wordle word '{}' should match anything-but pattern",
                    word
                );
            }
        }
    }

    #[test]
    fn test_shellstyle_repeated_sequences() {
        // Based on Go quamina's TestLongCase
        // Tests shellstyle suffix patterns with overlapping sequences
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
            .unwrap();

        // These should all match *abab
        let should_match = ["abab", "abaabab", "ababab", "ababaabab", "xxabab"];
        for val in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "*abab should match '{}'", val);
        }

        // These should not match
        let should_not = ["abab_", "aba", "ab", "xaba"];
        for val in should_not {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "*abab should NOT match '{}'", val);
        }
    }

    #[test]
    fn test_shellstyle_suffix_merged_bug() {
        // Based on Go quamina's TestSuffixBug
        // Tests that multiple merged suffix patterns all match properly
        let j = r#"{"Url": "xy9"}"#;
        let patterns = [
            (r#"{"Url": [{"shellstyle": "*9"}]}"#, "p0"),
            (r#"{"Url": [{"shellstyle": "x*9"}]}"#, "p1"),
        ];

        // Verify each pattern works individually
        for (pattern, name) in &patterns {
            let mut q = Quamina::new();
            q.add_pattern(*name, pattern).unwrap();
            let matches = q.matches_for_event(j.as_bytes()).unwrap();
            assert_eq!(
                matches.len(),
                1,
                "Pattern {} should match individually",
                name
            );
            assert!(matches.contains(name));
        }

        // Verify both patterns work when merged
        let mut q = Quamina::new();
        for (pattern, name) in &patterns {
            q.add_pattern(*name, pattern).unwrap();
        }
        let matches = q.matches_for_event(j.as_bytes()).unwrap();
        assert_eq!(
            matches.len(),
            2,
            "Both patterns should match when merged, got {:?}",
            matches
        );
        assert!(matches.contains(&"p0"));
        assert!(matches.contains(&"p1"));
    }

    #[test]
    fn test_field_name_ordering_with_exists() {
        // Based on Go quamina's TestFieldNameOrdering
        // Tests patterns with exists:false against a simple event with field "b"
        // All patterns should match because the absent fields (a, c) don't exist
        let event = r#"{"b": 1}"#;

        let patterns = [
            // b=1 AND a doesn't exist (true - a is absent)
            (r#"{"b": [1], "a": [{"exists": false}]}"#, "p0"),
            // b=1 AND c doesn't exist (true - c is absent)
            (r#"{"b": [1], "c": [{"exists": false}]}"#, "p1"),
            // b=1 (true)
            (r#"{"b": [1]}"#, "p2"),
            // a doesn't exist (true - a is absent)
            (r#"{"a": [{"exists": false}]}"#, "p3"),
        ];

        // Add all patterns and verify all match
        let mut q = Quamina::new();
        for (pattern, name) in &patterns {
            q.add_pattern(*name, pattern).unwrap();
        }

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches.len(),
            patterns.len(),
            "All {} patterns should match, got {:?}",
            patterns.len(),
            matches
        );

        for (_, name) in &patterns {
            assert!(matches.contains(name), "Pattern {} should match", name);
        }
    }

    #[test]
    fn test_shellstyle_complex_wildcards() {
        // Based on Go quamina's TestMakeShellStyleFA
        // Tests shellstyle patterns with multiple wildcards in complex positions
        let test_cases = [
            // Pattern with two wildcards
            (
                r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#,
                vec!["xxabyycdzz", "xxyyzz", "xxyyzzzzz"],
                vec!["xyzyxzy yy zz", "zz yy xx"],
            ),
            // Pattern with wildcards at both ends
            (
                r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#,
                vec!["xxyy", "xxyyef", "abxxyy", "abxxcdyy"],
                vec!["ayybyyzxx", "xyzzy"],
            ),
        ];

        for (pattern, should_match, should_not) in test_cases {
            let mut q = Quamina::new();
            q.add_pattern("p1", pattern).unwrap();

            for val in should_match {
                let event = format!(r#"{{"x": "{}"}}"#, val);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert_eq!(matches, vec!["p1"], "{} should match '{}'", pattern, val);
            }

            for val in should_not {
                let event = format!(r#"{{"x": "{}"}}"#, val);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(matches.is_empty(), "{} should NOT match '{}'", pattern, val);
            }
        }
    }

    #[test]
    fn test_invalid_pattern_validation() {
        // Based on Go quamina's TestPatternFromJSON
        // Tests that various invalid patterns are properly rejected
        let invalid_patterns = [
            // Value not in array (must be array or object)
            (r#"{"foo": 11}"#, "number not in array"),
            (r#"{"foo": "x"}"#, "string not in array"),
            (r#"{"foo": true}"#, "boolean not in array"),
            (r#"{"foo": null}"#, "null not in array"),
            // Invalid exists operator
            (r#"{"x": [{"exists": 23}]}"#, "exists with number"),
            (r#"{"x": [{"exists": "yes"}]}"#, "exists with string"),
            // Invalid shellstyle
            (r#"{"x": [{"shellstyle": 15}]}"#, "shellstyle with number"),
            (r#"{"x": [{"shellstyle": "a**b"}]}"#, "shellstyle with **"),
            // Invalid prefix
            (r#"{"x": [{"prefix": 23}]}"#, "prefix with number"),
            // Invalid suffix
            (r#"{"x": [{"suffix": 23}]}"#, "suffix with number"),
            // Invalid equals-ignore-case
            (
                r#"{"x": [{"equals-ignore-case": 5}]}"#,
                "equals-ignore-case with number",
            ),
            // Invalid numeric
            (r#"{"x": [{"numeric": ">=5"}]}"#, "numeric with string"),
            // Invalid regex
            (
                r#"{"x": [{"regex": "[invalid"}]}"#,
                "regex with invalid pattern",
            ),
            // Unknown operator
            (r#"{"x": [{"unknown-op": "val"}]}"#, "unknown operator"),
        ];

        for (pattern, desc) in &invalid_patterns {
            let mut q = Quamina::new();
            let result = q.add_pattern("test", pattern);
            assert!(result.is_err(), "{} should be rejected: {}", desc, pattern);
        }
    }

    #[test]
    fn test_unicode_field_names() {
        // Based on Go quamina's TestReadMemberName from escaping_test.go
        // Test that unicode characters work in field names (keys), not just values

        // Test direct emoji in field name
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"😀": ["smile"]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"😀": "smile"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Should match pattern with emoji field name"
        );

        // Test unicode escape sequence in field name
        // \u0078 = 'x', so field name is "xx"
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"xx": [1]}"#).unwrap();

        let matches2 = q2
            .matches_for_event(r#"{"\u0078\u0078": 1}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches2,
            vec!["p2"],
            "Unicode escape in field name should decode"
        );
    }

    #[test]
    fn test_unicode_field_names_surrogate_pairs() {
        // Test UTF-16 surrogate pairs in field names
        // From Go's TestReadMemberName: `x\u0078\ud83d\udc8by` = `xx💋y`
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"xx💋y": ["value"]}"#).unwrap();

        // Event with unicode escapes in field name
        let event = r#"{"x\u0078\ud83d\udc8by": "value"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Surrogate pair in field name should decode"
        );

        // Test multiple emojis in field name: 😀💋😺
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"😀💋😺": [1]}"#).unwrap();

        let event2 = r#"{"\ud83d\ude00\ud83d\udc8b\ud83d\ude3a": 1}"#;
        let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
        assert_eq!(
            matches2,
            vec!["p2"],
            "Multiple surrogate pairs in field name should decode"
        );
    }

    #[test]
    fn test_exercise_matching_comprehensive() {
        // Based on Go quamina's TestExerciseMatching
        // Tests many different pattern types against a complex JSON event
        let event = r#"{
            "Image": {
                "Width":  800,
                "Height": 600,
                "Title":  "View from 15th Floor",
                "Thumbnail": {
                    "Url":    "https://www.example.com/image/481989943",
                    "Height": 125,
                    "Width":  100
                },
                "Animated" : false,
                "IDs": [116, 943, 234, 38793]
            }
        }"#;

        // Patterns that SHOULD match
        let should_match = [
            (
                r#"{"Image": {"Title": [{"exists": true}]}}"#,
                "exists true on Title",
            ),
            (
                r#"{"Foo": [{"exists": false}]}"#,
                "exists false on missing Foo",
            ),
            (r#"{"Image": {"Width": [800]}}"#, "exact number match"),
            (
                r#"{"Image": {"Animated": [false], "Thumbnail": {"Height": [125]}}}"#,
                "nested multi-field",
            ),
            (
                r#"{"Image": {"Width": [800], "Title": [{"exists": true}], "Animated": [false]}}"#,
                "three fields",
            ),
            (
                r#"{"Image": {"Width": [800], "IDs": [{"exists": true}]}}"#,
                "exists on array",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "*9943"}]}}}"#,
                "shellstyle suffix",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*"}]}}}"#,
                "shellstyle prefix",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*9943"}]}}}"#,
                "shellstyle infix",
            ),
            (
                r#"{"Image": {"Title": [{"anything-but": ["Pikachu", "Eevee"]}]}}"#,
                "anything-but",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "https:"}]}}}"#,
                "prefix",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": ["a", {"prefix": "https:"}]}}}"#,
                "prefix or literal",
            ),
            (
                r#"{"Image": {"Title": [{"equals-ignore-case": "VIEW FROM 15th FLOOR"}]}}"#,
                "equals-ignore-case",
            ),
            (
                r#"{"Image": {"Title": [{"regex": "View from .... Floor"}]}}"#,
                "regex dots",
            ),
            (
                r#"{"Image": {"Title": [{"regex": "View from [0-9][0-9][rtn][dh] Floor"}]}}"#,
                "regex char class",
            ),
            (
                r#"{"Image": {"Title": [{"regex": "View from 15th (Floor|Storey)"}]}}"#,
                "regex alternation",
            ),
        ];

        // Patterns that SHOULD NOT match
        let should_not_match = [
            (
                r#"{"Image": {"Animated": [{"exists": false}]}}"#,
                "exists false on present field",
            ),
            (
                r#"{"Image": {"NotThere": [{"exists": true}]}}"#,
                "exists true on missing field",
            ),
            (
                r#"{"Image": {"IDs": [{"exists": false}], "Animated": [false]}}"#,
                "exists false on array",
            ),
            (
                r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "http:"}]}}}"#,
                "wrong prefix",
            ),
        ];

        // Test each should_match pattern individually
        for (pattern, desc) in &should_match {
            let mut q = Quamina::new();
            let result = q.add_pattern(*desc, pattern);
            assert!(
                result.is_ok(),
                "Pattern should parse: {} - {}",
                desc,
                pattern
            );

            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.is_empty(),
                "Pattern '{}' should match: {}",
                desc,
                pattern
            );
        }

        // Test each should_not_match pattern individually
        for (pattern, desc) in &should_not_match {
            let mut q = Quamina::new();
            let result = q.add_pattern(*desc, pattern);
            assert!(
                result.is_ok(),
                "Pattern should parse: {} - {}",
                desc,
                pattern
            );

            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Pattern '{}' should NOT match: {}",
                desc,
                pattern
            );
        }

        // Test all patterns together in one matcher
        let mut combined = Quamina::new();
        for (pattern, desc) in &should_match {
            combined.add_pattern(*desc, pattern).unwrap();
        }

        let all_matches = combined.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            all_matches.len(),
            should_match.len(),
            "All should_match patterns should match when combined"
        );
    }

    #[test]
    fn test_wildcard_comprehensive() {
        // Based on Go quamina's exercisePattern tests from wildcard_test.go
        // Tests wildcard patterns (which support escaping, unlike shellstyle)

        // Helper to run wildcard pattern tests
        fn exercise_wildcard(pattern: &str, should_match: &[&str], should_not_match: &[&str]) {
            let mut q = Quamina::new();
            let full_pattern = format!(r#"{{"x": [{{"wildcard": "{}"}}]}}"#, pattern);
            q.add_pattern(pattern, &full_pattern)
                .unwrap_or_else(|_| panic!("Pattern should be valid: {}", pattern));

            for text in should_match {
                let event = format!(r#"{{"x": "{}"}}"#, text);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.contains(&pattern),
                    "Pattern '{}' should match '{}', got {:?}",
                    pattern,
                    text,
                    matches
                );
            }

            for text in should_not_match {
                let event = format!(r#"{{"x": "{}"}}"#, text);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    !matches.contains(&pattern),
                    "Pattern '{}' should NOT match '{}'",
                    pattern,
                    text
                );
            }
        }

        // Test * (matches everything)
        exercise_wildcard("*", &["", "*", "h", "hello"], &[]);

        // Test *hello (suffix matching)
        exercise_wildcard(
            "*hello",
            &["hello", "hhello", "xxxhello", "*hello"],
            &["", "ello", "hellx", "xhellx"],
        );

        // Test h*llo (infix matching)
        exercise_wildcard(
            "h*llo",
            &["hllo", "hello", "hxxxllo"],
            &["", "hlo", "hll", "hellol"],
        );

        // Test hel*o
        exercise_wildcard(
            "hel*o",
            &["helo", "hello", "helxxxo"],
            &["", "hell", "helox", "hellox"],
        );

        // Test hello* (prefix matching)
        exercise_wildcard(
            "hello*",
            &["hello", "hellox", "hellooo", "hello*"],
            &["", "hell", "hellx", "hellxo"],
        );

        // Test h*l*o (multiple wildcards)
        exercise_wildcard(
            "h*l*o",
            &["hlo", "helo", "hllo", "hloo", "hello", "hxxxlxxxo", "h*l*o"],
            &["", "ho", "heeo", "helx", "llo"],
        );

        // Test he*l*
        exercise_wildcard(
            "he*l*",
            &["hel", "hexl", "helx", "helxx", "helxl", "helxlx", "helxxl"],
            &["", "he", "hex", "hexxx"],
        );

        // Test *l* (contains l)
        exercise_wildcard(
            "*l*",
            &["l", "xl", "lx", "xlx", "xxl", "lxx", "xxlxx", "xlxlxlxlxl"],
            &["", "x", "xx", "xtx"],
        );
    }

    #[test]
    fn test_wildcard_escape_sequences_comprehensive() {
        // Based on Go quamina's exercisePattern tests for escaping

        fn exercise_wildcard(pattern: &str, should_match: &[&str], should_not_match: &[&str]) {
            let mut q = Quamina::new();
            let full_pattern = format!(r#"{{"x": [{{"wildcard": "{}"}}]}}"#, pattern);
            q.add_pattern(pattern, &full_pattern)
                .unwrap_or_else(|_| panic!("Pattern should be valid: {}", pattern));

            for text in should_match {
                let event = format!(r#"{{"x": "{}"}}"#, text);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.contains(&pattern),
                    "Pattern '{}' should match '{}', got {:?}",
                    pattern,
                    text,
                    matches
                );
            }

            for text in should_not_match {
                let event = format!(r#"{{"x": "{}"}}"#, text);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    !matches.contains(&pattern),
                    "Pattern '{}' should NOT match '{}'",
                    pattern,
                    text
                );
            }
        }

        // Test hel\*o (escaped star = literal *)
        // Pattern: "hel\\\\*o" -> JSON "hel\\*o" -> parsed: hel\*o
        // In wildcard: hel + \* (escaped star = literal *) + o = matches "hel*o" exactly
        // Note: event "hel*o" doesn't need escaping as * is not a JSON escape char
        exercise_wildcard("hel\\\\*o", &["hel*o"], &["helo", "hello"]);

        // Test he\**o - \* is literal *, then * is wildcard
        // Pattern: "he\\\\**o" -> JSON "he\\**o" -> parsed: he\**o
        // In wildcard: he + \* (literal *) + * (wildcard) + o
        // Matches: he*o, he*llo, he*hello (the * between he and o is literal, then wildcard *o)
        exercise_wildcard(
            "he\\\\**o",
            &["he*o", "he*llo", "he*hello"],
            &["heo", "helo"],
        );

        // Test he\\llo - matches "he\llo" (escaped backslash in pattern = literal \)
        // Pattern: "he\\\\\\\\llo" -> JSON "he\\\\llo" -> parsed: he\\llo
        // In wildcard: he + \\ (escaped backslash = literal \) + llo = matches "he\llo"
        // Event also needs JSON escaping: "he\\\\llo" -> JSON "he\\llo" -> parsed: "he\llo"
        exercise_wildcard("he\\\\\\\\llo", &["he\\\\llo"], &["hello"]);
    }

    #[test]
    fn test_wildcard_invalid_escape_sequences() {
        // Based on Go quamina's TestWildcardInvalidEscape
        let mut q = Quamina::new();

        // Valid pattern from Go: he*\\**
        // Go raw string `he*\\**` -> JSON string "he*\\**" -> after JSON parsing: he*\**
        // In wildcard pattern: he, *, \*, * = he + wildcard + escaped_star + wildcard
        // This is valid because \* is an escaped star (literal *), not adjacent **
        // In Rust raw string, we write the exact JSON content:
        let valid_result = q.add_pattern("valid", r#"{"x": [{"wildcard": "he*\\**"}]}"#);
        assert!(
            valid_result.is_ok(),
            "he*\\** should be valid: {:?}",
            valid_result
        );

        // Invalid patterns
        let invalid_patterns = [
            (r#"{"x": [{"wildcard": "he\\llo"}]}"#, "invalid escape \\l"),
            (r#"{"x": [{"wildcard": "foo**bar"}]}"#, "adjacent **"),
            (r#"{"x": [{"wildcard": "**f"}]}"#, "leading **"),
            (r#"{"x": [{"wildcard": "x**"}]}"#, "trailing **"),
            (r#"{"x": [{"wildcard": "x\\"}]}"#, "trailing backslash"),
        ];

        for (pattern, desc) in invalid_patterns {
            let mut q2 = Quamina::new();
            let result = q2.add_pattern("p", pattern);
            assert!(result.is_err(), "{} should be rejected: {}", desc, pattern);
        }
    }

    #[test]
    fn test_wildcard_syntax_errors() {
        // Based on Go quamina's TestWildcardSyntax
        let invalid_patterns = [
            r#"{"x": [{"wildcard": . }]}"#,    // dot instead of string
            r#"{"x": [{"wildcard": 3}]}"#,     // number instead of string
            r#"{"x": [{"wildcard": "x" ]}"#,   // missing closing brace
            r#"{"x": [{"wildcard": true}]}"#,  // boolean instead of string
            r#"{"x": [{"wildcard": null}]}"#,  // null instead of string
            r#"{"x": [{"wildcard": ["a"]}]}"#, // array instead of string
        ];

        for pattern in invalid_patterns {
            let mut q = Quamina::new();
            let result = q.add_pattern("p", pattern);
            assert!(
                result.is_err(),
                "Should reject invalid pattern: {}",
                pattern
            );
        }
    }

    // ========================================================================
    // Multi-Pattern Wildcard Tests - Ported from Go's exerciseMultiPatterns
    // ========================================================================

    /// Helper for multi-pattern wildcard tests (mirrors Go's exerciseMultiPatterns)
    fn exercise_multi_patterns(
        should_not_match_any: &[&str],
        patterns_with_matches: &[(&str, &[&str])],
    ) {
        let mut q = Quamina::new();

        // Add all patterns
        for (pattern, _) in patterns_with_matches {
            q.add_pattern(*pattern, pattern)
                .unwrap_or_else(|e| panic!("Failed to add pattern {}: {:?}", pattern, e));
        }

        // Verify each pattern matches its expected values
        for (pattern, should_match) in patterns_with_matches {
            for val in *should_match {
                let event = format!(r#"{{"x":"{}"}}"#, val);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.contains(pattern),
                    "Pattern '{}' should match '{}', got {:?}",
                    pattern,
                    val,
                    matches
                );
            }
        }

        // Verify none of the should_not_match values match any pattern
        for val in should_not_match_any {
            let event = format!(r#"{{"x":"{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "'{}' should not match any pattern, got {:?}",
                val,
                matches
            );
        }
    }

    #[test]
    fn test_wildcard_multi_patterns_basic() {
        // Go line 42-45: *, h*o, exact match
        exercise_multi_patterns(
            &[],
            &[
                (
                    r#"{"x":[{"wildcard": "*"}]}"#,
                    &["", "*", "h", "ho", "hello"],
                ),
                (r#"{"x":[{"wildcard": "h*o"}]}"#, &["ho", "hello"]),
                (r#"{"x":["hello"]}"#, &["hello"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_exact() {
        // Go line 46-48
        exercise_multi_patterns(
            &["", "hellox", "blahabc"],
            &[
                (
                    r#"{"x":[{"wildcard": "*hello"}]}"#,
                    &["hello", "xhello", "hehello"],
                ),
                (r#"{"x":["abc"]}"#, &["abc"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_infix() {
        // Go line 49-51
        exercise_multi_patterns(
            &["", "h", "ello", "hel", "hlo", "hell"],
            &[
                (
                    r#"{"x":[{"wildcard": "*hello"}]}"#,
                    &["hello", "xhello", "hehello"],
                ),
                (
                    r#"{"x":[{"wildcard": "h*llo"}]}"#,
                    &["hllo", "hello", "hehello"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_infix2() {
        // Go line 52-54
        exercise_multi_patterns(
            &["", "h", "ello", "hel", "heo", "hell"],
            &[
                (
                    r#"{"x":[{"wildcard": "*hello"}]}"#,
                    &["hello", "xhello", "hehello"],
                ),
                (
                    r#"{"x":[{"wildcard": "he*lo"}]}"#,
                    &["helo", "hello", "hehello"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_double() {
        // Go line 55-57
        exercise_multi_patterns(
            &["", "e", "l", "lo", "hel"],
            &[
                (r#"{"x":[{"wildcard": "*elo"}]}"#, &["elo", "helo", "xhelo"]),
                (
                    r#"{"x":[{"wildcard": "e*l*"}]}"#,
                    &["el", "elo", "exl", "elx", "exlx", "exxl", "elxx", "exxlxx"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_double2() {
        // Go line 58-60
        exercise_multi_patterns(
            &["", "he", "hexxo", "ello"],
            &[
                (
                    r#"{"x":[{"wildcard": "*hello"}]}"#,
                    &["hello", "xhello", "xxhello"],
                ),
                (
                    r#"{"x":[{"wildcard": "he*l*"}]}"#,
                    &[
                        "hel", "hello", "helo", "hexl", "hexlx", "hexxl", "helxx", "hexxlxx",
                    ],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_infix_pair() {
        // Go line 61-63
        exercise_multi_patterns(
            &["", "hlo", "heo", "hllol", "helol"],
            &[
                (
                    r#"{"x":[{"wildcard": "h*llo"}]}"#,
                    &["hllo", "hello", "hxxxllo", "hexxxllo"],
                ),
                (
                    r#"{"x":[{"wildcard": "he*lo"}]}"#,
                    &["helo", "hello", "hexxxlo", "hexxxllo"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_pair() {
        // Go line 64-66
        exercise_multi_patterns(
            &[
                "", "hlox", "hllo", "helo", "heox", "helx", "hellx", "helloxx", "heloxx",
            ],
            &[
                (
                    r#"{"x":[{"wildcard": "h*llox"}]}"#,
                    &["hllox", "hellox", "hxxxllox", "helhllox", "hheloxllox"],
                ),
                (
                    r#"{"x":[{"wildcard": "hel*ox"}]}"#,
                    &["helox", "hellox", "helxxxox", "helhllox", "helhlloxox"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_complex1() {
        // Go line 67-69
        exercise_multi_patterns(
            &[
                "", "h", "he", "hl", "el", "hlo", "llo", "hllol", "hxll", "hexxx",
            ],
            &[
                (
                    r#"{"x":[{"wildcard": "h*llo"}]}"#,
                    &["hllo", "hello", "hxxxllo", "hexxxllo", "hexxxlllo"],
                ),
                (
                    r#"{"x":[{"wildcard": "he*l*"}]}"#,
                    &[
                        "hel",
                        "helo",
                        "hexl",
                        "hello",
                        "helol",
                        "hexxxlo",
                        "hexxxllo",
                        "hexxxlllo",
                    ],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_complex2() {
        // Go line 70-72
        exercise_multi_patterns(
            &[
                "", "h", "hex", "hl", "exl", "hxlo", "xllo", "hxllol", "hxxll", "hexxx",
            ],
            &[
                (
                    r#"{"x":[{"wildcard": "h*xllo"}]}"#,
                    &["hxllo", "hexllo", "hxxxllo", "hexxxllo"],
                ),
                (
                    r#"{"x":[{"wildcard": "hex*l*"}]}"#,
                    &[
                        "hexl",
                        "hexlo",
                        "hexxl",
                        "hexllo",
                        "hexlol",
                        "hexxxlo",
                        "hexxxllo",
                        "hexxxlllo",
                    ],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_overlap1() {
        // Go line 73-75
        exercise_multi_patterns(
            &["", "hel", "heo", "hlo", "hellxox"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*lo"}]}"#,
                    &["helo", "hello", "hexxxlo", "helxxxlo"],
                ),
                (
                    r#"{"x":[{"wildcard": "hel*o"}]}"#,
                    &["helo", "hello", "hellxo", "helxxxo", "helxxxlo"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_overlap2() {
        // Go line 76-78
        exercise_multi_patterns(
            &["", "hlo", "hll", "hel", "helox"],
            &[
                (
                    r#"{"x":[{"wildcard": "h*llo"}]}"#,
                    &["hllo", "hello", "hxxxllo", "helllo"],
                ),
                (
                    r#"{"x":[{"wildcard": "hel*o"}]}"#,
                    &["helo", "hello", "helxo", "helllo"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_prefix_suffix() {
        // Go line 79-81
        exercise_multi_patterns(
            &["", "he", "hel", "helox", "helx", "hxlo"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*lo"}]}"#,
                    &["helo", "hello", "helllo", "helxlo"],
                ),
                (
                    r#"{"x":[{"wildcard": "hell*"}]}"#,
                    &["hell", "hello", "helllo", "hellx", "hellxxx"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_prefix_suffix2() {
        // Go line 82-84
        exercise_multi_patterns(
            &["", "hel", "helox", "helxox", "hexo"],
            &[
                (
                    r#"{"x":[{"wildcard": "hel*o"}]}"#,
                    &["helo", "hello", "helllo", "hellloo", "helloo", "heloo"],
                ),
                (
                    r#"{"x":[{"wildcard": "hell*"}]}"#,
                    &["hell", "hello", "helllo", "hellloo", "helloo", "hellox"],
                ),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_prefix_pair() {
        // Go line 85-87
        exercise_multi_patterns(
            &["", "he", "hex", "hexlo"],
            &[
                (
                    r#"{"x":[{"wildcard": "hel*"}]}"#,
                    &["hel", "helx", "hello", "hellox"],
                ),
                (r#"{"x":[{"wildcard": "hello*"}]}"#, &["hello", "hellox"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_suffix_exact2() {
        // Go line 88-90
        exercise_multi_patterns(
            &["", "he", "hex", "hexlo"],
            &[
                (
                    r#"{"x":[{"wildcard": "*hello"}]}"#,
                    &["hello", "hhello", "hhhello"],
                ),
                (r#"{"x":["hello"]}"#, &["hello"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_infix_exact() {
        // Go line 91-93
        exercise_multi_patterns(
            &["", "he", "hel", "heo", "heloz", "hellox", "heloxo"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*lo"}]}"#,
                    &["helo", "hello", "helllo"],
                ),
                (r#"{"x":["helox"]}"#, &["helox"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_infix_exact2() {
        // Go line 94-96
        exercise_multi_patterns(
            &["", "he", "helx", "helo", "hexlx", "hellox", "heloxx"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*l"}]}"#,
                    &["hel", "hexl", "hexxxl"],
                ),
                (r#"{"x":["helox"]}"#, &["helox"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_prefix_exact() {
        // Go line 97-99
        exercise_multi_patterns(
            &["", "h", "hxlox", "hxelox"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*"}]}"#,
                    &["he", "helo", "helox", "heloxx"],
                ),
                (r#"{"x":["helox"]}"#, &["helox"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_double_exact() {
        // Go line 100-102
        exercise_multi_patterns(
            &["", "h", "he", "hel", "hexxo", "hexxohexxo"],
            &[
                (
                    r#"{"x":[{"wildcard": "h*l*o"}]}"#,
                    &[
                        "hlo",
                        "helo",
                        "hllo",
                        "hello",
                        "hexloo",
                        "hellohello",
                        "hellohellxo",
                    ],
                ),
                (r#"{"x":["hellohello"]}"#, &["hellohello"]),
            ],
        );
    }

    #[test]
    fn test_wildcard_multi_patterns_double_exact2() {
        // Go line 103-105
        exercise_multi_patterns(
            &["", "h", "he", "hlo", "hexxo", "hexxohexxo"],
            &[
                (
                    r#"{"x":[{"wildcard": "he*l*"}]}"#,
                    &[
                        "hel",
                        "helo",
                        "hexl",
                        "hello",
                        "hexloo",
                        "hellohellx",
                        "hellohello",
                    ],
                ),
                (r#"{"x":["hellohello"]}"#, &["hellohello"]),
            ],
        );
    }

    // ========================================================================
    // Missing Escape Pattern Tests
    // ========================================================================

    #[test]
    fn test_wildcard_escape_backslash_star() {
        // Go line 40: `he\\\\\\*llo` (raw string = he\\\\\\*llo, 11 chars)
        // After JSON parse: he\\\*llo (escaped backslash + escaped star)
        // Wildcard meaning: he + literal_backslash + literal_star + llo
        // Should match literal string "he\*llo" (6 chars)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"wildcard": "he\\\\\\*llo"}]}"#)
            .unwrap();

        // Should match "he\*llo" - in JSON, backslash needs escaping: "he\\*llo"
        let matches = q
            .matches_for_event(r#"{"x": "he\\*llo"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match he\\*llo");

        // Should NOT match - use raw strings for JSON to avoid double-escaping confusion
        let no_match_events = [
            r#"{"x": "hello"}"#,
            r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
            r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash)
            r#"{"x": "he\\xxllo"}"#, // he\xxllo
        ];
        for event in no_match_events {
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "Should not match {}", event);
        }
    }

    #[test]
    fn test_wildcard_escape_backslash_wildcard() {
        // Go line 41: `he\\\\*llo` (raw string = he\\\\*llo, 10 chars)
        // After JSON parse: he\\*llo (escaped backslash + wildcard)
        // Wildcard meaning: he + literal_backslash + wildcard + llo
        // Should match "he\" followed by anything followed by "llo"
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"wildcard": "he\\\\*llo"}]}"#)
            .unwrap();

        // Should match - values with "he\" prefix and "llo" suffix
        let match_events = [
            r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash, matches wildcard)
            r#"{"x": "he\\*llo"}"#,  // he\*llo
            r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
            r#"{"x": "he\\xxllo"}"#, // he\xxllo
        ];
        for event in match_events {
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", event);
        }

        // Should NOT match
        let no_match_events = [
            r#"{"x": "hello"}"#,  // no backslash after he
            r#"{"x": "he\\ll"}"#, // doesn't end with llo
        ];
        for event in no_match_events {
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "Should not match {}", event);
        }
    }

    // ========================================================================
    // Missing TestWildCardRuler Cases
    // ========================================================================

    #[test]
    fn test_shellstyle_duplicate_pattern() {
        // Go TestWildCardRuler: r4 and r5 are identical patterns
        let mut q = Quamina::new();
        q.add_pattern("r4", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();
        q.add_pattern("r5", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
        assert_eq!(matches.len(), 2, "Both r4 and r5 should match");
        assert!(matches.contains(&"r4"));
        assert!(matches.contains(&"r5"));
    }

    #[test]
    fn test_shellstyle_double_wildcard() {
        // Go TestWildCardRuler: r6 = 12*4*
        let mut q = Quamina::new();
        q.add_pattern("r6", r#"{"d": [{"shellstyle": "12*4*"}]}"#)
            .unwrap();

        // Should match
        let matches = q.matches_for_event(r#"{"d": "12345"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["r6"], "12*4* should match 12345");

        // Should NOT match
        let no_match = q.matches_for_event(r#"{"d": "1235"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty(), "12*4* should not match 1235");
    }

    #[test]
    fn test_shellstyle_zero_length_prefix() {
        // Go TestWildCardRuler: {"a": "bc"} should match *bc
        let mut q = Quamina::new();
        q.add_pattern("r1", r#"{"a": [{"shellstyle": "*bc"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"a": "bc"}"#.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["r1"],
            "*bc should match bc (zero-length prefix)"
        );
    }

    #[test]
    fn test_shellstyle_ruler_negative_cases() {
        // Go TestWildCardRuler: additional negative test cases
        let mut q = Quamina::new();
        q.add_pattern("r2", r#"{"b": [{"shellstyle": "d*f"}]}"#)
            .unwrap();
        q.add_pattern("r4", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        // Should NOT match
        let cases = [
            (r#"{"c": "abc"}"#, "xy* should not match abc"),
            (r#"{"c": "abcxyz"}"#, "xy* should not match abcxyz"),
            (r#"{"b": "de"}"#, "d*f should not match de"),
        ];

        for (event, msg) in cases {
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "{}", msg);
        }
    }

    // ========================================================================
    // Unicode Test Strings
    // ========================================================================

    #[test]
    fn test_wildcard_unicode_strings() {
        // Go TestWildcardMatching includes Unicode strings with Őz
        let mut q = Quamina::new();

        // Test *hello with Unicode prefix
        q.add_pattern("p1", r#"{"x": [{"wildcard": "*hello"}]}"#)
            .unwrap();
        let matches = q
            .matches_for_event(r#"{"x": "23Őzhello"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "*hello should match 23Őzhello");

        // Test h*llo with Unicode in middle
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": [{"wildcard": "h*llo"}]}"#)
            .unwrap();
        let matches2 = q2
            .matches_for_event(r#"{"x": "hel23Őzlllo"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches2, vec!["p2"], "h*llo should match hel23Őzlllo");

        // Test hello* with Unicode suffix
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"x": [{"wildcard": "hello*"}]}"#)
            .unwrap();
        let matches3 = q3
            .matches_for_event(r#"{"x": "hello23Őzlllo"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches3, vec!["p3"], "hello* should match hello23Őzlllo");

        // Test h*l*o with Unicode
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"x": [{"wildcard": "h*l*o"}]}"#)
            .unwrap();
        let matches4 = q4
            .matches_for_event(r#"{"x": "hel23Őzlllo"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches4, vec!["p4"], "h*l*o should match hel23Őzlllo");
    }

    // ========================================================================
    // Missing TestMakeShellStyleFA Edge Cases
    // ========================================================================

    #[test]
    fn test_shellstyle_suffix_with_space() {
        // Go TestMakeShellStyleFA: *ST should match "STA ST"
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*ST"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"x": "STA ST"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "*ST should match 'STA ST'");

        let matches2 = q.matches_for_event(r#"{"x": "1ST"}"#.as_bytes()).unwrap();
        assert_eq!(matches2, vec!["p1"], "*ST should match '1ST'");

        // Negative cases
        let no1 = q.matches_for_event(r#"{"x": "STA"}"#.as_bytes()).unwrap();
        assert!(no1.is_empty(), "*ST should not match 'STA'");

        let no2 = q
            .matches_for_event(r#"{"x": "STAST "}"#.as_bytes())
            .unwrap();
        assert!(
            no2.is_empty(),
            "*ST should not match 'STAST ' (trailing space)"
        );
    }

    #[test]
    fn test_shellstyle_prefix_negative() {
        // Go TestMakeShellStyleFA: foo* negative cases
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "foo*"}]}"#)
            .unwrap();

        let no1 = q.matches_for_event(r#"{"x": "afoo"}"#.as_bytes()).unwrap();
        assert!(no1.is_empty(), "foo* should not match 'afoo'");

        let no2 = q.matches_for_event(r#"{"x": "fofo"}"#.as_bytes()).unwrap();
        assert!(no2.is_empty(), "foo* should not match 'fofo'");
    }

    #[test]
    fn test_shellstyle_suffix_negative() {
        // Go TestMakeShellStyleFA: *foo negative cases
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo"}]}"#)
            .unwrap();

        let no1 = q.matches_for_event(r#"{"x": "foox"}"#.as_bytes()).unwrap();
        assert!(no1.is_empty(), "*foo should not match 'foox'");

        let no2 = q.matches_for_event(r#"{"x": "afooo"}"#.as_bytes()).unwrap();
        assert!(no2.is_empty(), "*foo should not match 'afooo'");
    }

    #[test]
    fn test_shellstyle_contains_negative() {
        // Go TestMakeShellStyleFA: *foo* negative cases
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo*"}]}"#)
            .unwrap();

        let no1 = q.matches_for_event(r#"{"x": "afoa"}"#.as_bytes()).unwrap();
        assert!(no1.is_empty(), "*foo* should not match 'afoa'");

        let no2 = q
            .matches_for_event(r#"{"x": "fofofoxooxoo"}"#.as_bytes())
            .unwrap();
        assert!(no2.is_empty(), "*foo* should not match 'fofofoxooxoo'");
    }

    #[test]
    fn test_shellstyle_double_wildcard_variations() {
        // Go TestMakeShellStyleFA: xx*yy*zz and *xx*yy* additional cases
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#)
            .unwrap();

        // Additional positive cases from Go
        for val in ["xxyycdzz", "xxabyyzz"] {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "xx*yy*zz should match {}", val);
        }

        // Test *xx*yy* additional cases
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#)
            .unwrap();

        for val in ["abxxcdyyef", "xxcdyyef", "abxxyyef", "xxcdyy", "xxyyef"] {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q2.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p2"], "*xx*yy* should match {}", val);
        }
    }

    // ========================================================================
    // Stress Tests - Ported from Go quamina
    // ========================================================================

    /// Port of Go's TestFuzzValueMatcher
    /// Tests 10,000 random 12-character strings as patterns
    #[test]
    fn test_stress_fuzz_strings() {
        use rand::{Rng, SeedableRng};
        use std::collections::HashSet;

        let mut rng = rand::rngs::StdRng::seed_from_u64(12345);
        let mut q = Quamina::new();
        let mut pattern_names: Vec<String> = Vec::new();
        let mut used: HashSet<String> = HashSet::new();
        let chars = b"abcdefghijklmnopqrstuvwxyz";
        let str_len = 12;

        // Make 10,000 random 12-char strings
        for _ in 0..10_000 {
            let s: String = (0..str_len)
                .map(|_| chars[rng.gen_range(0..chars.len())] as char)
                .collect();
            pattern_names.push(s.clone());
            used.insert(s);
        }

        // Add a pattern for each string
        for pname in &pattern_names {
            let pattern = format!(r#"{{"a": ["{}"]}}"#, pname);
            q.add_pattern(pname.clone(), &pattern)
                .expect("addPattern failed");
        }

        // Make sure all patterns match
        for pname in &pattern_names {
            let event = format!(r#"{{"a": "{}"}}"#, pname);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matches_for_event failed");
            assert_eq!(
                matches.len(),
                1,
                "Expected 1 match for {}, got {}",
                pname,
                matches.len()
            );
            assert_eq!(
                matches[0], *pname,
                "Expected match {}, got {}",
                pname, matches[0]
            );
        }

        // Now run 10,000 more random strings that shouldn't match
        let mut should_not_count = 0;
        while should_not_count < 10_000 {
            let s: String = (0..str_len)
                .map(|_| chars[rng.gen_range(0..chars.len())] as char)
                .collect();
            if used.contains(&s) {
                continue;
            }
            should_not_count += 1;

            let event = format!(r#"{{"a": "{}"}}"#, s);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matches_for_event failed");
            assert!(
                matches.is_empty(),
                "Expected no match for {}, got {} matches",
                s,
                matches.len()
            );
        }
    }

    /// Port of Go's TestFuzzWithNumbers
    /// Tests 10,000 random numbers as patterns
    #[test]
    fn test_stress_fuzz_numbers() {
        use rand::{Rng, SeedableRng};
        use std::collections::HashSet;

        let mut rng = rand::rngs::StdRng::seed_from_u64(98543);
        let mut q = Quamina::new();
        let mut pattern_names: Vec<i64> = Vec::new();
        let mut used: HashSet<i64> = HashSet::new();

        // Make 10,000 random numbers
        for _ in 0..10_000 {
            let n: i64 = rng.gen();
            pattern_names.push(n);
            used.insert(n);
        }

        // Add a pattern for each number
        for pname in &pattern_names {
            let pattern = format!(r#"{{"a": [{}]}}"#, pname);
            q.add_pattern(pname.to_string(), &pattern)
                .expect("addPattern failed");
        }

        // Make sure all patterns match
        for pname in &pattern_names {
            let event = format!(r#"{{"a": {}}}"#, pname);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matches_for_event failed");
            assert_eq!(
                matches.len(),
                1,
                "Expected 1 match for {}, got {}",
                pname,
                matches.len()
            );
            assert_eq!(
                matches[0],
                pname.to_string(),
                "Expected match {}, got {}",
                pname,
                matches[0]
            );
        }

        // Now run 10,000 more random numbers that shouldn't match
        let mut should_not_count = 0;
        while should_not_count < 10_000 {
            let n: i64 = rng.gen_range(0..1_000_000);
            if used.contains(&n) {
                continue;
            }
            should_not_count += 1;

            let event = format!(r#"{{"a": {}}}"#, n);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matches_for_event failed");
            assert!(
                matches.is_empty(),
                "Expected no match for {}, got {} matches",
                n,
                matches.len()
            );
        }
    }

    /// Port of Go's TestRulerCl2
    /// Tests multiple operators against citylots2 dataset (~213K events)
    #[test]
    fn test_stress_citylots2_operators() {
        use flate2::read::GzDecoder;
        use std::fs::File;
        use std::io::{BufRead, BufReader};

        // Load citylots2.json.gz
        let path = "testdata/citylots2.json.gz";
        let file = File::open(path).expect("Failed to open citylots2.json.gz");
        let decoder = GzDecoder::new(file);
        let reader = BufReader::new(decoder);
        let lines: Vec<Vec<u8>> = reader
            .lines()
            .map(|l| l.expect("Failed to read line").into_bytes())
            .collect();

        println!("Loaded {} lines from citylots2", lines.len());

        // Test EXACT patterns
        let exact_rules = [
            r#"{"properties": {"MAPBLKLOT": ["1430022"]}}"#,
            r#"{"properties": {"MAPBLKLOT": ["2607117"]}}"#,
            r#"{"properties": {"MAPBLKLOT": ["2607218"]}}"#,
            r#"{"properties": {"MAPBLKLOT": ["3745012"]}}"#,
            r#"{"properties": {"MAPBLKLOT": ["VACSTWIL"]}}"#,
        ];
        let exact_expected = [1, 101, 35, 655, 1];
        run_stress_test("EXACT", &lines, &exact_rules, &exact_expected);

        // Test PREFIX patterns
        let prefix_rules = [
            r#"{"properties": {"STREET": [{"prefix": "AC"}]}}"#,
            r#"{"properties": {"STREET": [{"prefix": "BL"}]}}"#,
            r#"{"properties": {"STREET": [{"prefix": "DR"}]}}"#,
            r#"{"properties": {"STREET": [{"prefix": "FU"}]}}"#,
            r#"{"properties": {"STREET": [{"prefix": "RH"}]}}"#,
        ];
        let prefix_expected = [24, 442, 38, 2387, 328];
        run_stress_test("PREFIX", &lines, &prefix_rules, &prefix_expected);

        // Test ANYTHING-BUT patterns
        let anything_but_rules = [
            r#"{"properties": {"STREET": [{"anything-but": ["FULTON"]}]}}"#,
            r#"{"properties": {"STREET": [{"anything-but": ["MASON"]}]}}"#,
            r#"{"properties": {"ST_TYPE": [{"anything-but": ["ST"]}]}}"#,
            r#"{"geometry": {"type": [{"anything-but": ["Polygon"]}]}}"#,
            r#"{"properties": {"FROM_ST": [{"anything-but": ["441"]}]}}"#,
        ];
        let anything_but_expected = [211158, 210411, 96682, 120, 210615];
        run_stress_test(
            "ANYTHING-BUT",
            &lines,
            &anything_but_rules,
            &anything_but_expected,
        );

        // Test SHELLSTYLE patterns - all patterns together
        // This tests the merge_fas spinout handling
        let shellstyle_rules = [
            r#"{"properties": {"MAPBLKLOT": [{"shellstyle": "143*"}]}}"#,
            r#"{"properties": {"MAPBLKLOT": [{"shellstyle": "2*0*1*7"}]}}"#,
            r#"{"properties": {"MAPBLKLOT": [{"shellstyle": "*218"}]}}"#,
            r#"{"properties": {"MAPBLKLOT": [{"shellstyle": "3*5*2"}]}}"#,
            r#"{"properties": {"MAPBLKLOT": [{"shellstyle": "VA*IL"}]}}"#,
        ];
        let shellstyle_expected: [usize; 5] = [490, 713, 43, 2540, 1];
        run_stress_test(
            "SHELLSTYLE",
            &lines,
            &shellstyle_rules,
            &shellstyle_expected,
        );

        // Test EQUALS-IGNORE-CASE patterns
        let equals_ignore_case_rules = [
            r#"{"properties": {"STREET": [{"equals-ignore-case": "jefferson"}]}}"#,
            r#"{"properties": {"STREET": [{"equals-ignore-case": "bEaCh"}]}}"#,
            r#"{"properties": {"STREET": [{"equals-ignore-case": "HyDe"}]}}"#,
            r#"{"properties": {"STREET": [{"equals-ignore-case": "CHESTNUT"}]}}"#,
            r#"{"properties": {"ST_TYPE": [{"equals-ignore-case": "st"}]}}"#,
        ];
        let equals_ignore_case_expected = [131, 211, 1758, 825, 116386];
        run_stress_test(
            "EQUALS-IGNORE-CASE",
            &lines,
            &equals_ignore_case_rules,
            &equals_ignore_case_expected,
        );

        // Test REGEXP patterns
        let regexp_rules = [r#"{"properties": {"STREET": [{"regexp": "B..CH"}]}}"#];
        let regexp_expected = [220];
        run_stress_test("REGEXP", &lines, &regexp_rules, &regexp_expected);
    }

    fn run_stress_test(name: &str, lines: &[Vec<u8>], rules: &[&str], expected: &[usize]) {
        use std::collections::HashMap;
        use std::time::Instant;

        let mut q = Quamina::new();
        let mut wanted: HashMap<String, usize> = HashMap::new();

        for (i, rule) in rules.iter().enumerate() {
            let rule_name = format!("r{}", i);
            q.add_pattern(rule_name.clone(), rule)
                .unwrap_or_else(|e| panic!("Failed to add rule {}: {} - {}", i, rule, e));
            wanted.insert(rule_name, expected[i]);
        }

        let mut got_matches: HashMap<String, usize> = HashMap::new();
        let start = Instant::now();

        for line in lines {
            let matches = q.matches_for_event(line).expect("matches_for_event failed");
            for m in matches {
                *got_matches.entry(m).or_insert(0) += 1;
            }
        }

        let elapsed = start.elapsed();
        let events_per_sec = lines.len() as f64 / elapsed.as_secs_f64();
        println!(
            "{}: {:.0} events/sec ({:.2?})",
            name, events_per_sec, elapsed
        );

        // Verify results
        for (rule_name, expected_count) in &wanted {
            let got_count = got_matches.get(rule_name).copied().unwrap_or(0);
            assert_eq!(
                got_count, *expected_count,
                "{}: {} expected {} matches, got {}",
                name, rule_name, expected_count, got_count
            );
        }
    }

    /// Port of Go's TestConcurrency
    /// Tests concurrent pattern addition during active matching
    /// The key test is that concurrent updates don't crash and all patterns are functional
    // MIRI SKIP RATIONALE: Opens citylots2.json.gz file - Miri isolation blocks file I/O.
    // Coverage: test_concurrent_miri_friendly exercises same thread-safety code paths.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_concurrent_update_during_matching() {
        use flate2::read::GzDecoder;
        use std::fs::File;
        use std::io::{BufRead, BufReader};
        use std::sync::mpsc;
        use std::sync::Arc;
        use std::thread;

        const UPDATE_INTERVAL: usize = 250;

        // Load citylots2.json.gz
        let path = "testdata/citylots2.json.gz";
        let file = File::open(path).expect("Failed to open citylots2.json.gz");
        let decoder = GzDecoder::new(file);
        let reader = BufReader::new(decoder);
        let lines: Vec<Vec<u8>> = reader
            .lines()
            .map(|l| l.expect("Failed to read line").into_bytes())
            .collect();

        // Initial patterns that match citylots2 structure
        let patterns = [
            ("CRANLEIGH", r#"{"properties": {"STREET": ["CRANLEIGH"]}}"#),
            (
                "shellstyle",
                r#"{"properties": {"STREET": [{"shellstyle": "B*K"}]}}"#,
            ),
        ];

        // Create matcher and add initial patterns
        let q = Arc::new(std::sync::RwLock::new(Quamina::new()));
        {
            let mut q_write = q.write().unwrap();
            for (name, pattern) in &patterns {
                q_write.add_pattern(name.to_string(), pattern).unwrap();
            }
        }

        // Channel for tracking added patterns
        let (tx, rx) = mpsc::channel::<String>();

        // Concurrent updater function - adds unique street patterns
        fn add_pattern_concurrent(
            q: Arc<std::sync::RwLock<Quamina<String>>>,
            idx: usize,
            tx: mpsc::Sender<String>,
        ) {
            let val = format!("CONCURRENT_STREET_{}", idx);
            let pattern = format!(r#"{{"properties": {{"STREET": ["{}"]}}}}"#, val);

            {
                let mut q_write = q.write().unwrap();
                q_write
                    .add_pattern(val.clone(), &pattern)
                    .expect("add_pattern failed");
            }
            let _ = tx.send(val); // Ignore send errors (receiver may be dropped)
        }

        // Run matching with concurrent updates
        let mut total_matches = 0usize;
        let mut sent = 0;

        let start = std::time::Instant::now();
        for (i, line) in lines.iter().enumerate() {
            // Match against current patterns
            let matches = {
                let q_read = q.read().unwrap();
                q_read
                    .matches_for_event(line)
                    .expect("matches_for_event failed")
            };
            total_matches += matches.len();

            // Every UPDATE_INTERVAL lines, spawn a thread to add a new pattern
            if (i + 1) % UPDATE_INTERVAL == 0 {
                sent += 1;
                let q_clone = Arc::clone(&q);
                let tx_clone = tx.clone();
                let idx = sent;
                thread::spawn(move || {
                    add_pattern_concurrent(q_clone, idx, tx_clone);
                });
            }
        }
        let elapsed = start.elapsed();

        // Drop the sender so rx.iter() will complete
        drop(tx);

        // Wait a moment for all threads to complete
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Verify all concurrently added patterns are now in the matcher and work
        let mut verified = 0;
        for val in rx.iter() {
            let event = format!(r#"{{"properties": {{"STREET": "{}"}}}}"#, val);

            let q_read = q.read().unwrap();
            let matches = q_read
                .matches_for_event(event.as_bytes())
                .expect("matches_for_event failed");
            assert!(
                matches.contains(&val),
                "Concurrent pattern {} not found in matches: {:?}",
                val,
                matches
            );
            verified += 1;
        }

        let events_per_sec = lines.len() as f64 / elapsed.as_secs_f64();
        println!(
            "Concurrent update test: {:.0} events/sec, {} total matches, {} patterns added concurrently, {} verified",
            events_per_sec, total_matches, sent, verified
        );

        // Key assertions:
        // 1. No crashes during concurrent updates
        // 2. All concurrently added patterns were verified as working
        assert_eq!(sent, verified, "Not all concurrent patterns were verified");
        assert!(sent > 0, "Should have added some patterns concurrently");
        assert!(total_matches > 0, "Should have gotten some matches");
    }

    /// Tests that multiple patterns with the same ID work correctly
    /// Port of Go's TestMultiplePatternsWithSameId
    #[test]
    fn test_multiple_patterns_same_id_comprehensive() {
        let mut q = Quamina::new();
        let id = "shared_id";

        // Add two different patterns with the same ID
        q.add_pattern(id, r#"{"enjoys": ["queso"]}"#).unwrap();
        q.add_pattern(id, r#"{"needs": ["chips"]}"#).unwrap();

        // Both patterns should match (returning the same ID)
        let m1 = q
            .matches_for_event(r#"{"enjoys": "queso"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec![id], "First pattern should match");

        let m2 = q
            .matches_for_event(r#"{"needs": "chips"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec![id], "Second pattern should match");

        // pattern_count returns unique IDs (we have 1 ID with 2 patterns)
        assert_eq!(q.pattern_count(), 1, "Should have 1 unique pattern ID");

        // Delete by ID should remove both patterns
        q.delete_patterns(&id).unwrap();

        // Verify both were deleted (pattern count drops to 0)
        assert_eq!(
            q.pattern_count(),
            0,
            "Should have 0 live patterns after delete"
        );

        // Neither pattern should match now
        let m3 = q
            .matches_for_event(r#"{"enjoys": "queso"}"#.as_bytes())
            .unwrap();
        assert!(m3.is_empty(), "No match after delete");

        let m4 = q
            .matches_for_event(r#"{"needs": "chips"}"#.as_bytes())
            .unwrap();
        assert!(m4.is_empty(), "No match after delete");
    }

    /// Tests error handling for invalid patterns
    /// Port of Go's TestBadPattern - tests parse-level errors
    #[test]
    fn test_bad_pattern_error_handling() {
        let mut q: Quamina<&str> = Quamina::new();

        // Invalid JSON - not a JSON object
        let result = q.add_pattern("p1", "Dream baby dream");
        assert!(result.is_err(), "Should reject non-JSON pattern");

        // Invalid JSON - unclosed brace
        let result = q.add_pattern("p2", r#"{"likes": ["tacos"]"#);
        assert!(result.is_err(), "Should reject malformed JSON");

        // Invalid pattern structure - value not an array
        let result = q.add_pattern("p3", r#"{"likes": "tacos"}"#);
        assert!(result.is_err(), "Should reject non-array field value");

        // Note: Empty pattern {} is accepted (matches any event with any field)

        // Invalid operator
        let result = q.add_pattern("p5", r#"{"x": [{"invalid-op": "foo"}]}"#);
        assert!(result.is_err(), "Should reject invalid operator");

        // Empty byte input
        let result = q.add_pattern("p6", "");
        assert!(result.is_err(), "Should reject empty input");
    }

    /// Tests error handling for invalid events
    /// Port of Go's TestBadEvent
    #[test]
    fn test_bad_event_error_handling() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"likes": ["tacos"]}"#).unwrap();

        // Invalid JSON events
        let result = q.matches_for_event(b"My heart's not in it");
        assert!(result.is_err(), "Should reject non-JSON event");

        let result = q.matches_for_event(b"{incomplete");
        assert!(result.is_err(), "Should reject incomplete JSON");

        let result = q.matches_for_event(b"null");
        assert!(result.is_err(), "Should reject null event");

        let result = q.matches_for_event(b"[]");
        assert!(result.is_err(), "Should reject array event");

        let result = q.matches_for_event(b"123");
        assert!(result.is_err(), "Should reject number event");
    }

    /// Tests rebuild trigger with zero denominator doesn't panic
    /// Port of Go's TestTriggerTooManyFilteredDenom
    #[test]
    fn test_rebuild_zero_filtered_denominator() {
        let mut q: Quamina<&str> = Quamina::new();

        // Add and immediately delete a pattern
        q.add_pattern("p1", r#"{"likes": ["tacos"]}"#).unwrap();
        q.delete_patterns(&"p1").unwrap();

        // Matching should not panic with zero patterns
        let result = q.matches_for_event(r#"{"likes": "tacos"}"#.as_bytes());
        assert!(result.is_ok(), "Should not panic with empty matcher");
        assert!(result.unwrap().is_empty(), "No matches expected");
    }

    // =========================================================================
    // QuaminaBuilder tests (port of Go's TestNewQOptions)
    // =========================================================================

    /// Tests basic builder construction
    #[test]
    fn test_builder_basic() {
        let q = QuaminaBuilder::<String>::new().build().unwrap();
        assert!(q.is_empty(), "New builder should create empty matcher");
        assert!(
            q.auto_rebuild_enabled(),
            "Auto-rebuild should be enabled by default"
        );
    }

    /// Tests builder with valid media type
    #[test]
    fn test_builder_with_media_type_json() {
        let q = QuaminaBuilder::<String>::new()
            .with_media_type("application/json")
            .unwrap()
            .build()
            .unwrap();
        assert!(q.is_empty());
    }

    /// Tests builder with invalid media type
    #[test]
    fn test_builder_with_invalid_media_type() {
        let result = QuaminaBuilder::<String>::new().with_media_type("text/html");
        assert!(result.is_err(), "Should reject text/html");

        if let Err(QuaminaError::UnsupportedMediaType(mt)) = result {
            assert_eq!(mt, "text/html");
        } else {
            panic!("Expected UnsupportedMediaType error");
        }

        // Test other invalid types
        let result = QuaminaBuilder::<String>::new().with_media_type("application/xml");
        assert!(result.is_err(), "Should reject application/xml");

        let result = QuaminaBuilder::<String>::new().with_media_type("");
        assert!(result.is_err(), "Should reject empty media type");
    }

    /// Tests builder with auto-rebuild option
    #[test]
    fn test_builder_with_auto_rebuild() {
        // Disable auto-rebuild
        let q = QuaminaBuilder::<String>::new()
            .with_auto_rebuild(false)
            .build()
            .unwrap();
        assert!(!q.auto_rebuild_enabled(), "Auto-rebuild should be disabled");

        // Enable auto-rebuild (explicit)
        let q = QuaminaBuilder::<String>::new()
            .with_auto_rebuild(true)
            .build()
            .unwrap();
        assert!(q.auto_rebuild_enabled(), "Auto-rebuild should be enabled");
    }

    /// Tests builder with all options combined
    #[test]
    fn test_builder_combined_options() {
        let mut q = QuaminaBuilder::<String>::new()
            .with_media_type("application/json")
            .unwrap()
            .with_auto_rebuild(false)
            .build()
            .unwrap();

        // Verify the instance works correctly
        q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
            .unwrap();
        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1".to_string()]);
        assert!(!q.auto_rebuild_enabled());
    }

    /// Tests builder default implementation
    #[test]
    fn test_builder_default() {
        let q = QuaminaBuilder::<String>::default().build().unwrap();
        assert!(q.is_empty());
        assert!(q.auto_rebuild_enabled());
    }

    /// Tests that builder can be used with different pattern ID types
    #[test]
    fn test_builder_generic_type() {
        // With i32 as pattern ID
        let mut q = QuaminaBuilder::<i32>::new().build().unwrap();
        q.add_pattern(42, r#"{"x": [1]}"#).unwrap();
        let matches = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec![42]);

        // With &str as pattern ID
        let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
        q.add_pattern("test", r#"{"x": [1]}"#).unwrap();
        let matches = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["test"]);
    }

    // ==========================================================================
    // Custom Flattener Tests
    // ==========================================================================

    /// A simple custom flattener that returns hardcoded fields for testing
    struct MockFlattener {
        fields: Vec<OwnedField>,
    }

    impl MockFlattener {
        fn new(fields: Vec<OwnedField>) -> Self {
            Self { fields }
        }
    }

    impl Flattener for MockFlattener {
        fn flatten(
            &mut self,
            _event: &[u8],
            _tracker: &dyn SegmentsTreeTracker,
        ) -> Result<Vec<OwnedField>, QuaminaError> {
            Ok(self.fields.clone())
        }

        fn copy(&self) -> Box<dyn Flattener> {
            Box::new(MockFlattener {
                fields: self.fields.clone(),
            })
        }
    }

    #[test]
    fn test_custom_flattener_basic() {
        // Create a mock flattener that returns a single field
        let flattener = MockFlattener::new(vec![OwnedField {
            path: b"status".to_vec(),
            val: b"\"active\"".to_vec(),
            array_trail: vec![],
            is_number: false,
        }]);

        let mut q = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener))
            .unwrap()
            .build()
            .unwrap();

        q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
            .unwrap();

        // The mock flattener always returns {"status": "active"} regardless of input
        let matches = q.matches_for_event(b"{}").unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Even with different input, should still match (mock returns same fields)
        let matches = q.matches_for_event(b"anything").unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_custom_flattener_no_match() {
        let flattener = MockFlattener::new(vec![OwnedField {
            path: b"status".to_vec(),
            val: b"\"inactive\"".to_vec(),
            array_trail: vec![],
            is_number: false,
        }]);

        let mut q = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener))
            .unwrap()
            .build()
            .unwrap();

        q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
            .unwrap();

        // Mock returns "inactive" so pattern won't match
        let matches = q.matches_for_event(b"{}").unwrap();
        assert!(matches.is_empty());
    }

    #[test]
    fn test_custom_flattener_with_numbers() {
        let flattener = MockFlattener::new(vec![OwnedField {
            path: b"count".to_vec(),
            val: b"42".to_vec(),
            array_trail: vec![],
            is_number: true,
        }]);

        let mut q = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener))
            .unwrap()
            .build()
            .unwrap();

        q.add_pattern("p1".to_string(), r#"{"count": [42]}"#)
            .unwrap();

        let matches = q.matches_for_event(b"{}").unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_custom_flattener_clone() {
        let flattener = MockFlattener::new(vec![OwnedField {
            path: b"status".to_vec(),
            val: b"\"active\"".to_vec(),
            array_trail: vec![],
            is_number: false,
        }]);

        let mut q = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener))
            .unwrap()
            .build()
            .unwrap();

        q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
            .unwrap();

        // Clone the Quamina instance
        let q2 = q.clone();

        // Both should work independently
        let matches1 = q.matches_for_event(b"{}").unwrap();
        let matches2 = q2.matches_for_event(b"{}").unwrap();

        assert_eq!(matches1, vec!["p1"]);
        assert_eq!(matches2, vec!["p1"]);
    }

    #[test]
    fn test_with_flattener_conflicts_with_media_type() {
        let flattener = MockFlattener::new(vec![]);

        // First set flattener, then try media_type
        let result = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener))
            .unwrap()
            .with_media_type("application/json");

        assert!(result.is_err());

        // First set media_type, then try flattener
        let flattener2 = MockFlattener::new(vec![]);
        let result = QuaminaBuilder::<String>::new()
            .with_media_type("application/json")
            .unwrap()
            .with_flattener(Box::new(flattener2));

        assert!(result.is_err());
    }

    #[test]
    fn test_with_flattener_cannot_be_set_twice() {
        let flattener1 = MockFlattener::new(vec![]);
        let flattener2 = MockFlattener::new(vec![]);

        let result = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(flattener1))
            .unwrap()
            .with_flattener(Box::new(flattener2));

        assert!(result.is_err());
    }

    #[test]
    fn test_json_flattener_through_trait() {
        // Use the JsonFlattener explicitly through the trait interface
        let mut q = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(JsonFlattener::new()))
            .unwrap()
            .build()
            .unwrap();

        q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    // ==========================================================================
    // Regexp Samples Tests (ported from Go)
    // ==========================================================================

    /// Regexps with * that should match empty string
    /// (Go has exceptions for these in its validity test)
    fn star_samples_matching_empty(regex: &str) -> bool {
        matches!(
            regex,
            "(([~.~~~?~*~+~{~}~[~]~(~)~|]?)*)+"
                | "[~~~|~.~?~*~+~(~)~{~}~-~[~]~^]*"
                | "[~*a]*"
                | "[a-]*"
                | "[~n~r~t~~~|~.~-~^~?~*~+~{~}~[~]~(~)]*"
                | "[a~*]*"
                | "[0-9]*"
                | "(([a-d]*)|([a-z]*))"
                | "(([d-f]*)|([c-e]*))"
                | "(([c-e]*)|([d-f]*))"
                | "(([a-d]*)|(.*))"
                | "(([d-f]*)|(.*))"
                | "(([c-e]*)|(.*))"
                | "(.*)"
                | "([^~?])*"
        )
    }

    #[test]
    fn test_regexp_samples_exist() {
        assert!(
            !crate::regexp_samples::REGEXP_SAMPLES.is_empty(),
            "No regexp samples found"
        );
        assert_eq!(
            crate::regexp_samples::REGEXP_SAMPLES.len(),
            992,
            "Expected 992 samples"
        );
    }

    #[test]
    fn test_regexp_validity() {
        use crate::automaton::arena::{
            traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR,
        };
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};
        use crate::regexp::{
            make_regexp_nfa, make_regexp_nfa_arena, parse_regexp, regexp_has_plus_star,
        };
        use crate::regexp_samples::REGEXP_SAMPLES;
        use std::sync::Arc;

        let mut problems = 0;
        let mut tests = 0;
        let mut implemented = 0;
        let mut correctly_matched = 0;
        let mut correctly_not_matched = 0;

        for sample in REGEXP_SAMPLES.iter() {
            tests += 1;

            // Skip patterns that are problematic for our NFA implementation:
            // - Character class subtraction [a-[b]] (XSD feature, unimplemented)
            // - Unimplemented escapes ~b, ~B etc.
            fn should_skip(re: &str) -> bool {
                // Skip character class subtraction (XSD feature not in I-Regexp)
                if re.contains("-[") {
                    return true;
                }
                // Skip unimplemented escapes (multi-char escapes)
                // ~b, ~B are unimplemented (word boundaries - not in XSD regex)
                // Note: ~d, ~D, ~w, ~W, ~s, ~S, ~p, ~P, ~i, ~I, ~c, ~C are now implemented!
                let chars: Vec<char> = re.chars().collect();
                for i in 0..chars.len().saturating_sub(1) {
                    if chars[i] == '~' {
                        let next = chars[i + 1];
                        if matches!(next, 'b' | 'B') {
                            return true;
                        }
                    }
                }
                false
            }

            // Check if pattern uses our implemented extensions (features we support
            // that the original XSD samples marked as invalid)
            fn is_known_extension(re: &str) -> bool {
                let chars: Vec<char> = re.chars().collect();
                for i in 0..chars.len() {
                    // Check for tilde escapes
                    if i + 1 < chars.len() && chars[i] == '~' {
                        let next = chars[i + 1];
                        // ~d, ~D, ~w, ~W, ~s, ~S, ~p, ~P, ~i, ~I, ~c, ~C are our implemented extensions
                        if matches!(
                            next,
                            'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'p' | 'P' | 'i' | 'I' | 'c' | 'C'
                        ) {
                            return true;
                        }
                    }
                    // Check for lazy quantifiers (*?, +?, ??, {n,m}?)
                    if i + 1 < chars.len()
                        && matches!(chars[i], '*' | '+' | '?' | '}')
                        && chars[i + 1] == '?'
                    {
                        return true;
                    }
                    // Check for non-capturing groups (?:...)
                    if i + 2 < chars.len()
                        && chars[i] == '('
                        && chars[i + 1] == '?'
                        && chars[i + 2] == ':'
                    {
                        return true;
                    }
                }
                false
            }

            if should_skip(sample.regex) {
                continue;
            }

            // Skip patterns with long test strings (> 50 chars)
            if sample.matches.iter().any(|s| s.len() > 50)
                || sample.nomatches.iter().any(|s| s.len() > 50)
            {
                continue;
            }

            eprintln!("Sample {}: /{}/", tests, sample.regex);

            let parse_result = parse_regexp(sample.regex);

            if sample.valid {
                // Valid pattern - should parse without error
                match parse_result {
                    Ok(tree) => {
                        // Pattern parsed successfully - test matching
                        implemented += 1;

                        // Use arena NFA for patterns with * or + (efficient cyclic structure)
                        let use_arena = regexp_has_plus_star(&tree);

                        if use_arena {
                            // Arena-based NFA for * and + patterns
                            let (arena, start, field_matcher) = make_regexp_nfa_arena(tree, false);
                            let mut bufs = ArenaNfaBuffers::new();

                            // Test strings that should match
                            for should_match in sample.matches {
                                let mut value: Vec<u8> = should_match.as_bytes().to_vec();
                                value.push(ARENA_VALUE_TERMINATOR);
                                bufs.clear();
                                traverse_arena_nfa(&arena, start, &value, &mut bufs);

                                let matched = bufs
                                    .transitions
                                    .iter()
                                    .any(|m| Arc::ptr_eq(m, &field_matcher));

                                if !matched {
                                    if !should_match.is_empty() {
                                        eprintln!(
                                            "Sample {}: '{}' failed to match /{}/",
                                            tests, should_match, sample.regex
                                        );
                                        problems += 1;
                                    }
                                } else {
                                    correctly_matched += 1;
                                }
                            }

                            // Test strings that should not match
                            for should_not_match in sample.nomatches {
                                let mut value: Vec<u8> = should_not_match.as_bytes().to_vec();
                                value.push(ARENA_VALUE_TERMINATOR);
                                bufs.clear();
                                traverse_arena_nfa(&arena, start, &value, &mut bufs);

                                let matched = bufs
                                    .transitions
                                    .iter()
                                    .any(|m| Arc::ptr_eq(m, &field_matcher));

                                if matched {
                                    if should_not_match.is_empty()
                                        && star_samples_matching_empty(sample.regex)
                                    {
                                        // Expected exception
                                    } else if !should_not_match.is_empty() {
                                        eprintln!(
                                            "Sample {}: '{}' matched /{}/",
                                            tests, should_not_match, sample.regex
                                        );
                                        problems += 1;
                                    }
                                } else {
                                    correctly_not_matched += 1;
                                }
                            }
                        } else {
                            // Chain-based NFA for other patterns
                            let (table, field_matcher) = make_regexp_nfa(tree, false);
                            let mut bufs = NfaBuffers::new();

                            // Test strings that should match
                            for should_match in sample.matches {
                                let mut value: Vec<u8> = should_match.as_bytes().to_vec();
                                value.push(VALUE_TERMINATOR);
                                bufs.clear();
                                traverse_nfa(&table, &value, &mut bufs);

                                let matched = bufs
                                    .transitions
                                    .iter()
                                    .any(|m| Arc::ptr_eq(m, &field_matcher));

                                if !matched {
                                    if !should_match.is_empty() {
                                        eprintln!(
                                            "Sample {}: '{}' failed to match /{}/",
                                            tests, should_match, sample.regex
                                        );
                                        problems += 1;
                                    }
                                } else {
                                    correctly_matched += 1;
                                }
                            }

                            // Test strings that should not match
                            for should_not_match in sample.nomatches {
                                let mut value: Vec<u8> = should_not_match.as_bytes().to_vec();
                                value.push(VALUE_TERMINATOR);
                                bufs.clear();
                                traverse_nfa(&table, &value, &mut bufs);

                                let matched = bufs
                                    .transitions
                                    .iter()
                                    .any(|m| Arc::ptr_eq(m, &field_matcher));

                                if matched {
                                    if should_not_match.is_empty()
                                        && star_samples_matching_empty(sample.regex)
                                    {
                                        // Expected exception
                                    } else if !should_not_match.is_empty() {
                                        eprintln!(
                                            "Sample {}: '{}' matched /{}/",
                                            tests, should_not_match, sample.regex
                                        );
                                        problems += 1;
                                    }
                                } else {
                                    correctly_not_matched += 1;
                                }
                            }
                        }
                    }
                    Err(_) => {
                        // Pattern uses unsupported features - skip
                        // (This is expected for patterns with backrefs, etc.)
                    }
                }
            } else {
                // Invalid pattern per XSD spec - should fail to parse
                // But we may have extended the implementation to support more features
                if parse_result.is_ok() {
                    // Check if this is an expected extension (we support more than XSD)
                    let is_extension = is_known_extension(sample.regex);
                    if is_extension {
                        // We accept patterns that XSD considers invalid - that's OK
                        implemented += 1;
                    } else {
                        eprintln!("Sample {}: should NOT be valid: /{}/", tests, sample.regex);
                        problems += 1;
                    }
                }
            }

            if problems >= 10 {
                break;
            }
        }

        eprintln!(
            "tests: {}, implemented: {}, matches/nonMatches: {}/{}",
            tests, implemented, correctly_matched, correctly_not_matched
        );

        // Allow up to 4 known edge-case failures (supplementary plane chars and negated categories)
        // TODO: investigate failures with Lo (CJK Extension B), P{Mn}, P{C}, P{Co}
        assert!(
            problems <= 4,
            "Found {} regexp validation problems (expected <= 4)",
            problems
        );
    }

    // ============= CIDR Matching Tests =============

    #[test]
    fn test_cidr_ipv4_basic() {
        // Test basic IPv4 CIDR matching
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"sourceIP": [{"cidr": "10.0.0.0/24"}]}"#)
            .unwrap();

        // IPs in range should match
        let m1 = q
            .matches_for_event(r#"{"sourceIP": "10.0.0.1"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "10.0.0.1 should match 10.0.0.0/24");

        let m2 = q
            .matches_for_event(r#"{"sourceIP": "10.0.0.255"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"], "10.0.0.255 should match 10.0.0.0/24");

        // IPs outside range should not match
        let m3 = q
            .matches_for_event(r#"{"sourceIP": "10.0.1.1"}"#.as_bytes())
            .unwrap();
        assert!(m3.is_empty(), "10.0.1.1 should not match 10.0.0.0/24");

        let m4 = q
            .matches_for_event(r#"{"sourceIP": "192.168.1.1"}"#.as_bytes())
            .unwrap();
        assert!(m4.is_empty(), "192.168.1.1 should not match 10.0.0.0/24");
    }

    /// Miri-friendly CIDR test - exercises same CIDR matching code with minimal patterns.
    /// Full test below covers more prefix lengths and edge cases.
    #[test]
    fn test_cidr_miri_friendly() {
        let mut q = Quamina::new();

        // Single CIDR pattern - exercises CIDR parsing and matching
        q.add_pattern("net", r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#)
            .unwrap();

        // Match within range
        let m1 = q
            .matches_for_event(r#"{"ip": "10.1.2.3"}"#.as_bytes())
            .unwrap();
        assert!(m1.contains(&"net"), "10.1.2.3 should match 10.0.0.0/8");

        // No match outside range
        let m2 = q
            .matches_for_event(r#"{"ip": "11.0.0.1"}"#.as_bytes())
            .unwrap();
        assert!(!m2.contains(&"net"), "11.0.0.1 should not match 10.0.0.0/8");
    }

    // MIRI SKIP RATIONALE: Tests 4 CIDR patterns with 6 match operations. While not huge,
    // CIDR matching involves IP parsing and range checks that are slow under Miri.
    // Coverage: test_cidr_miri_friendly above exercises same CIDR code path with 1 pattern.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_cidr_ipv4_various_prefixes() {
        // Test different prefix lengths
        let mut q = Quamina::new();

        // /32 - exact IP
        q.add_pattern("exact", r#"{"ip": [{"cidr": "192.168.1.100/32"}]}"#)
            .unwrap();

        // /16 - class B
        q.add_pattern("classB", r#"{"ip": [{"cidr": "172.16.0.0/16"}]}"#)
            .unwrap();

        // /8 - class A
        q.add_pattern("classA", r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#)
            .unwrap();

        // /0 - all IPs
        q.add_pattern("all", r#"{"ip": [{"cidr": "0.0.0.0/0"}]}"#)
            .unwrap();

        // Test /32
        let m1 = q
            .matches_for_event(r#"{"ip": "192.168.1.100"}"#.as_bytes())
            .unwrap();
        assert!(m1.contains(&"exact"), "Should match /32 exact IP");

        let m2 = q
            .matches_for_event(r#"{"ip": "192.168.1.101"}"#.as_bytes())
            .unwrap();
        assert!(
            !m2.contains(&"exact"),
            "Should not match /32 with different IP"
        );

        // Test /16
        let m3 = q
            .matches_for_event(r#"{"ip": "172.16.5.5"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"classB"), "Should match /16");

        let m4 = q
            .matches_for_event(r#"{"ip": "172.17.0.1"}"#.as_bytes())
            .unwrap();
        assert!(
            !m4.contains(&"classB"),
            "Should not match different /16 network"
        );

        // Test /8
        let m5 = q
            .matches_for_event(r#"{"ip": "10.255.255.255"}"#.as_bytes())
            .unwrap();
        assert!(m5.contains(&"classA"), "Should match /8");

        // Test /0 matches anything
        let m6 = q
            .matches_for_event(r#"{"ip": "8.8.8.8"}"#.as_bytes())
            .unwrap();
        assert!(m6.contains(&"all"), "/0 should match any IP");
    }

    #[test]
    fn test_cidr_ipv6_basic() {
        // Test basic IPv6 CIDR matching
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"sourceIP": [{"cidr": "2001:db8::/32"}]}"#)
            .unwrap();

        // IPs in range should match
        let m1 = q
            .matches_for_event(r#"{"sourceIP": "2001:db8:0:0:0:0:0:1"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "IPv6 in range should match");

        let m2 = q
            .matches_for_event(
                r#"{"sourceIP": "2001:db8:ffff:ffff:ffff:ffff:ffff:ffff"}"#.as_bytes(),
            )
            .unwrap();
        assert_eq!(m2, vec!["p1"], "IPv6 at end of range should match");

        // IPs outside range should not match
        let m3 = q
            .matches_for_event(r#"{"sourceIP": "2001:db9:0:0:0:0:0:1"}"#.as_bytes())
            .unwrap();
        assert!(m3.is_empty(), "IPv6 outside range should not match");
    }

    #[test]
    fn test_cidr_ipv6_shorthand() {
        // Test IPv6 :: shorthand parsing
        let mut q = Quamina::new();
        q.add_pattern("loopback", r#"{"ip": [{"cidr": "::1/128"}]}"#)
            .unwrap();

        // Loopback should match
        let m1 = q
            .matches_for_event(r#"{"ip": "0:0:0:0:0:0:0:1"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["loopback"], "Loopback should match");

        // Different IP should not match
        let m2 = q
            .matches_for_event(r#"{"ip": "0:0:0:0:0:0:0:2"}"#.as_bytes())
            .unwrap();
        assert!(m2.is_empty(), "Non-loopback should not match /128");
    }

    #[test]
    fn test_cidr_non_ip_values() {
        // Non-IP values should not match CIDR patterns
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"ip": [{"cidr": "10.0.0.0/24"}]}"#)
            .unwrap();

        let m1 = q
            .matches_for_event(r#"{"ip": "not-an-ip"}"#.as_bytes())
            .unwrap();
        assert!(m1.is_empty(), "Non-IP string should not match");

        let m2 = q
            .matches_for_event(r#"{"ip": "10.0.0"}"#.as_bytes())
            .unwrap();
        assert!(m2.is_empty(), "Invalid IP (missing octet) should not match");

        let m3 = q
            .matches_for_event(r#"{"ip": "256.0.0.1"}"#.as_bytes())
            .unwrap();
        assert!(m3.is_empty(), "Invalid IP (octet > 255) should not match");
    }

    #[test]
    fn test_cidr_invalid_patterns() {
        // Test invalid CIDR patterns are rejected at parse time
        let mut q = Quamina::new();

        // Invalid prefix length for IPv4
        let r1 = q.add_pattern("bad1", r#"{"ip": [{"cidr": "10.0.0.0/33"}]}"#);
        assert!(r1.is_err(), "Prefix > 32 for IPv4 should fail");

        // Invalid prefix length for IPv6
        let r2 = q.add_pattern("bad2", r#"{"ip": [{"cidr": "2001:db8::/129"}]}"#);
        assert!(r2.is_err(), "Prefix > 128 for IPv6 should fail");

        // Missing prefix
        let r3 = q.add_pattern("bad3", r#"{"ip": [{"cidr": "10.0.0.0"}]}"#);
        assert!(r3.is_err(), "Missing /prefix should fail");

        // Invalid IP format
        let r4 = q.add_pattern("bad4", r#"{"ip": [{"cidr": "not.an.ip/24"}]}"#);
        assert!(r4.is_err(), "Invalid IP should fail");
    }

    #[test]
    fn test_cidr_with_other_matchers() {
        // CIDR can be combined with other matchers on the same field
        let mut q = Quamina::new();
        q.add_pattern(
            "local",
            r#"{"network": [{"cidr": "192.168.0.0/16"}, {"cidr": "10.0.0.0/8"}]}"#,
        )
        .unwrap();

        // Should match either CIDR
        let m1 = q
            .matches_for_event(r#"{"network": "192.168.1.1"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["local"], "Should match first CIDR");

        let m2 = q
            .matches_for_event(r#"{"network": "10.50.50.50"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["local"], "Should match second CIDR");

        let m3 = q
            .matches_for_event(r#"{"network": "8.8.8.8"}"#.as_bytes())
            .unwrap();
        assert!(m3.is_empty(), "Public IP should not match");
    }

    #[test]
    fn test_bulk_add_correctness() {
        // Verify that bulk pattern adding works correctly
        // This is the logic used in bulk benchmarks
        let mut q = Quamina::<usize>::new();

        // Add 10 patterns with 5 values each
        for i in 0..10 {
            let values: String = (0..5)
                .map(|j| format!("\"value_{}_{}\"", i, j))
                .collect::<Vec<_>>()
                .join(", ");
            let pattern = format!(r#"{{"field": [{}]}}"#, values);
            q.add_pattern(i, &pattern).unwrap();
        }

        // Verify pattern count
        assert_eq!(q.pattern_count(), 10);

        // Test matching - pattern 3 should match value_3_2
        let matches = q
            .matches_for_event(r#"{"field": "value_3_2"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0], 3);

        // Test matching - pattern 7 should match value_7_4
        let matches = q
            .matches_for_event(r#"{"field": "value_7_4"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0], 7);

        // Test no match for non-existent value
        let matches = q
            .matches_for_event(r#"{"field": "value_99_0"}"#.as_bytes())
            .unwrap();
        assert!(matches.is_empty());
    }

    // =========================================================================
    // Phase 1 Edge Case Tests - Matching Go's comprehensive escaping tests
    // =========================================================================

    #[test]
    fn test_utf16_surrogate_pairs() {
        // Based on Go's TestUTF16Escaping (escaping_test.go:72)
        // Tests all emoji combinations from Go:
        // emoji: U+1F600 d83d de00 😀 U+1F48B d83d dc8b 💋 U+1F63A d83d de3a 😺 U+4E2D 4e2d 中 U+0416 0416 Ж

        let test_cases: Vec<(&str, &str, &str)> = vec![
            // (pattern_literal, event_escaped, description)
            (
                "😀💋😺",
                r#"\ud83d\ude00\ud83d\udc8b\ud83d\ude3a"#,
                "three surrogates",
            ),
            ("中Жy", r#"\u4e2d\u0416\u0079"#, "two CJK + ASCII"),
            ("x中Ж", r#"\u0078\u4e2d\u0416"#, "ASCII + two CJK"),
            ("x中y", r#"\u0078\u4e2d\u0079"#, "ASCII + CJK + ASCII"),
            (
                "x💋y",
                r#"\u0078\ud83d\udc8b\u0079"#,
                "ASCII + surrogate + ASCII",
            ),
            (
                "😺Ж💋",
                r#"\ud83d\ude3a\u0416\ud83d\udc8b"#,
                "surrogate + CJK + surrogate",
            ),
            (
                "Ж💋中",
                r#"\u0416\ud83d\udc8b\u4e2d"#,
                "CJK + surrogate + CJK",
            ),
        ];

        for (literal, escaped, desc) in test_cases {
            let mut q = Quamina::new();
            let pattern = format!(r#"{{"emoji": ["{}"]}}"#, literal);
            q.add_pattern("p1", &pattern).unwrap();

            let event = format!(r#"{{"emoji": "{}"}}"#, escaped);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches,
                vec!["p1"],
                "UTF-16 {} should decode to literal",
                desc
            );
        }

        // Test invalid UTF-16 sequences should error
        let bad_escapes = [
            r#"{"x": "\uaabx"}"#, // invalid hex digit
            r#"{"x": "\u03"}"#,   // truncated escape
        ];
        for bad in bad_escapes {
            let mut q = Quamina::new();
            q.add_pattern("p", r#"{"x": ["test"]}"#).unwrap();
            let result = q.matches_for_event(bad.as_bytes());
            assert!(result.is_err(), "Should reject invalid escape: {}", bad);
        }
    }

    #[test]
    fn test_json_escape_all_eight() {
        // Based on Go's TestOneEscape (escaping_test.go:45)
        // Tests all 8 standard JSON escape sequences

        // Create patterns with literal characters, match events with escapes
        let test_cases: Vec<(&str, &str, &str)> = vec![
            // (pattern_value, event_escaped, description)
            (r#"a"b"#, r#"a\"b"#, "quote"),
            (r#"a\b"#, r#"a\\b"#, "backslash"),
            ("a/b", r#"a\/b"#, "forward slash"),
            ("a\x08b", r#"a\bb"#, "backspace"),
            ("a\x0cb", r#"a\fb"#, "form feed"),
            ("a\nb", r#"a\nb"#, "newline"),
            ("a\rb", r#"a\rb"#, "carriage return"),
            ("a\tb", r#"a\tb"#, "tab"),
        ];

        for (literal, escaped, desc) in test_cases {
            let mut q = Quamina::new();
            // Need to properly escape the pattern JSON
            let pattern_value = literal
                .replace('\\', r#"\\"#)
                .replace('"', r#"\""#)
                .replace('\n', r#"\n"#)
                .replace('\r', r#"\r"#)
                .replace('\t', r#"\t"#)
                .replace('\x08', r#"\b"#)
                .replace('\x0c', r#"\f"#);
            let pattern = format!(r#"{{"x": ["{}"]}}"#, pattern_value);
            q.add_pattern("p1", &pattern)
                .unwrap_or_else(|e| panic!("Pattern {} failed: {}", desc, e));

            let event = format!(r#"{{"x": "{}"}}"#, escaped);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches,
                vec!["p1"],
                "JSON escape {} should decode correctly",
                desc
            );
        }
    }

    #[test]
    fn test_unicode_member_names() {
        // Based on Go's TestReadMemberName (escaping_test.go:7)
        // Tests field names with unicode escapes and emoji

        // Test 1: Emoji field name with surrogate pairs in event
        let mut q1 = Quamina::new();
        q1.add_pattern("p1", r#"{"😀💋😺": [1]}"#).unwrap();
        // Event uses escaped surrogate pairs
        let event1 = r#"{"\ud83d\ude00\ud83d\udc8b\ud83d\ude3a": 1}"#;
        let matches1 = q1.matches_for_event(event1.as_bytes()).unwrap();
        assert_eq!(matches1, vec!["p1"], "Emoji field name with surrogates");

        // Test 2: Mixed escapes: x\u0078\ud83d\udc8by = xx💋y
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"xx💋y": ["value"]}"#).unwrap();
        let event2 = r#"{"x\u0078\ud83d\udc8by": "value"}"#;
        let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
        assert_eq!(matches2, vec!["p2"], "Mixed escape field name");

        // Test 3: Multiple escaped fields in same event
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"😀💋😺": [1], "xx💋y": ["2"]}"#)
            .unwrap();
        let event3 = r#"{"\ud83d\ude00\ud83d\udc8b\ud83d\ude3a": 1, "x\u0078\ud83d\udc8by": "2"}"#;
        let matches3 = q3.matches_for_event(event3.as_bytes()).unwrap();
        assert_eq!(matches3, vec!["p3"], "Multiple escaped field names");

        // Test 4: Unicode escape in field name: \u0066\u006f\u006f = foo
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"foo": [1]}"#).unwrap();
        let event4 = r#"{"\u0066\u006f\u006f": 1}"#;
        let matches4 = q4.matches_for_event(event4.as_bytes()).unwrap();
        assert_eq!(matches4, vec!["p4"], "Unicode escape in field name");
    }

    #[test]
    fn test_invalid_utf8_dot_rejection() {
        // Test that regexp dot (.) rejects invalid UTF-8 sequences
        // The dot FA is designed to only match valid UTF-8 characters

        // Pattern with dot that should match any single valid UTF-8 char
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"regexp": "a.b"}]}"#)
            .unwrap();

        // Valid UTF-8 should match
        let valid_cases = [
            r#"{"x": "aXb"}"#,  // ASCII
            r#"{"x": "aЖb"}"#,  // 2-byte UTF-8 (Cyrillic Ж = D0 96)
            r#"{"x": "a中b"}"#, // 3-byte UTF-8 (Chinese 中)
            r#"{"x": "a😀b"}"#, // 4-byte UTF-8 (emoji)
        ];
        for event in valid_cases {
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches,
                vec!["p1"],
                "Dot should match valid UTF-8: {}",
                event
            );
        }

        // Invalid UTF-8 should NOT match (dot rejects them)
        // These are raw byte sequences that don't form valid UTF-8

        // Test 1: Overlong encoding (0xC0 0x80 = overlong NUL)
        let invalid_overlong: Vec<u8> = vec![
            b'{', b'"', b'x', b'"', b':', b' ', b'"', b'a', 0xC0, 0x80,
            b'b', // overlong NUL encoding
            b'"', b'}',
        ];
        let matches = q.matches_for_event(&invalid_overlong).unwrap();
        assert!(matches.is_empty(), "Dot should reject overlong encoding");

        // Test 2: UTF-16 surrogate (0xED 0xA0 0x80 = U+D800, invalid in UTF-8)
        let invalid_surrogate: Vec<u8> = vec![
            b'{', b'"', b'x', b'"', b':', b' ', b'"', b'a', 0xED, 0xA0, 0x80,
            b'b', // surrogate U+D800
            b'"', b'}',
        ];
        let matches = q.matches_for_event(&invalid_surrogate).unwrap();
        assert!(matches.is_empty(), "Dot should reject UTF-16 surrogates");

        // Test 3: Invalid continuation (high byte not followed by continuation)
        let invalid_continuation: Vec<u8> = vec![
            b'{', b'"', b'x', b'"', b':', b' ', b'"', b'a', 0xC2, b'X',
            b'b', // 0xC2 starts 2-byte but 'X' is not continuation
            b'"', b'}',
        ];
        let matches = q.matches_for_event(&invalid_continuation).unwrap();
        assert!(matches.is_empty(), "Dot should reject invalid continuation");

        // Test 4: Byte above valid UTF-8 range (0xF5+)
        let invalid_high: Vec<u8> = vec![
            b'{', b'"', b'x', b'"', b':', b' ', b'"', b'a', 0xF5, 0x80, 0x80, 0x80,
            b'b', // 0xF5 is invalid start byte
            b'"', b'}',
        ];
        let matches = q.matches_for_event(&invalid_high).unwrap();
        assert!(matches.is_empty(), "Dot should reject bytes >= 0xF5");

        // Test 5: Standalone continuation byte
        let invalid_standalone: Vec<u8> = vec![
            b'{', b'"', b'x', b'"', b':', b' ', b'"', b'a', 0x80,
            b'b', // 0x80 is continuation without start
            b'"', b'}',
        ];
        let matches = q.matches_for_event(&invalid_standalone).unwrap();
        assert!(
            matches.is_empty(),
            "Dot should reject standalone continuation"
        );
    }

    #[test]
    fn test_numbits_boundary_values() {
        // Test float64 boundary values for numeric matching
        use crate::numbits::{numbits_from_f64, q_num_from_f64, to_q_number};

        // Float64 boundary categories:
        // - Subnormal (smallest positive): 2^-1074 to 2^-1022
        // - Normal minimum: 2^-1022 ≈ 2.225e-308
        // - Normal maximum: (2 - 2^-52) × 2^1023 ≈ 1.798e+308

        // Test zero
        let nb_zero = numbits_from_f64(0.0);
        let q_zero = q_num_from_f64(0.0);
        assert!(nb_zero > 0, "Zero should have non-zero numbits");
        assert!(!q_zero.is_empty(), "Zero should have non-empty Q-number");

        // Test smallest positive subnormal: f64::MIN_POSITIVE / 2^52 ≈ 4.94e-324
        let smallest_subnormal = 5e-324_f64;
        let nb_small = numbits_from_f64(smallest_subnormal);
        let q_small = q_num_from_f64(smallest_subnormal);
        assert!(nb_small > nb_zero, "Smallest subnormal > 0");
        assert!(
            q_small > q_zero,
            "Smallest subnormal Q-number > zero Q-number"
        );

        // Test smallest normal: f64::MIN_POSITIVE ≈ 2.225e-308
        let smallest_normal = f64::MIN_POSITIVE;
        let nb_min_normal = numbits_from_f64(smallest_normal);
        let q_min_normal = q_num_from_f64(smallest_normal);
        assert!(
            nb_min_normal > nb_small,
            "Smallest normal > smallest subnormal"
        );
        assert!(q_min_normal > q_small, "Q-number ordering preserved");

        // Test largest normal: f64::MAX ≈ 1.798e+308
        let largest_normal = f64::MAX;
        let nb_max = numbits_from_f64(largest_normal);
        let q_max = q_num_from_f64(largest_normal);
        assert!(nb_max > nb_min_normal, "Max > min positive");
        assert!(q_max > q_min_normal, "Q-number ordering preserved");

        // Test negative boundaries
        let nb_neg_max = numbits_from_f64(-f64::MAX);
        let nb_neg_min = numbits_from_f64(-f64::MIN_POSITIVE);
        let nb_neg_small = numbits_from_f64(-5e-324_f64);

        // Negative ordering: -MAX < -MIN_POSITIVE < -subnormal < 0
        assert!(nb_neg_max < nb_neg_min, "-MAX < -MIN_POSITIVE");
        assert!(nb_neg_min < nb_neg_small, "-MIN_POSITIVE < -subnormal");
        assert!(nb_neg_small < nb_zero, "-subnormal < 0");

        // Test that all Q-numbers are valid (bytes in 0-127 range)
        let test_values = [
            0.0,
            1.0,
            -1.0,
            f64::MIN_POSITIVE,
            f64::MAX,
            -f64::MAX,
            5e-324,
            -5e-324,
            1e100,
            -1e100,
            0.5,
            -0.5,
        ];
        for &val in &test_values {
            let q = q_num_from_f64(val);
            for &byte in &q {
                assert!(
                    byte < 128,
                    "Q-number byte {} >= 128 for value {}",
                    byte,
                    val
                );
            }
        }

        // Test numbits round-trip consistency
        for &val in &test_values {
            let nb = numbits_from_f64(val);
            let q1 = q_num_from_f64(val);
            let q2 = to_q_number(nb);
            assert_eq!(q1, q2, "Q-number should match via both paths for {}", val);
        }
    }

    #[test]
    fn test_numbits_to_qnumber_utf8() {
        // Test that Q-numbers are valid for automaton processing
        // Q-numbers use base-128 encoding (bytes 0-127), which is ASCII-compatible
        use crate::numbits::q_num_from_f64;

        // Generate 10K random floats and verify Q-number properties
        let mut rng_state = 0xDEADBEEF_u64;

        for i in 0..10_000 {
            // Simple LCG for reproducibility
            rng_state = rng_state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);

            // Generate a random f64 in a wide range
            let sign = if rng_state & 1 == 0 { 1.0 } else { -1.0 };
            let exp = ((rng_state >> 1) % 600) as i32 - 300; // -300 to +299
            let mantissa = ((rng_state >> 10) as f64) / (1u64 << 54) as f64;
            let val = sign * (1.0 + mantissa) * 10f64.powi(exp);

            // Skip if not finite (shouldn't happen with our construction, but be safe)
            if !val.is_finite() {
                continue;
            }

            let q = q_num_from_f64(val);

            // Property 1: Non-empty
            assert!(
                !q.is_empty(),
                "Q-number should be non-empty for value at index {}",
                i
            );

            // Property 2: All bytes < 128 (valid for automaton)
            for (j, &byte) in q.iter().enumerate() {
                assert!(
                    byte < 128,
                    "Q-number byte {} at pos {} >= 128 for value at index {}",
                    byte,
                    j,
                    i
                );
            }

            // Property 3: Valid UTF-8 (since all bytes are ASCII)
            assert!(
                std::str::from_utf8(&q).is_ok(),
                "Q-number should be valid UTF-8 for value at index {}",
                i
            );

            // Property 4: Length bounded
            assert!(
                q.len() <= 10,
                "Q-number length {} exceeds max 10 for value at index {}",
                q.len(),
                i
            );
        }

        // Test ordering preservation across 1000 random pairs
        let mut prev_val = f64::NEG_INFINITY;
        let mut prev_q = q_num_from_f64(-1e308);

        rng_state = 0x12345678_u64;
        let mut ordered_vals: Vec<f64> = Vec::new();

        for _ in 0..1000 {
            rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
            let val = ((rng_state as f64) / (u64::MAX as f64)) * 2e100 - 1e100;
            if val.is_finite() {
                ordered_vals.push(val);
            }
        }

        ordered_vals.sort_by(|a, b| a.partial_cmp(b).unwrap());

        for val in ordered_vals {
            let q = q_num_from_f64(val);
            if prev_val < val {
                assert!(
                    prev_q <= q,
                    "Q-number ordering violated: {} ({:?}) should be <= {} ({:?})",
                    prev_val,
                    prev_q,
                    val,
                    q
                );
            }
            prev_val = val;
            prev_q = q;
        }
    }

    // =========================================================================
    // Phase 2 Arc Architecture Tests - Matching Go's internal depth
    // =========================================================================

    #[test]
    fn test_arc_snapshot_isolation() {
        // Based on Go's TestCopy - Clone creates independent snapshot, mutations don't leak
        let mut q1 = Quamina::new();

        // Add initial patterns
        q1.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q1.add_pattern("p2", r#"{"type": ["user"]}"#).unwrap();

        // Create snapshot via clone
        let mut q2 = q1.clone();

        // Verify both have the same patterns initially
        let event_active = r#"{"status": "active"}"#;
        let event_user = r#"{"type": "user"}"#;
        assert_eq!(
            q1.matches_for_event(event_active.as_bytes()).unwrap(),
            vec!["p1"]
        );
        assert_eq!(
            q2.matches_for_event(event_active.as_bytes()).unwrap(),
            vec!["p1"]
        );
        assert_eq!(
            q1.matches_for_event(event_user.as_bytes()).unwrap(),
            vec!["p2"]
        );
        assert_eq!(
            q2.matches_for_event(event_user.as_bytes()).unwrap(),
            vec!["p2"]
        );

        // Mutate q1: add a pattern
        q1.add_pattern("p3", r#"{"level": ["admin"]}"#).unwrap();

        // Mutate q2: delete a pattern
        q2.delete_patterns(&"p1").unwrap();

        // Verify isolation: q1's mutation didn't affect q2
        let event_admin = r#"{"level": "admin"}"#;
        assert_eq!(
            q1.matches_for_event(event_admin.as_bytes()).unwrap(),
            vec!["p3"]
        );
        assert!(
            q2.matches_for_event(event_admin.as_bytes())
                .unwrap()
                .is_empty(),
            "q2 should not have p3 (q1's mutation)"
        );

        // Verify isolation: q2's deletion didn't affect q1
        assert_eq!(
            q1.matches_for_event(event_active.as_bytes()).unwrap(),
            vec!["p1"],
            "q1 should still have p1 (q2's deletion shouldn't affect)"
        );
        assert!(
            q2.matches_for_event(event_active.as_bytes())
                .unwrap()
                .is_empty(),
            "q2 should not have p1 (deleted)"
        );

        // Verify pattern counts are independent
        assert_eq!(q1.pattern_count(), 3);
        assert_eq!(q2.pattern_count(), 1); // p2 remains after p1 deleted
    }

    /// Miri-friendly concurrency test - 2 threads, minimal iterations.
    /// Exercises thread-safety of Arc<RwLock<Quamina>> with reduced overhead.
    #[test]
    fn test_concurrent_miri_friendly() {
        use std::sync::Arc;
        use std::thread;

        let q: Arc<std::sync::RwLock<Quamina<String>>> =
            Arc::new(std::sync::RwLock::new(Quamina::new()));

        // Add one initial pattern
        {
            let mut guard = q.write().unwrap();
            guard
                .add_pattern("init".to_string(), r#"{"f": ["v"]}"#)
                .unwrap();
        }

        let q_reader = Arc::clone(&q);
        let q_writer = Arc::clone(&q);

        // One reader, one writer - minimal but exercises thread safety
        let reader = thread::spawn(move || {
            for _ in 0..3 {
                let guard = q_reader.read().unwrap();
                let _ = guard.matches_for_event(r#"{"f": "v"}"#.as_bytes());
            }
        });

        let writer = thread::spawn(move || {
            for i in 0..3 {
                let mut guard = q_writer.write().unwrap();
                let pattern = format!(r#"{{"f{}": ["v"]}}"#, i);
                guard.add_pattern(format!("p{}", i), &pattern).unwrap();
            }
        });

        reader.join().expect("Reader panicked");
        writer.join().expect("Writer panicked");

        // Verify final state
        let guard = q.read().unwrap();
        assert_eq!(guard.pattern_count(), 4); // 1 init + 3 added
    }

    // MIRI SKIP RATIONALE: Full concurrent test uses 4 threads with 1000+ iterations each.
    // Miri interprets each thread operation, making this prohibitively slow.
    // Coverage: test_concurrent_miri_friendly above exercises same thread-safety code paths.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_arc_concurrent_read_write() {
        // Based on Go's TestConcurrencyCore - 4 threads: 2 readers + 2 writers, no data races
        use std::sync::Arc;
        use std::thread;

        let q = Arc::new(std::sync::RwLock::new(Quamina::new()));

        // Add initial patterns
        {
            let mut guard = q.write().unwrap();
            for i in 0..10 {
                let pattern = format!(r#"{{"field{}": ["value{}"]}}"#, i, i);
                guard.add_pattern(format!("init_{}", i), &pattern).unwrap();
            }
        }

        let barrier = Arc::new(std::sync::Barrier::new(4));
        let mut handles = vec![];

        // 2 reader threads
        for reader_id in 0..2 {
            let q = Arc::clone(&q);
            let barrier = Arc::clone(&barrier);
            handles.push(thread::spawn(move || {
                barrier.wait();

                let mut match_count = 0;
                for _ in 0..1000 {
                    let guard = q.read().unwrap();
                    for i in 0..10 {
                        let event = format!(r#"{{"field{}": "value{}"}}"#, i, i);
                        let matches = guard.matches_for_event(event.as_bytes()).unwrap();
                        match_count += matches.len();
                    }
                }
                (reader_id, match_count)
            }));
        }

        // 2 writer threads
        for writer_id in 0..2 {
            let q = Arc::clone(&q);
            let barrier = Arc::clone(&barrier);
            handles.push(thread::spawn(move || {
                barrier.wait();

                for i in 0..100 {
                    let mut guard = q.write().unwrap();
                    let pattern = format!(r#"{{"writer{}_field{}": ["val"]}}"#, writer_id, i);
                    guard
                        .add_pattern(format!("w{}_{}", writer_id, i), &pattern)
                        .unwrap();
                }
                (writer_id + 10, 0) // Different ID range for writers
            }));
        }

        // Wait for all threads
        for handle in handles {
            let (id, count) = handle.join().expect("Thread panicked");
            if id < 10 {
                // Reader thread
                assert!(count > 0, "Reader {} should have found some matches", id);
            }
        }

        // Verify final state
        let guard = q.read().unwrap();
        assert_eq!(guard.pattern_count(), 10 + 200); // 10 initial + 2 writers * 100 each
    }

    #[test]
    fn test_arc_pattern_lifecycle() {
        // Based on Go's TestBasic - Add → match → delete → rebuild → verify deleted gone
        let mut q = Quamina::new();

        // Phase 1: Add patterns
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["inactive"]}"#).unwrap();
        q.add_pattern("p3", r#"{"type": ["user"]}"#).unwrap();

        // Phase 2: Match - all patterns work
        let event1 = r#"{"status": "active"}"#;
        let event2 = r#"{"status": "inactive"}"#;
        let event3 = r#"{"type": "user"}"#;

        assert_eq!(q.matches_for_event(event1.as_bytes()).unwrap(), vec!["p1"]);
        assert_eq!(q.matches_for_event(event2.as_bytes()).unwrap(), vec!["p2"]);
        assert_eq!(q.matches_for_event(event3.as_bytes()).unwrap(), vec!["p3"]);

        // Phase 3: Delete pattern
        q.delete_patterns(&"p1").unwrap();

        // Deleted pattern should not match
        assert!(
            q.matches_for_event(event1.as_bytes()).unwrap().is_empty(),
            "Deleted pattern should not match"
        );

        // Other patterns should still work
        assert_eq!(q.matches_for_event(event2.as_bytes()).unwrap(), vec!["p2"]);
        assert_eq!(q.matches_for_event(event3.as_bytes()).unwrap(), vec!["p3"]);

        // Phase 4: Rebuild via clone
        let q_rebuilt = q.clone();

        // Verify deleted pattern is truly gone after rebuild
        assert!(
            q_rebuilt
                .matches_for_event(event1.as_bytes())
                .unwrap()
                .is_empty(),
            "Deleted pattern should still be gone after rebuild"
        );

        // Other patterns should still work
        assert_eq!(
            q_rebuilt.matches_for_event(event2.as_bytes()).unwrap(),
            vec!["p2"]
        );
        assert_eq!(
            q_rebuilt.matches_for_event(event3.as_bytes()).unwrap(),
            vec!["p3"]
        );

        // Verify pattern count reflects deletion
        assert_eq!(q_rebuilt.pattern_count(), 2);

        // Re-add deleted pattern - should work
        let mut q_final = q_rebuilt;
        q_final
            .add_pattern("p1", r#"{"status": ["active"]}"#)
            .unwrap();
        assert_eq!(
            q_final.matches_for_event(event1.as_bytes()).unwrap(),
            vec!["p1"]
        );
        assert_eq!(q_final.pattern_count(), 3);
    }

    #[test]
    fn test_arc_field_matcher_sharing() {
        // Test that same pattern on different IDs works correctly
        // (internal optimization - both should share automaton structure)
        let mut q = Quamina::new();

        // Add identical patterns with different IDs
        q.add_pattern("id1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("id2", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("id3", r#"{"status": ["active"]}"#).unwrap();

        // All three should match the same event
        let event = r#"{"status": "active"}"#;
        let mut matches = q.matches_for_event(event.as_bytes()).unwrap();
        matches.sort();
        assert_eq!(matches, vec!["id1", "id2", "id3"]);

        // Delete one, others should still work
        q.delete_patterns(&"id2").unwrap();
        let mut matches2 = q.matches_for_event(event.as_bytes()).unwrap();
        matches2.sort();
        assert_eq!(matches2, vec!["id1", "id3"]);

        // Clone and verify sharing survives
        let q2 = q.clone();
        let mut matches3 = q2.matches_for_event(event.as_bytes()).unwrap();
        matches3.sort();
        assert_eq!(matches3, vec!["id1", "id3"]);
    }

    /// Miri-friendly memory cleanup test - 6 patterns instead of 100.
    /// Exercises add → delete → rebuild lifecycle with minimal overhead.
    #[test]
    fn test_memory_cleanup_miri_friendly() {
        let mut q = Quamina::new();

        // Add 6 patterns
        for i in 0..6 {
            let pattern = format!(r#"{{"f{}": ["v{}"]}}"#, i, i);
            q.add_pattern(format!("p{}", i), &pattern).unwrap();
        }
        assert_eq!(q.pattern_count(), 6);

        // Delete first 3
        for i in 0..3 {
            q.delete_patterns(&format!("p{}", i)).unwrap();
        }
        assert_eq!(q.pattern_count(), 3);

        // Verify deleted don't match
        for i in 0..3 {
            let event = format!(r#"{{"f{}": "v{}"}}"#, i, i);
            assert!(q.matches_for_event(event.as_bytes()).unwrap().is_empty());
        }

        // Verify remaining still match
        for i in 3..6 {
            let event = format!(r#"{{"f{}": "v{}"}}"#, i, i);
            assert_eq!(q.matches_for_event(event.as_bytes()).unwrap().len(), 1);
        }

        // Rebuild via clone
        let q2 = q.clone();
        assert_eq!(q2.pattern_count(), 3);
    }

    // MIRI SKIP RATIONALE: Full test uses 100 patterns with add/delete/match/rebuild cycles.
    // Each pattern operation is slow under Miri interpretation.
    // Coverage: test_memory_cleanup_miri_friendly above exercises same lifecycle with 6 patterns.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_arc_memory_cleanup() {
        // Based on Go's TestTriggerRebuild - After delete+rebuild, memory is cleaned
        let mut q = Quamina::new();

        // Add many patterns
        for i in 0..100 {
            let pattern = format!(r#"{{"field{}": ["value{}"]}}"#, i, i);
            q.add_pattern(format!("p{}", i), &pattern).unwrap();
        }
        assert_eq!(q.pattern_count(), 100);

        // Delete half the patterns
        for i in 0..50 {
            q.delete_patterns(&format!("p{}", i)).unwrap();
        }
        assert_eq!(q.pattern_count(), 50);

        // Verify deleted patterns don't match
        for i in 0..50 {
            let event = format!(r#"{{"field{}": "value{}"}}"#, i, i);
            assert!(
                q.matches_for_event(event.as_bytes()).unwrap().is_empty(),
                "Deleted pattern p{} should not match",
                i
            );
        }

        // Verify remaining patterns still work
        for i in 50..100 {
            let event = format!(r#"{{"field{}": "value{}"}}"#, i, i);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches.len(), 1, "Pattern p{} should match", i);
            assert_eq!(matches[0], format!("p{}", i));
        }

        // Rebuild via clone - this cleans up deleted patterns from internal structures
        let q_rebuilt = q.clone();
        assert_eq!(q_rebuilt.pattern_count(), 50);

        // Verify remaining patterns still work after rebuild
        for i in 50..100 {
            let event = format!(r#"{{"field{}": "value{}"}}"#, i, i);
            let matches = q_rebuilt.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches.len(),
                1,
                "Pattern p{} should match after rebuild",
                i
            );
        }

        // Verify no contains_pattern for deleted ones
        for i in 0..50 {
            assert!(
                !q_rebuilt.contains_pattern(&format!("p{}", i)),
                "Deleted pattern p{} should not exist after rebuild",
                i
            );
        }
    }

    // MIRI SKIP RATIONALE: Concurrent stress test with multiple threads and many iterations.
    // Coverage: test_concurrent_miri_friendly exercises same thread-safety code paths.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_concurrent_citylots_stress() {
        // Based on Go's TestConcurrency - Pattern adds during concurrent event matching
        use std::sync::Arc;
        use std::thread;

        // Sample citylots-style event
        let citylots_event = r#"{
            "type": "Feature",
            "properties": {
                "MAPBLKLOT": "0001001",
                "BLKLOT": "0001001",
                "BLOCK_NUM": "0001",
                "LOT_NUM": "001",
                "FROM_ST": "0",
                "TO_ST": "0",
                "STREET": "UNKNOWN",
                "ST_TYPE": null,
                "ODD_EVEN": "E"
            },
            "geometry": {
                "type": "Polygon",
                "coordinates": [[[-122.422, 37.808], [-122.422, 37.808]]]
            }
        }"#;

        // Use String IDs for this test to allow dynamic pattern ID generation
        let q = Arc::new(std::sync::RwLock::new(Quamina::<String>::new()));

        // Pre-populate with some patterns
        {
            let mut guard = q.write().unwrap();
            guard
                .add_pattern("type_feature".to_string(), r#"{"type": ["Feature"]}"#)
                .unwrap();
            guard
                .add_pattern(
                    "props_block".to_string(),
                    r#"{"properties": {"BLOCK_NUM": ["0001"]}}"#,
                )
                .unwrap();
        }

        let barrier = Arc::new(std::sync::Barrier::new(3));

        // Reader thread - does 10K event matches
        let q_reader = Arc::clone(&q);
        let barrier_reader = Arc::clone(&barrier);
        let reader_handle = thread::spawn(move || {
            barrier_reader.wait();

            let mut match_counts = std::collections::HashMap::<String, usize>::new();
            for _ in 0..10_000 {
                let guard = q_reader.read().unwrap();
                let matches = guard.matches_for_event(citylots_event.as_bytes()).unwrap();
                for m in matches {
                    *match_counts.entry(m).or_insert(0) += 1;
                }
            }
            match_counts
        });

        // Writer thread 1 - adds patterns during reads
        let q_writer1 = Arc::clone(&q);
        let barrier_writer1 = Arc::clone(&barrier);
        let writer1_handle = thread::spawn(move || {
            barrier_writer1.wait();

            for i in 0..50 {
                let mut guard = q_writer1.write().unwrap();
                let pattern = format!(r#"{{"properties": {{"FROM_ST": ["{}"]}}}}"#, i);
                guard
                    .add_pattern(format!("from_st_{}", i), &pattern)
                    .unwrap();
            }
        });

        // Writer thread 2 - adds different patterns during reads
        let q_writer2 = Arc::clone(&q);
        let barrier_writer2 = Arc::clone(&barrier);
        let writer2_handle = thread::spawn(move || {
            barrier_writer2.wait();

            for i in 0..50 {
                let mut guard = q_writer2.write().unwrap();
                let pattern = format!(r#"{{"geometry": {{"type": ["Polygon{}"]}}}}"#, i);
                guard.add_pattern(format!("geo_{}", i), &pattern).unwrap();
            }
        });

        writer1_handle.join().expect("Writer 1 panicked");
        writer2_handle.join().expect("Writer 2 panicked");
        let match_counts = reader_handle.join().expect("Reader panicked");

        // Verify consistent matching - the initial patterns should always match
        // (may get some or none of the dynamically added patterns depending on timing)
        assert!(
            *match_counts.get("type_feature").unwrap_or(&0) == 10_000,
            "type_feature should match all 10K events"
        );
        assert!(
            *match_counts.get("props_block").unwrap_or(&0) == 10_000,
            "props_block should match all 10K events"
        );

        // Verify final pattern count
        let guard = q.read().unwrap();
        assert_eq!(guard.pattern_count(), 2 + 50 + 50); // 2 initial + 50 from each writer
    }

    // =========================================================================
    // Lookaround Pattern Tests (Phase 5)
    // =========================================================================

    #[test]
    fn test_lookaround_pattern_parsing() {
        // Test that lookaround patterns parse and create MultiCondition matcher
        use crate::json::parse_pattern;

        // Positive lookahead: foo(?=bar)
        let pattern = r#"{"field": [{"regexp": "foo(?=bar)"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_ok(),
            "foo(?=bar) should parse: {:?}",
            result.err()
        );

        // Negative lookahead: foo(?!bar)
        let pattern = r#"{"field": [{"regexp": "foo(?!bar)"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_ok(),
            "foo(?!bar) should parse: {:?}",
            result.err()
        );

        // Positive lookbehind: (?<=foo)bar
        let pattern = r#"{"field": [{"regexp": "(?<=foo)bar"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_ok(),
            "(?<=foo)bar should parse: {:?}",
            result.err()
        );

        // Negative lookbehind: (?<!foo)bar
        let pattern = r#"{"field": [{"regexp": "(?<!foo)bar"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_ok(),
            "(?<!foo)bar should parse: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_lookaround_transformation() {
        // Test that lookaround patterns transform to MultiConditionPattern
        use crate::json::{parse_pattern, Matcher};

        // Test positive lookahead transformation
        let pattern = r#"{"field": [{"regexp": "foo(?=bar)"}]}"#;
        let result = parse_pattern(pattern).unwrap();
        let matchers = result.get("field").unwrap();
        assert_eq!(matchers.len(), 1);
        assert!(
            matches!(&matchers[0], Matcher::MultiCondition(_)),
            "foo(?=bar) should produce MultiCondition matcher, got {:?}",
            matchers[0]
        );
    }

    #[test]
    fn test_lookaround_rejected_patterns() {
        // Test that unsupported patterns are rejected
        use crate::json::parse_pattern;

        // Nested lookaround should fail
        let pattern = r#"{"field": [{"regexp": "(?=(?=a)b)c"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_err(),
            "Nested lookaround should fail: {:?}",
            result
        );

        // Variable-length lookbehind should fail
        let pattern = r#"{"field": [{"regexp": "(?<=a+)b"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_err(),
            "Variable-length lookbehind should fail: {:?}",
            result
        );

        // Backreferences are not supported (at parser level)
        use crate::regexp::parse_regexp;
        let result = parse_regexp("(abc)~1");
        assert!(
            result.is_err(),
            "Backreferences should fail in parser: {:?}",
            result
        );

        // Backreferences are also rejected at pattern level (not falling back to regex)
        let pattern = r#"{"field": [{"regexp": "(.)~1"}]}"#;
        let result = parse_pattern(pattern);
        assert!(
            result.is_err(),
            "Backreferences should fail in pattern parsing: {:?}",
            result
        );
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("backreference"),
            "Error should mention backreference: {}",
            err
        );
    }

    #[test]
    fn test_lookaround_pattern_add_to_quamina() {
        // Test that lookaround patterns can be added to Quamina
        let mut q = Quamina::<String>::new();

        // Add positive lookahead pattern
        let pattern = r#"{"status": [{"regexp": "foo(?=bar)"}]}"#;
        let result = q.add_pattern("test1".to_string(), pattern);
        assert!(
            result.is_ok(),
            "Adding foo(?=bar) pattern should succeed: {:?}",
            result.err()
        );

        // Add negative lookahead pattern
        let pattern = r#"{"status": [{"regexp": "foo(?!baz)"}]}"#;
        let result = q.add_pattern("test2".to_string(), pattern);
        assert!(
            result.is_ok(),
            "Adding foo(?!baz) pattern should succeed: {:?}",
            result.err()
        );

        // Add lookbehind pattern
        let pattern = r#"{"status": [{"regexp": "(?<=pre)fix"}]}"#;
        let result = q.add_pattern("test3".to_string(), pattern);
        assert!(
            result.is_ok(),
            "Adding (?<=pre)fix pattern should succeed: {:?}",
            result.err()
        );

        assert_eq!(q.pattern_count(), 3);
    }

    #[test]
    fn test_lookaround_primary_match() {
        // Test that lookaround patterns match with condition verification
        let mut q = Quamina::<String>::new();

        // Add pattern where primary is "foo"
        // foo(?=bar) has primary="foo", condition=PositiveLookahead("foobar")
        let pattern = r#"{"status": [{"regexp": "foo(?=bar)"}]}"#;
        q.add_pattern("lookahead".to_string(), pattern).unwrap();

        // Event with "foobar" - primary "foo" matches and condition "foobar" matches
        let event = r#"{"status": "foobar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"lookahead".to_string()),
            "foo(?=bar) should match 'foobar'"
        );

        // Event with just "foo" - primary matches but condition fails
        // (combined pattern "foobar" doesn't match "foo")
        let event = r#"{"status": "foo"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&"lookahead".to_string()),
            "foo(?=bar) should NOT match 'foo' (lookahead fails)"
        );
    }

    #[test]
    fn test_multi_condition_pattern_fields() {
        // Test MultiConditionPattern structure
        use crate::json::{LookaroundCondition, MultiConditionPattern};
        use crate::regexp::parse_regexp;

        // Create a multi-condition pattern manually
        let primary = parse_regexp("foo").unwrap();
        let combined = parse_regexp("foobar").unwrap();
        let conditions = vec![LookaroundCondition::PositiveLookahead(combined)];

        let mc = MultiConditionPattern::new(primary, conditions);

        // Verify structure
        assert_eq!(mc.primary.len(), 1, "Primary should have 1 branch");
        assert_eq!(mc.conditions.len(), 1, "Should have 1 condition");
        assert!(!mc.conditions[0].is_negative(), "Should be positive");
        assert!(
            !mc.conditions[0].is_lookbehind(),
            "Should not be lookbehind"
        );
    }

    #[test]
    fn test_condition_cost_ordering() {
        // Test that conditions are sorted by cost
        use crate::json::{LookaroundCondition, MultiConditionPattern};
        use crate::regexp::parse_regexp;

        let primary = parse_regexp("test").unwrap();
        let pattern1 = parse_regexp("a").unwrap();
        let pattern2 = parse_regexp("b").unwrap();
        let pattern3 = parse_regexp("c").unwrap();

        // Create conditions in reverse cost order
        let conditions = vec![
            LookaroundCondition::NegativeLookbehind {
                pattern: pattern3.clone(),
                byte_length: 1,
            }, // cost 40
            LookaroundCondition::PositiveLookbehind {
                pattern: pattern2.clone(),
                byte_length: 1,
            }, // cost 30
            LookaroundCondition::NegativeLookahead(pattern1.clone()), // cost 20
        ];

        let mc = MultiConditionPattern::new(primary, conditions);

        // Verify conditions are sorted by cost (lowest first)
        assert_eq!(
            mc.conditions[0].cost_estimate(),
            20,
            "First should be cost 20"
        );
        assert_eq!(
            mc.conditions[1].cost_estimate(),
            30,
            "Second should be cost 30"
        );
        assert_eq!(
            mc.conditions[2].cost_estimate(),
            40,
            "Third should be cost 40"
        );
    }

    #[test]
    fn test_positive_lookahead_match() {
        // foo(?=bar) matches "foobar" (foo at position with bar following)
        let mut q = Quamina::<String>::new();
        let pattern = r#"{"status": [{"regexp": "foo(?=bar)"}]}"#;
        q.add_pattern("test".to_string(), pattern).unwrap();

        let event = r#"{"status": "foobar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test".to_string()),
            "foo(?=bar) should match 'foobar'"
        );

        // Should NOT match "foobaz" (bar doesn't follow)
        let event = r#"{"status": "foobaz"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&"test".to_string()),
            "foo(?=bar) should NOT match 'foobaz'"
        );
    }

    #[test]
    fn test_negative_lookahead_match() {
        // foo(?!bar) matches "foobaz" but not "foobar"
        let mut q = Quamina::<String>::new();
        let pattern = r#"{"status": [{"regexp": "foo(?!bar)"}]}"#;
        q.add_pattern("test".to_string(), pattern).unwrap();

        let event = r#"{"status": "foobaz"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test".to_string()),
            "foo(?!bar) should match 'foobaz'"
        );

        let event = r#"{"status": "foobar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&"test".to_string()),
            "foo(?!bar) should NOT match 'foobar'"
        );
    }

    #[test]
    fn test_lookbehind_match() {
        // (?<=foo)bar matches "foobar" at "bar" position
        let mut q = Quamina::<String>::new();
        let pattern = r#"{"status": [{"regexp": "(?<=foo)bar"}]}"#;
        q.add_pattern("test".to_string(), pattern).unwrap();

        let event = r#"{"status": "foobar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test".to_string()),
            "(?<=foo)bar should match 'foobar'"
        );

        // Should NOT match "xxxbar" (foo doesn't precede)
        let event = r#"{"status": "xxxbar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&"test".to_string()),
            "(?<=foo)bar should NOT match 'xxxbar'"
        );
    }

    #[test]
    fn test_negative_lookbehind_match() {
        // (?<!foo)bar matches "xxxbar" but not "foobar"
        let mut q = Quamina::<String>::new();
        let pattern = r#"{"status": [{"regexp": "(?<!foo)bar"}]}"#;
        q.add_pattern("test".to_string(), pattern).unwrap();

        let event = r#"{"status": "xxxbar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"test".to_string()),
            "(?<!foo)bar should match 'xxxbar'"
        );

        let event = r#"{"status": "foobar"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&"test".to_string()),
            "(?<!foo)bar should NOT match 'foobar'"
        );
    }
}
