//! quamina-rs: Fast pattern-matching library for filtering JSON events

#![deny(missing_docs)]

// Internal modules exposed as `pub` only for benchmarks (benches/matching.rs).
// Not part of the public API — use `Quamina` instead.
#[doc(hidden)]
pub mod automaton;
mod case_folding;
#[doc(hidden)]
pub mod flatten_json;
mod flattener;
#[doc(hidden)]
pub mod json;
#[doc(hidden)]
pub mod numbits;
#[doc(hidden)]
pub mod regexp;
#[doc(hidden)]
pub mod segments_tree;
mod unicode_categories;

#[cfg(test)]
mod regexp_samples;

#[cfg(kani)]
mod kani_proofs;

// Re-export flattener types for custom implementations
pub use crate::flatten_json::ArrayPos;
pub use crate::flattener::{Flattener, JsonFlattener, OwnedField, SegmentsTreeTracker};

use automaton::{NfaBuffers, ThreadSafeCoreMatcher};
use flatten_json::FlattenJsonState;
use json::Matcher;
use parking_lot::Mutex;
use rustc_hash::{FxHashMap, FxHashSet};
use segments_tree::SegmentsTree;
use std::cell::RefCell;
use std::fmt;
use std::hash::Hash;
use std::sync::atomic::{AtomicU64, Ordering};

thread_local! {
    /// Thread-local JSON flattener state, avoiding per-call Mutex overhead.
    static TL_FLATTENER: RefCell<FlattenJsonState> = RefCell::new(FlattenJsonState::new());
    /// Thread-local NFA traversal buffers, avoiding per-call Mutex overhead.
    static TL_NFA_BUFS: RefCell<NfaBuffers> = RefCell::new(NfaBuffers::new());
}

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

    /// Count of live-pattern matches returned since the last rebuild.
    pub fn emitted(&self) -> u64 {
        self.emitted.load(Ordering::Relaxed)
    }

    /// Count of deleted-pattern matches suppressed since the last rebuild.
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
type PatternDef = FxHashMap<String, Vec<Matcher>>;

/// Errors that can occur during pattern matching
#[derive(Debug)]
pub enum QuaminaError {
    /// The event JSON was syntactically invalid.
    InvalidJson(String),
    /// The pattern JSON was malformed or used unsupported syntax.
    InvalidPattern(String),
    /// The input contained invalid UTF-8.
    InvalidUtf8,
    /// The requested media type is not supported (only `application/json`).
    UnsupportedMediaType(String),
    /// The pattern exceeded configured complexity limits (see [`PatternLimits`]).
    PatternTooComplex(String),
}

impl fmt::Display for QuaminaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidJson(msg) => write!(f, "invalid JSON: {}", msg),
            Self::InvalidPattern(msg) => write!(f, "invalid pattern: {}", msg),
            Self::InvalidUtf8 => write!(f, "invalid UTF-8"),
            Self::UnsupportedMediaType(mt) => {
                write!(f, "media type \"{}\" is not supported by Quamina", mt)
            }
            Self::PatternTooComplex(msg) => {
                write!(f, "pattern too complex: {}", msg)
            }
        }
    }
}

/// Limits on pattern complexity to prevent OOM and stack exhaustion.
///
/// Four complementary limits, each catching a different attack vector:
/// - **Nesting depth**: prevents stack exhaustion and deep-nesting attacks
/// - **Field count**: prevents wide patterns with hundreds of fields
/// - **Arena byte budget**: essential backstop that catches all forms of automaton complexity
/// - **State count**: prevents exponential field-matcher blowup from mixed-type matchers
///
/// # Defaults
/// - `max_pattern_depth`: 256 (jq precedent)
/// - `max_fields_per_pattern`: 256
/// - `arena_byte_budget`: 10 MB (regex crate precedent)
/// - `max_states_per_pattern`: 1024
#[derive(Debug, Clone)]
pub struct PatternLimits {
    /// Maximum nesting depth of a pattern (default: 256)
    pub max_pattern_depth: usize,
    /// Maximum number of fields per pattern (default: 256)
    pub max_fields_per_pattern: usize,
    /// Maximum arena byte size for the automaton (default: 10 MB)
    pub arena_byte_budget: usize,
    /// Maximum number of field-matcher states during pattern construction (default: 1024).
    ///
    /// When a field has N mixed-type matchers (e.g. exact + prefix), the state count
    /// multiplies by N for each such field. With K fields of N matchers each, states
    /// grow as N^K. This limit caps the product to prevent exponential memory blowup.
    /// All-exact fields use a bulk optimization that doesn't multiply states, so this
    /// limit only affects patterns mixing matcher types on the same field.
    pub max_states_per_pattern: usize,
}

impl Default for PatternLimits {
    fn default() -> Self {
        Self {
            max_pattern_depth: 256,
            max_fields_per_pattern: 256,
            arena_byte_budget: 10 * 1024 * 1024, // 10 MB
            max_states_per_pattern: 1024,
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
    /// Pattern complexity limits
    pattern_limits: PatternLimits,
    /// PhantomData to carry the X type parameter
    _phantom: std::marker::PhantomData<X>,
}

impl<X: Clone + Eq + Hash + Send + Sync> QuaminaBuilder<X> {
    /// Create a new QuaminaBuilder with default settings
    pub fn new() -> Self {
        Self {
            auto_rebuild_enabled: true,
            media_type_validated: false,
            custom_flattener: None,
            pattern_limits: PatternLimits::default(),
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

    /// Set the maximum nesting depth for patterns (default: 256).
    ///
    /// # Panics
    /// Panics if `depth` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_pattern_depth(1)
    ///     .build()?;
    /// let err = q.add_pattern("deep", r#"{"a": {"b": ["v"]}}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_max_pattern_depth(mut self, depth: usize) -> Self {
        assert!(depth > 0, "max_pattern_depth must be at least 1");
        self.pattern_limits.max_pattern_depth = depth;
        self
    }

    /// Set the maximum number of fields per pattern (default: 256).
    ///
    /// # Panics
    /// Panics if `count` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_fields_per_pattern(1)
    ///     .build()?;
    /// let err = q.add_pattern("wide", r#"{"a": ["1"], "b": ["2"]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_max_fields_per_pattern(mut self, count: usize) -> Self {
        assert!(count > 0, "max_fields_per_pattern must be at least 1");
        self.pattern_limits.max_fields_per_pattern = count;
        self
    }

    /// Set the arena byte budget for the automaton (default: 10 MB).
    ///
    /// # Panics
    /// Panics if `budget` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_arena_byte_budget(1)
    ///     .build()?;
    /// let err = q.add_pattern("p", r#"{"x": [{"prefix": "a"}]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_arena_byte_budget(mut self, budget: usize) -> Self {
        assert!(budget > 0, "arena_byte_budget must be at least 1");
        self.pattern_limits.arena_byte_budget = budget;
        self
    }

    /// Set the maximum field-matcher states per pattern (default: 1024).
    ///
    /// # Panics
    /// Panics if `max_states` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_states_per_pattern(1)
    ///     .build()?;
    /// let err = q.add_pattern("p", r#"{"a": ["x", {"prefix": "y"}]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_max_states_per_pattern(mut self, max_states: usize) -> Self {
        assert!(max_states > 0, "max_states_per_pattern must be at least 1");
        self.pattern_limits.max_states_per_pattern = max_states;
        self
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
            automaton: ThreadSafeCoreMatcher::with_limits(
                self.pattern_limits.arena_byte_budget,
                self.pattern_limits.max_states_per_pattern,
            ),
            pattern_defs: FxHashMap::default(),
            deleted_patterns: FxHashSet::default(),
            segments_tree: SegmentsTree::new(),
            custom_flattener: self.custom_flattener.map(Mutex::new),
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
            pattern_limits: self.pattern_limits,
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
/// Quamina uses automaton-based matching for all supported operators (exact, prefix, suffix,
/// wildcard, numeric comparisons, CIDR, I-Regexp with lookarounds, etc.)
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
    /// Automaton-based matcher
    automaton: ThreadSafeCoreMatcher<X>,
    /// All pattern definitions (source of truth for cloning)
    pattern_defs: FxHashMap<X, Vec<PatternDef>>,
    /// Deleted patterns (filtered from automaton results since automaton doesn't support deletion)
    deleted_patterns: FxHashSet<X>,
    /// Segments tree for fast field skipping during event parsing
    segments_tree: SegmentsTree,
    /// Custom flattener for non-JSON formats (if provided)
    custom_flattener: Option<Mutex<Box<dyn flattener::Flattener>>>,
    /// Statistics for auto-rebuild decisions
    pruner_stats: PrunerStats,
    /// Whether auto-rebuild is enabled (default: true)
    auto_rebuild_enabled: bool,
    /// Pattern complexity limits
    pattern_limits: PatternLimits,
}

impl<X: Clone + Eq + Hash + Send + Sync> Clone for Quamina<X> {
    fn clone(&self) -> Self {
        // Rebuild automaton from pattern_defs using the configured budget.
        let automaton = ThreadSafeCoreMatcher::with_limits(
            self.pattern_limits.arena_byte_budget,
            self.pattern_limits.max_states_per_pattern,
        );

        self.replay_patterns_into(&automaton);

        // Copy custom flattener if present
        let custom_flattener = self.custom_flattener.as_ref().map(|f| {
            let flattener = f.lock();
            Mutex::new(flattener.copy())
        });

        Self {
            automaton,
            pattern_defs: self.pattern_defs.clone(),
            deleted_patterns: self.deleted_patterns.clone(),
            segments_tree: self.segments_tree.clone(),
            custom_flattener,
            pruner_stats: self.pruner_stats.clone(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
            pattern_limits: self.pattern_limits.clone(),
        }
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Quamina<X> {
    /// Create a new Quamina instance with default pattern complexity limits.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// let mut q = Quamina::<String>::new();
    /// assert!(q.is_empty());
    /// ```
    pub fn new() -> Self {
        let limits = PatternLimits::default();
        Self {
            automaton: ThreadSafeCoreMatcher::with_limits(
                limits.arena_byte_budget,
                limits.max_states_per_pattern,
            ),
            pattern_defs: FxHashMap::default(),
            deleted_patterns: FxHashSet::default(),
            segments_tree: SegmentsTree::new(),
            custom_flattener: None,
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: true,
            pattern_limits: limits,
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// `pattern_json` is a JSON object whose values are arrays of match expressions;
    /// see the [README](https://github.com/baldawarishi/quamina-rs#patterns) for the full pattern syntax.
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("alert", r#"{"severity": ["high", "critical"]}"#)?;
    /// assert!(q.matches_for_event(br#"{"severity":"high"}"#)?.contains(&"alert"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_pattern(&mut self, x: X, pattern_json: &str) -> Result<(), QuaminaError> {
        let fields = json::parse_pattern(pattern_json, &self.pattern_limits)?;

        // Route to automaton first — if this fails (e.g. budget exceeded),
        // we must NOT store the pattern in pattern_defs, segments_tree, etc.
        let pattern_fields: Vec<(String, Vec<Matcher>)> = fields.clone().into_iter().collect();
        self.automaton.add_pattern(x.clone(), &pattern_fields)?;

        // Automaton accepted — now commit to bookkeeping state
        for field_path in fields.keys() {
            let segment_path = field_path.replace('.', "\n");
            self.segments_tree.add(&segment_path);
        }

        // If pattern was previously deleted, un-delete it
        self.deleted_patterns.remove(&x);

        // Store pattern definition for cloning/rebuild
        self.pattern_defs.entry(x).or_default().push(fields);

        Ok(())
    }

    /// Find all patterns that match the given event.
    ///
    /// `event` must be valid UTF-8 JSON bytes (objects, not arrays or scalars).
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("p1", r#"{"status": ["error"]}"#)?;
    /// q.add_pattern("p2", r#"{"level": [1, 2, 3]}"#)?;
    /// let hits = q.matches_for_event(br#"{"status":"error","level":2}"#)?;
    /// assert!(hits.contains(&"p1") && hits.contains(&"p2"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        // Check if we have a custom flattener
        if let Some(ref custom_flattener_mutex) = self.custom_flattener {
            // Use custom flattener path
            return self.matches_for_event_custom_flattener(event, custom_flattener_mutex);
        }

        // Default path: use thread-local flattener + NFA buffers (no Mutex overhead)
        TL_FLATTENER.with(|flattener_cell| {
            TL_NFA_BUFS.with(|bufs_cell| {
                let mut flattener = flattener_cell.borrow_mut();
                let mut bufs = bufs_cell.borrow_mut();

                let streaming_fields = flattener.flatten(event, &self.segments_tree)?;

                // Sort by path for automaton matching
                streaming_fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));

                let raw_matches = self
                    .automaton
                    .matches_for_fields_direct(streaming_fields, &mut bufs);

                Ok(self.filter_deleted_matches(raw_matches))
            })
        })
    }

    /// Match using a custom flattener (slower path with owned data)
    fn matches_for_event_custom_flattener(
        &self,
        event: &[u8],
        custom_flattener_mutex: &Mutex<Box<dyn flattener::Flattener>>,
    ) -> Result<Vec<X>, QuaminaError> {
        use std::sync::Arc;

        // Get owned fields from custom flattener (still needs Mutex — user-provided)
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

        // Get matches from automaton using thread-local NFA buffers
        let matches = TL_NFA_BUFS.with(|bufs_cell| {
            let mut bufs = bufs_cell.borrow_mut();
            let raw_matches = self
                .automaton
                .matches_for_fields_direct(&streaming_fields, &mut bufs);
            self.filter_deleted_matches(raw_matches)
        });

        Ok(matches)
    }

    /// Replay all live (non-deleted) pattern definitions into the given automaton.
    fn replay_patterns_into(&self, automaton: &ThreadSafeCoreMatcher<X>) {
        for (id, patterns) in &self.pattern_defs {
            if self.deleted_patterns.contains(id) {
                continue;
            }
            for fields in patterns {
                let pattern_fields: Vec<(String, Vec<Matcher>)> =
                    fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                automaton
                    .add_pattern(id.clone(), &pattern_fields)
                    .expect("pre-validated pattern should not fail on rebuild");
            }
        }
    }

    /// Remove soft-deleted patterns from raw match results and update pruner stats.
    fn filter_deleted_matches(&self, raw_matches: Vec<X>) -> Vec<X> {
        if self.deleted_patterns.is_empty() {
            self.pruner_stats.add_emitted(raw_matches.len() as u64);
            raw_matches
        } else {
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
    }

    /// Access the underlying automaton (for direct matching without Mutex).
    #[doc(hidden)]
    pub fn automaton(&self) -> &ThreadSafeCoreMatcher<X> {
        &self.automaton
    }

    /// Access the segments tree (for direct flattening without Mutex).
    #[doc(hidden)]
    pub fn segments_tree(&self) -> &SegmentsTree {
        &self.segments_tree
    }

    /// Flatten an event without matching (for benchmarking)
    #[doc(hidden)]
    pub fn flatten_only(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        TL_FLATTENER.with(|flattener_cell| {
            let mut flattener = flattener_cell.borrow_mut();
            let fields = flattener.flatten(event, &self.segments_tree)?;
            Ok(fields.len())
        })
    }

    /// Mark all patterns with the given identifier as deleted.
    ///
    /// Deleted patterns are excluded from match results immediately, but
    /// their automaton memory is not reclaimed until [`rebuild()`](Self::rebuild).
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("temp", r#"{"x": [1]}"#)?;
    /// q.delete_patterns(&"temp")?;
    /// assert!(q.matches_for_event(br#"{"x":1}"#)?.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError> {
        // Check if pattern exists
        if !self.pattern_defs.contains_key(x) || self.deleted_patterns.contains(x) {
            return Ok(()); // Pattern doesn't exist or already deleted
        }

        // Add to deleted set (automaton doesn't support deletion)
        // This will filter the pattern from automaton results
        self.deleted_patterns.insert(x.clone());

        Ok(())
    }

    /// Checks whether any pattern matches the event.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [1]}"#)?;
    /// assert!(q.has_matches(br#"{"x":1}"#)?);
    /// assert!(!q.has_matches(br#"{"x":2}"#)?);
    /// # Ok(())
    /// # }
    /// ```
    pub fn has_matches(&self, event: &[u8]) -> Result<bool, QuaminaError> {
        // Use matches_for_event and check if non-empty
        // This could be optimized to return early, but for now this is simpler
        Ok(!self.matches_for_event(event)?.is_empty())
    }

    /// Counts how many unique pattern IDs match the event.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [1]}"#)?;
    /// assert_eq!(q.count_matches(br#"{"x":1}"#)?, 2);
    /// # Ok(())
    /// # }
    /// ```
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

    /// Get aggregate arena statistics across all frozen value matchers.
    ///
    /// Returns stats covering state counts, table sizes, epsilon transitions,
    /// closure sizes, and flattened buffer usage. Useful for diagnostics and
    /// verifying optimization effectiveness.
    pub fn arena_stats(&self) -> automaton::arena::ArenaStats {
        self.automaton.arena_stats()
    }

    /// Enable or disable auto-rebuild
    pub fn set_auto_rebuild(&mut self, enabled: bool) {
        self.auto_rebuild_enabled = enabled;
    }

    /// Check if auto-rebuild is enabled
    pub fn auto_rebuild_enabled(&self) -> bool {
        self.auto_rebuild_enabled
    }

    /// Rebuild the automaton from only live patterns, reclaiming memory from soft-deleted patterns.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [2]}"#)?;
    /// assert_eq!(q.pattern_count(), 2);
    ///
    /// q.delete_patterns(&"a")?;
    /// let purged = q.rebuild();
    /// assert_eq!(purged, 1);
    /// assert_eq!(q.pattern_count(), 1);
    /// # Ok(())
    /// # }
    /// ```
    pub fn rebuild(&mut self) -> usize {
        let purged = self.deleted_patterns.len();
        if purged == 0 {
            return 0;
        }

        // Create new automaton with only live patterns, using the configured budget.
        let new_automaton = ThreadSafeCoreMatcher::with_limits(
            self.pattern_limits.arena_byte_budget,
            self.pattern_limits.max_states_per_pattern,
        );

        self.replay_patterns_into(&new_automaton);

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
    /// Returns true when filtered/emitted ratio exceeds 0.2 and at least 1 000
    /// total observations have been recorded.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// let q = Quamina::<&str>::new();
    /// // No activity yet — rebuild not recommended.
    /// assert!(!q.should_rebuild());
    /// ```
    pub fn should_rebuild(&self) -> bool {
        self.pruner_stats.should_rebuild()
    }

    /// Perform rebuild only when auto-rebuild is enabled and [`should_rebuild()`](Self::should_rebuild)
    /// returns true. Returns the number of patterns purged, or 0 if no rebuild occurred.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.delete_patterns(&"a")?;
    /// // Threshold not yet met, so no rebuild happens.
    /// assert_eq!(q.maybe_rebuild(), 0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn maybe_rebuild(&mut self) -> usize {
        if self.auto_rebuild_enabled && self.pruner_stats.should_rebuild() {
            self.rebuild()
        } else {
            0
        }
    }

    /// Removes all patterns and resets the matcher to its initial state.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// assert!(!q.is_empty());
    /// q.clear();
    /// assert!(q.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub fn clear(&mut self) {
        self.automaton = ThreadSafeCoreMatcher::with_limits(
            self.pattern_limits.arena_byte_budget,
            self.pattern_limits.max_states_per_pattern,
        );
        self.pattern_defs.clear();
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

#[cfg(test)]
#[macro_use]
mod test_helpers;
#[cfg(test)]
mod tests_core;
#[cfg(test)]
mod tests_operators;
#[cfg(test)]
mod tests_stress;
