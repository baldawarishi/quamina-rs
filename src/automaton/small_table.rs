//! Core data structures for the finite automaton.
//!
//! This module contains the fundamental types used throughout the automaton:
//! - `FaState`: A state in the finite automaton
//! - `SmallTable`: A compact byte-indexed transition table
//! - `FieldMatcher`: Matches field names and dispatches to value matchers
//! - `ValueMatcher`: Matches field values using the automaton
//! - `NfaBuffers`: Reusable buffers for NFA traversal

use std::collections::HashMap;
use std::sync::Arc;

/// Maximum byte value we handle. UTF-8 bytes 0xF5-0xFF can't appear in valid strings.
/// We use 0xF5 as a value terminator.
pub const BYTE_CEILING: usize = 0xF6;

/// Marks the end of a value being matched. This simplifies handling of exact matches
/// vs prefix matches - we always add this terminator to both the pattern and the value.
pub const VALUE_TERMINATOR: u8 = 0xF5;

/// A state in the finite automaton.
///
/// Each state has a transition table and optionally field transitions for when
/// a value match completes.
#[derive(Clone, Default)]
pub struct FaState {
    /// The transition table for this state
    pub table: SmallTable,
    /// Field matchers to transition to when this state is reached at end of value
    pub field_transitions: Vec<Arc<FieldMatcher>>,
}

impl FaState {
    pub fn new() -> Self {
        Self {
            table: SmallTable::new(),
            field_transitions: Vec::new(),
        }
    }

    pub fn with_table(table: SmallTable) -> Self {
        Self {
            table,
            field_transitions: Vec::new(),
        }
    }

    pub fn with_transitions(table: SmallTable, field_transitions: Vec<Arc<FieldMatcher>>) -> Self {
        Self {
            table,
            field_transitions,
        }
    }
}


/// A compact lookup table encoding byte value ranges to state transitions.
///
/// The table uses a ceilings/steps representation where each ceiling marks the
/// upper bound (exclusive) of a byte range that maps to the corresponding step.
/// This is more memory-efficient than a 256-element array when there are ranges.
///
/// Example: To map bytes 3-4 to state S1 and byte 0x34 to state S2:
/// ```text
/// ceilings: [3, 5, 0x34, 0x35, BYTE_CEILING]
/// steps:    [None, Some(S1), None, Some(S2), None]
/// ```
///
/// The `step()` method finds the appropriate state by scanning ceilings.
#[derive(Clone, Default)]
pub struct SmallTable {
    /// Upper bounds (exclusive) for each byte range
    pub ceilings: Vec<u8>,
    /// States to transition to for each range
    pub steps: Vec<Option<Arc<FaState>>>,
    /// Epsilon transitions (taken regardless of input byte)
    pub epsilons: Vec<Arc<FaState>>,
    /// Special state for handling wildcard patterns (escape from spin state)
    pub spinout: Option<Arc<FaState>>,
}

impl SmallTable {
    /// Create a new empty table with the required final ceiling entry.
    pub fn new() -> Self {
        Self {
            ceilings: vec![BYTE_CEILING as u8],
            steps: vec![None],
            epsilons: Vec::new(),
            spinout: None,
        }
    }

    /// Create a table with a default step for all bytes.
    ///
    /// # Arguments
    /// * `default_step` - State to transition to for any byte
    pub fn with_default_step(default_step: Option<Arc<FaState>>) -> Self {
        Self {
            ceilings: vec![BYTE_CEILING as u8],
            steps: vec![default_step],
            epsilons: Vec::new(),
            spinout: None,
        }
    }

    /// Create a table with a default step and specific byte-to-state mappings.
    ///
    /// # Arguments
    /// * `default_step` - Default state for bytes not otherwise specified
    /// * `indices` - Byte values that have specific transitions (must be sorted)
    /// * `steps` - States to transition to for each index
    pub fn with_mappings(
        default_step: Option<Arc<FaState>>,
        indices: &[u8],
        specific_steps: &[Arc<FaState>],
    ) -> Self {
        let mut ceilings = Vec::with_capacity(indices.len() * 2 + 1);
        let mut steps = Vec::with_capacity(indices.len() * 2 + 1);

        let mut last_index: u8 = 0;
        for (i, &index) in indices.iter().enumerate() {
            // Add default range before this index if there's a gap
            if index > last_index {
                ceilings.push(index);
                steps.push(default_step.clone());
            }
            // Add the specific mapping
            ceilings.push(index + 1);
            steps.push(Some(specific_steps[i].clone()));
            last_index = index + 1;
        }

        // Add final default range if needed
        if indices.is_empty() || indices[indices.len() - 1] < (BYTE_CEILING as u8 - 1) {
            ceilings.push(BYTE_CEILING as u8);
            steps.push(default_step);
        }

        Self {
            ceilings,
            steps,
            epsilons: Vec::new(),
            spinout: None,
        }
    }

    /// Take a step through the automaton on the given byte.
    ///
    /// Returns the state to transition to (if any) and the epsilon transitions.
    #[inline]
    pub fn step(&self, utf8_byte: u8) -> (Option<&Arc<FaState>>, &[Arc<FaState>]) {
        for (i, &ceiling) in self.ceilings.iter().enumerate() {
            if utf8_byte < ceiling {
                return (self.steps[i].as_ref(), &self.epsilons);
            }
        }
        // Should not reach here for valid UTF-8 bytes
        (None, &self.epsilons)
    }

    /// Deterministic step - for when we know there's no NFA branching.
    #[inline]
    pub fn dstep(&self, utf8_byte: u8) -> Option<&Arc<FaState>> {
        for (i, &ceiling) in self.ceilings.iter().enumerate() {
            if utf8_byte < ceiling {
                return self.steps[i].as_ref();
            }
        }
        None
    }

    /// Check if this table only has epsilon transitions (no byte transitions).
    pub fn is_epsilon_only(&self) -> bool {
        !self.epsilons.is_empty() && self.ceilings.len() == 1 && self.steps[0].is_none()
    }

    /// Add a transition for a specific byte value.
    pub fn add_byte_step(&mut self, utf8_byte: u8, step: Arc<FaState>) {
        let mut unpacked = self.unpack();
        unpacked[utf8_byte as usize] = Some(step);
        self.pack(&unpacked);
    }

    /// Unpack the compact representation into a full array.
    pub fn unpack(&self) -> [Option<Arc<FaState>>; BYTE_CEILING] {
        let mut result: [Option<Arc<FaState>>; BYTE_CEILING] = std::array::from_fn(|_| None);
        let mut unpacked_index = 0;
        for (packed_index, &ceiling) in self.ceilings.iter().enumerate() {
            let ceiling = ceiling as usize;
            while unpacked_index < ceiling && unpacked_index < BYTE_CEILING {
                result[unpacked_index] = self.steps[packed_index].clone();
                unpacked_index += 1;
            }
        }
        result
    }

    /// Pack an unpacked array back into compact form.
    pub fn pack(&mut self, unpacked: &[Option<Arc<FaState>>; BYTE_CEILING]) {
        let mut ceilings = Vec::with_capacity(16);
        let mut steps = Vec::with_capacity(16);

        let mut last_step = &unpacked[0];
        for (i, step) in unpacked.iter().enumerate() {
            if !arc_option_eq(step, last_step) {
                ceilings.push(i as u8);
                steps.push(last_step.clone());
            }
            last_step = step;
        }
        ceilings.push(BYTE_CEILING as u8);
        steps.push(last_step.clone());

        self.ceilings = ceilings;
        self.steps = steps;
    }
}

/// Compare two Option<Arc<T>> for pointer equality.
pub(crate) fn arc_option_eq<T>(a: &Option<Arc<T>>, b: &Option<Arc<T>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        _ => false,
    }
}

/// Matches field names and dispatches to value matchers.
#[derive(Clone, Default)]
pub struct FieldMatcher {
    /// Map from field paths to value matchers
    pub transitions: HashMap<String, Arc<ValueMatcher>>,
    /// Pattern identifiers that match when arriving at this state
    /// Using a unique ID that survives FA merging
    pub match_id: Option<u64>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: HashMap<String, Arc<FieldMatcher>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: HashMap<String, Arc<FieldMatcher>>,
}

impl FieldMatcher {
    pub fn new() -> Self {
        Self {
            transitions: HashMap::new(),
            match_id: None,
            exists_true: HashMap::new(),
            exists_false: HashMap::new(),
        }
    }

    /// Create a new FieldMatcher with a match ID
    pub fn with_match_id(id: u64) -> Self {
        Self {
            transitions: HashMap::new(),
            match_id: Some(id),
            exists_true: HashMap::new(),
            exists_false: HashMap::new(),
        }
    }

    /// Get transitions on a field path.
    pub fn get_value_matcher(&self, path: &str) -> Option<&Arc<ValueMatcher>> {
        self.transitions.get(path)
    }
}

/// Matches field values using a finite automaton.
#[derive(Clone, Default)]
pub struct ValueMatcher {
    /// The automaton start table
    pub start_table: Option<SmallTable>,
    /// Optimization: for single-value exact match, store it directly
    pub singleton_match: Option<Vec<u8>>,
    /// Transition for singleton match
    pub singleton_transition: Option<Arc<FieldMatcher>>,
    /// Whether this matcher has numeric patterns (for Q number conversion)
    pub has_numbers: bool,
    /// Whether this matcher requires NFA traversal (vs simpler DFA)
    pub is_nondeterministic: bool,
}

impl ValueMatcher {
    pub fn new() -> Self {
        Self {
            start_table: None,
            singleton_match: None,
            singleton_transition: None,
            has_numbers: false,
            is_nondeterministic: false,
        }
    }
}

/// Buffers reused during NFA traversal to minimize allocations.
#[derive(Default)]
pub struct NfaBuffers {
    pub current_states: Vec<Arc<FaState>>,
    pub next_states: Vec<Arc<FaState>>,
    /// Reusable buffer for collecting field transitions
    pub transitions: Vec<Arc<FieldMatcher>>,
}

impl NfaBuffers {
    pub fn new() -> Self {
        Self {
            current_states: Vec::with_capacity(16),
            next_states: Vec::with_capacity(16),
            transitions: Vec::with_capacity(8),
        }
    }

    pub fn clear(&mut self) {
        self.current_states.clear();
        self.next_states.clear();
        self.transitions.clear();
    }
}
