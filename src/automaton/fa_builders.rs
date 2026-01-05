//! FA (Finite Automaton) construction functions.
//!
//! This module contains functions for building various pattern matching automata:
//! - `make_string_fa`: Exact string matching
//! - `make_prefix_fa`: Prefix matching
//! - `make_shellstyle_fa`: Shell-style wildcard patterns
//! - `make_wildcard_fa`: Wildcard patterns with escaping
//! - `make_anything_but_fa`: Negative matching
//! - `make_monocase_fa`: Case-insensitive matching
//! - `merge_fas`: Merge two automata

use std::collections::HashMap;
use std::sync::Arc;

use crate::case_folding::case_fold_char;

use super::small_table::{FaState, FieldMatcher, SmallTable, BYTE_CEILING, VALUE_TERMINATOR};

/// Build a string-matching FA from a byte sequence.
///
/// Creates a chain of states where each byte transitions to the next,
/// with a final transition on VALUE_TERMINATOR to a match state.
pub fn make_string_fa(val: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    make_string_fa_step(val, 0, next_field)
}

fn make_string_fa_step(val: &[u8], index: usize, next_field: Arc<FieldMatcher>) -> SmallTable {
    if index >= val.len() {
        // Final step: transition on value terminator to match state
        let last_step = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[last_step]);
    }

    // Recursive step: transition on current byte to next state
    let next_table = make_string_fa_step(val, index + 1, next_field);
    let next_step = Arc::new(FaState::with_table(next_table));
    SmallTable::with_mappings(None, &[val[index]], &[next_step])
}

/// Build a prefix-matching FA.
///
/// Matches any value starting with the given prefix.
pub fn make_prefix_fa(prefix: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    make_prefix_fa_step(prefix, 0, next_field)
}

fn make_prefix_fa_step(prefix: &[u8], index: usize, next_field: Arc<FieldMatcher>) -> SmallTable {
    if index >= prefix.len() {
        // End of prefix: match state that accepts anything
        let match_state = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(Some(match_state), &[], &[]);
    }

    let next_table = make_prefix_fa_step(prefix, index + 1, next_field);
    let next_step = Arc::new(FaState::with_table(next_table));
    SmallTable::with_mappings(None, &[prefix[index]], &[next_step])
}

/// Build a shellstyle pattern FA.
///
/// Shellstyle patterns use `*` as a wildcard that matches zero or more characters.
/// This is equivalent to the regex `.*` construct.
///
/// The FA uses epsilon transitions and "spinout" states to handle the wildcard:
/// - For `*`, create a state with a self-epsilon loop
/// - The next character after `*` "escapes" from the spin state
///
/// # Arguments
/// * `pattern` - The pattern bytes (without quotes)
/// * `next_field` - The field matcher to transition to on match
pub fn make_shellstyle_fa(pattern: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    // Build iteratively using a vector of states
    // We'll build forward, collecting state info, then assemble backward

    // Parse the pattern to identify wildcards and literals
    let mut segments: Vec<ShellSegment> = Vec::new();
    let mut i = 0;

    while i < pattern.len() {
        if pattern[i] == b'*' {
            segments.push(ShellSegment::Wildcard);
            i += 1;
        } else {
            // Collect consecutive literal bytes
            let start = i;
            while i < pattern.len() && pattern[i] != b'*' {
                i += 1;
            }
            segments.push(ShellSegment::Literal(pattern[start..i].to_vec()));
        }
    }

    // Build from end to start
    build_shellstyle_from_segments(&segments, 0, next_field)
}

#[derive(Debug)]
enum ShellSegment {
    Literal(Vec<u8>),
    Wildcard,
}

fn build_shellstyle_from_segments(
    segments: &[ShellSegment],
    index: usize,
    next_field: Arc<FieldMatcher>,
) -> SmallTable {
    if index >= segments.len() {
        // End - transition on value terminator
        let match_state = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state]);
    }

    match &segments[index] {
        ShellSegment::Literal(bytes) => {
            // Build literal sequence
            let next = build_shellstyle_from_segments(segments, index + 1, next_field);
            build_literal_chain(bytes, next)
        }
        ShellSegment::Wildcard => {
            // Build wildcard (spinout) structure
            let next = build_shellstyle_from_segments(segments, index + 1, next_field);
            build_wildcard_spinout(&next)
        }
    }
}

/// Build a chain of states for a literal byte sequence
fn build_literal_chain(bytes: &[u8], continuation: SmallTable) -> SmallTable {
    if bytes.is_empty() {
        return continuation;
    }

    // Build from end to start
    let mut current = continuation;
    for &byte in bytes.iter().rev() {
        let next_state = Arc::new(FaState::with_table(current));
        current = SmallTable::with_mappings(None, &[byte], &[next_state]);
    }
    current
}

/// Build a wildcard spinout structure
fn build_wildcard_spinout(continuation: &SmallTable) -> SmallTable {
    // The spinout needs to match zero or more of any character before continuation.
    // We mark this with is_spinout=true and let the NFA traversal handle looping.

    let continuation_state = Arc::new(FaState::with_table(continuation.clone()));

    // Build a special spinout structure:
    // - All non-terminator bytes have a default step (will be handled by traversal)
    // - Epsilon to continuation (to try matching after each byte)
    // - Mark table as spinout so traversal knows to loop

    let mut spinout_table = SmallTable::new();

    // Mark as spinout - traversal will handle looping behavior
    let spinout_marker = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![],
    });
    spinout_table.spinout = Some(spinout_marker.clone());

    // Epsilon to continuation to try matching
    spinout_table.epsilons = vec![continuation_state];

    spinout_table
}

/// Build a wildcard pattern FA (like shellstyle but with escape sequences).
///
/// Wildcard patterns support:
/// - `*` as a wildcard matching zero or more characters
/// - `\*` as a literal asterisk
/// - `\\` as a literal backslash
///
/// # Arguments
/// * `pattern` - The pattern bytes
/// * `next_field` - The field matcher to transition to on match
pub fn make_wildcard_fa(pattern: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    make_wildcard_fa_step(pattern, 0, next_field)
}

fn make_wildcard_fa_step(
    pattern: &[u8],
    index: usize,
    next_field: Arc<FieldMatcher>,
) -> SmallTable {
    if index >= pattern.len() {
        // End of pattern - transition on value terminator to match
        let last_step = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[last_step]);
    }

    let ch = pattern[index];

    // Check for escape sequence
    if ch == b'\\' && index + 1 < pattern.len() {
        // Escaped character - treat next char as literal
        let escaped_char = pattern[index + 1];
        let next_table = make_wildcard_fa_step(pattern, index + 2, next_field);
        let next_state = Arc::new(FaState::with_table(next_table));
        return SmallTable::with_mappings(None, &[escaped_char], &[next_state]);
    }

    if ch == b'*' {
        // Wildcard - similar to shellstyle
        if index + 1 >= pattern.len() {
            // * at end
            let match_state = Arc::new(FaState {
                table: SmallTable::new(),
                field_transitions: vec![next_field],
            });

            let spinout = Arc::new(FaState {
                table: SmallTable::new(),
                field_transitions: vec![],
            });

            let spinout_table = SmallTable::with_mappings(
                Some(spinout.clone()),
                &[VALUE_TERMINATOR],
                &[match_state],
            );

            let spinout_final = Arc::new(FaState {
                table: spinout_table,
                field_transitions: vec![],
            });

            let mut result = SmallTable::new();
            result.epsilons = vec![spinout_final.clone()];
            result.spinout = Some(spinout_final);
            return result;
        }

        // * with more characters - need to handle escape in next char
        let next_index = index + 1;
        let escape_char = if pattern[next_index] == b'\\' && next_index + 1 < pattern.len() {
            // Next is an escape sequence
            pattern[next_index + 1]
        } else {
            pattern[next_index]
        };

        let skip = if pattern[next_index] == b'\\' && next_index + 1 < pattern.len() {
            2
        } else {
            1
        };

        let continuation = make_wildcard_fa_step(pattern, next_index + skip, next_field);
        let escape_state = Arc::new(FaState::with_table(continuation));

        let spinout = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![],
        });

        let spinout_table =
            SmallTable::with_mappings(Some(spinout.clone()), &[escape_char], &[escape_state]);

        let spinout_final = Arc::new(FaState {
            table: spinout_table,
            field_transitions: vec![],
        });

        let mut result = SmallTable::new();
        result.epsilons = vec![spinout_final.clone()];
        result.spinout = Some(spinout_final);
        return result;
    }

    // Regular character
    let next_table = make_wildcard_fa_step(pattern, index + 1, next_field);
    let next_state = Arc::new(FaState::with_table(next_table));
    SmallTable::with_mappings(None, &[ch], &[next_state])
}

/// Build an anything-but FA that matches any value NOT in the excluded list.
///
/// The automaton works by having a default "success" transition for all bytes,
/// except for bytes that start one of the excluded values. Those bytes lead to
/// states that track whether we're matching an excluded value. When we reach
/// the end of an excluded value (via VALUE_TERMINATOR), we transition to a
/// "failure" state with no field transitions.
///
/// # Arguments
/// * `excluded` - The list of excluded values (byte sequences)
/// * `next_field` - The field matcher to transition to on success
pub fn make_anything_but_fa(excluded: &[Vec<u8>], next_field: Arc<FieldMatcher>) -> SmallTable {
    // Success state - we match if we get here
    let success = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field],
    });

    make_anything_but_step(excluded, 0, &success)
}

/// Build one step of the anything-but automaton.
///
/// At each position, we group excluded values by their byte at that position.
/// The default transition is to success. For bytes that continue an excluded
/// value, we recurse. For bytes that end an excluded value, we go to failure.
fn make_anything_but_step(vals: &[Vec<u8>], index: usize, success: &Arc<FaState>) -> SmallTable {
    // Start with default transition to success for all bytes
    let mut unpacked: [Option<Arc<FaState>>; BYTE_CEILING] =
        std::array::from_fn(|_| Some(success.clone()));

    // Group values by the byte at current index
    // Track both values that continue AND values that end at this position
    let mut vals_with_bytes_remaining: HashMap<u8, Vec<&Vec<u8>>> = HashMap::new();
    let mut vals_ending_here: HashMap<u8, bool> = HashMap::new();

    for val in vals {
        let last_index = val.len().saturating_sub(1);
        if index <= last_index && !val.is_empty() {
            let utf8_byte = val[index];
            if index < last_index {
                // This value has more bytes after index
                vals_with_bytes_remaining
                    .entry(utf8_byte)
                    .or_default()
                    .push(val);
            }
            if index == last_index {
                // This value ends at index
                vals_ending_here.insert(utf8_byte, true);
            }
        }
    }

    // For each unique byte, build the appropriate state
    let all_bytes: std::collections::HashSet<u8> = vals_with_bytes_remaining
        .keys()
        .chain(vals_ending_here.keys())
        .copied()
        .collect();

    for utf8_byte in all_bytes {
        let has_continuation = vals_with_bytes_remaining.contains_key(&utf8_byte);
        let ends_here = vals_ending_here.contains_key(&utf8_byte);

        if has_continuation && ends_here {
            // This byte both continues some values AND ends others
            // We need a state that:
            // 1. On VALUE_TERMINATOR: fail (because some value ends here)
            // 2. On other bytes: check continuation recursively
            let continuing_vals = vals_with_bytes_remaining.get(&utf8_byte).unwrap();
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().cloned().cloned().collect();
            let continuation_table = make_anything_but_step(&owned_vals, index + 1, success);

            // Build table: fail on VALUE_TERMINATOR, use continuation table for others
            let fail_state = Arc::new(FaState::new());
            let mut combined_table = continuation_table;
            // Override the VALUE_TERMINATOR transition to fail
            let mut combined_unpacked = combined_table.unpack();
            combined_unpacked[VALUE_TERMINATOR as usize] = Some(fail_state);
            combined_table.pack(&combined_unpacked);

            unpacked[utf8_byte as usize] = Some(Arc::new(FaState::with_table(combined_table)));
        } else if has_continuation {
            // Only continues, doesn't end here
            let continuing_vals = vals_with_bytes_remaining.get(&utf8_byte).unwrap();
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().cloned().cloned().collect();
            let next_table = make_anything_but_step(&owned_vals, index + 1, success);
            let next_step = Arc::new(FaState::with_table(next_table));
            unpacked[utf8_byte as usize] = Some(next_step);
        } else if ends_here {
            // Only ends here, doesn't continue
            let fail_state = Arc::new(FaState::new());
            let last_table = SmallTable::with_mappings(
                Some(success.clone()),
                &[VALUE_TERMINATOR],
                &[fail_state],
            );
            unpacked[utf8_byte as usize] = Some(Arc::new(FaState::with_table(last_table)));
        }
    }

    let mut table = SmallTable::new();
    table.pack(&unpacked);
    table
}

/// Build an equals-ignore-case (monocase) FA.
///
/// This creates an automaton that matches a string in a case-insensitive manner.
/// For each Unicode character, if it has a case-folding alternate, both paths
/// lead to the same next state. This handles full Unicode case folding, not just ASCII.
///
/// # Arguments
/// * `val` - The pattern value to match case-insensitively (UTF-8 bytes)
/// * `next_field` - The field matcher to transition to on match
pub fn make_monocase_fa(val: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    // Empty string - match on value terminator only
    if val.is_empty() {
        let match_state = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state]);
    }

    // Convert to string for character iteration
    let s = match std::str::from_utf8(val) {
        Ok(s) => s,
        Err(_) => {
            // Invalid UTF-8 - fall back to byte-by-byte ASCII matching
            return make_monocase_fa_ascii(val, next_field);
        }
    };

    // Collect character info: (original bytes, alternate bytes if any)
    let chars: Vec<(Vec<u8>, Option<Vec<u8>>)> = s
        .char_indices()
        .map(|(offset, ch)| {
            let next_offset = s[offset..]
                .chars()
                .next()
                .map(|c| offset + c.len_utf8())
                .unwrap_or(val.len());
            let orig = val[offset..next_offset].to_vec();

            let alt = case_fold_char(ch).map(|alt_char| {
                let mut buf = [0u8; 4];
                alt_char.encode_utf8(&mut buf);
                buf[..alt_char.len_utf8()].to_vec()
            });

            (orig, alt)
        })
        .collect();

    // Build recursively from the last character backward
    make_monocase_recursive(&chars, 0, next_field)
}

/// Recursively build the monocase FA from character index forward.
/// Returns the SmallTable that starts matching from this character.
fn make_monocase_recursive(
    chars: &[(Vec<u8>, Option<Vec<u8>>)],
    idx: usize,
    next_field: Arc<FieldMatcher>,
) -> SmallTable {
    if idx >= chars.len() {
        // End of string - create state that matches on VALUE_TERMINATOR
        let match_state = Arc::new(FaState {
            table: SmallTable::new(),
            field_transitions: vec![next_field],
        });
        return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state]);
    }

    let (orig, alt) = &chars[idx];

    // First, build the state for after this character
    let next_table = make_monocase_recursive(chars, idx + 1, next_field);
    let next_step = Arc::new(FaState::with_table(next_table));

    // Now build the transition(s) for this character
    if let Some(alt_bytes) = alt {
        // Two paths to next state - handle common prefix
        let common_prefix = orig
            .iter()
            .zip(alt_bytes.iter())
            .take_while(|(a, b)| a == b)
            .count();

        if common_prefix == 0 {
            // No common prefix - both paths start with different bytes
            let orig_state = make_fa_fragment(orig, next_step.clone());
            let alt_state = make_fa_fragment(alt_bytes, next_step);

            let (b1, s1, b2, s2) = if orig[0] < alt_bytes[0] {
                (orig[0], orig_state, alt_bytes[0], alt_state)
            } else {
                (alt_bytes[0], alt_state, orig[0], orig_state)
            };
            SmallTable::with_mappings(None, &[b1, b2], &[s1, s2])
        } else {
            // Common prefix - share states for common bytes, then branch
            let orig_suffix = &orig[common_prefix..];
            let alt_suffix = &alt_bytes[common_prefix..];

            // Build the divergent part
            let diverge_table = if orig_suffix.is_empty() && alt_suffix.is_empty() {
                // Identical after common prefix (shouldn't happen but handle it)
                next_step.table.clone()
            } else if orig_suffix.is_empty() {
                // Original is done, alternate has more bytes
                let alt_state = make_fa_fragment(alt_suffix, next_step);
                SmallTable::with_mappings(None, &[alt_suffix[0]], &[alt_state])
            } else if alt_suffix.is_empty() {
                // Alternate is done, original has more bytes
                let orig_state = make_fa_fragment(orig_suffix, next_step);
                SmallTable::with_mappings(None, &[orig_suffix[0]], &[orig_state])
            } else {
                // Both have remaining bytes
                let orig_state = make_fa_fragment(orig_suffix, next_step.clone());
                let alt_state = make_fa_fragment(alt_suffix, next_step);

                let (b1, s1, b2, s2) = if orig_suffix[0] < alt_suffix[0] {
                    (orig_suffix[0], orig_state, alt_suffix[0], alt_state)
                } else {
                    (alt_suffix[0], alt_state, orig_suffix[0], orig_state)
                };
                SmallTable::with_mappings(None, &[b1, b2], &[s1, s2])
            };

            // Now build the common prefix chain
            let mut table = diverge_table;
            for i in (0..common_prefix).rev() {
                let state = Arc::new(FaState::with_table(table));
                table = SmallTable::with_mappings(None, &[orig[i]], &[state]);
            }
            table
        }
    } else {
        // No case alternate - single path
        let state = make_fa_fragment(orig, next_step);
        SmallTable::with_mappings(None, &[orig[0]], &[state])
    }
}

/// Build an FA fragment for a byte sequence, ending at the given state.
/// The returned state transitions on the first byte of val.
fn make_fa_fragment(val: &[u8], end_at: Arc<FaState>) -> Arc<FaState> {
    if val.is_empty() {
        return end_at;
    }
    if val.len() == 1 {
        return end_at;
    }

    // Build chain from last byte back to second byte
    let mut current = end_at;
    for i in (1..val.len()).rev() {
        let table = SmallTable::with_mappings(None, &[val[i]], &[current]);
        current = Arc::new(FaState::with_table(table));
    }

    current
}

/// Fallback byte-by-byte monocase FA for invalid UTF-8 (ASCII-only case folding)
fn make_monocase_fa_ascii(val: &[u8], next_field: Arc<FieldMatcher>) -> SmallTable {
    let final_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field],
    });
    let mut current_next = Arc::new(FaState {
        table: SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[final_state]),
        field_transitions: vec![],
    });

    for i in (0..val.len()).rev() {
        let byte = val[i];
        let alt_byte = if byte.is_ascii_lowercase() {
            Some(byte.to_ascii_uppercase())
        } else if byte.is_ascii_uppercase() {
            Some(byte.to_ascii_lowercase())
        } else {
            None
        };

        let table = if let Some(alt) = alt_byte {
            if byte < alt {
                SmallTable::with_mappings(
                    None,
                    &[byte, alt],
                    &[current_next.clone(), current_next.clone()],
                )
            } else {
                SmallTable::with_mappings(
                    None,
                    &[alt, byte],
                    &[current_next.clone(), current_next.clone()],
                )
            }
        } else {
            SmallTable::with_mappings(None, &[byte], &[current_next.clone()])
        };
        current_next = Arc::new(FaState::with_table(table));
    }

    current_next.table.clone()
}

/// Merge two finite automata into one that matches either pattern.
///
/// This computes the union of two automata by merging their transition tables.
pub fn merge_fas(table1: &SmallTable, table2: &SmallTable) -> SmallTable {
    let mut result = SmallTable::new();

    // Unpack both tables
    let unpacked1 = table1.unpack();
    let unpacked2 = table2.unpack();

    // Merge by taking non-None values (simplified - real implementation needs recursion)
    let mut merged: [Option<Arc<FaState>>; BYTE_CEILING] = std::array::from_fn(|_| None);
    for i in 0..BYTE_CEILING {
        merged[i] = match (&unpacked1[i], &unpacked2[i]) {
            (None, None) => None,
            (Some(s), None) | (None, Some(s)) => Some(s.clone()),
            (Some(s1), Some(s2)) => {
                // Need to recursively merge the states
                let merged_table = merge_fas(&s1.table, &s2.table);
                let mut merged_transitions = s1.field_transitions.clone();
                merged_transitions.extend(s2.field_transitions.iter().cloned());
                Some(Arc::new(FaState {
                    table: merged_table,
                    field_transitions: merged_transitions,
                }))
            }
        };
    }

    result.pack(&merged);

    // Merge epsilons
    result.epsilons = table1.epsilons.clone();
    result.epsilons.extend(table2.epsilons.iter().cloned());

    // Merge spinout - if either table has spinout, the merged table should have spinout
    result.spinout = match (&table1.spinout, &table2.spinout) {
        (None, None) => None,
        (Some(s), None) | (None, Some(s)) => Some(s.clone()),
        (Some(_), Some(_)) => {
            // Both have spinout - use either one (they're just markers)
            table1.spinout.clone()
        }
    };

    result
}
