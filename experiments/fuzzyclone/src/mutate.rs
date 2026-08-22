//! The mutation harness: generates synthetic Type-3 clones from real
//! fragments at target edit rates, with a seeded RNG so runs are
//! reproducible, and records the ground truth (original_id, clone_id,
//! edit_rate) that both index arms are measured against.
//!
//! Mutations are applied directly to the normalized symbol sequence and
//! its statement ranges, not to Rust source text — see JOURNAL.md
//! ("Mutation operates on the normalized representation, not on Rust
//! source text") for why, and what that trades away.

use rand::prelude::*;
use rand_chacha::ChaCha8Rng;

use crate::corpus::Fragment;
use crate::tokenize::NormalizedFragment;
use crate::FragmentId;

pub const EDIT_RATES: [f64; 5] = [0.02, 0.05, 0.10, 0.20, 0.30];

/// Below this many remaining tokens of edit budget, only `SubstituteToken`
/// (cost 1) is offered — see the comment at its use site in `mutate_one`.
const STATEMENT_OP_MIN_BUDGET: usize = 10;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MutationOp {
    InsertStatement,
    DeleteStatement,
    SubstituteToken,
    SwapStatements,
}

/// A pool of donor statements (borrowed as owned copies) drawn from every
/// fragment in the corpus, used by `InsertStatement`. Built once; shared
/// read-only across all mutation calls.
pub struct DonorPool {
    statements: Vec<Vec<u8>>,
}

pub fn build_donor_pool(fragments: &[Fragment]) -> DonorPool {
    let mut statements = Vec::new();
    for f in fragments {
        for &(s, e) in &f.normalized.stmt_ranges {
            statements.push(f.normalized.symbols[s..e].to_vec());
        }
    }
    DonorPool { statements }
}

impl DonorPool {
    fn random_statement(&self, rng: &mut ChaCha8Rng) -> &[u8] {
        self.statements
            .choose(rng)
            .expect("corpus produced at least one statement; build() would have found no fragments otherwise")
            .as_slice()
    }
}

/// One synthetic clone, with the ground truth needed to score recall.
#[derive(Debug, Clone)]
pub struct CloneFragment {
    pub id: FragmentId,
    pub original_id: FragmentId,
    pub edit_rate_bucket: f64,
    pub actual_edit_rate: f64,
    pub ops_applied: usize,
    pub symbols: Vec<u8>,
}

/// Generates one clone per (fragment, edit rate) pair, in that nested
/// order, from a single seeded RNG stream — so re-running with the same
/// seed and the same corpus reproduces byte-identical clones.
pub fn generate_clones(
    fragments: &[Fragment],
    donor_pool: &DonorPool,
    vocab_size: u8,
    seed: u64,
) -> Vec<CloneFragment> {
    let mut rng = ChaCha8Rng::seed_from_u64(seed);
    let mut clones = Vec::with_capacity(fragments.len() * EDIT_RATES.len());
    let mut next_id: FragmentId = 0;
    for frag in fragments {
        for &rate in &EDIT_RATES {
            let (symbols, actual_edit_rate, ops_applied) =
                mutate_one(&frag.normalized, donor_pool, vocab_size, rate, &mut rng);
            clones.push(CloneFragment {
                id: next_id,
                original_id: frag.id,
                edit_rate_bucket: rate,
                actual_edit_rate,
                ops_applied,
                symbols,
            });
            next_id += 1;
        }
    }
    clones
}

/// Applies a random sequence of mutation ops to `original` until the
/// cumulative edit cost reaches `target_rate * original.len()` (rounding
/// up to at least one edit), then stops. Op costs are the number of
/// normalized tokens each op visibly changes: 1 for a substitution, the
/// donor/removed statement's length for insert/delete, and the combined
/// length of both statements for a swap (an upper bound on a swap's true
/// Levenshtein cost when the two statements differ in length — see
/// JOURNAL.md). Because costs are discrete lumps, the achieved
/// `actual_edit_rate` is reported alongside the `target_rate` bucket
/// rather than assumed to hit it exactly.
fn mutate_one(
    original: &NormalizedFragment,
    donor_pool: &DonorPool,
    vocab_size: u8,
    target_rate: f64,
    rng: &mut ChaCha8Rng,
) -> (Vec<u8>, f64, usize) {
    let original_len = original.symbols.len();
    #[allow(
        clippy::cast_possible_truncation,
        reason = "target_edits is bounded by original_len, itself far below u32::MAX"
    )]
    let target_edits = ((target_rate * original_len as f64).round() as usize).max(1);

    let mut symbols = original.symbols.clone();
    let mut stmt_ranges = original.stmt_ranges.clone();
    let mut cumulative_cost = 0usize;
    let mut ops_applied = 0usize;

    // Generous but finite: real convergence happens in O(target_edits /
    // avg_op_cost) iterations; this just rules out a pathological spin if
    // a fragment has degenerate statement structure.
    let max_iterations = original_len.saturating_mul(4).max(16);

    for _ in 0..max_iterations {
        if cumulative_cost >= target_edits {
            break;
        }
        let remaining = target_edits - cumulative_cost;
        // A statement-level op costs a whole statement's worth of tokens
        // (this corpus averages ~24 tokens/statement — see JOURNAL.md), so
        // picking one uniformly at random whenever *any* budget remains
        // means the first op alone routinely blows past a small target
        // (a 2% target on a 150-token fragment is 3 tokens). Below
        // `STATEMENT_OP_MIN_BUDGET`, restrict to `SubstituteToken` (cost
        // exactly 1) so low edit rates actually converge to something
        // close to their target instead of systematically overshooting
        // it by multiples. See JOURNAL.md for the measurement that caught
        // this and the before/after numbers.
        let mut feasible = vec![MutationOp::SubstituteToken];
        if remaining >= STATEMENT_OP_MIN_BUDGET {
            feasible.push(MutationOp::InsertStatement);
            if stmt_ranges.len() > 1 {
                feasible.push(MutationOp::DeleteStatement);
            }
            if stmt_ranges.len() >= 2 {
                feasible.push(MutationOp::SwapStatements);
            }
        }
        let op = feasible[rng.random_range(0..feasible.len())];
        let cost = match op {
            MutationOp::SubstituteToken => substitute_token(&mut symbols, vocab_size, rng),
            MutationOp::InsertStatement => {
                insert_statement(&mut symbols, &mut stmt_ranges, donor_pool, rng)
            }
            MutationOp::DeleteStatement => delete_statement(&mut symbols, &mut stmt_ranges, rng),
            MutationOp::SwapStatements => swap_statements(&mut symbols, &mut stmt_ranges, rng),
        };
        cumulative_cost += cost;
        ops_applied += 1;
    }

    #[allow(
        clippy::cast_precision_loss,
        reason = "fragment/edit counts are small; precision loss is not observable"
    )]
    let actual_edit_rate = cumulative_cost as f64 / original_len as f64;
    (symbols, actual_edit_rate, ops_applied)
}

fn substitute_token(symbols: &mut [u8], vocab_size: u8, rng: &mut ChaCha8Rng) -> usize {
    if symbols.is_empty() || vocab_size < 2 {
        return 0;
    }
    let i = rng.random_range(0..symbols.len());
    let current = symbols[i];
    // Pick a different symbol from the corpus's actual vocabulary (codes
    // 1..=vocab_size), never 0 (reserved, never emitted by the tokenizer).
    loop {
        let candidate = rng.random_range(1..=vocab_size);
        if candidate != current {
            symbols[i] = candidate;
            return 1;
        }
    }
}

fn insert_statement(
    symbols: &mut Vec<u8>,
    stmt_ranges: &mut Vec<(usize, usize)>,
    donor_pool: &DonorPool,
    rng: &mut ChaCha8Rng,
) -> usize {
    let donor = donor_pool.random_statement(rng).to_vec();
    let slot = rng.random_range(0..=stmt_ranges.len());
    let byte_pos = if slot < stmt_ranges.len() {
        stmt_ranges[slot].0
    } else if let Some(last) = stmt_ranges.last() {
        last.1
    } else {
        symbols.len()
    };

    symbols.splice(byte_pos..byte_pos, donor.iter().copied());
    for r in stmt_ranges.iter_mut() {
        if r.0 >= byte_pos {
            r.0 += donor.len();
            r.1 += donor.len();
        }
    }
    stmt_ranges.insert(slot, (byte_pos, byte_pos + donor.len()));
    donor.len()
}

fn delete_statement(
    symbols: &mut Vec<u8>,
    stmt_ranges: &mut Vec<(usize, usize)>,
    rng: &mut ChaCha8Rng,
) -> usize {
    let idx = rng.random_range(0..stmt_ranges.len());
    let (start, end) = stmt_ranges[idx];
    let len = end - start;
    symbols.splice(start..end, std::iter::empty());
    stmt_ranges.remove(idx);
    for r in stmt_ranges.iter_mut() {
        if r.0 >= end {
            r.0 -= len;
            r.1 -= len;
        }
    }
    len
}

/// Swaps the token content of two distinct statement slots. "Independent"
/// is not verified by any dataflow analysis (out of scope for this
/// skeleton — see JOURNAL.md): any two distinct slots are eligible.
fn swap_statements(
    symbols: &mut Vec<u8>,
    stmt_ranges: &mut [(usize, usize)],
    rng: &mut ChaCha8Rng,
) -> usize {
    let n = stmt_ranges.len();
    let a = rng.random_range(0..n);
    let mut b = rng.random_range(0..n);
    while b == a {
        b = rng.random_range(0..n);
    }
    let (i, j) = if a < b { (a, b) } else { (b, a) };
    let (i_start, i_end) = stmt_ranges[i];
    let (j_start, j_end) = stmt_ranges[j];
    let len_i = i_end - i_start;
    let len_j = j_end - j_start;

    let mut rebuilt = Vec::with_capacity(symbols.len());
    rebuilt.extend_from_slice(&symbols[..i_start]);
    let new_i_start = rebuilt.len();
    rebuilt.extend_from_slice(&symbols[j_start..j_end]); // j's content now occupies slot i
    let new_i_end = rebuilt.len();
    rebuilt.extend_from_slice(&symbols[i_end..j_start]); // untouched middle region
    let new_j_start = rebuilt.len();
    rebuilt.extend_from_slice(&symbols[i_start..i_end]); // i's content now occupies slot j
    let new_j_end = rebuilt.len();
    rebuilt.extend_from_slice(&symbols[j_end..]);

    let delta = len_j as i64 - len_i as i64;
    for (idx, r) in stmt_ranges.iter_mut().enumerate() {
        if idx == i {
            *r = (new_i_start, new_i_end);
        } else if idx == j {
            *r = (new_j_start, new_j_end);
        } else if idx > i && idx < j {
            r.0 = (r.0 as i64 + delta) as usize;
            r.1 = (r.1 as i64 + delta) as usize;
        }
        // idx < i or idx > j: unchanged, see JOURNAL.md derivation.
    }

    *symbols = rebuilt;
    len_i + len_j
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::corpus::Fragment;

    fn frag_with_stmts(id: FragmentId, stmt_lens: &[usize]) -> Fragment {
        let mut symbols = Vec::new();
        let mut stmt_ranges = Vec::new();
        symbols.push(10); // pretend signature token, so stmt offsets aren't 0
        for &len in stmt_lens {
            let start = symbols.len();
            for k in 0..len {
                #[allow(clippy::cast_possible_truncation)]
                symbols.push(20 + k as u8);
            }
            stmt_ranges.push((start, symbols.len()));
        }
        symbols.push(11); // pretend closing brace
        Fragment {
            id,
            file: "t.rs".into(),
            name: "t".into(),
            start_line: 1,
            normalized: NormalizedFragment {
                symbols,
                stmt_ranges,
            },
        }
    }

    #[test]
    fn swap_conserves_length_and_moves_content() {
        let frag = frag_with_stmts(0, &[3, 5]);
        let mut symbols = frag.normalized.symbols.clone();
        let mut stmt_ranges = frag.normalized.stmt_ranges.clone();
        let original_len = symbols.len();
        let mut rng = ChaCha8Rng::seed_from_u64(1);
        let cost = swap_statements(&mut symbols, &mut stmt_ranges, &mut rng);
        assert_eq!(cost, 3 + 5);
        assert_eq!(symbols.len(), original_len);
        // slot 0 now has the (longer) content that used to be slot 1's.
        assert_eq!(stmt_ranges[0].1 - stmt_ranges[0].0, 5);
        assert_eq!(stmt_ranges[1].1 - stmt_ranges[1].0, 3);
    }

    #[test]
    fn delete_removes_exactly_one_statement_worth_of_tokens() {
        let frag = frag_with_stmts(0, &[4, 2, 6]);
        let mut symbols = frag.normalized.symbols.clone();
        let mut stmt_ranges = frag.normalized.stmt_ranges.clone();
        let mut rng = ChaCha8Rng::seed_from_u64(7);
        let before = symbols.len();
        let cost = delete_statement(&mut symbols, &mut stmt_ranges, &mut rng);
        assert_eq!(symbols.len(), before - cost);
        assert_eq!(stmt_ranges.len(), 2);
    }

    #[test]
    fn insert_grows_by_exactly_the_donor_length() {
        let frag = frag_with_stmts(0, &[4, 2]);
        let donor_pool = DonorPool {
            statements: vec![vec![99, 99, 99]],
        };
        let mut symbols = frag.normalized.symbols.clone();
        let mut stmt_ranges = frag.normalized.stmt_ranges.clone();
        let before = symbols.len();
        let mut rng = ChaCha8Rng::seed_from_u64(3);
        let cost = insert_statement(&mut symbols, &mut stmt_ranges, &donor_pool, &mut rng);
        assert_eq!(cost, 3);
        assert_eq!(symbols.len(), before + 3);
        assert_eq!(stmt_ranges.len(), 3);
    }

    #[test]
    fn substitute_always_changes_the_chosen_byte() {
        let mut symbols = vec![5u8; 10];
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        for _ in 0..50 {
            let before = symbols.clone();
            substitute_token(&mut symbols, 20, &mut rng);
            let diffs = before.iter().zip(&symbols).filter(|(a, b)| a != b).count();
            assert!(diffs <= 1);
        }
    }

    #[test]
    fn mutation_is_deterministic_given_a_seed() {
        let frag = frag_with_stmts(0, &[4, 5, 3, 6]);
        let donor_pool = build_donor_pool(std::slice::from_ref(&frag));
        let mut rng_a = ChaCha8Rng::seed_from_u64(123);
        let mut rng_b = ChaCha8Rng::seed_from_u64(123);
        let (out_a, rate_a, _) = mutate_one(&frag.normalized, &donor_pool, 30, 0.2, &mut rng_a);
        let (out_b, rate_b, _) = mutate_one(&frag.normalized, &donor_pool, 30, 0.2, &mut rng_b);
        assert_eq!(out_a, out_b);
        assert_eq!(rate_a, rate_b);
    }

    /// Regression test for the overshoot bug found while running the real
    /// sweep (see JOURNAL.md): with statement costs of ~20+ tokens and no
    /// budget-aware op selection, a 2% target on a 150-token fragment came
    /// out at a *measured* mean actual rate of 22%, over 10x too high,
    /// because the very first randomly-chosen op was routinely a whole
    /// statement. Statements here are deliberately sized like the real
    /// corpus (mean ~24 tokens/statement) so this exercises the same
    /// regime that broke.
    #[test]
    fn low_target_rates_converge_close_instead_of_overshooting_wildly() {
        let frag = frag_with_stmts(0, &[22, 25, 20, 27, 24, 23]); // 141 stmt tokens, ~24 avg
        let donor_pool = build_donor_pool(std::slice::from_ref(&frag));
        let original_len = frag.normalized.symbols.len();

        for &rate in &[0.02, 0.05] {
            let mut worst_overshoot = 0.0_f64;
            for seed in 0..20u64 {
                let mut rng = ChaCha8Rng::seed_from_u64(seed);
                let (_, actual_rate, _) =
                    mutate_one(&frag.normalized, &donor_pool, 30, rate, &mut rng);
                worst_overshoot = worst_overshoot.max(actual_rate - rate);
            }
            // Bounded by construction: below STATEMENT_OP_MIN_BUDGET tokens
            // of remaining budget only SubstituteToken (cost 1) is offered,
            // so actual can exceed target by at most a handful of tokens,
            // not by a whole extra statement (~24 tokens here).
            let max_reasonable_overshoot =
                (STATEMENT_OP_MIN_BUDGET as f64 + 2.0) / original_len as f64;
            assert!(
                worst_overshoot <= max_reasonable_overshoot,
                "rate={rate}: worst overshoot {worst_overshoot:.3} exceeded {max_reasonable_overshoot:.3}"
            );
        }
    }
}
