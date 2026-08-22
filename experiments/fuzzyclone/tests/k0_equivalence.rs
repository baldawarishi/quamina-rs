//! Integration test for the sanity check the task calls out explicitly:
//! "k=0 in arm B should match arm A's recall — if it doesn't, you have a
//! bug; fix it before continuing." This goes further than a recall-number
//! comparison: it asserts the two arms return the *exact same candidate
//! set*, not just the same recall fraction (two different-but-same-size
//! candidate sets could coincidentally produce equal recall while still
//! disagreeing on individual matches).
//!
//! Runs entirely on synthetic fragments (no repo parsing needed), so it's
//! fast enough to run on every `cargo test` rather than only in the full
//! sweep.

use fuzzyclone::corpus::Fragment;
use fuzzyclone::index_exact::ExactIndex;
use fuzzyclone::index_fst::FstIndex;
use fuzzyclone::mutate::{self, EDIT_RATES};
use fuzzyclone::tokenize::NormalizedFragment;

const VOCAB_SIZE: u8 = 40;

fn synthetic_corpus(n: usize, seed: u64) -> Vec<Fragment> {
    // A tiny xorshift, not `rand`, deliberately: this test should not
    // depend on the mutation harness's RNG choices to build its input
    // corpus, only to mutate it (via `mutate::generate_clones`, exercised
    // below).
    let mut state = seed | 1;
    let mut next = move || {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        state
    };

    (0..n)
        .map(|id| {
            let len = 60 + (next() % 80) as usize; // 60..140 symbols
            let mut symbols = Vec::with_capacity(len);
            for _ in 0..len {
                symbols.push(1 + (next() % u64::from(VOCAB_SIZE)) as u8);
            }
            // A handful of statement ranges spread evenly, so the mutation
            // harness's statement-based ops have something to work with.
            let stmt_count = 4;
            let stmt_len = len / stmt_count;
            let stmt_ranges = (0..stmt_count)
                .map(|i| (i * stmt_len, (i + 1) * stmt_len))
                .collect();
            #[allow(clippy::cast_possible_truncation)]
            Fragment {
                id: id as u32,
                file: "synthetic.rs".into(),
                name: format!("f{id}"),
                start_line: 1,
                normalized: NormalizedFragment {
                    symbols,
                    stmt_ranges,
                },
            }
        })
        .collect()
}

#[test]
fn exact_and_fst_k0_agree_on_every_original_fragment() {
    for &w in &[4usize, 8, 16] {
        let fragments = synthetic_corpus(15, 0x5EED_0001);
        let (exact, _) = ExactIndex::build(&fragments, w);
        let (fst, _) = FstIndex::build(&fragments, w);

        for frag in &fragments {
            let a = exact.query(&frag.normalized.symbols).candidates;
            let b = fst.query(&frag.normalized.symbols, 0).candidates;
            assert_eq!(
                a, b,
                "w={w} fragment {} disagreed between exact and fst(k=0)",
                frag.id
            );
        }
    }
}

#[test]
fn exact_and_fst_k0_agree_on_every_mutated_clone() {
    for &w in &[4usize, 8, 16] {
        let fragments = synthetic_corpus(15, 0x5EED_0002);
        let (exact, _) = ExactIndex::build(&fragments, w);
        let (fst, _) = FstIndex::build(&fragments, w);

        let donor_pool = mutate::build_donor_pool(&fragments);
        let clones = mutate::generate_clones(&fragments, &donor_pool, VOCAB_SIZE, 0x5EED_0003);
        assert_eq!(clones.len(), fragments.len() * EDIT_RATES.len());

        for clone in &clones {
            let a = exact.query(&clone.symbols).candidates;
            let b = fst.query(&clone.symbols, 0).candidates;
            assert_eq!(
                a, b,
                "w={w} clone {} (original {}, rate {}) disagreed between exact and fst(k=0)",
                clone.id, clone.original_id, clone.edit_rate_bucket
            );
        }
    }
}

#[test]
fn recall_is_identical_between_exact_and_fst_k0_across_all_edit_rates() {
    let fragments = synthetic_corpus(25, 0x5EED_0004);
    let donor_pool = mutate::build_donor_pool(&fragments);
    let clones = mutate::generate_clones(&fragments, &donor_pool, VOCAB_SIZE, 0x5EED_0005);

    for &w in &[4usize, 8, 12] {
        let (exact, _) = ExactIndex::build(&fragments, w);
        let (fst, _) = FstIndex::build(&fragments, w);

        for &rate in &EDIT_RATES {
            let query_set: Vec<_> = clones
                .iter()
                .filter(|c| (c.edit_rate_bucket - rate).abs() < 1e-9)
                .collect();
            let exact_hits = query_set
                .iter()
                .filter(|c| exact.query(&c.symbols).candidates.contains(&c.original_id))
                .count();
            let fst_hits = query_set
                .iter()
                .filter(|c| fst.query(&c.symbols, 0).candidates.contains(&c.original_id))
                .count();
            assert_eq!(
                exact_hits, fst_hits,
                "w={w} rate={rate}: exact recall and fst(k=0) recall diverged"
            );
        }
    }
}
