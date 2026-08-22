//! Arm B: the same k-grams as Arm A, but as FST keys queried through a
//! Levenshtein automaton — a fuzzy match at edit distance `k` instead of an
//! exact one.

use std::collections::BTreeMap;
use std::time::{Duration, Instant};

use fst::automaton::Levenshtein;
use fst::{IntoStreamer, Map, MapBuilder, Streamer};
use rustc_hash::FxHashSet;

use crate::corpus::Fragment;
use crate::kgram::unique_kgrams;
use crate::FragmentId;

pub struct FstIndex {
    pub w: usize,
    map: Map<Vec<u8>>,
    postings: Vec<Vec<FragmentId>>,
}

pub struct QueryResult {
    pub candidates: FxHashSet<FragmentId>,
    pub grams_queried: usize,
}

impl FstIndex {
    /// Builds the FST over `fragments`' `w`-grams. This does not depend on
    /// `k` — the same FST is reused for every distance the sweep tries.
    pub fn build(fragments: &[Fragment], w: usize) -> (Self, Duration) {
        let t0 = Instant::now();

        // FST keys must be inserted in strictly increasing byte order;
        // BTreeMap gives us that for free and de-duplicates grams that
        // appear in more than one fragment.
        let mut grouped: BTreeMap<Box<str>, Vec<FragmentId>> = BTreeMap::new();
        for frag in fragments {
            for gram in unique_kgrams(&frag.normalized.symbols, w) {
                grouped.entry(Box::from(gram)).or_default().push(frag.id);
            }
        }

        let mut postings = Vec::with_capacity(grouped.len());
        let mut builder = MapBuilder::memory();
        for (gram, ids) in grouped {
            #[allow(
                clippy::cast_possible_truncation,
                reason = "postings arena stays far below u32::MAX entries"
            )]
            let value = postings.len() as u64;
            postings.push(ids);
            builder.insert(gram.as_bytes(), value).expect(
                "keys inserted in sorted BTreeMap order, so this can't violate FST ordering",
            );
        }
        let map = builder.into_map();

        (Self { w, map, postings }, t0.elapsed())
    }

    /// Builds a fresh Levenshtein automaton for every gram, every call —
    /// deliberately uncached. An earlier version cached automata by
    /// `(gram, k)`, reasoning that real code's structural repetition would
    /// make that a good trade. It wasn't: each automaton's DFA can run to
    /// tens of MB (`fst`'s own source calls this out — "currently at least
    /// 20MB" per automaton, `automaton/levenshtein.rs`), mutated query
    /// grams turned out not to repeat nearly enough to earn that cost back,
    /// and the cache grew unbounded across a whole sweep cell. First real
    /// run OOM-killed at ~14GB RSS. Rebuilding per call is slower in
    /// aggregate but bounded and predictable — see JOURNAL.md.
    pub fn query(&self, symbols: &[u8], k: u32) -> QueryResult {
        let grams = unique_kgrams(symbols, self.w);
        let mut candidates = FxHashSet::default();
        for gram in &grams {
            let automaton = Levenshtein::new(gram, k)
                .expect("distance <= 2 over a <= 32-character gram stays well under fst's default DFA state limit");
            let mut stream = self.map.search(&automaton).into_stream();
            while let Some((_key, value)) = stream.next() {
                candidates.extend(self.postings[value as usize].iter().copied());
            }
        }
        QueryResult {
            candidates,
            grams_queried: grams.len(),
        }
    }

    /// Logical payload size: the FST's own serialized byte size (a
    /// well-defined number via `fst::raw::Fst::size`) plus the postings
    /// arena, on the same accounting basis as `ExactIndex::payload_bytes`.
    pub fn payload_bytes(&self) -> usize {
        const PER_ENTRY_OVERHEAD: usize = 24; // Vec header estimate for each postings list
        let postings_bytes: usize = self
            .postings
            .iter()
            .map(|v| v.len() * std::mem::size_of::<FragmentId>() + PER_ENTRY_OVERHEAD)
            .sum();
        self.map.as_fst().size() + postings_bytes
    }

    pub fn distinct_grams(&self) -> usize {
        self.postings.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::NormalizedFragment;

    fn frag(id: FragmentId, symbols: Vec<u8>) -> Fragment {
        Fragment {
            id,
            file: "t.rs".into(),
            name: "t".into(),
            start_line: 1,
            normalized: NormalizedFragment {
                symbols,
                stmt_ranges: vec![],
            },
        }
    }

    #[test]
    fn k0_exact_match_recovers_identical_fragment() {
        let fragments = vec![frag(0, vec![1, 2, 3, 4, 5, 6])];
        let (idx, _) = FstIndex::build(&fragments, 6);
        let result = idx.query(&[1, 2, 3, 4, 5, 6], 0);
        assert!(result.candidates.contains(&0));
    }

    #[test]
    fn k1_recovers_single_substitution_that_exact_match_would_miss() {
        let fragments = vec![frag(0, vec![1, 2, 3, 4, 5, 6])];
        let (idx, _) = FstIndex::build(&fragments, 6);
        let mutated = [1, 2, 9, 4, 5, 6];
        assert!(idx.query(&mutated, 0).candidates.is_empty());
        assert!(idx.query(&mutated, 1).candidates.contains(&0));
    }

    #[test]
    fn distance_beyond_k_still_misses() {
        let fragments = vec![frag(0, vec![1, 2, 3, 4, 5, 6])];
        let (idx, _) = FstIndex::build(&fragments, 6);
        let mutated = [9, 9, 9, 4, 5, 6]; // 3 substitutions
        assert!(!idx.query(&mutated, 2).candidates.contains(&0));
    }
}
