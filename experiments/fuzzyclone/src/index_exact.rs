//! Arm A (baseline): exact k-gram inverted index over normalized fragments.

use std::time::{Duration, Instant};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::corpus::Fragment;
use crate::kgram::unique_kgrams;
use crate::FragmentId;

pub struct ExactIndex {
    pub w: usize,
    postings: FxHashMap<Box<str>, Vec<FragmentId>>,
}

/// Result of one query: the candidate fragment ids (deduplicated) and how
/// many distinct query k-grams were looked up (a cheap proxy for query
/// work, independent of how many of those lookups hit).
pub struct QueryResult {
    pub candidates: FxHashSet<FragmentId>,
    pub grams_queried: usize,
}

impl ExactIndex {
    /// Builds the index over `fragments`' `w`-grams. Returns the index and
    /// how long building it took.
    pub fn build(fragments: &[Fragment], w: usize) -> (Self, Duration) {
        let t0 = Instant::now();
        let mut postings: FxHashMap<Box<str>, Vec<FragmentId>> = FxHashMap::default();
        for frag in fragments {
            for gram in unique_kgrams(&frag.normalized.symbols, w) {
                postings.entry(Box::from(gram)).or_default().push(frag.id);
            }
        }
        (Self { w, postings }, t0.elapsed())
    }

    pub fn query(&self, symbols: &[u8]) -> QueryResult {
        let grams = unique_kgrams(symbols, self.w);
        let mut candidates = FxHashSet::default();
        for gram in &grams {
            if let Some(ids) = self.postings.get(*gram) {
                candidates.extend(ids.iter().copied());
            }
        }
        QueryResult {
            candidates,
            grams_queried: grams.len(),
        }
    }

    /// Logical payload size: gram key bytes plus 4 bytes per posting entry,
    /// plus a fixed per-bucket overhead estimate. This is not measured
    /// process RSS (that would include allocator and `HashMap` bucket
    /// slack); it's a reproducible, comparable-across-arms lower bound on
    /// what the index actually stores, which is what "index size" means in
    /// FINDINGS.md.
    pub fn payload_bytes(&self) -> usize {
        const PER_ENTRY_OVERHEAD: usize = 32; // hashmap bucket + Vec header estimate
        self.postings
            .iter()
            .map(|(k, v)| {
                k.len() + v.len() * std::mem::size_of::<FragmentId>() + PER_ENTRY_OVERHEAD
            })
            .sum()
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
    fn exact_match_recovers_identical_fragment() {
        let fragments = vec![frag(0, vec![1, 2, 3, 4, 5, 6])];
        let (idx, _) = ExactIndex::build(&fragments, 3);
        let result = idx.query(&[1, 2, 3, 4, 5, 6]);
        assert!(result.candidates.contains(&0));
    }

    #[test]
    fn single_byte_substitution_breaks_the_only_gram() {
        let fragments = vec![frag(0, vec![1, 2, 3, 4, 5, 6])];
        // w == fragment length, so there's exactly one window: any single
        // substitution anywhere in it makes the exact-match miss total.
        let (idx, _) = ExactIndex::build(&fragments, 6);
        let mutated = [1, 2, 9, 4, 5, 6];
        let result = idx.query(&mutated);
        assert!(!result.candidates.contains(&0));
    }
}
