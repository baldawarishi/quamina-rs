//! Shared k-gram windowing over normalized symbol sequences.
//!
//! Every symbol is one ASCII byte (see `tokenize.rs`), so a byte-sliced
//! window of width `w` is, by construction, both a `w`-character `&str`
//! and exactly `w` normalized tokens — which is what lets the fst crate's
//! Levenshtein automaton (edit distance in Unicode *characters*) measure
//! token-level edit distance without any translation layer.

use rustc_hash::FxHashSet;

/// Returns the fragment's distinct `w`-grams as `&str` windows, in
/// first-occurrence order. Empty if the fragment has fewer than `w`
/// symbols. Deduplicated per fragment: a gram repeated within one fragment
/// should only contribute that fragment to a postings list once.
pub fn unique_kgrams(symbols: &[u8], w: usize) -> Vec<&str> {
    if w == 0 || symbols.len() < w {
        return Vec::new();
    }
    let mut seen: FxHashSet<&str> = FxHashSet::default();
    let mut out = Vec::new();
    for window in symbols.windows(w) {
        let s = std::str::from_utf8(window)
            .expect("symbols are asserted ASCII at fragment build time (see assert_ascii_symbols)");
        if seen.insert(s) {
            out.push(s);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn windows_are_w_chars_and_w_bytes() {
        let symbols: Vec<u8> = vec![1, 2, 3, 4, 5];
        for w in 1..=5 {
            for gram in unique_kgrams(&symbols, w) {
                assert_eq!(gram.len(), w, "byte length must equal w");
                assert_eq!(gram.chars().count(), w, "char length must equal w");
            }
        }
    }

    #[test]
    fn dedups_within_a_fragment() {
        let symbols: Vec<u8> = vec![1, 2, 1, 2, 1, 2];
        let grams = unique_kgrams(&symbols, 2);
        // windows are [1,2] [2,1] [1,2] [2,1] [1,2] -> only two distinct
        assert_eq!(grams.len(), 2);
    }

    #[test]
    fn too_short_yields_nothing() {
        let symbols: Vec<u8> = vec![1, 2, 3];
        assert!(unique_kgrams(&symbols, 10).is_empty());
    }
}
