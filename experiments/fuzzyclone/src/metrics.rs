//! One CSV row per (arm, w, k, edit_rate) sweep cell. Raw data — no
//! aggregation across cells happens here; each row already reports the
//! distribution stats (mean/p50/p99) for the queries that made up that
//! cell, but no row is ever averaged with another row.

use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Row {
    pub arm: &'static str, // "exact" | "fst_levenshtein"
    pub w: usize,
    /// Empty for the exact arm (edit distance has no meaning there).
    pub k: Option<u32>,
    pub edit_rate_bucket: f64,
    pub num_queries: usize,
    /// Fraction of queries whose ground-truth original id was in the
    /// returned candidate set.
    pub recall: f64,
    pub candidates_mean: f64,
    pub candidates_p50: usize,
    pub candidates_p99: usize,
    pub grams_per_query_mean: f64,
    pub query_p50_us: u128,
    pub query_p99_us: u128,
    pub query_mean_us: f64,
    pub index_build_ms: f64,
    pub index_distinct_grams: usize,
    pub index_payload_bytes: usize,
    pub corpus_size: usize,
}

/// Percentile over an already-sorted slice, nearest-rank method (matches
/// what people mean colloquially by "p50"/"p99" without pulling in a
/// stats crate for one function).
pub fn percentile(sorted: &[u128], pct: f64) -> u128 {
    if sorted.is_empty() {
        return 0;
    }
    let rank = ((pct / 100.0) * (sorted.len() as f64 - 1.0)).round() as usize;
    sorted[rank.min(sorted.len() - 1)]
}

pub fn percentile_usize(sorted: &[usize], pct: f64) -> usize {
    if sorted.is_empty() {
        return 0;
    }
    let rank = ((pct / 100.0) * (sorted.len() as f64 - 1.0)).round() as usize;
    sorted[rank.min(sorted.len() - 1)]
}

pub fn write_csv(path: &std::path::Path, rows: &[Row]) -> std::io::Result<()> {
    let mut writer = csv::Writer::from_path(path)?;
    for row in rows {
        writer
            .serialize(row)
            .expect("Row's fields are all directly serializable primitives");
    }
    writer.flush()?;
    Ok(())
}
