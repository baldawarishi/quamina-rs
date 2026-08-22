//! Orchestrates the whole experiment: build the corpus, generate synthetic
//! clones at each edit rate, build both index arms for each window size,
//! query every clone through every arm/k combination, and write raw
//! per-cell results to `results/*.csv`.
//!
//! Deliberately unparallelized and unoptimized (task constraint), but see
//! JOURNAL.md's "why not the full corpus at every k" entry: Levenshtein
//! automaton *construction* in the `fst` crate (not the FST search itself)
//! is expensive and grows steeply with both `w` and `k` — confirmed by
//! direct measurement before this binary was written, not assumed. `k=2`
//! is therefore run on a smaller nested subsample than `k in {0,1}` and
//! the exact arm; see `N_STANDARD` / `N_K2` below.

use std::path::{Path, PathBuf};
use std::time::Instant;

use fuzzyclone::corpus::{self, Fragment};
use fuzzyclone::index_exact::ExactIndex;
use fuzzyclone::index_fst::FstIndex;
use fuzzyclone::metrics::{percentile, percentile_usize, write_csv, Row};
use fuzzyclone::mutate::{self, CloneFragment, EDIT_RATES};

const W_VALUES: [usize; 5] = [8, 12, 16, 24, 32];

/// Sample size used for the exact arm and for arm B at k in {0,1}. See the
/// module doc and JOURNAL.md for why this isn't the full ~1500-fragment
/// corpus: it's a deliberate, measured compute-budget cut, not laziness.
const N_STANDARD: usize = 40;

/// Sample size for arm B at k=2, nested as the first `N_K2` fragments of
/// the standard sample (not an independent sample) so the two are
/// directly comparable subsets of the same population, just different
/// sizes.
const N_K2: usize = 10;

const MUTATION_SEED: u64 = 0xC10E_2026;

fn main() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let repo_root = std::fs::canonicalize(&repo_root).unwrap_or(repo_root);

    println!(
        "fuzzyclone sweep: parsing corpus from {}",
        repo_root.display()
    );
    let t0 = Instant::now();
    let corpus = corpus::build(&repo_root);
    println!(
        "corpus: {} fragments, {} distinct symbols, parsed in {:?}",
        corpus.fragments.len(),
        corpus.vocab.len(),
        t0.elapsed()
    );
    assert!(
        corpus.fragments.len() >= N_STANDARD,
        "corpus only produced {} fragments, need at least N_STANDARD={N_STANDARD}",
        corpus.fragments.len()
    );

    let standard_sample = stride_sample(&corpus.fragments, N_STANDARD);
    let k2_sample_ids: rustc_hash::FxHashSet<_> =
        standard_sample[..N_K2].iter().map(|f| f.id).collect();
    println!(
        "standard sample: {} fragments (stride over {} total); k=2 subsample: {} fragments",
        standard_sample.len(),
        corpus.fragments.len(),
        k2_sample_ids.len()
    );

    #[allow(
        clippy::cast_possible_truncation,
        reason = "corpus.vocab.len() is well under 126 by construction, see tokenize.rs"
    )]
    let vocab_size = corpus.vocab.len() as u8;

    // Donor statements are drawn from the *whole* corpus for realistic
    // variety, independent of which fragments end up sampled as queries.
    let donor_pool = mutate::build_donor_pool(&corpus.fragments);
    let clones = mutate::generate_clones(&standard_sample, &donor_pool, vocab_size, MUTATION_SEED);
    println!(
        "generated {} clones ({} fragments x {} edit rates)",
        clones.len(),
        standard_sample.len(),
        EDIT_RATES.len()
    );

    write_ground_truth(&repo_root, &clones);

    let mut rows = Vec::new();

    for &w in &W_VALUES {
        println!("\n=== w={w} ===");

        let (exact_idx, exact_build_ms) = timed_ms(|| ExactIndex::build(&standard_sample, w));
        println!(
            "  exact index: build {:.1}ms, {} distinct grams, {} bytes payload",
            exact_build_ms,
            exact_idx.distinct_grams(),
            exact_idx.payload_bytes()
        );
        for &rate in &EDIT_RATES {
            let query_set = clones_at_rate(&clones, rate, None);
            let row = measure_exact(
                &exact_idx,
                &query_set,
                w,
                rate,
                exact_build_ms,
                standard_sample.len(),
            );
            println!(
                "  exact  w={w:>2} rate={rate:>4.2}: recall={:.3} candidates_mean={:.1} p50_us={} p99_us={}",
                row.recall, row.candidates_mean, row.query_p50_us, row.query_p99_us
            );
            rows.push(row);
        }

        let (fst_idx, fst_build_ms) = timed_ms(|| FstIndex::build(&standard_sample, w));
        println!(
            "  fst index:   build {:.1}ms, {} distinct grams, {} bytes payload",
            fst_build_ms,
            fst_idx.distinct_grams(),
            fst_idx.payload_bytes()
        );

        for k in [0u32, 1] {
            for &rate in &EDIT_RATES {
                let query_set = clones_at_rate(&clones, rate, None);
                let row = measure_fst(
                    &fst_idx,
                    &query_set,
                    w,
                    k,
                    rate,
                    fst_build_ms,
                    standard_sample.len(),
                );
                println!(
                    "  fst k={k} w={w:>2} rate={rate:>4.2}: recall={:.3} candidates_mean={:.1} p50_us={} p99_us={}",
                    row.recall, row.candidates_mean, row.query_p50_us, row.query_p99_us
                );
                rows.push(row);
            }
        }

        // k=2: nested subsample only (see module doc).
        for &rate in &EDIT_RATES {
            let query_set = clones_at_rate(&clones, rate, Some(&k2_sample_ids));
            let row = measure_fst(&fst_idx, &query_set, w, 2, rate, fst_build_ms, N_K2);
            println!(
                "  fst k=2 w={w:>2} rate={rate:>4.2}: recall={:.3} candidates_mean={:.1} p50_us={} p99_us={} (n={})",
                row.recall,
                row.candidates_mean,
                row.query_p50_us,
                row.query_p99_us,
                row.num_queries
            );
            rows.push(row);
        }
    }

    let results_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("results");
    std::fs::create_dir_all(&results_dir).expect("results/ directory must be creatable");
    let out_path = results_dir.join("sweep.csv");
    write_csv(&out_path, &rows).expect("writing results/sweep.csv must succeed");
    println!("\nwrote {} rows to {}", rows.len(), out_path.display());
}

fn stride_sample(fragments: &[Fragment], n: usize) -> Vec<Fragment> {
    let stride = (fragments.len() / n).max(1);
    fragments.iter().step_by(stride).take(n).cloned().collect()
}

fn clones_at_rate<'a>(
    clones: &'a [CloneFragment],
    rate: f64,
    restrict_to: Option<&rustc_hash::FxHashSet<u32>>,
) -> Vec<&'a CloneFragment> {
    clones
        .iter()
        .filter(|c| (c.edit_rate_bucket - rate).abs() < 1e-9)
        .filter(|c| restrict_to.is_none_or(|ids| ids.contains(&c.original_id)))
        .collect()
}

fn measure_exact(
    idx: &ExactIndex,
    query_set: &[&CloneFragment],
    w: usize,
    rate: f64,
    build_ms: f64,
    corpus_size: usize,
) -> Row {
    let mut hits = 0usize;
    let mut candidate_counts = Vec::with_capacity(query_set.len());
    let mut gram_counts = Vec::with_capacity(query_set.len());
    let mut latencies_us = Vec::with_capacity(query_set.len());

    for clone in query_set {
        let t0 = Instant::now();
        let result = idx.query(&clone.symbols);
        latencies_us.push(t0.elapsed().as_micros());
        if result.candidates.contains(&clone.original_id) {
            hits += 1;
        }
        candidate_counts.push(result.candidates.len());
        gram_counts.push(result.grams_queried);
    }

    build_row(
        "exact",
        w,
        None,
        rate,
        hits,
        candidate_counts,
        gram_counts,
        latencies_us,
        build_ms,
        idx.distinct_grams(),
        idx.payload_bytes(),
        corpus_size,
    )
}

fn measure_fst(
    idx: &FstIndex,
    query_set: &[&CloneFragment],
    w: usize,
    k: u32,
    rate: f64,
    build_ms: f64,
    corpus_size: usize,
) -> Row {
    let mut hits = 0usize;
    let mut candidate_counts = Vec::with_capacity(query_set.len());
    let mut gram_counts = Vec::with_capacity(query_set.len());
    let mut latencies_us = Vec::with_capacity(query_set.len());

    for clone in query_set {
        let t0 = Instant::now();
        let result = idx.query(&clone.symbols, k);
        latencies_us.push(t0.elapsed().as_micros());
        if result.candidates.contains(&clone.original_id) {
            hits += 1;
        }
        candidate_counts.push(result.candidates.len());
        gram_counts.push(result.grams_queried);
    }

    build_row(
        "fst_levenshtein",
        w,
        Some(k),
        rate,
        hits,
        candidate_counts,
        gram_counts,
        latencies_us,
        build_ms,
        idx.distinct_grams(),
        idx.payload_bytes(),
        corpus_size,
    )
}

#[allow(
    clippy::too_many_arguments,
    reason = "one row aggregates a lot of independent measurements; splitting it up would just move the parameter list into a builder"
)]
fn build_row(
    arm: &'static str,
    w: usize,
    k: Option<u32>,
    edit_rate_bucket: f64,
    hits: usize,
    mut candidate_counts: Vec<usize>,
    gram_counts: Vec<usize>,
    mut latencies_us: Vec<u128>,
    index_build_ms: f64,
    index_distinct_grams: usize,
    index_payload_bytes: usize,
    corpus_size: usize,
) -> Row {
    let num_queries = candidate_counts.len();
    candidate_counts.sort_unstable();
    latencies_us.sort_unstable();

    #[allow(
        clippy::cast_precision_loss,
        reason = "query/candidate counts are small integers; precision loss is not observable"
    )]
    let candidates_mean = candidate_counts.iter().sum::<usize>() as f64 / num_queries.max(1) as f64;
    #[allow(
        clippy::cast_precision_loss,
        reason = "gram counts are small integers; precision loss is not observable"
    )]
    let grams_per_query_mean = gram_counts.iter().sum::<usize>() as f64 / num_queries.max(1) as f64;
    #[allow(
        clippy::cast_precision_loss,
        reason = "hit/query counts are small integers; precision loss is not observable"
    )]
    let recall = hits as f64 / num_queries.max(1) as f64;
    #[allow(
        clippy::cast_precision_loss,
        reason = "latency sums stay far below f64's exact-integer range for this experiment's scale"
    )]
    let query_mean_us = latencies_us.iter().sum::<u128>() as f64 / num_queries.max(1) as f64;

    Row {
        arm,
        w,
        k,
        edit_rate_bucket,
        num_queries,
        recall,
        candidates_mean,
        candidates_p50: percentile_usize(&candidate_counts, 50.0),
        candidates_p99: percentile_usize(&candidate_counts, 99.0),
        grams_per_query_mean,
        query_p50_us: percentile(&latencies_us, 50.0),
        query_p99_us: percentile(&latencies_us, 99.0),
        query_mean_us,
        index_build_ms,
        index_distinct_grams,
        index_payload_bytes,
        corpus_size,
    }
}

fn timed_ms<T>(f: impl FnOnce() -> (T, std::time::Duration)) -> (T, f64) {
    let (value, build_time) = f();
    (value, build_time.as_secs_f64() * 1000.0)
}

fn write_ground_truth(repo_root: &Path, clones: &[CloneFragment]) {
    #[derive(serde::Serialize)]
    struct GtRow {
        original_id: u32,
        clone_id: u32,
        edit_rate_bucket: f64,
        actual_edit_rate: f64,
        ops_applied: usize,
    }
    let _ = repo_root; // kept for symmetry/logging if this grows; unused today
    let results_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("results");
    std::fs::create_dir_all(&results_dir).expect("results/ directory must be creatable");
    let path: PathBuf = results_dir.join("ground_truth.csv");
    let mut writer = csv::Writer::from_path(&path).expect("ground_truth.csv must be creatable");
    for c in clones {
        writer
            .serialize(GtRow {
                original_id: c.original_id,
                clone_id: c.id,
                edit_rate_bucket: c.edit_rate_bucket,
                actual_edit_rate: c.actual_edit_rate,
                ops_applied: c.ops_applied,
            })
            .expect("GtRow's fields are all directly serializable primitives");
    }
    writer
        .flush()
        .expect("flushing ground_truth.csv must succeed");
    println!(
        "wrote {} ground-truth rows to {}",
        clones.len(),
        path.display()
    );
}
