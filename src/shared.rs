//! A [`Quamina`] that many threads can match against while another updates it.

use crate::automaton::{FrozenFieldMatcher, ThreadSafeCoreMatcher};
use crate::segments_tree::SegmentsTree;
use crate::{PrunerStats, Quamina, QuaminaError, filter_deleted, match_json_event};
use arc_swap::ArcSwap;
use parking_lot::Mutex;
use rustc_hash::FxHashSet;
use std::hash::Hash;
use std::sync::Arc;

/// Everything a match reads, published as one unit so a reader never pairs
/// one update's automaton with another's segments tree or deleted set.
struct Snapshot<X: Clone + Eq + Hash> {
    root: Arc<FrozenFieldMatcher<X>>,
    segments_tree: SegmentsTree,
    deleted: FxHashSet<X>,
}

/// A [`Quamina`] whose methods all take `&self`, so matches never wait on updates.
///
/// Matches read an atomically published snapshot. Updates are serialized
/// through a mutex and do their freezing and rebuilding off the read path.
///
/// Share it across threads with `Arc<SharedQuamina<X>>`. A match that runs
/// concurrently with an update sees the matcher from either before or after it.
pub struct SharedQuamina<X: Clone + Eq + Hash + Send + Sync = String> {
    snapshot: ArcSwap<Snapshot<X>>,
    writer: Mutex<Quamina<X>>,
    /// Counts from the read path; the writer's own stats never see a match.
    pruner_stats: PrunerStats,
}

impl<X: Clone + Eq + Hash + Send + Sync> SharedQuamina<X> {
    /// Wrap `q`, keeping its patterns and settings.
    ///
    /// # Errors
    /// Returns `QuaminaError::InvalidPattern` if `q` uses a custom flattener,
    /// which this type does not yet support.
    pub fn new(q: Quamina<X>) -> Result<Self, QuaminaError> {
        if q.custom_flattener.is_some() {
            return Err(QuaminaError::InvalidPattern(
                "SharedQuamina does not support custom flatteners".into(),
            ));
        }
        Ok(Self {
            snapshot: ArcSwap::from_pointee(Self::snapshot_of(&q)),
            writer: Mutex::new(q),
            pruner_stats: PrunerStats::new(),
        })
    }

    fn snapshot_of(q: &Quamina<X>) -> Snapshot<X> {
        Snapshot {
            root: q.automaton.frozen_root(),
            segments_tree: q.segments_tree.clone(),
            deleted: q.deleted_patterns.clone(),
        }
    }

    fn publish(&self, q: &Quamina<X>) {
        self.snapshot.store(Arc::new(Self::snapshot_of(q)));
    }

    /// See [`Quamina::matches_for_event`].
    ///
    /// # Errors
    /// Returns an error if the event is not valid JSON.
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        let snapshot = self.snapshot.load();
        let raw_matches = match_json_event(event, &snapshot.segments_tree, |fields, bufs| {
            ThreadSafeCoreMatcher::matches_for_root(&snapshot.root, fields, bufs)
        })?;
        Ok(filter_deleted(
            raw_matches,
            &snapshot.deleted,
            &self.pruner_stats,
        ))
    }

    /// See [`Quamina::add_pattern`].
    ///
    /// # Errors
    /// Returns an error if the pattern is invalid or too complex.
    pub fn add_pattern(&self, x: X, pattern_json: &str) -> Result<(), QuaminaError> {
        let mut q = self.writer.lock();
        q.add_pattern(x, pattern_json)?;
        self.publish(&q);
        Ok(())
    }

    /// See [`Quamina::delete_patterns`].
    ///
    /// # Errors
    /// Never fails; returns `Result` to mirror [`Quamina::delete_patterns`].
    pub fn delete_patterns(&self, x: &X) -> Result<(), QuaminaError> {
        let mut q = self.writer.lock();
        if q.contains_pattern(x) {
            q.delete_patterns(x)?;
            self.publish(&q);
        }
        Ok(())
    }

    /// See [`Quamina::rebuild`].
    pub fn rebuild(&self) -> usize {
        let mut q = self.writer.lock();
        let purged = q.rebuild();
        self.pruner_stats.reset();
        self.publish(&q);
        purged
    }

    /// See [`Quamina::maybe_rebuild`].
    pub fn maybe_rebuild(&self) -> usize {
        let auto_rebuild = self.writer.lock().auto_rebuild_enabled();
        if auto_rebuild && self.pruner_stats.should_rebuild() {
            self.rebuild()
        } else {
            0
        }
    }

    /// See [`Quamina::pattern_count`].
    pub fn pattern_count(&self) -> usize {
        self.writer.lock().pattern_count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{QuaminaBuilder, flattener::JsonFlattener};

    #[test]
    fn updates_are_visible_to_later_matches() {
        let q = SharedQuamina::new(Quamina::<String>::new()).unwrap();
        let event = br#"{"a": "x", "b": "y"}"#;
        q.add_pattern("pa".into(), r#"{"a": ["x"]}"#).unwrap();
        q.add_pattern("pb".into(), r#"{"b": ["y"]}"#).unwrap();
        let mut got = q.matches_for_event(event).unwrap();
        got.sort();
        assert_eq!(got, ["pa", "pb"]);

        q.delete_patterns(&"pa".into()).unwrap();
        assert_eq!(q.matches_for_event(event).unwrap(), ["pb"]);
        assert_eq!(q.pattern_count(), 1);

        assert_eq!(q.rebuild(), 1);
        assert_eq!(q.pruner_stats.emitted(), 0);
        assert_eq!(q.matches_for_event(event).unwrap(), ["pb"]);
    }

    #[test]
    fn keeps_patterns_of_the_wrapped_quamina() {
        let mut inner = Quamina::<String>::new();
        inner.add_pattern("p".into(), r#"{"a": ["x"]}"#).unwrap();
        let q = SharedQuamina::new(inner).unwrap();
        assert_eq!(q.matches_for_event(br#"{"a": "x"}"#).unwrap(), ["p"]);
    }

    #[test]
    fn rejects_custom_flattener() {
        let inner = QuaminaBuilder::<String>::new()
            .with_flattener(Box::new(JsonFlattener::new()))
            .unwrap()
            .build()
            .unwrap();
        assert!(SharedQuamina::new(inner).is_err());
    }

    #[test]
    fn readers_keep_matching_while_writer_churns() {
        let q = SharedQuamina::new(Quamina::<String>::new()).unwrap();
        q.add_pattern("stable".into(), r#"{"a": ["x"]}"#).unwrap();
        let event = br#"{"a": "x"}"#;
        std::thread::scope(|s| {
            let readers: Vec<_> = (0..2)
                .map(|_| {
                    s.spawn(|| {
                        for _ in 0..20 {
                            let got = q.matches_for_event(event).unwrap();
                            assert!(got.contains(&"stable".to_string()));
                        }
                    })
                })
                .collect();
            for i in 0..10 {
                q.add_pattern("churn".into(), &format!(r#"{{"a": ["x{i}"]}}"#))
                    .unwrap();
                q.delete_patterns(&"churn".into()).unwrap();
                if i % 3 == 0 {
                    q.rebuild();
                }
            }
            for r in readers {
                r.join().unwrap();
            }
        });
    }
}
