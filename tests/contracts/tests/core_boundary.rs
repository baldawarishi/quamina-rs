#![cfg(feature = "core-boundary")]

use std::error::Error as _;
use std::panic::{AssertUnwindSafe, catch_unwind};

use quamina::{
    ArrayTrailBuilder, CanonicalField, CanonicalValue, DecoderBoundary, EventFormat, EventLimits,
    FieldPath, FieldSetBuilder, PatternFieldTracker, QuaminaError, RawArrayPos, RawField,
};

fn tracker(paths: &[&[&str]]) -> PatternFieldTracker {
    PatternFieldTracker::from_paths(
        paths
            .iter()
            .map(|segments| FieldPath::from_segments(segments.iter().copied())),
    )
}

fn boundary(limits: EventLimits) -> DecoderBoundary {
    DecoderBoundary::new(EventFormat::Custom("contract"), limits)
}

fn assert_rejected_without_panic(raw: Vec<RawField>, limits: EventLimits) -> QuaminaError {
    let result = catch_unwind(AssertUnwindSafe(|| boundary(limits).validate(raw)));
    assert!(result.is_ok(), "custom-field validation panicked");
    result
        .unwrap()
        .expect_err("malicious custom fields were accepted")
}

#[test]
fn segment_paths_distinguish_nesting_from_embedded_newlines() {
    let nested = FieldPath::from_segments(["a", "b"]);
    let newline = FieldPath::from_segments(["a\nb"]);

    assert_ne!(nested, newline);
    assert_eq!(nested.segments().collect::<Vec<_>>(), ["a", "b"]);
    assert_eq!(newline.segments().collect::<Vec<_>>(), ["a\nb"]);
    assert_ne!(nested.matcher_path_bytes(), newline.matcher_path_bytes());
}

#[test]
fn typed_values_own_string_escaping_and_scalar_type_tags() {
    let string = CanonicalValue::String("quote=\" slash=\\ control=\n Grüße 🌍".into());
    let boolean = CanonicalValue::Bool(true);
    let null = CanonicalValue::Null;

    assert_eq!(
        string.matcher_bytes(),
        "\"quote=\" slash=\\ control=\n Grüße 🌍\"".as_bytes()
    );
    assert!(!string.is_number());
    assert_eq!(boolean.matcher_bytes(), b"true");
    assert!(!boolean.is_number());
    assert_eq!(null.matcher_bytes(), b"null");
    assert!(!null.is_number());
}

#[test]
fn numbers_are_canonicalized_centrally_and_reject_non_finite_values() {
    let integer = CanonicalValue::number("42").unwrap();
    let decimal = CanonicalValue::number("42.000").unwrap();
    let exponent = CanonicalValue::number("4.2e1").unwrap();

    assert_eq!(integer.matcher_bytes(), decimal.matcher_bytes());
    assert_eq!(decimal.matcher_bytes(), exponent.matcher_bytes());
    assert!(integer.is_number());
    for invalid in ["NaN", "inf", "-Infinity", "42x", ""] {
        assert!(
            CanonicalValue::number(invalid).is_err(),
            "accepted {invalid:?}"
        );
    }
}

#[test]
fn array_ids_are_allocated_once_and_snapshots_preserve_all_ancestors() {
    let mut trails = ArrayTrailBuilder::new();
    let outer = trails.enter_array().unwrap();
    trails.set_position(outer, 1).unwrap();
    let inner = trails.enter_array().unwrap();
    trails.set_position(inner, 2).unwrap();
    let nested = trails.snapshot();
    trails.leave_array(inner).unwrap();
    let sibling = trails.enter_array().unwrap();
    trails.set_position(sibling, 1).unwrap();

    assert_eq!(nested.positions(), [(outer.id(), 1), (inner.id(), 2)]);
    assert_ne!(inner.id(), sibling.id());
    assert!(!nested.conflicts_with(&trails.snapshot()));
}

#[test]
fn array_positions_reject_negative_values_and_id_overflow() {
    let mut trails = ArrayTrailBuilder::with_next_id(i32::MAX as u64);
    let last = trails.enter_array().unwrap();
    assert!(trails.set_position(last, -1).is_err());
    assert!(trails.set_position(last, 0).is_err());
    assert!(trails.enter_sibling_array().is_err());
}

#[test]
fn field_construction_is_tracker_aware_and_returns_only_referenced_paths() {
    let tracker = tracker(&[&["status"], &["customer", "id"]]);
    let mut fields = FieldSetBuilder::new(&tracker, EventLimits::default());
    fields
        .emit(["status"], CanonicalValue::String("active".into()))
        .unwrap();
    fields
        .emit(["customer", "id"], CanonicalValue::String("c-1".into()))
        .unwrap();
    fields
        .emit(
            ["unreferenced", "secret"],
            CanonicalValue::String("discarded".into()),
        )
        .unwrap();

    let output = fields.finish().unwrap();
    assert_eq!(output.len(), 2);
    assert!(output.is_sorted_by_path());
}

#[test]
fn invalid_utf8_paths_are_deterministic_errors_before_matcher_conversion() {
    let raw = RawField::new(vec![0xff, b'x'], br#"\"value\""#.to_vec(), false, vec![]);
    let error = assert_rejected_without_panic(vec![raw], EventLimits::default());

    assert_eq!(error.format(), EventFormat::Custom("contract"));
    assert_eq!(error.location().field_index(), Some(0));
    assert!(error.to_string().contains("UTF-8"));
}

#[test]
fn raw_newline_paths_cannot_bypass_segment_validation() {
    let raw = RawField::new(b"a\nb".to_vec(), br#"\"value\""#.to_vec(), false, vec![]);
    let error = assert_rejected_without_panic(vec![raw], EventLimits::default());

    assert!(matches!(error, QuaminaError::AmbiguousEventPath { .. }));
}

#[test]
fn invalid_scalar_representations_and_numeric_flag_mismatches_are_rejected() {
    let cases = [
        RawField::new(b"x".to_vec(), b"unterminated".to_vec(), false, vec![]),
        RawField::new(b"x".to_vec(), b"not-a-number".to_vec(), true, vec![]),
        RawField::new(b"x".to_vec(), b"42".to_vec(), false, vec![]),
    ];

    for raw in cases {
        let error = assert_rejected_without_panic(vec![raw], EventLimits::default());
        assert!(matches!(error, QuaminaError::InvalidCanonicalField { .. }));
    }
}

#[test]
fn conflicting_array_ids_and_positions_are_rejected() {
    let fields = vec![
        RawField::new(
            b"orders\nsku".to_vec(),
            br#"\"A\""#.to_vec(),
            false,
            vec![RawArrayPos::new(4, 0)],
        ),
        RawField::new(
            b"labels".to_vec(),
            br#"\"red\""#.to_vec(),
            false,
            vec![RawArrayPos::new(4, 0)],
        ),
    ];
    let error = assert_rejected_without_panic(fields, EventLimits::default());

    assert!(matches!(
        error,
        QuaminaError::ConflictingArrayId { id: 4, .. }
    ));
}

#[test]
fn duplicate_fields_are_rejected_instead_of_silently_collapsed() {
    let field = RawField::new(b"status".to_vec(), br#"\"active\""#.to_vec(), false, vec![]);
    let error = assert_rejected_without_panic(vec![field.clone(), field], EventLimits::default());

    assert!(matches!(error, QuaminaError::DuplicateEventField { .. }));
}

#[test]
fn path_value_field_and_allocation_limits_apply_at_the_shared_boundary() {
    let limits = EventLimits {
        max_depth: 2,
        max_fields: 1,
        max_path_bytes: 8,
        max_scalar_bytes: 8,
        max_container_items: 2,
        max_total_allocated_bytes: 12,
    };
    let oversized_path = RawField::new(
        b"path-that-is-too-long".to_vec(),
        br#"\"x\""#.to_vec(),
        false,
        vec![],
    );
    let oversized_value = RawField::new(
        b"x".to_vec(),
        br#"\"value-that-is-too-long\""#.to_vec(),
        false,
        vec![],
    );

    for raw in [oversized_path, oversized_value] {
        let error = assert_rejected_without_panic(vec![raw], limits);
        assert!(matches!(error, QuaminaError::EventLimitExceeded { .. }));
    }
}

#[test]
fn format_neutral_errors_retain_format_location_and_source_chain() {
    let source = std::io::Error::new(std::io::ErrorKind::InvalidData, "bad varint");
    let error = QuaminaError::invalid_event(EventFormat::Custom("contract"))
        .at_byte_offset(17)
        .with_source(source);

    assert_eq!(error.format(), EventFormat::Custom("contract"));
    assert_eq!(error.location().byte_offset(), Some(17));
    assert_eq!(error.source().unwrap().to_string(), "bad varint");
}

#[test]
fn typed_fields_cannot_be_constructed_with_a_false_numeric_tag() {
    let field = CanonicalField::new(
        FieldPath::from_segments(["count"]),
        CanonicalValue::number("42").unwrap(),
    );

    assert!(field.is_number());
    assert_eq!(field.matcher_bytes(), b"42");
}
