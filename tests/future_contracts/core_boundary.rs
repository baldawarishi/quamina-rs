//! Compile contract for a format-neutral field construction boundary.
//!
//! Individual decoders should not hand-roll quoting, numeric flags, path
//! delimiters, or array ids. These tests describe helpers that make invalid or
//! ambiguous `OwnedField` values difficult to construct.

use quamina::{ArrayTrailBuilder, CanonicalField, CanonicalValue, FieldPath, FieldSetBuilder};

#[test]
fn paths_are_segment_sequences_and_do_not_collide_with_separator_characters() {
    let nested = FieldPath::from_segments(["customer", "name"]);
    let literal_newline = FieldPath::from_segments(["customer\nname"]);

    assert_ne!(nested, literal_newline);
    assert_eq!(nested.segments().collect::<Vec<_>>(), ["customer", "name"]);
    assert_eq!(
        literal_newline.segments().collect::<Vec<_>>(),
        ["customer\nname"]
    );
}

#[test]
fn typed_values_centralize_the_matcher_wire_representation() {
    let string = CanonicalField::new(
        FieldPath::from_segments(["value"]),
        CanonicalValue::String("42".into()),
    );
    let number = CanonicalField::new(
        FieldPath::from_segments(["value"]),
        CanonicalValue::Number("42".parse().unwrap()),
    );
    let boolean = CanonicalField::new(
        FieldPath::from_segments(["enabled"]),
        CanonicalValue::Bool(true),
    );

    assert_eq!(string.matcher_bytes(), br#""42""#);
    assert!(!string.is_number());
    assert_eq!(number.matcher_bytes(), b"42");
    assert!(number.is_number());
    assert_eq!(boolean.matcher_bytes(), b"true");
    assert!(!boolean.is_number());
}

#[test]
fn array_ids_are_unique_while_fields_in_one_element_share_positions() {
    let mut trails = ArrayTrailBuilder::new();
    let orders = trails.enter_array();

    trails.set_position(orders, 0);
    let first_sku = trails.snapshot();
    let first_quantity = trails.snapshot();

    trails.set_position(orders, 1);
    let second_sku = trails.snapshot();

    let labels = trails.enter_sibling_array();
    trails.set_position(labels, 0);
    let first_label = trails.snapshot();

    assert_eq!(first_sku, first_quantity);
    assert_ne!(first_sku, second_sku);
    assert_ne!(orders.id(), labels.id());
    assert!(!first_label.conflicts_with(&second_sku));
}

#[test]
fn field_set_builder_filters_with_tracker_and_returns_match_ready_fields() {
    let tracker = quamina::PatternFieldTracker::from_paths([
        FieldPath::from_segments(["status"]),
        FieldPath::from_segments(["customer", "id"]),
    ]);
    let mut fields = FieldSetBuilder::new(&tracker);

    fields.emit(["status"], CanonicalValue::String("active".into()));
    fields.emit(["customer", "id"], CanonicalValue::String("c-1".into()));
    fields.emit(
        ["unreferenced", "secret"],
        CanonicalValue::String("discarded".into()),
    );

    let output = fields.finish();
    assert_eq!(output.len(), 2);
    assert!(output.is_sorted_by_path());
}
