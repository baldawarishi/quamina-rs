#![cfg(feature = "cbor")]

#[path = "support/cross_format.rs"]
mod cross_format;

use std::panic::{AssertUnwindSafe, catch_unwind};

use quamina::{
    BinaryValuePolicy, CborFlattener, CborSimpleValuePolicy, CborTagPolicy, DuplicateKeyPolicy,
    EventFormat, EventLimits, MapKeyPolicy, NonCanonicalPolicy, NumericPolicy, Quamina,
    QuaminaBuilder, QuaminaError, RootValuePolicy, SharedReferencePolicy,
};

#[test]
fn every_shared_golden_case_matches_json_fields_and_patterns() {
    for case in cross_format::cases() {
        let wire = cross_format::fixture("cbor", case.name, "cbor");
        cross_format::assert_matches_json(Box::new(CborFlattener::new()), &wire, case);
    }
}

fn matcher(flattener: CborFlattener) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap()
}

fn assert_cbor_error(error: &QuaminaError) {
    assert_eq!(error.format(), EventFormat::Cbor);
}

#[test]
fn definite_and_indefinite_maps_and_arrays_flatten_identically() {
    let mut q = matcher(CborFlattener::new());
    q.add_pattern("match", r#"{"items":[1,2]}"#).unwrap();
    let definite = [0xa1, 0x65, b'i', b't', b'e', b'm', b's', 0x82, 0x01, 0x02];
    let indefinite = [
        0xbf, 0x65, b'i', b't', b'e', b'm', b's', 0x9f, 0x01, 0x02, 0xff, 0xff,
    ];

    assert_eq!(q.matches_for_event(&definite).unwrap(), ["match"]);
    assert_eq!(q.matches_for_event(&indefinite).unwrap(), ["match"]);
}

#[test]
fn chunked_text_is_joined_but_byte_strings_follow_binary_policy() {
    let mut q = matcher(
        CborFlattener::builder()
            .binary_values(BinaryValuePolicy::TaggedBase64)
            .build(),
    );
    q.add_pattern("text", r#"{"text":["hello"]}"#).unwrap();
    q.add_pattern("bytes", r#"{"bytes":["base64:AP8="]}"#)
        .unwrap();
    let text = [
        0xa1, 0x64, b't', b'e', b'x', b't', 0x7f, 0x62, b'h', b'e', 0x63, b'l', b'l', b'o', 0xff,
    ];
    let bytes = [
        0xa1, 0x65, b'b', b'y', b't', b'e', b's', 0x5f, 0x41, 0x00, 0x41, 0xff, 0xff,
    ];

    assert_eq!(q.matches_for_event(&text).unwrap(), ["text"]);
    assert_eq!(q.matches_for_event(&bytes).unwrap(), ["bytes"]);
}

#[test]
fn integers_negative_integers_and_half_f32_f64_are_numeric() {
    let mut q = matcher(CborFlattener::new());
    q.add_pattern(
        "numbers",
        r#"{"positive":[42],"negative":[-42],"half":[1.5],"single":[42],"double":[42]}"#,
    )
    .unwrap();
    let wire = [
        0xa5, 0x68, b'p', b'o', b's', b'i', b't', b'i', b'v', b'e', 0x18, 0x2a, 0x68, b'n', b'e',
        b'g', b'a', b't', b'i', b'v', b'e', 0x38, 0x29, 0x64, b'h', b'a', b'l', b'f', 0xf9, 0x3e,
        0x00, 0x66, b's', b'i', b'n', b'g', b'l', b'e', 0xfa, 0x42, 0x28, 0x00, 0x00, 0x66, b'd',
        b'o', b'u', b'b', b'l', b'e', 0xfb, 0x40, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    assert_eq!(q.matches_for_event(&wire).unwrap(), ["numbers"]);
}

#[test]
fn checked_shared_scalar_fixture_matches_the_json_baseline() {
    let mut q = matcher(CborFlattener::new());
    q.add_pattern(
        "scalars",
        r#"{"text":["42"],"count":[42],"enabled":[true],"deleted_at":[null]}"#,
    )
    .unwrap();

    assert_eq!(
        q.matches_for_event(include_bytes!("../fixtures/cbor/scalars.cbor"))
            .unwrap(),
        ["scalars"]
    );
}

#[test]
fn nan_infinity_and_inexact_numbers_are_rejected() {
    let q = matcher(
        CborFlattener::builder()
            .numbers(NumericPolicy::LosslessQuamina)
            .build(),
    );
    let cases: &[&[u8]] = &[
        &[0xa1, 0x61, b'n', 0xf9, 0x7e, 0x00],
        &[0xa1, 0x61, b'n', 0xf9, 0x7c, 0x00],
        &[0xa1, 0x61, b'n', 0xf9, 0xfc, 0x00],
    ];

    for wire in cases {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::UnsupportedEventValue { .. }));
        assert_cbor_error(&error);
    }
}

#[test]
fn text_keys_are_required_and_duplicate_keys_are_rejected() {
    let q = matcher(
        CborFlattener::builder()
            .map_keys(MapKeyPolicy::TextOnly)
            .duplicate_keys(DuplicateKeyPolicy::Reject)
            .build(),
    );
    let non_text = [0xa1, 0x01, 0x61, b'x'];
    let duplicate = [0xa2, 0x61, b'x', 0x01, 0x61, b'x', 0x02];

    assert!(matches!(
        q.matches_for_event(&non_text).unwrap_err(),
        QuaminaError::UnsupportedMapKey { .. }
    ));
    assert!(matches!(
        q.matches_for_event(&duplicate).unwrap_err(),
        QuaminaError::DuplicateEventField { .. }
    ));
}

#[test]
fn undefined_and_unassigned_simple_values_are_explicitly_unsupported() {
    let q = matcher(
        CborFlattener::builder()
            .simple_values(CborSimpleValuePolicy::RejectUnsupported)
            .build(),
    );
    for wire in [
        &[0xa1, 0x61, b'x', 0xf7][..],
        &[0xa1, 0x61, b'x', 0xf8, 0x20][..],
    ] {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::UnsupportedEventValue { .. }));
        assert_cbor_error(&error);
    }
}

#[test]
fn known_datetime_tags_have_canonical_values_and_unknown_tags_fail() {
    let mut q = matcher(
        CborFlattener::builder()
            .tags(CborTagPolicy::KnownSemanticValues)
            .build(),
    );
    q.add_pattern("date", r#"{"at":["1970-01-01T00:00:01Z"]}"#)
        .unwrap();
    let epoch_seconds = [0xa1, 0x62, b'a', b't', 0xc1, 0x01];
    assert_eq!(q.matches_for_event(&epoch_seconds).unwrap(), ["date"]);

    let unknown = [0xa1, 0x61, b'x', 0xd9, 0x03, 0xe7, 0x01];
    assert!(matches!(
        q.matches_for_event(&unknown).unwrap_err(),
        QuaminaError::UnsupportedFormatFeature { .. }
    ));
}

#[test]
fn bignum_decimal_fraction_and_bigfloat_tags_preserve_numeric_semantics_or_fail() {
    let q = matcher(
        CborFlattener::builder()
            .tags(CborTagPolicy::KnownSemanticValues)
            .numbers(NumericPolicy::LosslessQuamina)
            .build(),
    );
    let cases: &[&[u8]] = &[
        &[
            0xa1, 0x61, b'n', 0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ],
        &[0xa1, 0x61, b'n', 0xc4, 0x82, 0x21, 0x19, 0x04, 0xd2],
        &[0xa1, 0x61, b'n', 0xc5, 0x82, 0x20, 0x03],
    ];

    for wire in cases {
        let result = q.matches_for_event(wire);
        assert!(
            result.is_ok() || matches!(result, Err(QuaminaError::UnsupportedEventValue { .. })),
            "tagged number must be lossless or explicitly rejected"
        );
    }
}

#[test]
fn shared_reference_and_cycle_related_tags_are_rejected() {
    let q = matcher(
        CborFlattener::builder()
            .shared_references(SharedReferencePolicy::Reject)
            .build(),
    );
    let shareable = [0xa1, 0x61, b'x', 0xd8, 0x1c, 0x81, 0x01];
    let reference = [0xa1, 0x61, b'x', 0xd8, 0x1d, 0x00];

    for wire in [&shareable[..], &reference[..]] {
        assert!(matches!(
            q.matches_for_event(wire).unwrap_err(),
            QuaminaError::UnsupportedFormatFeature { .. }
        ));
    }
}

#[test]
fn noncanonical_encodings_follow_an_explicit_policy() {
    let reject = matcher(
        CborFlattener::builder()
            .noncanonical(NonCanonicalPolicy::Reject)
            .build(),
    );
    let non_shortest_integer = [0xa1, 0x61, b'x', 0x18, 0x01];
    let error = reject.matches_for_event(&non_shortest_integer).unwrap_err();

    assert!(matches!(error, QuaminaError::InvalidEvent { .. }));
    assert_cbor_error(&error);
}

#[test]
fn root_scalar_and_array_trailing_values_and_truncation_are_rejected_with_locations() {
    let q = matcher(
        CborFlattener::builder()
            .root_values(RootValuePolicy::MapOnly)
            .build(),
    );
    let cases: &[&[u8]] = &[&[0x01], &[0x81, 0x01], &[0xa0, 0x01], &[0xa1, 0x61]];

    for wire in cases {
        let error = q.matches_for_event(wire).unwrap_err();
        assert_cbor_error(&error);
        assert!(error.location().byte_offset().is_some());
    }
}

#[test]
fn all_resource_limits_cover_indefinite_and_declared_size_attacks() {
    let limits = EventLimits {
        max_depth: 2,
        max_fields: 2,
        max_path_bytes: 16,
        max_scalar_bytes: 4,
        max_container_items: 2,
        max_total_allocated_bytes: 32,
    };
    let q = matcher(CborFlattener::builder().limits(limits).build());
    let cases: &[&[u8]] = &[
        &[0xa1, 0x61, b'x', 0x81, 0x81, 0x81, 0xf6],
        &[
            0xbf, 0x61, b'a', 0x01, 0x61, b'b', 0x02, 0x61, b'c', 0x03, 0xff,
        ],
        &[0xa1, 0x61, b'x', 0x65, b'1', b'2', b'3', b'4', b'5'],
        &[0xa1, 0x61, b'x', 0x9a, 0xff, 0xff, 0xff, 0xff],
    ];

    for wire in cases {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::EventLimitExceeded { .. }));
        assert_cbor_error(&error);
    }
}

#[test]
fn malformed_corpus_never_panics_or_releases_partial_matches() {
    let mut q = matcher(CborFlattener::new());
    q.add_pattern("must-not-leak", r#"{"x":[1]}"#).unwrap();
    let corpus: &[&[u8]] = &[
        &[],
        &[0xa1],
        &[0xa1, 0x61, b'x'],
        &[0xa1, 0x61, b'x', 0x7a, 0xff, 0xff, 0xff, 0xff],
        &[0xbf, 0x61, b'x', 0x01],
        &[0xff],
    ];

    for wire in corpus {
        let outcome = catch_unwind(AssertUnwindSafe(|| q.matches_for_event(wire)));
        assert!(outcome.is_ok(), "CBOR decoder panicked for {wire:02x?}");
        assert!(
            outcome.unwrap().is_err(),
            "partial match escaped for {wire:02x?}"
        );
    }
}

#[test]
fn tracker_filtering_does_not_bypass_structural_validation() {
    let mut q = matcher(
        CborFlattener::builder()
            .limits(EventLimits::strict())
            .build(),
    );
    q.add_pattern("safe", r#"{"safe":[true]}"#).unwrap();
    let ignored_indefinite_without_break = [
        0xbf, 0x64, b's', b'a', b'f', b'e', 0xf5, 0x67, b'i', b'g', b'n', b'o', b'r', b'e', b'd',
        0x9f, 0x01, 0x02,
    ];

    assert!(
        q.matches_for_event(&ignored_indefinite_without_break)
            .is_err()
    );
}
