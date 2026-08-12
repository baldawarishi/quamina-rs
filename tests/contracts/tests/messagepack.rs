#![cfg(feature = "messagepack")]

#[path = "support/cross_format.rs"]
mod cross_format;

use std::panic::{AssertUnwindSafe, catch_unwind};

use quamina::{
    BinaryValuePolicy, DuplicateKeyPolicy, EventFormat, EventLimits, ExtensionValuePolicy,
    MapKeyPolicy, MessagePackFlattener, MessagePackTimestampPolicy, NumericPolicy, Quamina,
    QuaminaBuilder, QuaminaError, RootValuePolicy,
};

#[test]
fn every_shared_golden_case_matches_json_fields_and_patterns() {
    for case in cross_format::cases() {
        let wire = cross_format::fixture("messagepack", case.name, "msgpack");
        cross_format::assert_matches_json(Box::new(MessagePackFlattener::new()), &wire, case);
    }
}

fn matcher(flattener: MessagePackFlattener) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap()
}

fn assert_format_error(error: &QuaminaError) {
    assert_eq!(error.format(), EventFormat::MessagePack);
}

#[test]
fn nil_bool_signed_unsigned_and_strings_use_json_scalar_semantics() {
    let mut q = matcher(MessagePackFlattener::new());
    q.add_pattern(
        "scalars",
        r#"{"nil":[null],"yes":[true],"negative":[-32],"positive":[127],"text":["42"]}"#,
    )
    .unwrap();
    q.add_pattern("wrong-type", r#"{"text":[42]}"#).unwrap();
    let wire = include_bytes!("../fixtures/messagepack/scalars.msgpack");

    assert_eq!(q.matches_for_event(wire).unwrap(), ["scalars"]);
}

#[test]
fn integer_boundaries_reject_values_outside_lossless_quamina_numbers() {
    let q = matcher(
        MessagePackFlattener::builder()
            .numbers(NumericPolicy::LosslessQuamina)
            .build(),
    );
    let unsigned_max = [
        0x81, 0xa1, b'n', 0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    ];
    let error = q.matches_for_event(&unsigned_max).unwrap_err();

    assert!(matches!(error, QuaminaError::UnsupportedEventValue { .. }));
    assert_format_error(&error);
}

#[test]
fn finite_f32_and_f64_share_numeric_equivalence() {
    let mut q = matcher(MessagePackFlattener::new());
    q.add_pattern("forty-two", r#"{"f32":[42],"f64":[42]}"#)
        .unwrap();
    let wire = [
        0x82, 0xa3, b'f', b'3', b'2', 0xca, 0x42, 0x28, 0x00, 0x00, 0xa3, b'f', b'6', b'4', 0xcb,
        0x40, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    assert_eq!(q.matches_for_event(&wire).unwrap(), ["forty-two"]);
}

#[test]
fn nan_and_infinities_are_rejected_by_default() {
    let q = matcher(MessagePackFlattener::new());
    let cases: &[&[u8]] = &[
        &[0x81, 0xa1, b'n', 0xca, 0x7f, 0xc0, 0x00, 0x00],
        &[
            0x81, 0xa1, b'n', 0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ],
        &[
            0x81, 0xa1, b'n', 0xcb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ],
    ];

    for wire in cases {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::UnsupportedEventValue { .. }));
        assert_format_error(&error);
    }
}

#[test]
fn binary_values_are_rejected_or_explicitly_encoded_without_string_collision() {
    let wire = [0x81, 0xa4, b'b', b'l', b'o', b'b', 0xc4, 0x02, 0x00, 0xff];
    let reject = matcher(MessagePackFlattener::new());
    assert!(matches!(
        reject.matches_for_event(&wire).unwrap_err(),
        QuaminaError::UnsupportedEventValue { .. }
    ));

    let mut encode = matcher(
        MessagePackFlattener::builder()
            .binary_values(BinaryValuePolicy::TaggedBase64)
            .build(),
    );
    encode
        .add_pattern("bytes", r#"{"blob":["base64:AP8="]}"#)
        .unwrap();
    assert_eq!(encode.matches_for_event(&wire).unwrap(), ["bytes"]);
}

#[test]
fn arrays_of_maps_preserve_same_element_correlation() {
    let mut q = matcher(MessagePackFlattener::new());
    q.add_pattern("same", r#"{"orders":{"sku":["A"],"quantity":[1]}}"#)
        .unwrap();
    q.add_pattern("cross", r#"{"orders":{"sku":["A"],"quantity":[2]}}"#)
        .unwrap();
    let wire = include_bytes!("../fixtures/messagepack/object_arrays_positive.msgpack");

    assert_eq!(q.matches_for_event(wire).unwrap(), ["same"]);
}

#[test]
fn non_string_and_duplicate_map_keys_are_never_collapsed() {
    let q = matcher(
        MessagePackFlattener::builder()
            .map_keys(MapKeyPolicy::TextOnly)
            .duplicate_keys(DuplicateKeyPolicy::Reject)
            .build(),
    );
    let non_string = [0x81, 0x01, 0xa1, b'x'];
    let duplicate = [0x82, 0xa1, b'x', 0x01, 0xa1, b'x', 0x02];

    assert!(matches!(
        q.matches_for_event(&non_string).unwrap_err(),
        QuaminaError::UnsupportedMapKey { .. }
    ));
    assert!(matches!(
        q.matches_for_event(&duplicate).unwrap_err(),
        QuaminaError::DuplicateEventField { .. }
    ));
}

#[test]
fn extension_values_and_timestamp_extension_have_separate_policies() {
    let reject = matcher(
        MessagePackFlattener::builder()
            .extensions(ExtensionValuePolicy::RejectUnknown)
            .timestamps(MessagePackTimestampPolicy::CanonicalRfc3339)
            .build(),
    );
    let unknown_fixext = [0x81, 0xa1, b'x', 0xd4, 0x2a, 0x00];
    assert!(matches!(
        reject.matches_for_event(&unknown_fixext).unwrap_err(),
        QuaminaError::UnsupportedFormatFeature { .. }
    ));

    let mut timestamps = matcher(
        MessagePackFlattener::builder()
            .timestamps(MessagePackTimestampPolicy::CanonicalRfc3339)
            .build(),
    );
    timestamps
        .add_pattern("epoch", r#"{"at":["1970-01-01T00:00:01Z"]}"#)
        .unwrap();
    let timestamp32 = [0x81, 0xa2, b'a', b't', 0xd6, 0xff, 0x00, 0x00, 0x00, 0x01];
    assert_eq!(
        timestamps.matches_for_event(&timestamp32).unwrap(),
        ["epoch"]
    );
}

#[test]
fn length_truncation_invalid_markers_and_trailing_values_report_offsets() {
    let q = matcher(MessagePackFlattener::new());
    let malformed: &[&[u8]] = &[
        &[0x81, 0xa1, b'x'],
        &[0x81, 0xa1, b'x', 0xdb, 0x00, 0x00, 0x00, 0x10, b'a'],
        &[0x81, 0xa1, b'x', 0xc1],
        &[0x80, 0x01],
    ];

    for wire in malformed {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::InvalidEvent { .. }));
        assert_format_error(&error);
        assert!(error.location().byte_offset().is_some());
    }
}

#[test]
fn root_scalar_and_array_policy_is_explicit() {
    let q = matcher(
        MessagePackFlattener::builder()
            .root_values(RootValuePolicy::MapOnly)
            .build(),
    );

    for wire in [&[0x2a][..], &[0x91, 0x2a][..]] {
        assert!(matches!(
            q.matches_for_event(wire).unwrap_err(),
            QuaminaError::UnsupportedEventValue { .. }
        ));
    }
}

#[test]
fn depth_container_field_scalar_and_total_allocation_limits_are_enforced() {
    let limits = EventLimits {
        max_depth: 2,
        max_fields: 2,
        max_path_bytes: 32,
        max_scalar_bytes: 4,
        max_container_items: 2,
        max_total_allocated_bytes: 32,
    };
    let q = matcher(MessagePackFlattener::builder().limits(limits).build());
    let cases: &[&[u8]] = &[
        &[0x81, 0xa1, b'x', 0x91, 0x91, 0x91, 0xc0],
        &[0x83, 0xa1, b'a', 0x01, 0xa1, b'b', 0x02, 0xa1, b'c', 0x03],
        &[0x81, 0xa1, b'x', 0xa5, b'1', b'2', b'3', b'4', b'5'],
        &[0x81, 0xa1, b'x', 0xdd, 0xff, 0xff, 0xff, 0xff],
    ];

    for wire in cases {
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(error, QuaminaError::EventLimitExceeded { .. }));
        assert_format_error(&error);
    }
}

#[test]
fn malformed_corpus_never_panics_or_returns_partial_matches() {
    let mut q = matcher(MessagePackFlattener::new());
    q.add_pattern("must-not-leak", r#"{"x":[1]}"#).unwrap();
    let corpus: &[&[u8]] = &[
        &[],
        &[0x81],
        &[0x81, 0xa1, b'x'],
        &[0x81, 0xa1, b'x', 0xc5, 0xff, 0xff],
        &[0x81, 0xa1, b'x', 0x01, 0xc1],
    ];

    for wire in corpus {
        let outcome = catch_unwind(AssertUnwindSafe(|| q.matches_for_event(wire)));
        assert!(
            outcome.is_ok(),
            "MessagePack decoder panicked for {wire:02x?}"
        );
        assert!(
            outcome.unwrap().is_err(),
            "partial match escaped for {wire:02x?}"
        );
    }
}

#[test]
fn tracker_skipping_does_not_skip_declared_size_or_structure_validation() {
    let mut q = matcher(
        MessagePackFlattener::builder()
            .limits(EventLimits::strict())
            .build(),
    );
    q.add_pattern("only-safe", r#"{"safe":[true]}"#).unwrap();
    let unreferenced_allocation_attack = [
        0x82, 0xa4, b's', b'a', b'f', b'e', 0xc3, 0xa7, b'i', b'g', b'n', b'o', b'r', b'e', b'd',
        0xdb, 0xff, 0xff, 0xff, 0xff,
    ];

    assert!(matches!(
        q.matches_for_event(&unreferenced_allocation_attack)
            .unwrap_err(),
        QuaminaError::EventLimitExceeded { .. }
    ));
}
