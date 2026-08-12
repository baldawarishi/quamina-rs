//! Compile contract for first-class MessagePack and CBOR flatteners.
//!
//! This file intentionally references APIs that have not been implemented.

use quamina::{
    BinaryValuePolicy, CborFlattener, DuplicateKeyPolicy, EventFormat, EventLimits, MapKeyPolicy,
    MessagePackFlattener, NumericPolicy, QuaminaBuilder, QuaminaError, TaggedValuePolicy,
};

const MESSAGEPACK_EVENT: &[u8] = &[
    0x82, // map(2)
    0xa6, b's', b't', b'a', b't', b'u', b's', 0xa6, b'a', b'c', b't', b'i', b'v', b'e', 0xa5, b'c',
    b'o', b'u', b'n', b't', 0x2a,
];

const CBOR_EVENT: &[u8] = &[
    0xa2, // map(2)
    0x66, b's', b't', b'a', b't', b'u', b's', 0x66, b'a', b'c', b't', b'i', b'v', b'e', 0x65, b'c',
    b'o', b'u', b'n', b't', 0x18, 0x2a,
];

#[test]
fn messagepack_has_a_zero_schema_constructor_and_matches_canonical_scalars() {
    let flattener = MessagePackFlattener::builder()
        .map_keys(MapKeyPolicy::TextOnly)
        .binary_values(BinaryValuePolicy::Reject)
        .limits(EventLimits::default())
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("match", r#"{"status":["active"],"count":[42]}"#)
        .unwrap();

    assert_eq!(q.matches_for_event(MESSAGEPACK_EVENT).unwrap(), ["match"]);
}

#[test]
fn cbor_makes_tag_and_byte_string_policies_explicit() {
    let flattener = CborFlattener::builder()
        .map_keys(MapKeyPolicy::TextOnly)
        .binary_values(BinaryValuePolicy::Reject)
        .tagged_values(TaggedValuePolicy::RejectUnknown)
        .limits(EventLimits::default())
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("match", r#"{"status":["active"],"count":[42]}"#)
        .unwrap();

    assert_eq!(q.matches_for_event(CBOR_EVENT).unwrap(), ["match"]);
}

#[test]
fn document_formats_reject_non_map_roots_with_a_format_neutral_error() {
    let cases: &[(EventFormat, Box<dyn quamina::Flattener>, &[u8])] = &[
        (
            EventFormat::MessagePack,
            Box::new(MessagePackFlattener::new()),
            &[0x92, 0x01, 0x02],
        ),
        (
            EventFormat::Cbor,
            Box::new(CborFlattener::new()),
            &[0x82, 0x01, 0x02],
        ),
    ];

    for (expected_format, flattener, wire) in cases {
        let q = QuaminaBuilder::<&str>::new()
            .with_flattener(flattener.copy())
            .unwrap()
            .build()
            .unwrap();
        let error = q.matches_for_event(wire).unwrap_err();
        assert!(matches!(
            error,
            QuaminaError::InvalidEvent { format, .. } if format == *expected_format
        ));
    }
}

#[test]
fn binary_values_can_be_rejected_or_encoded_without_type_collisions() {
    // CBOR {"blob": h'00ff'}.
    let wire = [0xa1, 0x64, b'b', b'l', b'o', b'b', 0x42, 0x00, 0xff];
    let flattener = CborFlattener::builder()
        .binary_values(BinaryValuePolicy::StandardBase64)
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("binary", r#"{"blob":["AP8="]}"#).unwrap();

    assert_eq!(q.matches_for_event(&wire).unwrap(), ["binary"]);
}

#[test]
fn integers_outside_lossless_json_range_follow_the_configured_numeric_policy() {
    // MessagePack {"value": 18446744073709551615}.
    let wire = [
        0x81, 0xa5, b'v', b'a', b'l', b'u', b'e', 0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff,
    ];
    let flattener = MessagePackFlattener::builder()
        .numbers(NumericPolicy::LosslessJsonNumber)
        .build();
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    assert!(matches!(
        q.matches_for_event(&wire).unwrap_err(),
        QuaminaError::UnsupportedEventValue {
            format: EventFormat::MessagePack,
            ..
        }
    ));
}

#[test]
fn duplicate_and_non_text_map_keys_are_never_silently_collapsed() {
    let flattener = CborFlattener::builder()
        .map_keys(MapKeyPolicy::TextOnly)
        .duplicate_keys(DuplicateKeyPolicy::Reject)
        .build();
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    // {1: "numeric key"}
    let non_text_key = [
        0xa1, 0x01, 0x6b, b'n', b'u', b'm', b'e', b'r', b'i', b'c', b' ', b'k', b'e', b'y',
    ];
    assert!(matches!(
        q.matches_for_event(&non_text_key).unwrap_err(),
        QuaminaError::UnsupportedMapKey {
            format: EventFormat::Cbor,
            ..
        }
    ));
}
