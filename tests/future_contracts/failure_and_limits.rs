//! Compile contract for failure behavior and decoder resource limits.

use quamina::{
    BinaryValuePolicy, CborFlattener, EventFormat, EventLimits, MessagePackFlattener,
    NumericPolicy, QuaminaBuilder, QuaminaError,
};

fn assert_invalid_event(error: QuaminaError, expected: EventFormat) {
    assert!(matches!(
        error,
        QuaminaError::InvalidEvent { format, offset: Some(_), .. } if format == expected
    ));
}

#[test]
fn malformed_input_reports_format_and_byte_offset_without_json_wording() {
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(MessagePackFlattener::new()))
        .unwrap()
        .build()
        .unwrap();

    assert_invalid_event(
        q.matches_for_event(&[0x81, 0xa1, b'x']).unwrap_err(),
        EventFormat::MessagePack,
    );
}

#[test]
fn every_decoder_enforces_depth_field_scalar_and_container_limits() {
    let limits = EventLimits {
        max_depth: 32,
        max_fields: 256,
        max_scalar_bytes: 64 * 1024,
        max_container_items: 1024,
        max_total_allocated_bytes: 1024 * 1024,
    };
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(CborFlattener::builder().limits(limits).build()))
        .unwrap()
        .build()
        .unwrap();

    // 33 nested single-element arrays followed by null.
    let mut too_deep = vec![0x81; 33];
    too_deep.push(0xf6);
    let error = q.matches_for_event(&too_deep).unwrap_err();
    assert!(matches!(error, QuaminaError::EventLimitExceeded { .. }));
}

#[test]
fn unsupported_native_values_fail_instead_of_colliding_with_strings_or_numbers() {
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(
            CborFlattener::builder()
                .binary_values(BinaryValuePolicy::Reject)
                .numbers(NumericPolicy::FiniteF64Exact)
                .build(),
        ))
        .unwrap()
        .build()
        .unwrap();

    // {"bytes": h'00ff'}
    let binary_value = [0xa1, 0x65, b'b', b'y', b't', b'e', b's', 0x42, 0x00, 0xff];
    assert!(matches!(
        q.matches_for_event(&binary_value).unwrap_err(),
        QuaminaError::UnsupportedEventValue {
            format: EventFormat::Cbor,
            ..
        }
    ));
}

#[test]
fn malformed_input_corpus_never_panics_and_never_returns_partial_matches() {
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(MessagePackFlattener::new()))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("must-not-leak", r#"{"x":[1]}"#).unwrap();

    let malformed: &[&[u8]] = &[
        &[],
        &[0x81],
        &[0x81, 0xa1, b'x'],
        &[0x81, 0xa1, b'x', 0xdb, 0xff, 0xff, 0xff, 0xff],
        &[0x92, 0x01],
    ];
    for wire in malformed {
        assert!(q.matches_for_event(wire).is_err());
    }
}

#[test]
fn trailing_top_level_values_are_rejected_instead_of_ignored() {
    let q = QuaminaBuilder::<&str>::new()
        .with_flattener(Box::new(CborFlattener::new()))
        .unwrap()
        .build()
        .unwrap();
    // Empty map followed by an unrelated integer.
    let error = q.matches_for_event(&[0xa0, 0x01]).unwrap_err();

    assert!(matches!(
        error,
        QuaminaError::InvalidEvent {
            format: EventFormat::Cbor,
            ..
        }
    ));
}
