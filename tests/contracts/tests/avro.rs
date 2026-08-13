#![cfg(feature = "avro")]

#[path = "support/cross_format.rs"]
mod cross_format;

use std::panic::{AssertUnwindSafe, catch_unwind};

use quamina::{
    AvroCodecPolicy, AvroFlattener, AvroInput, AvroUnionPolicy, BinaryValuePolicy, EventFormat,
    EventLimits, FingerprintResolver, LogicalTypePolicy, Quamina, QuaminaBuilder, QuaminaError,
};

const SCHEMA: &str = include_str!("../fixtures/avro/corpus.avsc");

#[test]
fn every_schema_applicable_golden_case_matches_json_fields_and_patterns() {
    for case in cross_format::cases()
        .iter()
        .filter(|case| case.name != "escaped_unicode")
    {
        let schema = String::from_utf8(cross_format::fixture("avro", case.name, "avsc"))
            .expect("UTF-8 Avro schema");
        let wire = cross_format::fixture("avro", case.name, "avro");
        let flattener = AvroFlattener::builder(&schema)
            .unwrap()
            .input(AvroInput::RawDatum)
            .build();
        cross_format::assert_matches_json(Box::new(flattener), &wire, case);
    }
}

fn matcher(flattener: AvroFlattener) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap()
}

#[test]
fn writer_schema_is_required_and_reader_schema_is_explicit() {
    assert!(AvroFlattener::builder("").is_err());
    let evolved = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .reader_schema(SCHEMA)
        .unwrap()
        .build();
    assert!(evolved.has_reader_schema());
}

#[test]
fn records_nested_records_maps_arrays_and_array_correlation_use_checked_fixtures() {
    let schema = include_str!("../fixtures/avro/object_arrays_positive.avsc");
    let mut q = matcher(
        AvroFlattener::builder(schema)
            .unwrap()
            .input(AvroInput::RawDatum)
            .build(),
    );
    q.add_pattern("same", r#"{"orders":{"sku":["A"],"quantity":[1]}}"#)
        .unwrap();
    q.add_pattern("cross", r#"{"orders":{"sku":["A"],"quantity":[2]}}"#)
        .unwrap();
    assert_eq!(
        q.matches_for_event(include_bytes!(
            "../fixtures/avro/object_arrays_positive.avro"
        ))
        .unwrap(),
        ["same"]
    );
}

#[test]
fn explicit_union_null_is_present_and_multibranch_ambiguity_is_rejected() {
    let flattener = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .unions(AvroUnionPolicy::ExplicitNullAndRejectAmbiguous)
        .build();
    assert!(flattener.union_null_is_present());
    assert!(matches!(
        flattener.resolve_ambiguous_union(&["long", "double"], b"\x02"),
        Err(QuaminaError::UnsupportedEventValue { .. })
    ));
}

#[test]
fn enum_evolution_aliases_and_defaults_follow_schema_resolution() {
    let flattener = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .reader_schema(SCHEMA)
        .unwrap()
        .build();
    assert!(flattener.enum_values_are_symbolic());
    assert!(flattener.applies_reader_aliases());
    assert!(flattener.applies_reader_defaults());
}

#[test]
fn fixed_bytes_and_decimal_have_noncolliding_canonical_policies() {
    let flattener = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .binary_values(BinaryValuePolicy::TaggedBase64)
        .logical_types(LogicalTypePolicy::CanonicalString)
        .build();
    assert_ne!(
        flattener.canonical_fixed(b"42"),
        flattener.canonical_string("42")
    );
    assert!(
        flattener
            .canonical_decimal(&[0x04, 0xd2], 2)
            .unwrap()
            .is_number()
    );
}

#[test]
fn logical_types_are_deterministic_and_unknown_logical_types_fail() {
    let flattener = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .logical_types(LogicalTypePolicy::CanonicalString)
        .build();
    for logical in [
        "date",
        "time-millis",
        "time-micros",
        "timestamp-millis",
        "timestamp-micros",
        "local-timestamp-millis",
        "duration",
        "uuid",
    ] {
        assert!(
            flattener.logical_type_contract(logical).is_ok(),
            "missing {logical}"
        );
    }
    assert!(matches!(
        flattener.logical_type_contract("invented"),
        Err(QuaminaError::UnsupportedFormatFeature { .. })
    ));
}

#[test]
fn recursive_named_types_are_preprocessed_without_unbounded_recursion() {
    let flattener = AvroFlattener::builder(SCHEMA)
        .unwrap()
        .limits(EventLimits::strict())
        .build();
    assert!(flattener.schema_graph().recursive_names_are_resolved());
}

#[test]
fn positive_and_negative_array_map_blocks_are_supported_and_malformed_blocks_fail() {
    let flattener = AvroFlattener::builder(SCHEMA).unwrap().build();
    assert!(flattener.supports_positive_and_negative_blocks());
    for wire in [&[0x03][..], &[0x01, 0xff][..], &[0x04, 0x7f][..]] {
        let error = flattener.validate_datum(wire).unwrap_err();
        assert_eq!(error.format(), EventFormat::Avro);
        assert!(error.location().byte_offset().is_some());
    }
}

#[test]
fn raw_container_and_single_object_inputs_and_codecs_are_named_policies() {
    for input in [
        AvroInput::RawDatum,
        AvroInput::ObjectContainerFile,
        AvroInput::SingleObject,
    ] {
        let flattener = AvroFlattener::builder(SCHEMA)
            .unwrap()
            .input(input)
            .codecs(AvroCodecPolicy::NullOnly)
            .build();
        assert_eq!(flattener.input(), input);
    }
    assert!(matches!(
        AvroCodecPolicy::NullOnly.validate("snappy"),
        Err(QuaminaError::UnsupportedFormatFeature { .. })
    ));
}

#[test]
fn single_object_fingerprints_require_explicit_schema_resolution() {
    let empty = FingerprintResolver::new();
    let flattener = AvroFlattener::single_object(empty);
    let unknown = [0xc3, 0x01, 0, 1, 2, 3, 4, 5, 6, 7];
    assert!(matches!(
        flattener.validate_datum(&unknown),
        Err(QuaminaError::MissingEventSchema { .. })
    ));
}

#[test]
fn truncation_trailing_data_allocation_limits_and_malformed_corpus_never_panic() {
    let mut q = matcher(
        AvroFlattener::builder(SCHEMA)
            .unwrap()
            .limits(EventLimits::strict())
            .build(),
    );
    q.add_pattern("leak", r#"{"count":[1]}"#).unwrap();
    for wire in [
        &[][..],
        &[0x80][..],
        &[0xff, 0xff, 0xff, 0xff, 0x7f][..],
        &[0x00, 0x00][..],
    ] {
        let outcome = catch_unwind(AssertUnwindSafe(|| q.matches_for_event(wire)));
        assert!(outcome.is_ok());
        assert!(outcome.unwrap().is_err());
    }
}
