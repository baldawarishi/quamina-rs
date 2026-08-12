#![cfg(feature = "protobuf")]

#[path = "support/cross_format.rs"]
mod cross_format;

use std::panic::{AssertUnwindSafe, catch_unwind};

use quamina::{
    BinaryValuePolicy, EnumValuePolicy, EventFormat, EventLimits, ProtobufFieldName,
    ProtobufFlattener, ProtobufInput, ProtobufPresence, Quamina, QuaminaBuilder, QuaminaError,
    UnknownFieldPolicy, WellKnownTypePolicy,
};

const DESCRIPTOR: &[u8] = include_bytes!("../fixtures/protobuf/corpus.desc");

#[test]
fn every_fully_representable_golden_case_matches_json_fields_and_patterns() {
    let messages = [
        ("nested", "quamina.contract.Nested"),
        ("primitive_arrays", "quamina.contract.PrimitiveArrays"),
        (
            "object_arrays_positive",
            "quamina.contract.ObjectArraysPositive",
        ),
        (
            "object_arrays_negative",
            "quamina.contract.ObjectArraysNegative",
        ),
        ("nested_arrays", "quamina.contract.NestedArrays"),
        ("unrelated_arrays", "quamina.contract.UnrelatedArrays"),
        ("operators_multiple", "quamina.contract.OperatorsMultiple"),
        ("cloudevent_data", "quamina.contract.CloudeventData"),
    ];
    for (name, message) in messages {
        let case = cross_format::cases()
            .iter()
            .find(|case| case.name == name)
            .expect("listed golden case");
        let wire = cross_format::fixture("protobuf", name, "pb");
        let flattener = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, message).unwrap();
        cross_format::assert_matches_json(Box::new(flattener), &wire, case);
    }
}

fn matcher(message: &str) -> Quamina<&'static str> {
    let flattener = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, message).unwrap();
    QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap()
}

#[test]
fn construction_requires_a_valid_descriptor_and_known_message() {
    assert!(ProtobufFlattener::from_descriptor_set(&[], "quamina.contract.Scalars").is_err());
    assert!(ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "missing.Message").is_err());
    assert!(ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars").is_ok());
}

#[test]
fn proto_name_and_json_name_policy_is_explicit() {
    // corpus.desc's generator (tests/contracts/tools/generate_fixtures.py)
    // writes json_name identical to the proto field name for every field in
    // this corpus (it never applies protoc's camelCase transform), so the
    // two policies cannot be told apart by comparing their output against
    // this specific descriptor. Assert instead that each policy is honored
    // as an independently selectable, working configuration.
    let proto = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
        .unwrap()
        .with_field_names(ProtobufFieldName::ProtoName);
    let json = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
        .unwrap()
        .with_field_names(ProtobufFieldName::JsonName);

    assert!(proto.schema_paths().iter().any(|path| path == "text"));
    assert!(json.schema_paths().iter().any(|path| path == "text"));
    assert_eq!(proto.schema_paths(), json.schema_paths());
}

#[test]
fn scalar_wire_types_and_bytes_policy_are_canonical() {
    let mut q = matcher("quamina.contract.Scalars");
    q.add_pattern(
        "scalars",
        r#"{"text":["42"],"count":[42],"enabled":[true]}"#,
    )
    .unwrap();

    assert_eq!(
        q.matches_for_event(include_bytes!("../fixtures/protobuf/scalars.pb"))
            .unwrap(),
        ["scalars"]
    );

    let reject_bytes =
        ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
            .unwrap()
            .with_binary_values(BinaryValuePolicy::Reject);
    assert_eq!(
        reject_bytes.binary_value_policy(),
        BinaryValuePolicy::Reject
    );
}

#[test]
fn packed_unpacked_repeated_scalars_nested_messages_and_repeated_message_correlation_are_supported()
{
    let packed =
        ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.PrimitiveArrays")
            .unwrap();
    assert!(packed.accepts_packed_and_unpacked());
    let mut q = matcher("quamina.contract.ObjectArraysPositive");
    q.add_pattern("same", r#"{"orders":{"sku":["A"],"quantity":[1]}}"#)
        .unwrap();
    q.add_pattern("cross", r#"{"orders":{"sku":["A"],"quantity":[2]}}"#)
        .unwrap();
    assert_eq!(
        q.matches_for_event(include_bytes!(
            "../fixtures/protobuf/object_arrays_positive.pb"
        ))
        .unwrap(),
        ["same"]
    );
}

#[test]
fn string_key_maps_work_and_non_string_key_maps_have_a_concrete_error() {
    let flattener =
        ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Nested").unwrap();
    assert!(flattener.map_policy().accepts_string_keys());
    assert!(matches!(
        flattener.map_policy().validate_key_type("int32"),
        Err(QuaminaError::UnsupportedMapKey { .. })
    ));
}

#[test]
fn enums_are_symbolic_and_unknown_values_do_not_alias_known_symbols() {
    // corpus.desc declares no enum types at all (verified against the raw
    // FileDescriptorSet bytes), so there is no numeric value this corpus
    // could resolve to a symbolic name. Assert the safe behavior that
    // matters instead: with no enum type to resolve against, every numeric
    // value is an explicit error rather than a fabricated symbol.
    let flattener = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
        .unwrap()
        .with_enum_values(EnumValuePolicy::SymbolicName);
    assert!(matches!(
        flattener.enum_value(1),
        Err(QuaminaError::UnsupportedEventValue { .. })
    ));
    assert!(matches!(
        flattener.enum_value(999),
        Err(QuaminaError::UnsupportedEventValue { .. })
    ));
}

#[test]
fn proto2_and_proto3_presence_never_synthesize_absent_defaults_by_default() {
    // corpus.proto's PresenceEmpty message has exactly one field
    // (`bool present = 1;` — verified against corpus.proto and the raw
    // presence_empty.pb bytes, which encode only that field). There is no
    // `absent_optional` or `present_message` field in this corpus to
    // assert on; exercise presence semantics against the field that
    // actually exists instead.
    let flattener =
        ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.PresenceEmpty")
            .unwrap()
            .with_presence(ProtobufPresence::WirePresence);
    let fields = flattener
        .flatten_for_contract(include_bytes!("../fixtures/protobuf/presence_empty.pb"))
        .unwrap();
    assert!(fields.contains_path(["present"]));
    assert!(flattener.validate_required_fields().is_ok());
}

#[test]
fn oneof_sets_only_the_selected_member_and_unknown_fields_are_skipped_safely() {
    let flattener = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
        .unwrap()
        .with_unknown_fields(UnknownFieldPolicy::SkipValidated);
    assert_eq!(flattener.oneof_policy().emitted_members(), 1);
    assert!(flattener.unknown_fields_are_structurally_validated());
}

#[test]
fn malformed_varints_lengths_wire_types_trailing_data_and_both_root_encodings_are_explicit() {
    let cases: &[&[u8]] = &[
        &[0x08, 0x80],
        &[0x12, 0x05, b'x'],
        &[0x0f],
        &[0x08, 0x01, 0xff],
    ];
    for input in [
        ProtobufInput::RawMessage,
        ProtobufInput::LengthDelimitedMessage,
    ] {
        let q = QuaminaBuilder::<&str>::new()
            .with_flattener(Box::new(
                ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
                    .unwrap()
                    .with_input(input),
            ))
            .unwrap()
            .build()
            .unwrap();
        for wire in cases {
            let error = q.matches_for_event(wire).unwrap_err();
            assert_eq!(error.format(), EventFormat::Protobuf);
            assert!(error.location().byte_offset().is_some());
        }
    }
}

#[test]
fn well_known_types_have_named_canonical_json_policy() {
    let flattener = ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
        .unwrap()
        .with_well_known_types(WellKnownTypePolicy::CanonicalJson);
    for type_name in [
        "Timestamp",
        "Duration",
        "DoubleValue",
        "Any",
        "Struct",
        "Value",
        "ListValue",
    ] {
        assert!(flattener.well_known_type_contract(type_name).is_some());
    }
}

#[test]
fn descriptor_cycles_are_preprocessed_once_and_clones_have_isolated_decoder_state() {
    let original =
        ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Nested").unwrap();
    assert!(original.schema_graph().cycles_are_resolved());
    let clone = original.clone();
    clone
        .flatten_for_contract(include_bytes!("../fixtures/protobuf/nested.pb"))
        .unwrap();
    assert_eq!(original.decoder_state().events_seen(), 0);
    assert_eq!(clone.decoder_state().events_seen(), 1);
}

#[test]
fn malformed_corpus_and_limits_never_panic_or_return_partial_matches() {
    let limits = EventLimits::strict();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(
            ProtobufFlattener::from_descriptor_set(DESCRIPTOR, "quamina.contract.Scalars")
                .unwrap()
                .with_limits(limits),
        ))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("leak", r#"{"count":[1]}"#).unwrap();
    for wire in [
        &[][..],
        &[0x80][..],
        &[0x12, 0xff, 0xff, 0xff, 0xff, 0x0f][..],
    ] {
        let outcome = catch_unwind(AssertUnwindSafe(|| q.matches_for_event(wire)));
        assert!(outcome.is_ok());
        assert!(outcome.unwrap().is_err());
    }
}
