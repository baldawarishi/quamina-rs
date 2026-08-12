//! Compile contract for schema-bound Protobuf and Avro flatteners.
//!
//! This file intentionally references APIs and a generated descriptor fixture
//! that have not been implemented yet.

use quamina::{
    AvroFlattener, AvroInput, BinaryValuePolicy, EnumValuePolicy, LogicalTypePolicy,
    ProtobufFieldName, ProtobufFlattener, ProtobufPresence, QuaminaBuilder, WellKnownTypePolicy,
};

const AVRO_SCHEMA: &str = r#"
{
  "type": "record",
  "name": "ContractEvent",
  "fields": [
    {"name": "status", "type": "string"},
    {"name": "count", "type": "long"}
  ]
}
"#;

// Avro binary datum for {"status":"active", "count":42}.
// String length 6 is zig-zag encoded as 12; long 42 as 84.
const AVRO_DATUM: &[u8] = &[0x0c, b'a', b'c', b't', b'i', b'v', b'e', 0x54];

#[test]
fn protobuf_constructor_requires_a_descriptor_and_declares_name_presence_policies() {
    let descriptor_set = include_bytes!("../fixtures/contract_descriptor.bin");
    let flattener = ProtobufFlattener::from_descriptor_set(descriptor_set, "quamina.contract.Task")
        .unwrap()
        .with_field_names(ProtobufFieldName::ProtoName)
        .with_enum_values(EnumValuePolicy::SymbolicName)
        .with_presence(ProtobufPresence::WirePresence)
        .with_binary_values(BinaryValuePolicy::Reject);

    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern(
        "running",
        r#"{"status":["RUNNING"],"retry_count":[{"exists":false}]}"#,
    )
    .unwrap();

    // status = RUNNING (enum value 1), labels = "urgent".
    let wire = [0x08, 0x01, 0x12, 0x06, b'u', b'r', b'g', b'e', b'n', b't'];
    assert_eq!(q.matches_for_event(&wire).unwrap(), ["running"]);
}

#[test]
fn avro_constructor_distinguishes_raw_datum_container_and_single_object_inputs() {
    let flattener = AvroFlattener::builder(AVRO_SCHEMA)
        .unwrap()
        .input(AvroInput::RawDatum)
        .logical_types(LogicalTypePolicy::CanonicalString)
        .binary_values(BinaryValuePolicy::Reject)
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("match", r#"{"status":["active"],"count":[42]}"#)
        .unwrap();

    assert_eq!(q.matches_for_event(AVRO_DATUM).unwrap(), ["match"]);
}

#[test]
fn avro_reader_schema_is_optional_and_explicit_for_schema_evolution() {
    let writer_schema = AVRO_SCHEMA;
    let reader_schema = r#"
    {
      "type":"record",
      "name":"ContractEvent",
      "fields":[
        {"name":"status","type":"string"},
        {"name":"count","type":"long"},
        {"name":"region","type":"string","default":"unknown"}
      ]
    }
    "#;
    let flattener = AvroFlattener::builder(writer_schema)
        .unwrap()
        .reader_schema(reader_schema)
        .unwrap()
        .input(AvroInput::RawDatum)
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("resolved-default", r#"{"region":["unknown"]}"#)
        .unwrap();

    assert_eq!(
        q.matches_for_event(AVRO_DATUM).unwrap(),
        ["resolved-default"]
    );
}

#[test]
fn protobuf_well_known_types_have_stable_pattern_representations() {
    let descriptor_set = include_bytes!("../fixtures/well_known_types_descriptor.bin");
    let flattener =
        ProtobufFlattener::from_descriptor_set(descriptor_set, "quamina.contract.TimedEvent")
            .unwrap()
            .with_well_known_types(WellKnownTypePolicy::CanonicalJson);
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("timestamp", r#"{"created_at":["2026-08-11T12:34:56Z"]}"#)
        .unwrap();

    let encoded_timed_event = include_bytes!("../fixtures/timed_event.bin");
    assert_eq!(
        q.matches_for_event(encoded_timed_event).unwrap(),
        ["timestamp"]
    );
}

#[test]
fn avro_logical_type_policy_is_observable_and_not_platform_dependent() {
    let schema = r#"
    {
      "type":"record",
      "name":"TimedEvent",
      "fields":[
        {"name":"created_at","type":{"type":"long","logicalType":"timestamp-millis"}}
      ]
    }
    "#;
    let flattener = AvroFlattener::builder(schema)
        .unwrap()
        .logical_types(LogicalTypePolicy::CanonicalString)
        .build();
    let mut q = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();
    q.add_pattern("timestamp", r#"{"created_at":["1970-01-01T00:00:01Z"]}"#)
        .unwrap();

    // timestamp-millis 1000, zig-zag/varint encoded.
    assert_eq!(q.matches_for_event(&[0xd0, 0x0f]).unwrap(), ["timestamp"]);
}
