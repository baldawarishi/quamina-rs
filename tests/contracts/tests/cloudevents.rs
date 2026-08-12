#![cfg(feature = "cloudevents")]

use quamina::{
    AvroFlattener, BinaryCloudEventFlattener, CborFlattener, CloudEventModePolicy,
    CloudEventsVersionPolicy, EmptyDataPolicy, Envelope, EventFormat, FlattenerRegistry,
    JsonFlattener, MessagePackFlattener, ProtobufFlattener, Quamina, QuaminaBuilder, QuaminaError,
    UnknownMediaTypePolicy,
};

fn matcher(flattener: BinaryCloudEventFlattener) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_envelope_flattener(Box::new(flattener))
        .build()
        .unwrap()
}

fn required_http(body: &[u8], content_type: &str) -> quamina::EnvelopeBuilder {
    Envelope::http(body)
        .header("ce-specversion", b"1.0")
        .header("ce-id", b"evt-123")
        .header("ce-type", b"com.example.created")
        .header("ce-source", b"/source")
        .header("content-type", content_type.as_bytes())
}

#[test]
fn transport_neutral_http_and_kafka_envelopes_normalize_to_identical_metadata() {
    let normalizer = BinaryCloudEventFlattener::metadata_only();
    let http = required_http(b"body", "text/plain").build().unwrap();
    let kafka = Envelope::kafka(b"body")
        .header("ce_specversion", b"1.0")
        .header("ce_id", b"evt-123")
        .header("ce_type", b"com.example.created")
        .header("ce_source", b"/source")
        .header("content-type", b"text/plain")
        .build()
        .unwrap();

    assert_eq!(
        normalizer.flatten_metadata(&http).unwrap(),
        normalizer.flatten_metadata(&kafka).unwrap()
    );
}

#[test]
fn required_attributes_and_version_policy_are_validated() {
    let q = matcher(
        BinaryCloudEventFlattener::builder(FlattenerRegistry::new())
            .versions(CloudEventsVersionPolicy::V1Only)
            .build(),
    );
    let missing_source = Envelope::http(b"")
        .header("ce-specversion", b"1.0")
        .header("ce-id", b"evt")
        .header("ce-type", b"example")
        .build()
        .unwrap();
    let version_03 = Envelope::http(b"")
        .header("ce-specversion", b"0.3")
        .header("ce-id", b"evt")
        .header("ce-type", b"example")
        .header("ce-source", b"/source")
        .build()
        .unwrap();

    assert!(matches!(
        q.matches_for_envelope(&missing_source).unwrap_err(),
        QuaminaError::InvalidEnvelope {
            attribute: "source",
            ..
        }
    ));
    assert!(matches!(
        q.matches_for_envelope(&version_03).unwrap_err(),
        QuaminaError::UnsupportedFormatFeature { .. }
    ));
}

#[test]
fn extension_attributes_have_canonical_types_and_transport_mappings() {
    let mut q = matcher(BinaryCloudEventFlattener::metadata_only());
    q.add_pattern(
        "extension",
        r#"{"attempt":[42],"verified":[true],"traceparent":["00-abcd"]}"#,
    )
    .unwrap();
    let http = required_http(b"", "text/plain")
        .header("ce-attempt", b"42")
        .header("ce-verified", b"true")
        .header("ce-traceparent", b"00-abcd")
        .build()
        .unwrap();

    assert_eq!(q.matches_for_envelope(&http).unwrap(), ["extension"]);
}

#[test]
fn content_type_maps_to_datacontenttype_with_case_and_parameter_normalization() {
    let flattener = BinaryCloudEventFlattener::metadata_only();
    let envelope = required_http(
        b"{}",
        "Application/JSON; Charset=UTF-8; profile=\"example\"",
    )
    .build()
    .unwrap();
    let fields = flattener.flatten_metadata(&envelope).unwrap();

    assert_eq!(
        fields.value(["datacontenttype"]).unwrap().as_str(),
        Some("application/json; charset=utf-8; profile=example")
    );
}

#[test]
fn payload_registry_dispatches_json_messagepack_cbor_protobuf_and_avro_without_guessing() {
    let descriptor = include_bytes!("../fixtures/protobuf/corpus.desc");
    let avro_schema = include_str!("../fixtures/avro/corpus.avsc");
    let mut registry = FlattenerRegistry::new();
    registry
        .register("application/json", Box::new(JsonFlattener::new()))
        .unwrap();
    registry
        .register("application/msgpack", Box::new(MessagePackFlattener::new()))
        .unwrap();
    registry
        .register("application/cbor", Box::new(CborFlattener::new()))
        .unwrap();
    registry
        .register(
            "application/protobuf",
            Box::new(
                ProtobufFlattener::from_descriptor_set(descriptor, "quamina.contract.Scalars")
                    .unwrap(),
            ),
        )
        .unwrap();
    registry
        .register(
            "application/avro",
            Box::new(AvroFlattener::from_writer_schema(avro_schema).unwrap()),
        )
        .unwrap();

    assert_eq!(registry.registered_media_types().len(), 5);
}

#[test]
fn unknown_media_types_can_match_metadata_but_never_invent_data_fields() {
    let mut q = matcher(
        BinaryCloudEventFlattener::builder(FlattenerRegistry::new())
            .unknown_media_types(UnknownMediaTypePolicy::MetadataOnly)
            .build(),
    );
    q.add_pattern("metadata", r#"{"type":["com.example.created"]}"#)
        .unwrap();
    q.add_pattern("invented", r#"{"data":{"x":[1]}}"#).unwrap();
    let envelope = required_http(&[0xff, 0x00], "application/x-unknown")
        .build()
        .unwrap();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["metadata"]);
}

#[test]
fn absent_body_empty_body_and_kafka_tombstone_are_distinct() {
    let flattener = BinaryCloudEventFlattener::builder(FlattenerRegistry::new())
        .empty_data(EmptyDataPolicy::DistinguishAbsentEmptyAndTombstone)
        .build();
    let absent = Envelope::http_without_body()
        .cloud_event_required("evt", "example", "/source", "1.0")
        .build()
        .unwrap();
    let empty = required_http(b"", "application/octet-stream")
        .build()
        .unwrap();
    let tombstone = Envelope::kafka_tombstone()
        .cloud_event_required("evt", "example", "/source", "1.0")
        .build()
        .unwrap();

    assert_ne!(
        flattener.flatten_metadata(&absent).unwrap(),
        flattener.flatten_metadata(&empty).unwrap()
    );
    assert_ne!(
        flattener.flatten_metadata(&empty).unwrap(),
        flattener.flatten_metadata(&tombstone).unwrap()
    );
}

#[test]
fn conflicting_invalid_percent_encoded_and_quoted_headers_fail_deterministically() {
    let q = matcher(BinaryCloudEventFlattener::metadata_only());
    let conflicts = Envelope::http(b"")
        .header("ce-specversion", b"1.0")
        .header("ce-id", b"one")
        .header("Ce-Id", b"two")
        .header("ce-type", b"bad%ZZ")
        .header("ce-source", b"\"unterminated")
        .build()
        .unwrap();
    let error = q.matches_for_envelope(&conflicts).unwrap_err();

    assert_eq!(error.format(), EventFormat::CloudEventsBinary);
    assert!(matches!(
        error,
        QuaminaError::ConflictingEnvelopeHeaders { .. }
    ));
}

#[test]
fn structured_and_batch_modes_are_explicitly_rejected_by_binary_flattener() {
    let q = matcher(
        BinaryCloudEventFlattener::builder(FlattenerRegistry::new())
            .modes(CloudEventModePolicy::BinaryOnly)
            .build(),
    );
    let structured = Envelope::http(br#"{"specversion":"1.0"}"#)
        .header("content-type", b"application/cloudevents+json")
        .build()
        .unwrap();
    let batch = Envelope::http(b"[]")
        .header("content-type", b"application/cloudevents-batch+json")
        .build()
        .unwrap();

    for envelope in [&structured, &batch] {
        assert!(matches!(
            q.matches_for_envelope(envelope).unwrap_err(),
            QuaminaError::UnsupportedFormatFeature { .. }
        ));
    }
}

#[test]
fn attributes_and_nested_data_fields_can_participate_in_one_pattern() {
    let mut registry = FlattenerRegistry::new();
    registry
        .register("application/json", Box::new(JsonFlattener::new()))
        .unwrap();
    let mut q = matcher(BinaryCloudEventFlattener::new(registry));
    q.add_pattern(
        "combined",
        r#"{"type":["com.example.created"],"source":["/source"],"data":{"status":["active"]}}"#,
    )
    .unwrap();
    let envelope = required_http(br#"{"status":"active"}"#, "application/json")
        .build()
        .unwrap();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["combined"]);
}
