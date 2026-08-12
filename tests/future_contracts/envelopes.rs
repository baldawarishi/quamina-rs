//! Compile contract for key-value headers and CloudEvents binary mode.
//!
//! This file intentionally references envelope APIs that have not been
//! implemented. A byte-only `Flattener` cannot satisfy these tests.

use quamina::{
    BinaryCloudEventFlattener, Envelope, FlattenerRegistry, HeaderNamePolicy, HeaderValuePolicy,
    HeadersFlattener, MessagePackFlattener, QuaminaBuilder, QuaminaError, UnknownMediaTypePolicy,
};

#[test]
fn generic_headers_are_supplied_per_event_and_use_a_reserved_namespace() {
    let q = QuaminaBuilder::<&str>::new()
        .with_envelope_flattener(Box::new(
            HeadersFlattener::builder()
                .namespace("headers")
                .names(HeaderNamePolicy::AsciiLowercase)
                .values(HeaderValuePolicy::Utf8Strings)
                .build(),
        ))
        .build()
        .unwrap();
    let mut q = q;
    q.add_pattern(
        "tenant",
        r#"{"headers":{"x-tenant-id":["acme"],"x-role":["auditor"]}}"#,
    )
    .unwrap();

    let envelope = Envelope::builder(b"opaque body")
        .header("X-Tenant-ID", b"acme")
        .header("X-Role", b"reader")
        .header("X-Role", b"auditor")
        .build();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["tenant"]);
}

#[test]
fn binary_cloudevents_normalize_transport_headers_and_dispatch_the_body_decoder() {
    let mut registry = FlattenerRegistry::new();
    registry.register("application/msgpack", Box::new(MessagePackFlattener::new()));
    let cloud_event_flattener = BinaryCloudEventFlattener::new(registry);
    let mut q = QuaminaBuilder::new()
        .with_envelope_flattener(Box::new(cloud_event_flattener))
        .build()
        .unwrap();
    q.add_pattern(
        "order",
        r#"{
          "type":["com.example.order.created"],
          "source":["/orders"],
          "data":{"status":["active"],"count":[42]}
        }"#,
    )
    .unwrap();

    let msgpack_body = [
        0x82, 0xa6, b's', b't', b'a', b't', b'u', b's', 0xa6, b'a', b'c', b't', b'i', b'v', b'e',
        0xa5, b'c', b'o', b'u', b'n', b't', 0x2a,
    ];
    let envelope = Envelope::builder(&msgpack_body)
        .header("ce-specversion", b"1.0")
        .header("ce-type", b"com.example.order.created")
        .header("ce-source", b"/orders")
        .header("ce-id", b"evt-123")
        .header("content-type", b"application/msgpack")
        .build();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["order"]);
}

#[test]
fn http_and_kafka_bindings_produce_the_same_transport_neutral_fields() {
    let http = Envelope::http_request(b"body")
        .header("ce-id", b"evt-123")
        .header("ce-type", b"example")
        .header("ce-source", b"/source")
        .header("ce-specversion", b"1.0")
        .header("content-type", b"text/plain")
        .build();
    let kafka = Envelope::kafka_record(b"body")
        .header("ce_id", b"evt-123")
        .header("ce_type", b"example")
        .header("ce_source", b"/source")
        .header("ce_specversion", b"1.0")
        .header("content-type", b"text/plain")
        .build();

    let normalizer = BinaryCloudEventFlattener::metadata_only();
    assert_eq!(
        normalizer.flatten_metadata(&http).unwrap(),
        normalizer.flatten_metadata(&kafka).unwrap()
    );
}

#[test]
fn cloud_event_validation_reports_missing_required_context_attributes() {
    let flattener = BinaryCloudEventFlattener::metadata_only();
    let q = QuaminaBuilder::<&str>::new()
        .with_envelope_flattener(Box::new(flattener))
        .build()
        .unwrap();
    let missing_source = Envelope::http_request(b"")
        .header("ce-specversion", b"1.0")
        .header("ce-type", b"example")
        .header("ce-id", b"evt-123")
        .build();

    assert!(matches!(
        q.matches_for_envelope(&missing_source).unwrap_err(),
        QuaminaError::InvalidEnvelope {
            attribute: "source",
            ..
        }
    ));
}

#[test]
fn unknown_data_content_type_can_match_metadata_without_guessing_body_format() {
    let flattener = BinaryCloudEventFlattener::builder(FlattenerRegistry::new())
        .unknown_media_types(UnknownMediaTypePolicy::MetadataOnly)
        .build();
    let mut q = QuaminaBuilder::new()
        .with_envelope_flattener(Box::new(flattener))
        .build()
        .unwrap();
    q.add_pattern("metadata", r#"{"type":["example.opaque"]}"#)
        .unwrap();
    q.add_pattern("invented-data", r#"{"data":{"x":[1]}}"#)
        .unwrap();
    let envelope = Envelope::http_request(&[0xff, 0x00, 0x81])
        .header("ce-specversion", b"1.0")
        .header("ce-type", b"example.opaque")
        .header("ce-source", b"/source")
        .header("ce-id", b"evt-opaque")
        .header("content-type", b"application/x-unknown")
        .build();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["metadata"]);
}
