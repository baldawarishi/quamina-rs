#![cfg(feature = "headers")]

use quamina::{
    ContentTypeDuplicatePolicy, Envelope, EventFormat, HeaderCollisionPolicy, HeaderLimits,
    HeaderNamePolicy, HeaderValuePolicy, HeadersFlattener, HttpListValuePolicy, HttpValueDecoding,
    Quamina, QuaminaBuilder, QuaminaError, RepeatedHeaderPolicy, WhitespacePolicy,
};

fn matcher(flattener: HeadersFlattener) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_envelope_flattener(Box::new(flattener))
        .build()
        .unwrap()
}

#[test]
fn headers_use_a_reserved_namespace_and_ascii_lowercase_names() {
    let mut q = matcher(
        HeadersFlattener::builder()
            .namespace("headers")
            .names(HeaderNamePolicy::AsciiLowercase)
            .build(),
    );
    q.add_pattern("tenant", r#"{"headers":{"x-tenant-id":["acme"]}}"#)
        .unwrap();
    let envelope = Envelope::http(b"")
        .header("X-Tenant-ID", b"acme")
        .build()
        .unwrap();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["tenant"]);
}

#[test]
fn repeated_values_are_distinct_and_preserve_transport_order() {
    let mut q = matcher(
        HeadersFlattener::builder()
            .repeated_headers(RepeatedHeaderPolicy::DistinctArrayValuesInWireOrder)
            .build(),
    );
    q.add_pattern("first", r#"{"headers":{"x-role":["reader"]}}"#)
        .unwrap();
    q.add_pattern("second", r#"{"headers":{"x-role":["auditor"]}}"#)
        .unwrap();
    let envelope = Envelope::kafka(b"")
        .header("x-role", b"reader")
        .header("x-role", b"auditor")
        .build()
        .unwrap();
    let mut matches = q.matches_for_envelope(&envelope).unwrap();
    matches.sort_unstable();

    assert_eq!(matches, ["first", "second"]);
    assert_eq!(
        envelope.headers().values("x-role"),
        [b"reader".as_slice(), b"auditor".as_slice()]
    );
}

#[test]
fn http_comma_joining_is_never_assumed_without_a_named_policy() {
    let distinct = HeadersFlattener::builder()
        .http_lists(HttpListValuePolicy::DistinctFieldLines)
        .build();
    let joined = HeadersFlattener::builder()
        .http_lists(HttpListValuePolicy::CommaSeparatedValues)
        .build();
    let envelope = Envelope::http(b"")
        .header("accept", b"text/plain, application/json")
        .build()
        .unwrap();

    assert_ne!(
        distinct.flatten_headers(&envelope).unwrap(),
        joined.flatten_headers(&envelope).unwrap()
    );
}

#[test]
fn kafka_non_utf8_values_are_rejected_or_tagged_without_string_collision() {
    let envelope = Envelope::kafka(b"")
        .header("trace-bin", &[0xff, 0x00])
        .build()
        .unwrap();
    let reject = matcher(
        HeadersFlattener::builder()
            .values(HeaderValuePolicy::Utf8Strings)
            .build(),
    );
    assert!(matches!(
        reject.matches_for_envelope(&envelope).unwrap_err(),
        QuaminaError::UnsupportedEventValue { .. }
    ));

    let mut tagged = matcher(
        HeadersFlattener::builder()
            .values(HeaderValuePolicy::TaggedBase64)
            .build(),
    );
    tagged
        .add_pattern("binary", r#"{"headers":{"trace-bin":["base64:/wA="]}}"#)
        .unwrap();
    assert_eq!(tagged.matches_for_envelope(&envelope).unwrap(), ["binary"]);
}

#[test]
fn http_quoted_percent_and_whitespace_normalization_is_explicit() {
    let mut q = matcher(
        HeadersFlattener::builder()
            .http_value_decoding(HttpValueDecoding::QuotedStringAndPercent)
            .whitespace(WhitespacePolicy::TrimOptionalWhitespace)
            .build(),
    );
    q.add_pattern("decoded", r#"{"headers":{"x-name":["Grüße world"]}}"#)
        .unwrap();
    let envelope = Envelope::http(b"")
        .header("x-name", br#"  \"Gr%C3%BC%C3%9Fe world\"  "#)
        .build()
        .unwrap();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["decoded"]);
}

#[test]
fn empty_values_are_present_empty_strings_not_absent_headers() {
    let mut q = matcher(HeadersFlattener::new());
    q.add_pattern("empty", r#"{"headers":{"x-empty":[""]}}"#)
        .unwrap();
    q.add_pattern("absent", r#"{"headers":{"x-empty":[{"exists":false}]}}"#)
        .unwrap();
    let envelope = Envelope::http(b"").header("x-empty", b"").build().unwrap();

    assert_eq!(q.matches_for_envelope(&envelope).unwrap(), ["empty"]);
}

#[test]
fn duplicate_content_type_has_a_dedicated_rejection_policy() {
    let q = matcher(
        HeadersFlattener::builder()
            .duplicate_content_type(ContentTypeDuplicatePolicy::Reject)
            .build(),
    );
    let envelope = Envelope::http(b"body")
        .header("content-type", b"application/json")
        .header("Content-Type", b"text/plain")
        .build()
        .unwrap();
    let error = q.matches_for_envelope(&envelope).unwrap_err();

    assert!(matches!(
        error,
        QuaminaError::ConflictingEnvelopeHeaders { .. }
    ));
    assert_eq!(error.format(), EventFormat::Headers);
}

#[test]
fn header_count_and_aggregate_size_limits_are_enforced() {
    let limits = HeaderLimits {
        max_count: 2,
        max_name_bytes: 16,
        max_value_bytes: 8,
        max_aggregate_bytes: 24,
    };
    let q = matcher(HeadersFlattener::builder().limits(limits).build());
    let too_many = Envelope::kafka(b"")
        .header("a", b"1")
        .header("b", b"2")
        .header("c", b"3")
        .build()
        .unwrap();
    let too_large = Envelope::kafka(b"")
        .header("a", b"123456789")
        .build()
        .unwrap();

    for envelope in [&too_many, &too_large] {
        assert!(matches!(
            q.matches_for_envelope(envelope).unwrap_err(),
            QuaminaError::EventLimitExceeded { .. }
        ));
    }
}

#[test]
fn separators_in_header_names_cannot_collide_with_payload_paths() {
    let q = matcher(
        HeadersFlattener::builder()
            .namespace("headers")
            .collisions(HeaderCollisionPolicy::Reject)
            .build(),
    );
    let newline_name = Envelope::kafka(b"")
        .header_bytes(b"x\ninjected", b"value")
        .build()
        .unwrap();

    assert!(matches!(
        q.matches_for_envelope(&newline_name).unwrap_err(),
        QuaminaError::InvalidEnvelope { .. }
    ));
}

#[test]
fn reserved_metadata_namespace_cannot_be_overwritten_by_payload_fields() {
    let q = matcher(
        HeadersFlattener::builder()
            .namespace("headers")
            .collisions(HeaderCollisionPolicy::Reject)
            .build(),
    );
    let envelope = Envelope::http(br#"{"headers":{"x-role":"payload"}}"#)
        .header("x-role", b"metadata")
        .build()
        .unwrap();
    let error = q.matches_for_envelope(&envelope).unwrap_err();

    assert!(matches!(error, QuaminaError::EnvelopePathCollision { .. }));
}
