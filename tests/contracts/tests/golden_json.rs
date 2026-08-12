use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use quamina::{
    ArrayPos, Flattener, JsonFlattener, OwnedField, QuaminaBuilder, QuaminaError,
    SegmentsTreeTracker,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct GoldenField {
    path: Vec<u8>,
    value: Vec<u8>,
    array_trail: Vec<(i32, i32)>,
    is_number: bool,
}

impl From<OwnedField> for GoldenField {
    fn from(field: OwnedField) -> Self {
        Self {
            path: field.path,
            value: field.val,
            array_trail: field
                .array_trail
                .into_iter()
                .map(|ArrayPos { array, pos }| (array, pos))
                .collect(),
            is_number: field.is_number,
        }
    }
}

struct CapturingJsonFlattener {
    inner: Box<dyn Flattener>,
    captured: Arc<Mutex<Vec<OwnedField>>>,
}

impl CapturingJsonFlattener {
    fn new(captured: Arc<Mutex<Vec<OwnedField>>>) -> Self {
        Self {
            inner: Box::new(JsonFlattener::new()),
            captured,
        }
    }
}

impl Flattener for CapturingJsonFlattener {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let fields = self.inner.flatten(event, tracker)?;
        *self.captured.lock().expect("capture mutex poisoned") = fields.clone();
        Ok(fields)
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(Self::new(Arc::clone(&self.captured)))
    }
}

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures")
}

fn fixture(relative: &str) -> Vec<u8> {
    let path = fixtures_dir().join(relative);
    std::fs::read(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

fn run_case(
    case_name: &str,
    patterns: &[(&str, &str)],
) -> Result<(Vec<String>, Vec<GoldenField>), QuaminaError> {
    let captured = Arc::new(Mutex::new(Vec::new()));
    let flattener = CapturingJsonFlattener::new(Arc::clone(&captured));
    let mut quamina = QuaminaBuilder::new()
        .with_flattener(Box::new(flattener))?
        .build()?;
    for &(id, pattern) in patterns {
        quamina.add_pattern(id.to_owned(), pattern)?;
    }

    let event = fixture(&format!("json/{case_name}.json"));
    let mut matches = quamina.matches_for_event(&event)?;
    matches.sort_unstable();
    let mut fields: Vec<_> = captured
        .lock()
        .expect("capture mutex poisoned")
        .clone()
        .into_iter()
        .map(GoldenField::from)
        .collect();
    fields.sort_unstable();
    Ok((matches, fields))
}

fn field(path: &str, value: &[u8], trail: &[(i32, i32)], is_number: bool) -> GoldenField {
    GoldenField {
        path: path.as_bytes().to_vec(),
        value: value.to_vec(),
        array_trail: trail.to_vec(),
        is_number,
    }
}

fn assert_case(
    name: &str,
    patterns: &[(&str, &str)],
    expected_matches: &[&str],
    mut expected_fields: Vec<GoldenField>,
) {
    let (actual_matches, actual_fields) = run_case(name, patterns).expect("valid golden case");
    let expected_matches: Vec<String> = expected_matches
        .iter()
        .map(|value| (*value).to_owned())
        .collect();
    expected_fields.sort_unstable();
    assert_eq!(actual_matches, expected_matches, "match set for {name}");
    assert_eq!(
        actual_fields, expected_fields,
        "canonical fields for {name}"
    );
}

#[test]
fn golden_scalars_preserve_type_tags_and_numeric_equivalence() {
    assert_case(
        "scalars",
        &[
            ("string", r#"{"text":["42"]}"#),
            ("number", r#"{"count":[42]}"#),
            ("exponent-equivalent", r#"{"count":[4.2e1]}"#),
            ("boolean", r#"{"enabled":[true]}"#),
            ("null", r#"{"deleted_at":[null]}"#),
            ("negative", r#"{"negative":[-7]}"#),
            ("wrong-type", r#"{"text":[42]}"#),
        ],
        &[
            "boolean",
            "exponent-equivalent",
            "negative",
            "null",
            "number",
            "string",
        ],
        vec![
            field("text", br#""42""#, &[], false),
            field("count", b"42", &[], true),
            field("enabled", b"true", &[], false),
            field("deleted_at", b"null", &[], false),
            field("negative", b"-7", &[], true),
        ],
    );
}

#[test]
fn golden_unicode_and_escaped_strings_use_decoded_matcher_content() {
    assert_case(
        "escaped_unicode",
        &[
            ("unicode", r#"{"métadata":{"greeting":["Grüße 🌍"]}}"#),
            ("quote", r#"{"métadata":{"quotation":["say \"hello\""]}}"#),
            ("backslash", r#"{"métadata":{"slash":["a\\b"]}}"#),
            ("control", r#"{"métadata":{"control":["line\nnext"]}}"#),
            ("newline-key", "{\"a\\nb\":[\"literal newline key\"]}"),
        ],
        &["backslash", "control", "quote", "unicode"],
        vec![
            field("métadata\ngreeting", "\"Grüße 🌍\"".as_bytes(), &[], false),
            field("métadata\nquotation", b"\"say \"hello\"\"", &[], false),
            field("métadata\nslash", b"\"a\\b\"", &[], false),
            field("métadata\ncontrol", b"\"line\nnext\"", &[], false),
        ],
    );
}

#[test]
fn golden_nested_fields_are_order_independent_and_tracker_filtered() {
    assert_case(
        "nested",
        &[
            ("nested", r#"{"tenant":{"id":["acme"]}}"#),
            ("numeric", r#"{"priority":[{"numeric":[">=",5]}]}"#),
            ("tracker-target", r#"{"tenant":{"region":["west"]}}"#),
        ],
        &["nested", "numeric", "tracker-target"],
        vec![
            field("tenant\nid", br#""acme""#, &[], false),
            field("tenant\nregion", br#""west""#, &[], false),
            field("priority", b"7", &[], true),
        ],
    );
}

#[test]
fn golden_primitive_arrays_have_stable_distinct_trails() {
    assert_case(
        "primitive_arrays",
        &[
            ("middle", r#"{"tags":["green"]}"#),
            ("number-in-array", r#"{"scores":[2]}"#),
            ("exists", r#"{"tags":[{"exists":true}]}"#),
            ("missing", r#"{"tags":["orange"]}"#),
        ],
        &["exists", "middle", "number-in-array"],
        vec![
            field("tags", br#""red""#, &[(1, 1)], false),
            field("tags", br#""green""#, &[(1, 2)], false),
            field("tags", br#""blue""#, &[(1, 3)], false),
            field("scores", b"1", &[(2, 1)], true),
            field("scores", b"2", &[(2, 2)], true),
            field("scores", b"3", &[(2, 3)], true),
        ],
    );
}

fn order_fields() -> Vec<GoldenField> {
    vec![
        field("orders\nsku", br#""A""#, &[(1, 1)], false),
        field("orders\nquantity", b"1", &[(1, 1)], true),
        field("orders\nsku", br#""B""#, &[(1, 2)], false),
        field("orders\nquantity", b"2", &[(1, 2)], true),
    ]
}

#[test]
fn golden_object_array_positive_requires_one_element() {
    assert_case(
        "object_arrays_positive",
        &[("same-element", r#"{"orders":{"sku":["B"],"quantity":[2]}}"#)],
        &["same-element"],
        order_fields(),
    );
}

#[test]
fn golden_object_array_negative_rejects_cross_element_join() {
    assert_case(
        "object_arrays_negative",
        &[(
            "cross-element",
            r#"{"orders":{"sku":["A"],"quantity":[2]}}"#,
        )],
        &[],
        order_fields(),
    );
}

#[test]
fn golden_nested_arrays_retain_parent_and_child_positions() {
    assert_case(
        "nested_arrays",
        &[
            (
                "coherent",
                r#"{"shipments":{"region":["east"],"boxes":{"sku":["B"]}}}"#,
            ),
            (
                "cross-parent",
                r#"{"shipments":{"region":["west"],"boxes":{"sku":["B"]}}}"#,
            ),
            (
                "nested-repeated",
                r#"{"shipments":{"boxes":{"sku":["C"]}}}"#,
            ),
        ],
        &["coherent", "nested-repeated"],
        vec![
            field("shipments\nregion", br#""west""#, &[(1, 1)], false),
            field("shipments\nboxes\nsku", br#""A""#, &[(1, 1), (2, 1)], false),
            field("shipments\nboxes\nsku", br#""C""#, &[(1, 1), (2, 2)], false),
            field("shipments\nregion", br#""east""#, &[(1, 2)], false),
            field("shipments\nboxes\nsku", br#""B""#, &[(1, 2), (3, 1)], false),
        ],
    );
}

#[test]
fn golden_unrelated_arrays_use_nonconflicting_ids() {
    assert_case(
        "unrelated_arrays",
        &[(
            "independent-arrays",
            r#"{"wanted_regions":["west"],"blocked_skus":["B-2"]}"#,
        )],
        &["independent-arrays"],
        vec![
            field("wanted_regions", br#""west""#, &[(1, 1)], false),
            field("wanted_regions", br#""north""#, &[(1, 2)], false),
            field("blocked_skus", br#""B-1""#, &[(2, 1)], false),
            field("blocked_skus", br#""B-2""#, &[(2, 2)], false),
        ],
    );
}

#[test]
fn golden_presence_distinguishes_null_absent_and_empty_collections() {
    assert_case(
        "presence_empty",
        &[
            ("explicit-null", r#"{"explicit_null":[null]}"#),
            ("absent", r#"{"absent_field":[{"exists":false}]}"#),
            (
                "empty-array-is-absent",
                r#"{"empty_array":[{"exists":false}]}"#,
            ),
            ("empty-map-is-absent", r#"{"empty_map":[{"exists":false}]}"#),
            ("present", r#"{"present":[{"exists":true}]}"#),
        ],
        &[
            "absent",
            "empty-array-is-absent",
            "empty-map-is-absent",
            "explicit-null",
            "present",
        ],
        vec![
            field("explicit_null", b"null", &[], false),
            field("present", b"true", &[], false),
        ],
    );
}

#[test]
fn golden_operator_set_allows_multiple_patterns_to_match() {
    assert_case(
        "operators_multiple",
        &[
            ("exact", r#"{"exact":["yes"]}"#),
            ("prefix", r#"{"name":[{"prefix":"Prod-"}]}"#),
            ("suffix", r#"{"file":[{"suffix":".JPG"}]}"#),
            ("wildcard", r#"{"name":[{"wildcard":"Prod-*"}]}"#),
            (
                "ignore-case",
                r#"{"name":[{"equals-ignore-case":"prod-service"}]}"#,
            ),
            (
                "anything-but",
                r#"{"status":[{"anything-but":["deleted","archived"]}]}"#,
            ),
            ("numeric", r#"{"count":[{"numeric":[">",41,"<=",42]}]}"#),
            ("exists", r#"{"status":[{"exists":true}]}"#),
            ("combined", r#"{"status":["active"],"count":[42]}"#),
        ],
        &[
            "anything-but",
            "combined",
            "exact",
            "exists",
            "ignore-case",
            "numeric",
            "prefix",
            "suffix",
            "wildcard",
        ],
        vec![
            field("name", br#""Prod-Service""#, &[], false),
            field("file", br#""photo.JPG""#, &[], false),
            field("status", br#""active""#, &[], false),
            field("count", b"42", &[], true),
            field("exact", br#""yes""#, &[], false),
        ],
    );
}

#[test]
fn golden_cloudevent_payload_reuses_the_logical_data_corpus() {
    assert_case(
        "cloudevent_data",
        &[
            ("payload-order", r#"{"order_id":["A-42"]}"#),
            ("payload-total", r#"{"total":[{"numeric":[">",10]}]}"#),
        ],
        &["payload-order", "payload-total"],
        vec![
            field("order_id", br#""A-42""#, &[], false),
            field("total", b"19.95", &[], true),
        ],
    );
}

#[test]
fn fixture_inventory_is_complete_for_every_shared_encoding() {
    let cases = [
        "scalars",
        "escaped_unicode",
        "nested",
        "primitive_arrays",
        "object_arrays_positive",
        "object_arrays_negative",
        "nested_arrays",
        "unrelated_arrays",
        "presence_empty",
        "operators_multiple",
        "cloudevent_data",
    ];
    for case in cases {
        for (directory, extension) in [
            ("json", "json"),
            ("patterns", "json"),
            ("messagepack", "msgpack"),
            ("cbor", "cbor"),
        ] {
            let path = fixtures_dir().join(format!("{directory}/{case}.{extension}"));
            let metadata = std::fs::metadata(&path)
                .unwrap_or_else(|error| panic!("fixture {}: {error}", path.display()));
            assert!(metadata.len() > 0, "fixture {} is empty", path.display());
        }
        for (directory, extension) in [("protobuf", "pb"), ("avro", "avro")] {
            let path = fixtures_dir().join(format!("{directory}/{case}.{extension}"));
            if case == "escaped_unicode" {
                assert!(
                    !path.exists(),
                    "{case} must retain its documented schema-format exclusion"
                );
            } else {
                let metadata = std::fs::metadata(&path)
                    .unwrap_or_else(|error| panic!("fixture {}: {error}", path.display()));
                assert!(metadata.len() > 0, "fixture {} is empty", path.display());
            }
        }
    }
    for required in [
        "corpus.json",
        "MANIFEST.sha256",
        "protobuf/corpus.proto",
        "protobuf/corpus.desc",
    ] {
        let path = fixtures_dir().join(required);
        assert!(path.is_file(), "missing fixture {}", path.display());
    }
}
