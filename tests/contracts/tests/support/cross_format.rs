use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use quamina::{
    Flattener, JsonFlattener, OwnedField, QuaminaBuilder, QuaminaError, SegmentsTreeTracker,
};

pub struct GoldenCase {
    pub name: &'static str,
    pub patterns: &'static [(&'static str, &'static str)],
    pub expected_matches: &'static [&'static str],
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct ComparableField {
    path: Vec<u8>,
    value: Vec<u8>,
    array_trail: Vec<(i32, i32)>,
    is_number: bool,
}

impl From<OwnedField> for ComparableField {
    fn from(field: OwnedField) -> Self {
        Self {
            path: field.path,
            value: field.val,
            array_trail: field
                .array_trail
                .into_iter()
                .map(|position| (position.array, position.pos))
                .collect(),
            is_number: field.is_number,
        }
    }
}

struct Capture {
    inner: Box<dyn Flattener>,
    fields: Arc<Mutex<Vec<OwnedField>>>,
}

impl Capture {
    fn new(inner: Box<dyn Flattener>, fields: Arc<Mutex<Vec<OwnedField>>>) -> Self {
        Self { inner, fields }
    }
}

impl Flattener for Capture {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let output = self.inner.flatten(event, tracker)?;
        *self.fields.lock().expect("capture mutex poisoned") = output.clone();
        Ok(output)
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(Self::new(self.inner.copy(), Arc::clone(&self.fields)))
    }
}

fn execute(
    flattener: Box<dyn Flattener>,
    wire: &[u8],
    case: &GoldenCase,
) -> Result<(Vec<&'static str>, Vec<ComparableField>), QuaminaError> {
    let captured = Arc::new(Mutex::new(Vec::new()));
    let mut matcher = QuaminaBuilder::new()
        .with_flattener(Box::new(Capture::new(flattener, Arc::clone(&captured))))?
        .build()?;
    for &(id, pattern) in case.patterns {
        matcher.add_pattern(id, pattern)?;
    }
    let mut matches = matcher.matches_for_event(wire)?;
    matches.sort_unstable();
    let mut fields: Vec<_> = captured
        .lock()
        .expect("capture mutex poisoned")
        .clone()
        .into_iter()
        .map(ComparableField::from)
        .collect();
    fields.sort_unstable();
    Ok((matches, fields))
}

pub fn assert_matches_json(flattener: Box<dyn Flattener>, wire: &[u8], case: &GoldenCase) {
    let json = fixture("json", case.name, "json");
    let (json_matches, json_fields) =
        execute(Box::new(JsonFlattener::new()), &json, case).expect("valid JSON baseline");
    assert_eq!(
        json_matches, case.expected_matches,
        "JSON matches for {}",
        case.name
    );

    let (format_matches, format_fields) =
        execute(flattener, wire, case).expect("valid format fixture");
    assert_eq!(
        format_matches, json_matches,
        "format matches for {}",
        case.name
    );
    assert_eq!(
        format_fields, json_fields,
        "canonical fields for {}",
        case.name
    );
}

pub fn fixture(format: &str, case: &str, extension: &str) -> Vec<u8> {
    let path = fixtures_dir().join(format!("{format}/{case}.{extension}"));
    std::fs::read(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures")
}

pub fn cases() -> &'static [GoldenCase] {
    &[
        GoldenCase {
            name: "scalars",
            patterns: &[
                ("string", r#"{"text":["42"]}"#),
                ("number", r#"{"count":[42]}"#),
                ("exponent-equivalent", r#"{"count":[4.2e1]}"#),
                ("boolean", r#"{"enabled":[true]}"#),
                ("null", r#"{"deleted_at":[null]}"#),
                ("negative", r#"{"negative":[-7]}"#),
                ("wrong-type", r#"{"text":[42]}"#),
            ],
            expected_matches: &[
                "boolean",
                "exponent-equivalent",
                "negative",
                "null",
                "number",
                "string",
            ],
        },
        GoldenCase {
            name: "escaped_unicode",
            patterns: &[
                ("unicode", r#"{"métadata":{"greeting":["Grüße 🌍"]}}"#),
                ("quote", r#"{"métadata":{"quotation":["say \"hello\""]}}"#),
                ("backslash", r#"{"métadata":{"slash":["a\\b"]}}"#),
                ("control", r#"{"métadata":{"control":["line\nnext"]}}"#),
                ("newline-key", "{\"a\\nb\":[\"literal newline key\"]}"),
            ],
            expected_matches: &["backslash", "control", "quote", "unicode"],
        },
        GoldenCase {
            name: "nested",
            patterns: &[
                ("nested", r#"{"tenant":{"id":["acme"]}}"#),
                ("numeric", r#"{"priority":[{"numeric":[">=",5]}]}"#),
                ("tracker-target", r#"{"tenant":{"region":["west"]}}"#),
            ],
            expected_matches: &["nested", "numeric", "tracker-target"],
        },
        GoldenCase {
            name: "primitive_arrays",
            patterns: &[
                ("middle", r#"{"tags":["green"]}"#),
                ("number-in-array", r#"{"scores":[2]}"#),
                ("exists", r#"{"tags":[{"exists":true}]}"#),
                ("missing", r#"{"tags":["orange"]}"#),
            ],
            expected_matches: &["exists", "middle", "number-in-array"],
        },
        GoldenCase {
            name: "object_arrays_positive",
            patterns: &[("same-element", r#"{"orders":{"sku":["B"],"quantity":[2]}}"#)],
            expected_matches: &["same-element"],
        },
        GoldenCase {
            name: "object_arrays_negative",
            patterns: &[(
                "cross-element",
                r#"{"orders":{"sku":["A"],"quantity":[2]}}"#,
            )],
            expected_matches: &[],
        },
        GoldenCase {
            name: "nested_arrays",
            patterns: &[
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
            expected_matches: &["coherent", "nested-repeated"],
        },
        GoldenCase {
            name: "unrelated_arrays",
            patterns: &[(
                "independent-arrays",
                r#"{"wanted_regions":["west"],"blocked_skus":["B-2"]}"#,
            )],
            expected_matches: &["independent-arrays"],
        },
        GoldenCase {
            name: "presence_empty",
            patterns: &[
                ("explicit-null", r#"{"explicit_null":[null]}"#),
                ("absent", r#"{"absent_field":[{"exists":false}]}"#),
                (
                    "empty-array-is-absent",
                    r#"{"empty_array":[{"exists":false}]}"#,
                ),
                ("empty-map-is-absent", r#"{"empty_map":[{"exists":false}]}"#),
                ("present", r#"{"present":[{"exists":true}]}"#),
            ],
            expected_matches: &[
                "absent",
                "empty-array-is-absent",
                "empty-map-is-absent",
                "explicit-null",
                "present",
            ],
        },
        GoldenCase {
            name: "operators_multiple",
            patterns: &[
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
            expected_matches: &[
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
        },
        GoldenCase {
            name: "cloudevent_data",
            patterns: &[
                ("payload-order", r#"{"order_id":["A-42"]}"#),
                ("payload-total", r#"{"total":[{"numeric":[">",10]}]}"#),
            ],
            expected_matches: &["payload-order", "payload-total"],
        },
    ]
}
