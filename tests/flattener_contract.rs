//! Proposed behavioral contract for built-in non-JSON flatteners.
//!
//! These are intentionally decoder-independent. `FieldsFlattener` stands in for
//! a MessagePack, CBOR, Protobuf, Avro, header, or CloudEvents decoder and emits
//! the `OwnedField`s that decoder would be expected to produce. The assertions
//! exercise those fields through Quamina's public API, so they describe
//! observable behavior rather than a particular parsing implementation.
//!
//! Requirements that need missing public APIs live in `tests/future_contracts`.
//! Those files are deliberately outside Cargo's automatic integration-test
//! discovery: they are compile contracts to promote into this suite as the
//! corresponding APIs are implemented.

use quamina::{
    ArrayPos, Flattener, OwnedField, Quamina, QuaminaBuilder, QuaminaError, SegmentsTreeTracker,
};

#[derive(Clone)]
struct FieldsFlattener {
    fields: Vec<OwnedField>,
}

impl FieldsFlattener {
    const fn new(fields: Vec<OwnedField>) -> Self {
        Self { fields }
    }
}

impl Flattener for FieldsFlattener {
    fn flatten(
        &mut self,
        _event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        // A real implementation should consult the tracker before decoding or
        // allocating values. Filtering here models the required result while
        // keeping this fixture independent of any wire format.
        Ok(self
            .fields
            .iter()
            .filter(|field| tracked_path(tracker, &field.path))
            .cloned()
            .collect())
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(self.clone())
    }
}

fn tracked_path(mut tracker: &dyn SegmentsTreeTracker, path: &[u8]) -> bool {
    let mut segments = path.split(|byte| *byte == b'\n').peekable();
    while let Some(segment) = segments.next() {
        if segments.peek().is_none() {
            return tracker.path_for_segment(segment).is_some();
        }
        let Some(child) = tracker.get(segment) else {
            return false;
        };
        tracker = child;
    }
    false
}

fn trail(positions: &[(i32, i32)]) -> Vec<ArrayPos> {
    positions
        .iter()
        .map(|&(array, pos)| ArrayPos { array, pos })
        .collect()
}

fn string(path: &str, value: &str, positions: &[(i32, i32)]) -> OwnedField {
    OwnedField {
        path: path.as_bytes().to_vec(),
        // This fixture deliberately uses simple strings. A real flattener must
        // apply the same escaping/canonicalization rules as JSON values.
        val: format!("\"{value}\"").into_bytes(),
        array_trail: trail(positions),
        is_number: false,
    }
}

fn number(path: &str, representation: &str, positions: &[(i32, i32)]) -> OwnedField {
    OwnedField {
        path: path.as_bytes().to_vec(),
        val: representation.as_bytes().to_vec(),
        array_trail: trail(positions),
        is_number: true,
    }
}

fn literal(path: &str, representation: &str) -> OwnedField {
    literal_with_trail(path, representation, &[])
}

fn literal_with_trail(path: &str, representation: &str, positions: &[(i32, i32)]) -> OwnedField {
    OwnedField {
        path: path.as_bytes().to_vec(),
        val: representation.as_bytes().to_vec(),
        array_trail: trail(positions),
        is_number: false,
    }
}

fn quamina_for(fields: Vec<OwnedField>) -> Quamina<&'static str> {
    QuaminaBuilder::new()
        .with_flattener(Box::new(FieldsFlattener::new(fields)))
        .expect("fixture flattener should be accepted")
        .build()
        .expect("fixture Quamina should build")
}

fn matches(
    fields: Vec<OwnedField>,
    patterns: &[(&'static str, &str)],
) -> Result<Vec<&'static str>, QuaminaError> {
    let mut q = quamina_for(fields);
    for &(id, pattern) in patterns {
        q.add_pattern(id, pattern)?;
    }

    let mut result = q.matches_for_event(b"format-specific wire bytes")?;
    result.sort_unstable();
    Ok(result)
}

#[test]
fn contract_all_formats_emit_json_compatible_scalar_representations() {
    let fields = vec![
        string("text", "42", &[]),
        number("count", "4.2e1", &[]),
        literal("enabled", "true"),
        literal("deleted_at", "null"),
    ];

    let actual = matches(
        fields,
        &[
            ("boolean", r#"{"enabled":[true]}"#),
            ("null", r#"{"deleted_at":[null]}"#),
            ("number", r#"{"count":[42]}"#),
            ("string", r#"{"text":["42"]}"#),
            // A string containing digits must not acquire numeric semantics.
            ("wrong-type", r#"{"text":[42]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["boolean", "null", "number", "string"]);
}

#[test]
fn contract_strings_use_decoded_unicode_content_with_a_string_type_tag() {
    let fields = vec![
        string("greeting", "Grüße 🌍", &[]),
        string("quotation", "say \"hello\"", &[]),
        string("slash", r"a\b", &[]),
    ];

    let actual = matches(
        fields,
        &[
            ("unicode", r#"{"greeting":["Grüße 🌍"]}"#),
            ("escaped-quote", r#"{"quotation":["say \"hello\""]}"#),
            ("escaped-slash", r#"{"slash":["a\\b"]}"#),
            ("not-a-literal", r#"{"greeting":[true]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["escaped-quote", "escaped-slash", "unicode"]);
}

#[test]
fn contract_numeric_equivalence_is_independent_of_wire_representation() {
    let fields = vec![
        number("integer", "42", &[]),
        number("decimal", "42.000", &[]),
        number("exponent", "4.2e1", &[]),
        string("numeric_string", "42", &[]),
    ];

    let actual = matches(
        fields,
        &[
            ("integer", r#"{"integer":[42]}"#),
            ("decimal", r#"{"decimal":[42]}"#),
            ("exponent", r#"{"exponent":[42]}"#),
            ("range", r#"{"decimal":[{"numeric":[">",41,"<=",42]}]}"#),
            ("wrong-type", r#"{"numeric_string":[42]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["decimal", "exponent", "integer", "range"]);
}

#[test]
fn contract_tracker_excludes_fields_that_no_pattern_references() {
    let fields = vec![
        string("visible", "yes", &[]),
        string("unreferenced\nsecret", "must not be emitted", &[]),
    ];

    let actual = matches(
        fields,
        &[
            ("visible", r#"{"visible":["yes"]}"#),
            ("missing", r#"{"missing":[{"exists":false}]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["missing", "visible"]);
}

#[test]
fn contract_field_emission_order_does_not_change_matching() {
    // Wire formats and schema libraries need not visit map/record fields in
    // pattern order. Quamina sorts the flattened result before matching.
    let fields = vec![
        number("z", "3", &[]),
        string("a\nnested", "first", &[]),
        literal("middle", "true"),
    ];

    let actual = matches(
        fields,
        &[(
            "all-fields",
            r#"{"a":{"nested":["first"]},"middle":[true],"z":[3]}"#,
        )],
    )
    .unwrap();

    assert_eq!(actual, vec!["all-fields"]);
}

#[test]
fn contract_flattener_copy_keeps_configuration_but_not_required_mutable_state() {
    let fields = vec![string("status", "active", &[])];
    let mut q = quamina_for(fields);
    q.add_pattern("active", r#"{"status":["active"]}"#).unwrap();
    let cloned = q.clone();

    assert_eq!(
        q.matches_for_event(b"first decoder state").unwrap(),
        ["active"]
    );
    assert_eq!(
        cloned
            .matches_for_event(b"independent decoder state")
            .unwrap(),
        ["active"]
    );
}

#[test]
fn contract_messagepack_and_cbor_text_maps_follow_pattern_object_paths() {
    // Proposed logical document in either self-describing format:
    // {"tenant":{"id":"acme"}, "priority":7, "ignored":"large value"}
    let fields = vec![
        string("tenant\nid", "acme", &[]),
        number("priority", "7", &[]),
        string("ignored", "large value", &[]),
    ];

    let actual = matches(
        fields,
        &[
            ("nested", r#"{"tenant":{"id":["acme"]}}"#),
            ("priority", r#"{"priority":[{"numeric":[">=",5]}]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["nested", "priority"]);
}

#[test]
fn contract_arrays_preserve_element_correlation() {
    // Proposed logical value:
    // {"orders":[{"sku":"A","quantity":1},{"sku":"B","quantity":2}]}
    //
    // Array id 1 identifies `orders`; positions 1 and 2 identify its two
    // elements. The exact starting position is not significant, but all fields
    // from one element must agree on it.
    let fields = vec![
        string("orders\nsku", "A", &[(1, 1)]),
        number("orders\nquantity", "1", &[(1, 1)]),
        string("orders\nsku", "B", &[(1, 2)]),
        number("orders\nquantity", "2", &[(1, 2)]),
    ];

    let actual = matches(
        fields,
        &[
            ("same-element", r#"{"orders":{"sku":["B"],"quantity":[2]}}"#),
            (
                "cross-element",
                r#"{"orders":{"sku":["A"],"quantity":[2]}}"#,
            ),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["same-element"]);
}

#[test]
fn contract_nested_arrays_carry_the_complete_array_trail() {
    // Proposed logical value:
    // {"shipments":[{"boxes":[{"sku":"A"},{"sku":"B"}]}]}
    // Array 1 is `shipments`; array 2 is the first shipment's `boxes`.
    let fields = vec![
        string("shipments\nboxes\nsku", "A", &[(1, 1), (2, 1)]),
        string("shipments\nboxes\nsku", "B", &[(1, 1), (2, 2)]),
    ];

    let actual = matches(
        fields,
        &[(
            "nested-repeated",
            r#"{"shipments":{"boxes":{"sku":["B"]}}}"#,
        )],
    )
    .unwrap();

    assert_eq!(actual, vec!["nested-repeated"]);
}

#[test]
fn contract_primitive_arrays_match_any_element() {
    let fields = vec![
        string("tags", "red", &[(1, 1)]),
        string("tags", "green", &[(1, 2)]),
        string("tags", "blue", &[(1, 3)]),
    ];

    let actual = matches(
        fields,
        &[
            ("middle", r#"{"tags":["green"]}"#),
            ("missing", r#"{"tags":["orange"]}"#),
            ("exists", r#"{"tags":[{"exists":true}]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["exists", "middle"]);
}

#[test]
fn contract_distinct_arrays_have_distinct_ids_and_do_not_conflict() {
    let fields = vec![
        string("wanted_regions", "west", &[(1, 1)]),
        string("blocked_skus", "B-2", &[(2, 2)]),
    ];

    let actual = matches(
        fields,
        &[(
            "independent-arrays",
            r#"{"wanted_regions":["west"],"blocked_skus":["B-2"]}"#,
        )],
    )
    .unwrap();

    assert_eq!(actual, vec!["independent-arrays"]);
}

#[test]
fn contract_parent_array_positions_prevent_nested_cross_element_matches() {
    // {"shipments":[
    //   {"region":"west", "boxes":[{"sku":"A"}]},
    //   {"region":"east", "boxes":[{"sku":"B"}]}
    // ]}
    let fields = vec![
        string("shipments\nregion", "west", &[(1, 1)]),
        string("shipments\nboxes\nsku", "A", &[(1, 1), (2, 1)]),
        string("shipments\nregion", "east", &[(1, 2)]),
        string("shipments\nboxes\nsku", "B", &[(1, 2), (3, 1)]),
    ];

    let actual = matches(
        fields,
        &[
            (
                "coherent",
                r#"{"shipments":{"region":["east"],"boxes":{"sku":["B"]}}}"#,
            ),
            (
                "cross-parent",
                r#"{"shipments":{"region":["west"],"boxes":{"sku":["B"]}}}"#,
            ),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["coherent"]);
}

#[test]
fn contract_empty_collections_emit_no_synthetic_leaf() {
    // Empty arrays/maps do not invent a value. Consequently they are absent at
    // the leaf-matching layer, matching the existing JSON flattener behavior.
    let actual = matches(
        Vec::new(),
        &[
            ("absent", r#"{"items":[{"exists":false}]}"#),
            ("present", r#"{"items":[{"exists":true}]}"#),
            ("synthetic-null", r#"{"items":[null]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["absent"]);
}

#[test]
fn contract_protobuf_uses_schema_names_presence_and_repeated_field_semantics() {
    // Proposed proto mapping:
    // - paths use .proto field names, not numeric tags;
    // - enums use symbolic names;
    // - unset fields are absent rather than materialized with proto3 defaults;
    // - repeated scalars behave like JSON array values.
    let fields = vec![
        string("task\nstatus", "RUNNING", &[]),
        string("labels", "urgent", &[(1, 1)]),
        string("labels", "customer-facing", &[(1, 2)]),
    ];

    let actual = matches(
        fields,
        &[
            ("enum-name", r#"{"task":{"status":["RUNNING"]}}"#),
            ("repeated", r#"{"labels":["urgent"]}"#),
            ("presence", r#"{"retry_count":[{"exists":false}]}"#),
            ("no-default", r#"{"retry_count":[0]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["enum-name", "presence", "repeated"]);
}

#[test]
fn contract_protobuf_maps_are_objects_and_repeated_messages_are_correlated() {
    let fields = vec![
        string("attributes\nregion", "us-west", &[]),
        string("attempts\nworker", "alpha", &[(1, 1)]),
        literal_with_trail("attempts\nsuccess", "false", &[(1, 1)]),
        string("attempts\nworker", "beta", &[(1, 2)]),
        literal_with_trail("attempts\nsuccess", "true", &[(1, 2)]),
    ];

    let actual = matches(
        fields,
        &[
            ("map-entry", r#"{"attributes":{"region":["us-west"]}}"#),
            (
                "same-message",
                r#"{"attempts":{"worker":["beta"],"success":[true]}}"#,
            ),
            (
                "cross-message",
                r#"{"attempts":{"worker":["alpha"],"success":[true]}}"#,
            ),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["map-entry", "same-message"]);
}

#[test]
fn contract_protobuf_oneof_emits_only_the_selected_field() {
    let fields = vec![string("email", "person@example.com", &[])];

    let actual = matches(
        fields,
        &[
            ("email", r#"{"email":["person@example.com"]}"#),
            ("phone-absent", r#"{"phone":[{"exists":false}]}"#),
            ("phone-default", r#"{"phone":[""]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["email", "phone-absent"]);
}

#[test]
fn contract_avro_unwraps_records_arrays_enums_and_nullable_unions() {
    // Proposed Avro mapping after schema-aware decoding:
    // - record fields become path segments;
    // - enum symbols become strings;
    // - a non-null union branch contributes its contained value;
    // - arrays use the common array-trail rules.
    let fields = vec![
        string("customer\ntier", "GOLD", &[]),
        string("note", "fragile", &[]),
        number("line_items\nprice", "12.50", &[(1, 1)]),
        number("line_items\nprice", "8", &[(1, 2)]),
    ];

    let actual = matches(
        fields,
        &[
            ("enum", r#"{"customer":{"tier":["GOLD"]}}"#),
            ("nullable-union", r#"{"note":["fragile"]}"#),
            (
                "array-number",
                r#"{"line_items":{"price":[{"numeric":["<",10]}]}}"#,
            ),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["array-number", "enum", "nullable-union"]);
}

#[test]
fn contract_avro_null_union_branch_is_an_explicit_null_value() {
    // Unlike an absent Protobuf field, an Avro record field whose selected
    // union branch is null exists and can match both null and exists:true.
    let fields = vec![literal("note", "null")];

    let actual = matches(
        fields,
        &[
            ("null", r#"{"note":[null]}"#),
            ("present", r#"{"note":[{"exists":true}]}"#),
            ("not-absent", r#"{"note":[{"exists":false}]}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["null", "present"]);
}

#[test]
fn contract_avro_maps_follow_dynamic_object_key_paths() {
    let fields = vec![
        string("properties\nregion", "eu-central", &[]),
        string("properties\nteam", "payments", &[]),
    ];

    let actual = matches(
        fields,
        &[(
            "map-values",
            r#"{"properties":{"region":["eu-central"],"team":["payments"]}}"#,
        )],
    )
    .unwrap();

    assert_eq!(actual, vec!["map-values"]);
}

#[test]
fn contract_headers_live_under_a_reserved_namespace() {
    // Header lookup is normalized to lowercase here. Repeated header values
    // share an array id so a pattern can match any one value.
    let fields = vec![
        string("headers\nx-tenant-id", "acme", &[]),
        string("headers\nx-role", "reader", &[(1, 1)]),
        string("headers\nx-role", "auditor", &[(1, 2)]),
    ];

    let actual = matches(
        fields,
        &[
            ("tenant-header", r#"{"headers":{"x-tenant-id":["acme"]}}"#),
            ("repeated-header", r#"{"headers":{"x-role":["auditor"]}}"#),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["repeated-header", "tenant-header"]);
}

#[test]
fn contract_binary_cloudevent_composes_attributes_and_decoded_data() {
    // This is the proposed transport-neutral view of a binary CloudEvent.
    // Context attributes are root fields and body fields are nested under
    // `data`, matching the shape of a structured-mode CloudEvent.
    let fields = vec![
        string("specversion", "1.0", &[]),
        string("type", "com.example.order.created", &[]),
        string("source", "/orders", &[]),
        string("id", "evt-123", &[]),
        string("datacontenttype", "application/msgpack", &[]),
        string("data\norder_id", "A-42", &[]),
        number("data\ntotal", "19.95", &[]),
    ];

    let actual = matches(
        fields,
        &[
            ("attribute", r#"{"type":["com.example.order.created"]}"#),
            (
                "attribute-and-data",
                r#"{"source":["/orders"],"data":{"total":[{"numeric":[">",10]}]}}"#,
            ),
        ],
    )
    .unwrap();

    assert_eq!(actual, vec!["attribute", "attribute-and-data"]);
}
