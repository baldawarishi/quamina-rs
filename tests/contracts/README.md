# Future flattener golden and compile contracts

This directory is a standalone, test-only crate for developing non-JSON
flatteners without changing the behavior of the root crate's ordinary test
suite. Its fixed fixtures and expected fields define the proposed boundary;
the crate does not provide production decoders.

JSON is the behavioral baseline. A format is complete only when its decoded
event has both the same sorted pattern-ID set as JSON and the same canonical
fields for every non-excluded logical value. Match-only success is insufficient
because a wrong scalar tag or array trail can accidentally satisfy one pattern.

## Commands

Run these commands from the repository root.

```sh
# Existing Quamina suite; the nested contract crate is not discovered here.
cargo test

# Formatting for the root and nested sources.
cargo fmt -- --check
cargo fmt --manifest-path tests/contracts/Cargo.toml -- --check

# Runnable harness, manifest/fixture checks, and JSON golden baseline.
cargo test --manifest-path tests/contracts/Cargo.toml

# Deterministically verify every checked-in fixture without rewriting it.
python3 tests/contracts/tools/generate_fixtures.py --check

# Intentionally regenerate fixtures after reviewing generator/schema changes.
python3 tests/contracts/tools/generate_fixtures.py

# Shared safe decoder-boundary compile contract.
cargo test --manifest-path tests/contracts/Cargo.toml --features core-boundary

# One decoder or envelope capability at a time.
cargo test --manifest-path tests/contracts/Cargo.toml --features messagepack
cargo test --manifest-path tests/contracts/Cargo.toml --features cbor
cargo test --manifest-path tests/contracts/Cargo.toml --features protobuf
cargo test --manifest-path tests/contracts/Cargo.toml --features avro
cargo test --manifest-path tests/contracts/Cargo.toml --features headers
cargo test --manifest-path tests/contracts/Cargo.toml --features cloudevents

# All compile contracts together. This is useful after the individual loops.
cargo test --manifest-path tests/contracts/Cargo.toml --features all-formats
```

The no-feature command must compile and pass at every revision. Each named
feature deliberately makes its capability's contract visible to Rust. Until
that API exists in `quamina`, a feature command is expected to stop at a
missing Quamina import, type, error variant, or method. A syntax error, an
unreadable fixture, generated source that is absent, or a dependency/network
failure is a broken contract harness rather than an expected failure.

The current inventory is 12 runnable no-feature tests and 89 feature-gated
compile contracts: 14 core-boundary, 14 MessagePack, 16 CBOR, 13 Protobuf, 12
Avro, 10 headers, and 10 CloudEvents tests. The four document/schema format
suites include a whole-corpus comparison that checks sorted matches and exact
canonical fields against JSON for every representable case.

`all-formats` enables `core-boundary`, all four payload formats, headers, and
CloudEvents. CloudEvents payload-dispatch cases therefore become fully runnable
only after its selected payload decoders are available. Ordinary tests never
enable these feature gates implicitly.

## Feature-to-implementation loop

Use one feature as a narrow implementation loop:

1. Run its command above and keep the first missing Quamina symbol or failing
   assertion in view.
2. Add the smallest production API or behavior that satisfies that contract.
3. Re-run the same command until its canonical-field, matching, malformed
   input, and resource-limit cases pass.
4. Run the no-feature command and root `cargo test` to catch regressions.
5. Run `all-formats` once dependencies between capabilities are implemented.

A test is promoted from a compile contract by preserving its fixture and
assertions, replacing only names that were intentionally changed during API
review, and moving/gating it with the capability's runnable integration tests.
Promotion is complete only when the test reaches decoder execution and passes;
turning a missing operation into a vacuous assertion or suppressing the test is
not promotion. Intentionally unsupported native values remain executable tests
and must return `UnsupportedEventValue`, `UnsupportedFormatFeature`, or the
equivalent final public error.

## Canonical logical corpus

Each case has a stable name and records:

- one format-neutral logical event;
- JSON patterns and the sorted expected matching IDs;
- expected path segments, typed scalar, matcher bytes, `is_number`, and the
  complete outer-to-inner array trail;
- a checked-in wire fixture for every applicable encoding;
- a descriptor/schema reference for Protobuf and Avro; and
- a named exclusion when an encoding cannot represent the logical value
  without changing its meaning.

The logical corpus is shared. A format adapter selects applicable fixture bytes
but does not redefine expected matches or fields. Pattern results are sorted
when ordering is not part of the API contract. Wire bytes are always read from
the repository; tests perform no network access and do not generate a fresh
event during comparison.

| Stable case | Primary purpose |
|---|---|
| `scalars` | string, boolean, null, integer, decimal/exponent equivalence, and string-versus-number distinction |
| `escaped_unicode` | Unicode values and field names, quotes, backslashes, and control escaping |
| `nested` | nested object/record/message paths and emission order independent of pattern order |
| `primitive_arrays` | primitive/repeated scalar values and array positions |
| `object_arrays_positive` | fields from one object element correlate and match |
| `object_arrays_negative` | fields from different object elements cannot form a match |
| `nested_arrays` | complete parent and child array trails |
| `unrelated_arrays` | unrelated arrays receive nonconflicting identities |
| `presence_empty` | absent versus explicit null and empty map/array exists semantics |
| `operators_multiple` | exact, prefix, suffix, wildcard, equals-ignore-case, anything-but, numeric, exists, and multiple matching IDs |
| `cloudevent_data` | a CloudEvents-shaped event whose decoded `data` uses the same logical payload fields |

Tracker filtering and deliberately reordered field emission are harness-wide
properties because they concern decoder traversal rather than an additional
logical event. Format-specific exceptional values remain outside the equality
corpus and have fixed malformed/unsupported wire cases in their feature suite.

### Canonical field representation

The durable representation is a typed field, not a decoder-written
`OwnedField` byte tuple:

- A path is an ordered sequence of UTF-8 segments. `['a', 'b']` and
  `['a\nb']` are distinct. Conversion to the matcher's legacy separator form
  happens centrally only after validation; decoders do not concatenate path
  bytes.
- `String` contains decoded Unicode. Its matcher bytes use the existing
  matcher-compatible string type tag: surrounding quotes around decoded UTF-8
  content, without re-escaping interior content. Construction is centralized
  so decoders cannot hand-write this representation, and `is_number` is false.
- `Number` is finite and losslessly canonicalizable under Quamina's numeric
  comparison semantics. Its matcher bytes are the central numeric form and
  `is_number` is true. Integer, decimal, and exponent spellings that denote the
  same supported number compare equally.
- `Bool` becomes `true` or `false`, and `Null` becomes `null`; both have
  `is_number` false.
- Unsupported binary, tagged, extension, logical, or schema values are either
  converted by an explicitly selected collision-free policy or rejected. They
  never masquerade as a string, number, boolean, or null.
- An array trail is an ordered snapshot of `(array_id, position)` pairs from
  outermost to innermost array. Golden IDs are allocated in positive preorder
  and positions are one-based; both are bounded. Fields emitted from the same
  element share the pair; unrelated arrays have different IDs; nested fields
  retain every parent pair.
- Empty arrays and maps emit no synthetic scalar leaf. At the existing JSON
  leaf-matching boundary they therefore satisfy `exists:false`, not
  `exists:true`; explicit null emits a present `Null` field.
- Field construction consults `SegmentsTreeTracker` before retaining an
  unreferenced scalar, while the decoder still parses enough structure to
  validate lengths, nesting, duplicate keys, and allocation limits.

Before custom fields reach legacy unchecked UTF-8 paths, the boundary validates
path encoding and ambiguity, scalar encoding/tag agreement, array trails,
duplicate fields, and configured size limits. A violation is a deterministic
event error and cannot panic or leak partial matches.

## Policy decisions

These defaults are proposals shared by every contract. A future configuration
may add an explicit alternative without weakening the default tests.

- Roots are objects, maps, records, or messages. Root scalars and arrays are
  rejected, and trailing top-level values are rejected.
- Map keys must be text. Duplicate keys are rejected before tracker filtering;
  no last-write or first-write collapse is allowed.
- Numbers must preserve Quamina numeric semantics. Non-finite floats,
  out-of-policy large integers, imprecise conversions, and invalid decimal
  forms are rejected.
- Binary values are rejected by default. An opt-in base64 representation is a
  typed policy and is tested for collision safety.
- Unknown MessagePack extensions, CBOR tags/simple values, and Avro logical
  values are rejected unless a named policy defines a stable representation.
  MessagePack timestamps and recognized CBOR date/time tags are likewise
  policy-controlled rather than guessed.
- Protobuf requires a descriptor and uses proto source field names by default,
  not `json_name`. Enums are symbolic strings. Unset proto3 scalars and absent
  proto2/optional/message/oneof fields remain absent; schema defaults are not
  synthesized unless a named presence policy requests them. Unknown fields are
  skipped only after their wire encoding is safely validated. A raw root
  message is the default; length-delimited roots require an explicit input
  policy.
- Avro requires a writer schema. Reader-schema resolution, raw datum, object
  container, and single-object encodings are explicit input modes. A selected
  union null is present null. Multi-branch ambiguity, unknown fingerprints,
  unsupported codecs/logical types, and unavailable schemas are errors.
- Header names use a reserved `headers` namespace and ASCII lowercase
  normalization. Repeated values remain distinct ordered values; HTTP comma
  joining is not performed implicitly. HTTP quoted/percent decoding is
  explicit and strict; surrounding optional whitespace is normalized. Kafka
  non-UTF-8 values are rejected under the default UTF-8 policy. Empty values
  are present empty strings. Conflicting `Content-Type` values are rejected.
- A metadata/payload path collision is rejected; metadata is not allowed to
  overwrite payload data. Segment construction prevents separator collisions.
- CloudEvents input is a transport-neutral envelope. HTTP `ce-*` and Kafka
  `ce_*` bindings normalize to the same attributes. CloudEvents 1.0 binary mode
  is accepted; 0.3 is rejected unless explicitly enabled. Structured and batch
  modes return a concrete unsupported-mode error in this binary-mode API.
- CloudEvents `Content-Type` becomes `datacontenttype`; media type and parameter
  handling is standards-aware and case-insensitive where appropriate. Payload
  dispatch uses an explicit registry and never sniffs bytes. Unknown media
  types are errors by default; an explicit metadata-only policy may match
  attributes without inventing `data` fields.
- Decoded payload fields live below `data`; context and extension attributes
  remain at the event root. Absent data, an empty body, and a Kafka tombstone
  remain distinguishable inputs.
- Every format enforces maximum depth, fields, scalar bytes, container items,
  and total allocation. Limit and structural checks still apply to branches
  skipped by the tracker.

## Fixture regeneration

The corpus has 79 checked-in generated files: 77 fixture/schema/pattern
artifacts plus `fixtures/corpus.json` and `fixtures/MANIFEST.sha256`. Binary
fixtures are the test inputs; wire data is grouped under `fixtures/json`,
`fixtures/messagepack`, `fixtures/cbor`, `fixtures/protobuf`, and
`fixtures/avro`, with JSON patterns under `fixtures/patterns`. The Protobuf
schema and descriptor are
`fixtures/protobuf/corpus.proto` and `fixtures/protobuf/corpus.desc`; the Avro
schema is `fixtures/avro/corpus.avsc`.

Regeneration is deterministic: pinned schemas/descriptors and logical values
are encoded in a fixed order, timestamps are constants, maps are sorted before
encoding, and no randomness or network source participates. Verify without
writing:

```sh
python3 tests/contracts/tools/generate_fixtures.py --check
```

After an intentional corpus, schema, or generator change, regenerate and then
review both the binary diff and manifest:

```sh
python3 tests/contracts/tools/generate_fixtures.py
python3 tests/contracts/tools/generate_fixtures.py --check
cargo test --manifest-path tests/contracts/Cargo.toml
```

The `--check` invocation never writes; a mismatch is reported with a nonzero
exit status.

Schema and descriptor sources are committed beside their generated bytes.
Protobuf descriptor sets are generated with source information excluded and a
fixed file order. Shared Avro fixtures are raw datums encoded from their
per-case committed writer schemas. Future object-container or single-object
fixtures must fix metadata order, sync values, and schema fingerprints. If a
local encoder version cannot reproduce identical bytes, do not replace the
corpus: record the exact tool version and regeneration command with the fixture
change.

## Coverage matrix: shared golden behavior

Legend: **golden** is exercised by the no-feature JSON/canonical harness;
**compile** is selected by the named feature and becomes an integration test
when its Quamina API exists; **N/A** is a documented representation exclusion.

| Behavior | JSON | MessagePack | CBOR | Protobuf | Avro | Headers | CloudEvents |
|---|---|---|---|---|---|---|---|
| String, bool, explicit null | golden | compile | compile | compile | compile | string policy | payload compile |
| Integer, decimal, exponent equivalence | golden | compile | compile | compile | compile | string only | payload compile |
| `"42"` distinct from `42` | golden | compile | compile | compile | compile | string only | payload compile |
| Unicode, quotes, slash, controls, non-ASCII field name | golden | compile | compile | N/A: schema name rules | N/A: schema name rules | normalization compile | payload compile |
| Nested map/record/message | golden | compile | compile | compile | compile | namespace compile | `data` compile |
| Primitive array/repeated scalar | golden | compile | compile | compile | compile | repeated values | payload compile |
| Array of objects/messages/records | golden | compile | compile | compile | compile | N/A | payload compile |
| Same-element positive / cross-element negative | golden | compile | compile | compile | compile | repeated scalar only | payload compile |
| Nested complete parent/child trails | golden | compile | compile | compile | compile | N/A | payload compile |
| Unrelated array IDs do not conflict | golden | compile | compile | compile | compile | compile | payload compile |
| Absent versus explicit null | golden | compile | compile | presence compile | union compile | absent/empty compile | data-state compile |
| Empty map/array exists semantics | golden | compile | compile | repeated/message compile | compile | N/A | payload compile |
| Emission order independent of pattern order | golden | compile | compile | compile | compile | compile | compile |
| Tracker filters unreferenced leaves safely | golden | compile | compile | compile | compile | compile | compile |
| Exact/prefix/suffix/wildcard/equals-ignore-case | golden | compile | compile | compile | compile | compile | compile |
| Anything-but/numeric/exists | golden | compile | compile | compile | compile | exists/string | compile |
| Multiple patterns for one event | golden | compile | compile | compile | compile | compile | compile |
| Same sorted match set as JSON | baseline | compile | compile | compile | compile | N/A | decoded data compile |
| Canonical paths/bytes/tags/trails | golden | compile | compile | compile | compile | compile | compile |

## Coverage matrix: core boundary and hostile custom fields

| Contract | `core-boundary` expectation |
|---|---|
| Segment path distinguishes nested path from embedded newline | distinct `FieldPath` values and matcher paths |
| Typed values and central escaping | valid canonical matcher bytes only |
| Central numeric canonicalization | finite, lossless bytes with consistent numeric tag |
| Array allocation and trail snapshots | unique bounded IDs; complete stable trails |
| Tracker-aware construction | omit unreferenced leaf allocation without skipping safety checks |
| Format-neutral error | format plus offset/location when available and retained source |
| Resource limits | depth, fields, scalar, items, and total allocation enforced |
| Invalid UTF-8 path / newline ambiguity | deterministic validation error |
| Invalid scalar bytes or representation | deterministic validation error |
| Numeric tag on non-number / string tag on number | deterministic type-tag error |
| Duplicate/conflicting array ID | deterministic trail error |
| Negative/overflowing position | deterministic trail error |
| Duplicate field | deterministic duplicate-field error |
| Oversized path/value | deterministic limit error |
| Every hostile case | no panic, unchecked access, or partial matches |

## Coverage matrix: format-specific contracts

| Capability | Selected contract groups |
|---|---|
| MessagePack | nil/bool; signed and unsigned boundaries; f32/f64; strings; non-finite rejection; binary policy; arrays/maps; non-text and duplicate keys; extensions and timestamp; truncated lengths; invalid markers; trailing values; depth and declared-size attacks |
| CBOR | definite/indefinite arrays and maps; chunked text/bytes; positive/negative integers; half/f32/f64; non-finite rejection; text/non-text/duplicate keys; simple/undefined; known/unknown tags; date/time, bignum, decimal fraction, bigfloat, and shared/cycle tags; noncanonical encodings; truncation/trailing/depth/allocation attacks |
| Protobuf | descriptor-required construction; proto versus JSON names; every scalar wire type; packed/unpacked repeated; nested/recursive messages; correlated repeated messages; string/non-string maps; enums and unknown values; proto2 required/optional/default; proto3 scalar/optional/message presence; oneof; unknown fields/tags; malformed varint/length; raw/length-delimited root; Timestamp, Duration, wrappers, Any, Struct, Value, ListValue; descriptor cycles/preprocessing; bytes; clone isolation |
| Avro | writer and optional reader schemas; records/nested/maps/arrays and correlation; null/nullable/multi-branch unions; enums/evolution; fixed/bytes/decimal; date/time/timestamps/local timestamp/duration/UUID; aliases/defaults; recursive names; positive/negative array-map blocks; raw datum/container/single-object; codec support or explicit error; fingerprints and missing resolution; malformed blocks/truncation/trailing/allocation limits |
| Headers | reserved namespace; case normalization; repeated order; no implicit comma join; HTTP/Kafka UTF-8 policies; quoted and percent decoding; whitespace; empty values; duplicate content type; count/aggregate limits; segment collision; metadata/payload collision |
| CloudEvents | transport-neutral envelope; HTTP/Kafka parity; required attributes; 1.0 and explicit 0.3 policy; extensions/canonical attribute types; binding names; content type mapping and media parameters; explicit decoder registry for JSON/MessagePack/CBOR/Protobuf/Avro; unknown type and metadata-only policy; absent/empty/tombstone data; conflicting headers; HTTP decoding; structured/batch behavior; `data` nesting; combined attribute/payload patterns |

## Error and resource contract shared by every decoder

For arbitrary malformed bytes, the public match call returns an error and does
not panic. No matches or partially constructed fields escape after an error.
The error names the event format and includes a byte offset or equivalent
schema/path location when available, while preserving the underlying source.
The suites also exercise root policy, trailing data, all five resource limits,
collision-free handling of unsupported values, and tracker-skipped hostile
containers.

| Error/resource behavior | MessagePack | CBOR | Protobuf | Avro | Headers | CloudEvents |
|---|---|---|---|---|---|---|
| Arbitrary malformed input returns error without panic | contract | contract | contract | contract | malformed envelope | malformed envelope/body |
| No partial match after decoder error | contract | contract | contract | contract | contract | contract |
| Format identified in error | contract | contract | contract | contract | envelope format | envelope and payload format |
| Byte offset or equivalent location when available | contract | contract | wire/schema location | block/schema location | header name/index | attribute/body location |
| Root scalar/array policy explicit | map-only | map-only | message root mode | datum root mode | N/A | binary envelope mode |
| Trailing values/data rejected | contract | contract | contract | contract | N/A | delegated to payload decoder |
| Maximum depth | contract | contract | contract | contract | path depth | metadata plus payload |
| Maximum fields | contract | contract | contract | contract | header count | combined field count |
| Maximum scalar bytes | contract | contract | contract | contract | value bytes | attribute/body scalar bytes |
| Maximum container items | contract | contract | contract | contract | repeated/count limit | envelope plus payload |
| Maximum total allocation | contract | contract | contract | contract | aggregate header bytes | combined allocation |
| Unsupported native values are collision-free | contract | contract | contract | contract | byte policy | registry/payload policy |
| Tracker skipping retains structural safety | contract | contract | contract | contract | contract | contract |

## Current expected state

The no-feature JSON/canonical suite is the stable runnable baseline. Feature
commands are compile contracts until the corresponding public Quamina boundary
and flattener types are implemented; their current compilation failures are
the expected implementation queue, not evidence that fixture generation is
required at test time.

| Feature | Contract source | Expected missing Quamina API |
|---|---|---|
| `core-boundary` | `tests/core_boundary.rs` | `FieldPath`, `CanonicalValue`, `CanonicalField`, `ArrayTrailBuilder`, `FieldSetBuilder`, `PatternFieldTracker`, `DecoderBoundary`, `RawField`, `RawArrayPos`, `EventFormat`, `EventLimits`, and validation/error variants |
| `messagepack` | `tests/messagepack.rs` | `MessagePackFlattener`, document policies, limits, and format-neutral decoder errors |
| `cbor` | `tests/cbor.rs` | `CborFlattener`, map/binary/tag/numeric policies, limits, and format-neutral decoder errors |
| `protobuf` | `tests/protobuf.rs` | `ProtobufFlattener`, descriptor/input/name/presence/enum/well-known-type policies, limits, and errors |
| `avro` | `tests/avro.rs` | `AvroFlattener`, schema/input/evolution/logical/codec/fingerprint policies, limits, and errors |
| `headers` | `tests/headers.rs` | `Envelope`, `HeadersFlattener`, header normalization/value policies, envelope matching, limits, and errors |
| `cloudevents` | `tests/cloudevents.rs` | `BinaryCloudEventFlattener`, `FlattenerRegistry`, transport/envelope and unknown-media-type policies, payload dispatch, and errors |
| `all-formats` | all sources above | the union of the seven rows above; fix an individual feature first for clearer compiler output |

When a feature first passes, update this section and the matrix cells from
**compile** to **golden** in the same change so this document continues to
describe executable reality.

## Post-implementation omission backlog

The items below are deliberately sequenced after the shared boundary and the
seven primary capabilities pass their existing contract commands. They are not
required to unblock the first MessagePack, CBOR, Protobuf, Avro, headers, or
CloudEvents binary implementations. Once all primary commands pass, revisit
each item explicitly and either promote its asserted unsupported behavior to a
supported golden case or record the final permanent exclusion with rationale.

1. Remove the current JSON/legacy-`OwnedField` exclusion for a literal newline
   inside one path segment. After segment-safe paths are used end to end, add a
   positive JSON, MessagePack, and CBOR golden assertion distinguishing
   `["a", "b"]` from `["a\nb"]`.
2. Revisit schema-format field-name exclusions in `escaped_unicode`. Protobuf
   and Avro cannot directly declare the corpus's non-ASCII/newline field names;
   after their baseline decoders pass, decide whether a named mapping policy is
   worthwhile or retain the exclusions as permanent schema constraints.
3. Revisit Protobuf's lack of native explicit null after its normal presence
   semantics pass. Consider only an explicit wrapper/`Value` mapping policy;
   never synthesize null from an absent scalar. Until then, the scalar and
   presence cases retain their named Protobuf field/match exclusions.
4. Decide which optional Avro object-container codecs to support after raw
   datum, container framing, and single-object decoding are stable. Every codec
   left unsupported must continue returning `UnsupportedFormatFeature`.
5. Consider opt-in representations for currently rejected MessagePack
   extensions, CBOR tagged/simple/shared-reference values, and Avro logical
   types only after ordinary canonical scalar behavior is complete. Defaults
   remain rejection, and any opt-in must stay collision-free.
6. Add CloudEvents structured and batch modes only after binary-mode transport
   normalization and all registered payload decoders pass. Until then, both
   modes retain concrete unsupported-mode errors.
7. Reassess whether any non-finite or wider-than-canonical numeric type can be
   represented without weakening Quamina numeric equivalence. If not, keep NaN,
   infinity, and lossy numbers as permanent explicit errors.
8. Keep headers outside cross-format document equality because their native
   domain is ordered string/byte pairs. After the primary headers suite passes,
   revisit optional non-UTF-8 encodings and HTTP list parsing only as named,
   transport-specific policies.
