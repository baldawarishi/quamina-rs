# Future flattener compile contracts

The Rust files in this directory describe proposed public APIs that do not
exist yet. Cargo does not automatically discover integration tests in nested
directories, so the ordinary test suite remains runnable while these files are
allowed to reference missing Quamina types and methods.

These are deliberately concrete tests rather than pseudocode placeholders.
When an API is implemented, move the corresponding file into `tests/`, adjust
only names that changed during implementation, and make its assertions pass.

The contracts are split by architectural boundary:

- `document_formats.rs`: self-describing MessagePack and CBOR documents.
- `schema_formats.rs`: descriptor-backed Protobuf and schema-backed Avro.
- `envelopes.rs`: headers and transport-neutral CloudEvents binary mode.
- `failure_and_limits.rs`: malformed input, unsupported native types, and
  resource limits shared by every decoder.
- `core_boundary.rs`: segment-safe paths, typed scalar construction, and array
  identity helpers needed to keep separate decoder implementations consistent.

The runnable semantic tests in `../flattener_contract.rs` are the source of
truth for the `OwnedField` output these future APIs must produce.

## Coverage matrix

| Area | Runnable semantics | Future compile contract |
|---|---|---|
| Canonical strings/numbers/bools/null | yes | typed value builder |
| Nested maps/records | yes | MessagePack, CBOR, Protobuf, Avro constructors |
| Primitive/repeated/nested arrays | yes | shared array identity allocator |
| Presence, null, empty collections | yes | format-neutral errors |
| Protobuf enum/map/oneof/repeated/presence | yes | descriptors and well-known types |
| Avro record/map/array/enum/union | yes | schemas, evolution, logical types |
| Headers | yes | per-event envelope API and normalization |
| CloudEvents binary mode | yes | transport normalization and media dispatch |
| Bytes, huge integers, non-text keys | output policy only | explicit policy/error tests |
| Malformed input and resource exhaustion | matcher boundary only | decoder errors and limits |
| Path separator collisions | not expressible safely today | segment-safe path test |
