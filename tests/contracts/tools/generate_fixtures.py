#!/usr/bin/env python3
"""Generate the dependency-free, deterministic flattener contract corpus."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import struct
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "fixtures"


CASES = [
    {
        "name": "scalars",
        "event": {
            "text": "42",
            "count": 42,
            "enabled": True,
            "deleted_at": None,
            "negative": -7,
        },
        "patterns": [
            ("string", {"text": ["42"]}),
            ("number", {"count": [42]}),
            ("exponent-equivalent", {"count": [4.2e1]}),
            ("boolean", {"enabled": [True]}),
            ("null", {"deleted_at": [None]}),
            ("negative", {"negative": [-7]}),
            ("wrong-type", {"text": [42]}),
        ],
        "matches": ["boolean", "exponent-equivalent", "negative", "null", "number", "string"],
        "protobuf_exclusions": ["deleted_at: Protobuf has no native null scalar"],
    },
    {
        "name": "escaped_unicode",
        "event": {
            "métadata": {
                "greeting": "Grüße 🌍",
                "quotation": 'say "hello"',
                "slash": "a\\b",
                "control": "line\nnext",
            },
            "a\nb": "literal newline key",
        },
        "patterns": [
            ("unicode", {"métadata": {"greeting": ["Grüße 🌍"]}}),
            ("quote", {"métadata": {"quotation": ['say "hello"']}}),
            ("backslash", {"métadata": {"slash": ["a\\b"]}}),
            ("control", {"métadata": {"control": ["line\nnext"]}}),
            ("newline-key", {"a\nb": ["literal newline key"]}),
        ],
        "matches": ["backslash", "control", "quote", "unicode"],
        "canonical_field_exclusions": [
            {
                "path_segments": ["a\nb"],
                "reason": "the current newline-delimited OwnedField path cannot distinguish this segment from nested ['a', 'b']; core-boundary promotes it after segment paths exist",
            }
        ],
        "format_exclusions": {
            "protobuf": "Protobuf field names cannot express the non-ASCII and newline keys",
            "avro": "Avro field names cannot express the non-ASCII and newline keys",
        },
    },
    {
        "name": "nested",
        "event": {"tenant": {"id": "acme", "region": "west"}, "priority": 7, "ignored": "large value"},
        "patterns": [
            ("nested", {"tenant": {"id": ["acme"]}}),
            ("numeric", {"priority": [{"numeric": [">=", 5]}]}),
            ("tracker-target", {"tenant": {"region": ["west"]}}),
        ],
        "matches": ["nested", "numeric", "tracker-target"],
        "canonical_field_exclusions": [
            {
                "path_segments": ["ignored"],
                "reason": "intentionally unreferenced by every pattern to assert tracker filtering",
            }
        ],
    },
    {
        "name": "primitive_arrays",
        "event": {"tags": ["red", "green", "blue"], "scores": [1, 2, 3]},
        "patterns": [
            ("middle", {"tags": ["green"]}),
            ("number-in-array", {"scores": [2]}),
            ("exists", {"tags": [{"exists": True}]}),
            ("missing", {"tags": ["orange"]}),
        ],
        "matches": ["exists", "middle", "number-in-array"],
    },
    {
        "name": "object_arrays_positive",
        "event": {"orders": [{"sku": "A", "quantity": 1}, {"sku": "B", "quantity": 2}]},
        "patterns": [
            ("same-element", {"orders": {"sku": ["B"], "quantity": [2]}}),
        ],
        "matches": ["same-element"],
    },
    {
        "name": "object_arrays_negative",
        "event": {"orders": [{"sku": "A", "quantity": 1}, {"sku": "B", "quantity": 2}]},
        "patterns": [
            ("cross-element", {"orders": {"sku": ["A"], "quantity": [2]}}),
        ],
        "matches": [],
    },
    {
        "name": "nested_arrays",
        "event": {
            "shipments": [
                {"region": "west", "boxes": [{"sku": "A"}, {"sku": "C"}]},
                {"region": "east", "boxes": [{"sku": "B"}]},
            ]
        },
        "patterns": [
            ("coherent", {"shipments": {"region": ["east"], "boxes": {"sku": ["B"]}}}),
            ("cross-parent", {"shipments": {"region": ["west"], "boxes": {"sku": ["B"]}}}),
            ("nested-repeated", {"shipments": {"boxes": {"sku": ["C"]}}}),
        ],
        "matches": ["coherent", "nested-repeated"],
    },
    {
        "name": "unrelated_arrays",
        "event": {"wanted_regions": ["west", "north"], "blocked_skus": ["B-1", "B-2"]},
        "patterns": [
            ("independent-arrays", {"wanted_regions": ["west"], "blocked_skus": ["B-2"]}),
        ],
        "matches": ["independent-arrays"],
    },
    {
        "name": "presence_empty",
        "event": {"explicit_null": None, "empty_array": [], "empty_map": {}, "present": True},
        "patterns": [
            ("explicit-null", {"explicit_null": [None]}),
            ("absent", {"absent_field": [{"exists": False}]}),
            ("empty-array-is-absent", {"empty_array": [{"exists": False}]}),
            ("empty-map-is-absent", {"empty_map": [{"exists": False}]}),
            ("present", {"present": [{"exists": True}]}),
        ],
        "matches": ["absent", "empty-array-is-absent", "empty-map-is-absent", "explicit-null", "present"],
        "protobuf_exclusions": ["explicit_null: Protobuf has no native null scalar"],
    },
    {
        "name": "operators_multiple",
        "event": {"name": "Prod-Service", "file": "photo.JPG", "status": "active", "count": 42, "exact": "yes"},
        "patterns": [
            ("exact", {"exact": ["yes"]}),
            ("prefix", {"name": [{"prefix": "Prod-"}]}),
            ("suffix", {"file": [{"suffix": ".JPG"}]}),
            ("wildcard", {"name": [{"wildcard": "Prod-*"}]}),
            ("ignore-case", {"name": [{"equals-ignore-case": "prod-service"}]}),
            ("anything-but", {"status": [{"anything-but": ["deleted", "archived"]}]}),
            ("numeric", {"count": [{"numeric": [">", 41, "<=", 42]}]}),
            ("exists", {"status": [{"exists": True}]}),
            ("combined", {"status": ["active"], "count": [42]}),
        ],
        "matches": ["anything-but", "combined", "exact", "exists", "ignore-case", "numeric", "prefix", "suffix", "wildcard"],
    },
    {
        "name": "cloudevent_data",
        "event": {"order_id": "A-42", "total": 19.95, "expedited": True},
        "patterns": [
            ("payload-order", {"order_id": ["A-42"]}),
            ("payload-total", {"total": [{"numeric": [">", 10]}]}),
        ],
        "matches": ["payload-order", "payload-total"],
        "canonical_field_exclusions": [
            {
                "path_segments": ["expedited"],
                "reason": "intentionally unreferenced payload leaf to assert tracker filtering during CloudEvents dispatch",
            }
        ],
        "cloudevent": {
            "specversion": "1.0",
            "id": "evt-123",
            "source": "/orders",
            "type": "com.example.order.created",
            "datacontenttype_by_format": {
                "json": "application/json",
                "messagepack": "application/msgpack",
                "cbor": "application/cbor",
                "protobuf": "application/protobuf",
                "avro": "application/avro",
            },
        },
    },
]


def json_bytes(value: object) -> bytes:
    return (json.dumps(value, ensure_ascii=False, separators=(",", ":")) + "\n").encode()


def uvarint(value: int) -> bytes:
    if value < 0:
        value &= (1 << 64) - 1
    out = bytearray()
    while value > 0x7F:
        out.append((value & 0x7F) | 0x80)
        value >>= 7
    out.append(value)
    return bytes(out)


def zigzag(value: int) -> int:
    return (value << 1) ^ (value >> 63)


def proto_field(number: int, wire_type: int, payload: bytes) -> bytes:
    key = uvarint((number << 3) | wire_type)
    return key + (uvarint(len(payload)) if wire_type == 2 else b"") + payload


def pb_string(number: int, value: str) -> bytes:
    return proto_field(number, 2, value.encode())


def pb_varint(number: int, value: int) -> bytes:
    return proto_field(number, 0, uvarint(value))


def pb_message(number: int, value: bytes) -> bytes:
    return proto_field(number, 2, value)


def msgpack(value: object) -> bytes:
    if value is None:
        return b"\xc0"
    if value is False:
        return b"\xc2"
    if value is True:
        return b"\xc3"
    if isinstance(value, int):
        if 0 <= value <= 0x7F:
            return bytes([value])
        if -32 <= value < 0:
            return bytes([value & 0xFF])
        if value >= 0:
            return b"\xcf" + struct.pack(">Q", value)
        return b"\xd3" + struct.pack(">q", value)
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ValueError("non-finite corpus number")
        return b"\xcb" + struct.pack(">d", value)
    if isinstance(value, str):
        data = value.encode()
        if len(data) < 32:
            return bytes([0xA0 | len(data)]) + data
        return b"\xdb" + struct.pack(">I", len(data)) + data
    if isinstance(value, list):
        prefix = bytes([0x90 | len(value)]) if len(value) < 16 else b"\xdd" + struct.pack(">I", len(value))
        return prefix + b"".join(msgpack(item) for item in value)
    if isinstance(value, dict):
        prefix = bytes([0x80 | len(value)]) if len(value) < 16 else b"\xdf" + struct.pack(">I", len(value))
        return prefix + b"".join(msgpack(key) + msgpack(item) for key, item in value.items())
    raise TypeError(type(value))


def cbor_head(major: int, value: int) -> bytes:
    if value < 24:
        return bytes([(major << 5) | value])
    if value <= 0xFF:
        return bytes([(major << 5) | 24, value])
    if value <= 0xFFFF:
        return bytes([(major << 5) | 25]) + struct.pack(">H", value)
    if value <= 0xFFFFFFFF:
        return bytes([(major << 5) | 26]) + struct.pack(">I", value)
    return bytes([(major << 5) | 27]) + struct.pack(">Q", value)


def cbor(value: object) -> bytes:
    if value is None:
        return b"\xf6"
    if value is False:
        return b"\xf4"
    if value is True:
        return b"\xf5"
    if isinstance(value, int):
        return cbor_head(0, value) if value >= 0 else cbor_head(1, -1 - value)
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ValueError("non-finite corpus number")
        return b"\xfb" + struct.pack(">d", value)
    if isinstance(value, str):
        data = value.encode()
        return cbor_head(3, len(data)) + data
    if isinstance(value, list):
        return cbor_head(4, len(value)) + b"".join(cbor(item) for item in value)
    if isinstance(value, dict):
        return cbor_head(5, len(value)) + b"".join(cbor(key) + cbor(item) for key, item in value.items())
    raise TypeError(type(value))


def canonical_scalar(value: object) -> tuple[str, bool]:
    if isinstance(value, str):
        # Quamina's matcher representation is a type-tagging quote byte on
        # each side of decoded UTF-8. Interior quotes, backslashes, and control
        # bytes are decoded content, not JSON re-escapes.
        return f'"{value}"', False
    if value is None:
        return "null", False
    if isinstance(value, bool):
        return "true" if value else "false", False
    if isinstance(value, int):
        return str(value), True
    if isinstance(value, float):
        return json.dumps(value, allow_nan=False, separators=(",", ":")), True
    raise TypeError(type(value))


def flatten(event: object, excluded_paths: set[tuple[str, ...]] | None = None) -> list[dict[str, object]]:
    fields: list[dict[str, object]] = []
    next_array_id = 1
    excluded_paths = excluded_paths or set()

    def visit(value: object, path: list[str], trail: list[dict[str, int]]) -> None:
        nonlocal next_array_id
        if isinstance(value, dict):
            for key, child in value.items():
                visit(child, path + [key], trail)
        elif isinstance(value, list):
            array_id = next_array_id
            next_array_id += 1
            for pos, child in enumerate(value, 1):
                visit(child, path, trail + [{"array": array_id, "pos": pos}])
        else:
            if tuple(path) in excluded_paths:
                return
            scalar, is_number = canonical_scalar(value)
            fields.append({
                "path_segments": path,
                "legacy_newline_path": "\n".join(path),
                "scalar": scalar,
                "scalar_utf8_hex": scalar.encode().hex(),
                "is_number": is_number,
                "array_trail": trail,
            })

    visit(event, [], [])
    return fields


# Each tuple is (field name, protobuf kind, repeated). Message kinds name a
# package-level message; "map_string_string" is emitted as a real map entry.
PROTO_DEFS: dict[str, list[tuple[str, str, bool]]] = {
    "Scalars": [("text", "string", False), ("count", "int64", False), ("enabled", "bool", False), ("negative", "sint64", False)],
    "Tenant": [("id", "string", False), ("region", "string", False)],
    "Nested": [("tenant", "Tenant", False), ("priority", "int64", False), ("ignored", "string", False)],
    "PrimitiveArrays": [("tags", "string", True), ("scores", "int64", True)],
    "Order": [("sku", "string", False), ("quantity", "int64", False)],
    "ObjectArraysPositive": [("orders", "Order", True)],
    "ObjectArraysNegative": [("orders", "Order", True)],
    "Box": [("sku", "string", False)],
    "Shipment": [("region", "string", False), ("boxes", "Box", True)],
    "NestedArrays": [("shipments", "Shipment", True)],
    "UnrelatedArrays": [("wanted_regions", "string", True), ("blocked_skus", "string", True)],
    "PresenceEmpty": [("present", "bool", False)],
    "OperatorsMultiple": [("name", "string", False), ("file", "string", False), ("status", "string", False), ("count", "int64", False), ("exact", "string", False)],
    "CloudeventData": [("order_id", "string", False), ("total", "double", False), ("expedited", "bool", False)],
}

CASE_PROTO = {
    "scalars": "Scalars", "nested": "Nested", "primitive_arrays": "PrimitiveArrays",
    "object_arrays_positive": "ObjectArraysPositive", "object_arrays_negative": "ObjectArraysNegative",
    "nested_arrays": "NestedArrays", "unrelated_arrays": "UnrelatedArrays", "presence_empty": "PresenceEmpty",
    "operators_multiple": "OperatorsMultiple", "cloudevent_data": "CloudeventData",
}


def proto_kind_type(kind: str) -> int:
    return {"double": 1, "int64": 3, "bool": 8, "string": 9, "sint64": 18}.get(kind, 11)


def encode_proto_message(message_name: str, value: dict[str, object]) -> bytes:
    out = bytearray()
    for number, (name, kind, repeated) in enumerate(PROTO_DEFS[message_name], 1):
        if name not in value:
            continue
        values = value[name] if repeated else [value[name]]
        assert isinstance(values, list)
        for item in values:
            if kind == "string":
                out += pb_string(number, str(item))
            elif kind == "bool":
                out += pb_varint(number, int(bool(item)))
            elif kind == "int64":
                out += pb_varint(number, int(item))
            elif kind == "sint64":
                out += pb_varint(number, zigzag(int(item)))
            elif kind == "double":
                out += proto_field(number, 1, struct.pack("<d", float(item)))
            else:
                assert isinstance(item, dict)
                out += pb_message(number, encode_proto_message(kind, item))
    return bytes(out)


def descriptor_bytes() -> bytes:
    descriptors = bytearray()
    for message_name, fields in PROTO_DEFS.items():
        message = bytearray(pb_string(1, message_name))
        for number, (name, kind, repeated) in enumerate(fields, 1):
            field = bytearray(pb_string(1, name))
            field += pb_varint(3, number)
            field += pb_varint(4, 3 if repeated else 1)
            field += pb_varint(5, proto_kind_type(kind))
            if proto_kind_type(kind) == 11:
                field += pb_string(6, f".quamina.contract.{kind}")
            field += pb_string(10, name)
            message += pb_message(2, bytes(field))
        descriptors += pb_message(4, bytes(message))
    file_descriptor = pb_string(1, "corpus.proto") + pb_string(2, "quamina.contract") + descriptors + pb_string(12, "proto3")
    return pb_message(1, file_descriptor)


def proto_source() -> str:
    lines = ['syntax = "proto3";', "", "package quamina.contract;", ""]
    for message_name, fields in PROTO_DEFS.items():
        lines.append(f"message {message_name} {{")
        for number, (name, kind, repeated) in enumerate(fields, 1):
            label = "repeated " if repeated else ""
            lines.append(f"  {label}{kind} {name} = {number};")
        lines.extend(["}", ""])
    return "\n".join(lines)


AVRO_SCHEMAS: dict[str, dict[str, object]] = {
    "scalars": {"type": "record", "name": "Scalars", "namespace": "quamina.contract", "fields": [
        {"name": "text", "type": "string"}, {"name": "count", "type": "long"}, {"name": "enabled", "type": "boolean"},
        {"name": "deleted_at", "type": ["null", "string"]}, {"name": "negative", "type": "long"}]},
    "nested": {"type": "record", "name": "Nested", "namespace": "quamina.contract", "fields": [
        {"name": "tenant", "type": {"type": "record", "name": "Tenant", "fields": [{"name": "id", "type": "string"}, {"name": "region", "type": "string"}]}},
        {"name": "priority", "type": "long"}, {"name": "ignored", "type": "string"}]},
    "primitive_arrays": {"type": "record", "name": "PrimitiveArrays", "namespace": "quamina.contract", "fields": [
        {"name": "tags", "type": {"type": "array", "items": "string"}}, {"name": "scores", "type": {"type": "array", "items": "long"}}]},
    "object_arrays_positive": {"type": "record", "name": "ObjectArraysPositive", "namespace": "quamina.contract", "fields": [
        {"name": "orders", "type": {"type": "array", "items": {"type": "record", "name": "PositiveOrder", "fields": [{"name": "sku", "type": "string"}, {"name": "quantity", "type": "long"}]}}}]},
    "object_arrays_negative": {"type": "record", "name": "ObjectArraysNegative", "namespace": "quamina.contract", "fields": [
        {"name": "orders", "type": {"type": "array", "items": {"type": "record", "name": "NegativeOrder", "fields": [{"name": "sku", "type": "string"}, {"name": "quantity", "type": "long"}]}}}]},
    "nested_arrays": {"type": "record", "name": "NestedArrays", "namespace": "quamina.contract", "fields": [
        {"name": "shipments", "type": {"type": "array", "items": {"type": "record", "name": "Shipment", "fields": [
            {"name": "region", "type": "string"}, {"name": "boxes", "type": {"type": "array", "items": {"type": "record", "name": "Box", "fields": [{"name": "sku", "type": "string"}]}}}]}}}]},
    "unrelated_arrays": {"type": "record", "name": "UnrelatedArrays", "namespace": "quamina.contract", "fields": [
        {"name": "wanted_regions", "type": {"type": "array", "items": "string"}}, {"name": "blocked_skus", "type": {"type": "array", "items": "string"}}]},
    "presence_empty": {"type": "record", "name": "PresenceEmpty", "namespace": "quamina.contract", "fields": [
        {"name": "explicit_null", "type": ["null", "string"]}, {"name": "empty_array", "type": {"type": "array", "items": "string"}},
        {"name": "empty_map", "type": {"type": "map", "values": "string"}}, {"name": "present", "type": "boolean"}]},
    "operators_multiple": {"type": "record", "name": "OperatorsMultiple", "namespace": "quamina.contract", "fields": [
        {"name": "name", "type": "string"}, {"name": "file", "type": "string"}, {"name": "status", "type": "string"},
        {"name": "count", "type": "long"}, {"name": "exact", "type": "string"}]},
    "cloudevent_data": {"type": "record", "name": "CloudeventData", "namespace": "quamina.contract", "fields": [
        {"name": "order_id", "type": "string"}, {"name": "total", "type": "double"}, {"name": "expedited", "type": "boolean"}]},
}


def avro_long(value: int) -> bytes:
    return uvarint(zigzag(value))


def avro_encode(schema: object, value: object) -> bytes:
    if isinstance(schema, list):
        branch = 0 if value is None else 1
        return avro_long(branch) + avro_encode(schema[branch], value)
    if isinstance(schema, str):
        if schema == "null": return b""
        if schema == "boolean": return bytes([1 if value else 0])
        if schema in ("int", "long"): return avro_long(int(value))
        if schema == "double": return struct.pack("<d", float(value))
        if schema in ("string", "bytes"):
            data = value.encode() if isinstance(value, str) else bytes(value)
            return avro_long(len(data)) + data
        raise ValueError(f"unsupported Avro schema {schema!r}")
    assert isinstance(schema, dict)
    kind = schema["type"]
    if kind == "record":
        assert isinstance(value, dict)
        return b"".join(avro_encode(field["type"], value[field["name"]]) for field in schema["fields"])
    if kind == "array":
        assert isinstance(value, list)
        return (avro_long(len(value)) + b"".join(avro_encode(schema["items"], item) for item in value) + avro_long(0)) if value else avro_long(0)
    if kind == "map":
        assert isinstance(value, dict)
        body = b"".join(avro_encode("string", key) + avro_encode(schema["values"], item) for key, item in value.items())
        return (avro_long(len(value)) + body + avro_long(0)) if value else avro_long(0)
    return avro_encode(kind, value)


def create_outputs() -> dict[str, bytes]:
    outputs: dict[str, bytes] = {}
    # Compatibility alias used by the smallest compile contract. Other cases
    # always reference their exact per-case writer schema.
    outputs["avro/corpus.avsc"] = json_bytes(AVRO_SCHEMAS["scalars"])
    manifest_cases = []
    for case in CASES:
        name = case["name"]
        event = case["event"]
        patterns_doc = {"case": name, "patterns": [{"id": pattern_id, "pattern": pattern} for pattern_id, pattern in case["patterns"]], "expected_match_ids": case["matches"]}
        outputs[f"json/{name}.json"] = json_bytes(event)
        outputs[f"patterns/{name}.json"] = json_bytes(patterns_doc)
        outputs[f"messagepack/{name}.msgpack"] = msgpack(event)
        outputs[f"cbor/{name}.cbor"] = cbor(event)

        refs: dict[str, object] = {
            "json": f"json/{name}.json", "patterns": f"patterns/{name}.json",
            "messagepack": f"messagepack/{name}.msgpack", "cbor": f"cbor/{name}.cbor",
        }
        exclusions = dict(case.get("format_exclusions", {}))
        if name in CASE_PROTO:
            outputs[f"protobuf/{name}.pb"] = encode_proto_message(CASE_PROTO[name], event)
            refs["protobuf"] = {"wire": f"protobuf/{name}.pb", "descriptor": "protobuf/corpus.desc", "message": f"quamina.contract.{CASE_PROTO[name]}"}
        else:
            exclusions.setdefault("protobuf", "logical field names are not representable in a .proto schema")
        if name in AVRO_SCHEMAS:
            schema_path = f"avro/{name}.avsc"
            outputs[schema_path] = json_bytes(AVRO_SCHEMAS[name])
            outputs[f"avro/{name}.avro"] = avro_encode(AVRO_SCHEMAS[name], event)
            refs["avro"] = {"wire": f"avro/{name}.avro", "writer_schema": schema_path, "framing": "raw datum"}
        else:
            exclusions.setdefault("avro", "logical field names are not representable in an Avro schema")
        canonical_exclusions = case.get("canonical_field_exclusions", [])
        excluded_paths = {tuple(exclusion["path_segments"]) for exclusion in canonical_exclusions}
        case_entry = {
            "name": name,
            "logical_event": event,
            "expected_match_ids": case["matches"],
            "expected_canonical_fields": flatten(event, excluded_paths),
            "fixtures": refs,
            "format_exclusions": exclusions,
        }
        if case.get("protobuf_exclusions"):
            case_entry["protobuf_field_exclusions"] = case["protobuf_exclusions"]
            case_entry["expected_match_ids_by_format"] = {
                "protobuf": [match_id for match_id in case["matches"] if match_id not in {"null", "explicit-null"}]
            }
        if canonical_exclusions:
            case_entry["canonical_field_exclusions"] = canonical_exclusions
        if case.get("cloudevent"):
            case_entry["cloudevent_binary_envelope"] = case["cloudevent"]
        manifest_cases.append(case_entry)

    outputs["protobuf/corpus.proto"] = proto_source().encode()
    outputs["protobuf/corpus.desc"] = descriptor_bytes()
    corpus = {
        "schema_version": 1,
        "canonical_field_policy": {
            "paths": "ordered UTF-8 segment arrays; legacy_newline_path is diagnostic only and is ambiguous",
            "strings": "matcher-compatible bytes: one quote type tag, decoded UTF-8 content without JSON re-escaping, then one quote type tag",
            "numbers": "finite base-10 JSON number spelling with is_number=true",
            "booleans_and_null": "unquoted JSON literals with is_number=false",
            "arrays": "preorder positive array IDs and one-based positions; nested leaves carry complete parent-to-child trails",
            "empty_collections": "emit no synthetic leaf and therefore have absent-leaf exists semantics",
        },
        "selected_policies": {
            "map_keys": "reject non-text keys",
            "duplicate_keys": "reject; never collapse",
            "non_finite_numbers": "reject",
            "unknown_tagged_values": "reject unless explicitly configured",
            "schemas": "Protobuf descriptors and Avro writer schemas are required",
            "protobuf_names": "use proto field names; enum symbols are strings; do not synthesize absent defaults",
            "avro_union_null": "present explicit null",
            "cloud_events_dispatch": "exact normalized media type; never guess",
        },
        "cases": manifest_cases,
    }
    outputs["corpus.json"] = (json.dumps(corpus, ensure_ascii=False, indent=2) + "\n").encode()
    digest_lines = [f"{hashlib.sha256(data).hexdigest()}  {path}" for path, data in sorted(outputs.items())]
    outputs["MANIFEST.sha256"] = ("\n".join(digest_lines) + "\n").encode()
    return outputs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="verify checked-in fixtures without writing")
    args = parser.parse_args()
    outputs = create_outputs()
    stale = []
    for relative, expected in sorted(outputs.items()):
        path = FIXTURES / relative
        if args.check:
            if not path.is_file() or path.read_bytes() != expected:
                stale.append(relative)
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(expected)
    expected_paths = {FIXTURES / relative for relative in outputs}
    actual_paths = {path for path in FIXTURES.rglob("*") if path.is_file()}
    extras = sorted(str(path.relative_to(FIXTURES)) for path in actual_paths - expected_paths)
    if args.check and (stale or extras):
        for relative in stale:
            print(f"stale or missing: {relative}", file=sys.stderr)
        for relative in extras:
            print(f"unexpected generated file: {relative}", file=sys.stderr)
        return 1
    if not args.check:
        for path in actual_paths - expected_paths:
            path.unlink()
        print(f"generated {len(outputs) - 2} fixture artifacts plus corpus.json and MANIFEST.sha256")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
