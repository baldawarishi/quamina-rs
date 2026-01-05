//! Benchmark comparing different JSON parsing approaches:
//! 1. Current streaming flattener (FlattenJson)
//! 2. simd-json value (DOM) API

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use simd_json::prelude::*;

fn load_status_json() -> Vec<u8> {
    std::fs::read("testdata/status.json").expect("Failed to read testdata/status.json")
}

// ============================================================================
// simd-json borrowed value (DOM) approach
// ============================================================================

fn simd_dom_extract<'a>(value: &'a simd_json::BorrowedValue<'a>, path: &str) -> Option<String> {
    let segments: Vec<&str> = path.split('.').collect();
    let mut current = value;

    for segment in segments {
        current = current.get(segment)?;
    }

    current.as_str().map(|s| s.to_string())
}

fn simd_dom_extract_multi<'a>(
    value: &'a simd_json::BorrowedValue<'a>,
    paths: &[&str],
) -> Vec<String> {
    let mut results = Vec::with_capacity(paths.len());
    for path in paths {
        if let Some(s) = simd_dom_extract(value, path) {
            results.push(s);
        }
    }
    results
}

// ============================================================================
// Benchmarks
// ============================================================================

fn bench_simd_parse_only(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd_parse_only");

    // Small JSON
    let small_json = r#"{"status": "active", "id": 123, "name": "test"}"#;
    group.bench_function("small", |b| {
        b.iter_batched(
            || small_json.as_bytes().to_vec(),
            |mut event| {
                let _: simd_json::BorrowedValue = simd_json::to_borrowed_value(&mut event).unwrap();
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Status.json (large)
    group.bench_function("status_json", |b| {
        b.iter_batched(
            load_status_json,
            |mut event| {
                let _: simd_json::BorrowedValue = simd_json::to_borrowed_value(&mut event).unwrap();
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // 50-field JSON
    let mut large_json = String::from("{");
    for i in 0..50 {
        if i > 0 {
            large_json.push_str(", ");
        }
        large_json.push_str(&format!(r#""field_{}": "value_{}""#, i, i));
    }
    large_json.push('}');

    group.bench_function("50_fields", |b| {
        b.iter_batched(
            || large_json.as_bytes().to_vec(),
            |mut event| {
                let _: simd_json::BorrowedValue = simd_json::to_borrowed_value(&mut event).unwrap();
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.finish();
}

fn bench_simd_extract(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd_extract");

    // Small JSON with 1 target field
    let small_json = r#"{"status": "active", "id": 123, "name": "test"}"#;
    group.bench_function("small_1_field", |b| {
        b.iter_batched(
            || small_json.as_bytes().to_vec(),
            |mut event| {
                let value: simd_json::BorrowedValue =
                    simd_json::to_borrowed_value(&mut event).unwrap();
                simd_dom_extract(black_box(&value), "status")
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Nested JSON
    let nested_json = r#"{
        "context": {
            "user": {
                "id": "123",
                "name": "alice"
            },
            "request_id": "abc123"
        },
        "payload": {
            "action": "click",
            "target": "button"
        }
    }"#;
    group.bench_function("nested_1_field", |b| {
        b.iter_batched(
            || nested_json.as_bytes().to_vec(),
            |mut event| {
                let value: simd_json::BorrowedValue =
                    simd_json::to_borrowed_value(&mut event).unwrap();
                simd_dom_extract(black_box(&value), "context.user.id")
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Status.json - context field (early in JSON)
    group.bench_function("status_context", |b| {
        b.iter_batched(
            load_status_json,
            |mut event| {
                let value: simd_json::BorrowedValue =
                    simd_json::to_borrowed_value(&mut event).unwrap();
                simd_dom_extract(black_box(&value), "context.user_id")
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Status.json - payload.user.id_str (middle nested)
    group.bench_function("status_middle_nested", |b| {
        b.iter_batched(
            load_status_json,
            |mut event| {
                let value: simd_json::BorrowedValue =
                    simd_json::to_borrowed_value(&mut event).unwrap();
                simd_dom_extract(black_box(&value), "payload.user.id_str")
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // 50-field JSON - extract 5 fields from different positions
    let mut large_json = String::from("{");
    for i in 0..50 {
        if i > 0 {
            large_json.push_str(", ");
        }
        large_json.push_str(&format!(r#""field_{}": "value_{}""#, i, i));
    }
    large_json.push('}');

    let paths = &["field_5", "field_15", "field_25", "field_35", "field_45"];
    group.bench_function("50_fields_extract_5", |b| {
        b.iter_batched(
            || large_json.as_bytes().to_vec(),
            |mut event| {
                let value: simd_json::BorrowedValue =
                    simd_json::to_borrowed_value(&mut event).unwrap();
                simd_dom_extract_multi(black_box(&value), paths)
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.finish();
}

fn bench_field_position_impact(c: &mut Criterion) {
    let mut group = c.benchmark_group("field_position");

    // Generate JSON with 50 fields
    let mut json = String::from("{");
    for i in 0..50 {
        if i > 0 {
            json.push_str(", ");
        }
        json.push_str(&format!(r#""field_{}": "value_{}""#, i, i));
    }
    json.push('}');

    // Test first field
    group.bench_with_input(
        BenchmarkId::new("simd_dom", "first"),
        &"field_0",
        |b, path| {
            b.iter_batched(
                || json.as_bytes().to_vec(),
                |mut event| {
                    let value: simd_json::BorrowedValue =
                        simd_json::to_borrowed_value(&mut event).unwrap();
                    simd_dom_extract(black_box(&value), path)
                },
                criterion::BatchSize::SmallInput,
            )
        },
    );

    // Test middle field
    group.bench_with_input(
        BenchmarkId::new("simd_dom", "middle"),
        &"field_25",
        |b, path| {
            b.iter_batched(
                || json.as_bytes().to_vec(),
                |mut event| {
                    let value: simd_json::BorrowedValue =
                        simd_json::to_borrowed_value(&mut event).unwrap();
                    simd_dom_extract(black_box(&value), path)
                },
                criterion::BatchSize::SmallInput,
            )
        },
    );

    // Test last field
    group.bench_with_input(
        BenchmarkId::new("simd_dom", "last"),
        &"field_49",
        |b, path| {
            b.iter_batched(
                || json.as_bytes().to_vec(),
                |mut event| {
                    let value: simd_json::BorrowedValue =
                        simd_json::to_borrowed_value(&mut event).unwrap();
                    simd_dom_extract(black_box(&value), path)
                },
                criterion::BatchSize::SmallInput,
            )
        },
    );

    group.finish();
}

criterion_group!(
    benches,
    bench_simd_parse_only,
    bench_simd_extract,
    bench_field_position_impact,
);
criterion_main!(benches);
