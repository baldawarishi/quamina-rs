# Experiment: simd-json Integration

## Background

After implementing the segment-based JSON flattener (10-12x speedup), exploring whether SIMD-accelerated JSON parsing could provide additional gains.

## Resources

- **Paper**: [Parsing Gigabytes of JSON per Second](https://arxiv.org/abs/1902.08318) (Langdale & Lemire, 2019)
- **Crate**: [simd-json](https://crates.io/crates/simd-json) - Rust port of simdjson
- **Docs**: https://docs.rs/simd-json

## Key Insights from Paper

- simdjson uses SIMD (Single Instruction, Multiple Data) to process multiple bytes simultaneously
- Achieves **4x fewer instructions** than traditional parsers like RapidJSON
- First parser to achieve **gigabytes/second** on single core
- Key technique: vectorized character classification and structural index building

## simd-json APIs

| API | Description | Use Case |
|-----|-------------|----------|
| **Values API** | DOM-like (borrowed/owned) | Full document access |
| **Serde API** | Type-driven deserialization | Structured data |
| **Tape API** | Sequential access to elements | Selective extraction |
| **Lazy module** | On-demand field access | Partial parsing |

## Experiment Design

### Hypothesis

SIMD-accelerated parsing might outperform our current field-skipping approach, especially for:
- Events with many fields (SIMD processes 32+ bytes at once)
- Deeply nested structures
- Large JSON documents

### Trade-offs to Evaluate

| Approach | Pros | Cons |
|----------|------|------|
| Current (streaming) | Skips unused fields, low memory | Single-byte processing |
| simd-json (tape) | SIMD speed, selective access | Must scan full document |
| simd-json (DOM) | Full access, fast | Higher memory usage |

### Benchmark Plan

```rust
// Add to Cargo.toml
[dependencies]
simd-json = { version = "0.17", optional = true }

[features]
simd = ["simd-json"]
```

Test cases:
1. **Small event, few fields**: `{"status": "active"}` - 1 pattern field
2. **Large event, few fields**: 50-field event, 1 pattern field (our sweet spot)
3. **Large event, many fields**: 50-field event, 20 pattern fields
4. **Deeply nested**: 5 levels deep, field at various depths
5. **CityLots**: Real-world large documents

### Implementation Options

#### Option A: Replace flattener entirely
```rust
#[cfg(feature = "simd")]
fn flatten_with_simd(event: &mut [u8], tree: &SegmentsTree) -> Vec<Field> {
    let tape = simd_json::to_tape(event).unwrap();
    // Extract only fields in tree
}
```

#### Option B: Hybrid approach
- Use simd-json for initial structural parsing (find field boundaries)
- Use our SegmentsTree logic for field selection
- Skip string decoding for unused fields

#### Option C: simd-json's lazy API
```rust
use simd_json::lazy::Value;
// Only deserialize fields we need
```

## Platform Considerations

- **x86_64**: AVX2 or SSE4.2 (auto-detected)
- **aarch64**: NEON instructions
- **wasm**: simd128 when available
- **Fallback**: Scalar implementation (slower than our current approach)

## Questions to Answer

1. Does simd-json's tape API allow true selective parsing, or does it scan everything?
2. What's the memory overhead of the tape representation?
3. How does performance compare on ARM (M1/M2) vs x86?
4. Is the simd-json dependency size acceptable? (it's a larger crate)

## Experiment Results (2026-01-05)

Benchmarks run on Apple Silicon (M-series), using simd-json 0.14 with NEON instructions.

### Benchmark Results

| Benchmark | Current Flattener | simd-json DOM | Winner |
|-----------|------------------|---------------|--------|
| status_context_fields (early field) | **658 ns** | 6,640 ns | Current (10x faster) |
| status_middle_nested | 7,132 ns | 6,675 ns | simd-json (~5% faster) |
| status_last_field | 6,828 ns | 6,675 ns | simd-json (~2% faster) |
| small_json parse | - | 220 ns | - |
| 50_fields parse | - | 1,239 ns | - |
| 50_fields extract_5 | - | 1,458 ns | - |

### Key Findings

1. **Early termination is our superpower**: For fields early in the JSON, our streaming flattener is ~10x faster because it stops parsing after finding needed fields. simd-json always parses the entire document.

2. **simd-json wins at end of document**: When the target field is near the end, simd-json's SIMD parsing is slightly faster (~5%) since both approaches must scan most of the document anyway.

3. **Parse cost dominates**: simd-json's DOM approach has O(1) field lookup, but the full-document parsing cost (~6.6µs for status.json) is the bottleneck. Our streaming approach avoids this.

4. **Field position independence in simd-json**: Extracting from first/middle/last field takes identical time (~1.3µs for 50-field JSON) after parsing, confirming O(1) lookup.

### Answers to Original Questions

1. **Does simd-json allow selective parsing?** No - the tape/DOM APIs always scan the full document. SIMD accelerates scanning but doesn't skip content.

2. **Memory overhead?** simd-json's borrowed value holds references to input buffer plus structural metadata. Acceptable for most use cases.

3. **ARM (M1/M2) performance?** Uses NEON instructions effectively. Results above are from Apple Silicon.

4. **Dependency size?** simd-json adds several dependencies (value-trait, halfbrown, simdutf8, etc.). Larger footprint than current approach.

### Conclusion

**Stay with current streaming flattener.** The early termination optimization provides significant wins for typical quamina use cases where patterns match a small subset of fields early in the document.

simd-json would only help if:
- Most patterns target fields at the end of large documents
- Many fields need extraction from each event (amortizes parse cost)
- Events are very large and dense (SIMD shines on bulk processing)

None of these match quamina's typical usage pattern.

## Next Steps

1. [x] Add simd-json as dev-dependency
2. [x] Create benchmark comparing all approaches
3. [x] Test on various event sizes and field counts
4. [x] Profile to understand where time is spent
5. [x] Decide: adopt, hybrid, or stay with current approach

**Decision: Keep current approach. Remove simd-json experiment code before release.**
