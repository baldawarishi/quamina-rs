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

## Next Steps

1. [ ] Add simd-json as optional dev-dependency
2. [ ] Create benchmark comparing all approaches
3. [ ] Test on various event sizes and field counts
4. [ ] Profile to understand where time is spent
5. [ ] Decide: adopt, hybrid, or stay with current approach
