# Regexp Features Implementation Plan

This document outlines the incremental implementation of two missing regex features:
1. **Character Class Subtraction**: `[a-z-[aeiou]]`
2. **Word Boundaries**: `~b` and `~B`

## Design Principles

Follow existing quamina-rs patterns:
- **RuneRange operations** for character class manipulation (see `parser.rs:607-735`)
- **Lookaround rewriting** for zero-width assertions (see `parser.rs:758-850`)
- **TDD**: Write failing tests first, then implement
- **Incremental commits**: Each session produces working, tested code

## Feature 1: Character Class Subtraction

### Overview

Transform `[a-z-[aeiou]]` into a `RuneRange` that excludes the subtracted characters.

**Current behavior**: Parser treats `-[` as literal characters (incorrect)
**Target behavior**: Compute set difference: `[a-z] - [aeiou]` = `[bcdfghjklmnpqrstvwxyz]`

### Session 1.1: Add `subtract_rune_range` Function

**Location**: `src/regexp/parser.rs`

**Tasks**:
1. Add unit tests for `subtract_rune_range(base, subtract) -> RuneRange`
2. Implement the function using existing `invert_rune_range` as reference
3. Test edge cases: empty subtract, complete subtract, overlapping ranges

```rust
// Test cases to add:
#[test]
fn test_subtract_rune_range_basic() {
    let base = vec![RunePair { lo: 'a', hi: 'z' }];
    let subtract = vec![RunePair { lo: 'a', hi: 'a' }];
    let result = subtract_rune_range(base, subtract);
    assert_eq!(result, vec![RunePair { lo: 'b', hi: 'z' }]);
}

#[test]
fn test_subtract_rune_range_vowels() {
    let base = vec![RunePair { lo: 'a', hi: 'z' }];
    let vowels = vec![
        RunePair { lo: 'a', hi: 'a' },
        RunePair { lo: 'e', hi: 'e' },
        RunePair { lo: 'i', hi: 'i' },
        RunePair { lo: 'o', hi: 'o' },
        RunePair { lo: 'u', hi: 'u' },
    ];
    let result = subtract_rune_range(base, vowels);
    // Should match bcdfghjklmnpqrstvwxyz
}
```

**Commit**: `feat(regexp): add subtract_rune_range function with tests`

### Session 1.2: Parser Support for `-[` Syntax

**Location**: `src/regexp/parser.rs` in `read_class_expression()`

**Tasks**:
1. Add test that `parse_regexp("[a-z-[aeiou]]")` succeeds
2. Modify `read_class_expression` to detect `-[` after a range/char
3. Recursively parse the subtracted class
4. Apply `subtract_rune_range`

**Key code location**: Around line 1050-1150 in `read_class_expression()`

```rust
// Pseudo-code for the change:
// After collecting base_runes in the character class...
if parse.peek() == Some('-') && parse.peek_ahead(1) == Some('[') {
    parse.advance(); // consume '-'
    let subtract_runes = read_class_expression(parse)?; // recursive
    base_runes = subtract_rune_range(base_runes, subtract_runes);
}
```

**Commit**: `feat(regexp): parse character class subtraction syntax`

### Session 1.3: Enable Samples & Add Coverage

**Tasks**:
1. Update `regexp_samples.rs` samples 55, 59-61 from `valid: false` to `valid: true`
2. Add match/nomatch test data for these samples
3. Run full test suite including miri minimal tests
4. Add fuzz target if not already covering this code path

**Samples to enable** (from `regexp_samples.rs`):
- Sample 55: `[a-d-[b-c]]` - matches `a`, `d`; nomatches `b`, `c`
- Sample 59: `[a-b-[0-9]]+` - currently marked invalid, may stay invalid (subtract non-overlapping)
- Sample 60: `[a-c-[^a-c]]` - edge case with negation

**Commit**: `test(regexp): enable character class subtraction samples`

### Session 1.4: Nested Subtraction Support (Optional)

Support `[0-9-[0-6-[0-3]]]` = `[0-37-9]`

**Tasks**:
1. Add test for nested subtraction
2. Verify recursive parsing handles this
3. Add sample if not present

**Commit**: `feat(regexp): support nested character class subtraction`

---

## Feature 2: Word Boundaries

### Overview

Implement `~b` (word boundary) and `~B` (non-word boundary) as zero-width assertions.

**Strategy**: Rewrite `~b` and `~B` as equivalent lookaround expressions during parsing.

```
~b ≡ (?:(?<=~w)(?=~W)|(?<=~W)(?=~w)|^(?=~w)|(?<=~w)$)
~B ≡ (?:(?<=~w)(?=~w)|(?<=~W)(?=~W)|^(?=~W)|(?<=~W)$)
```

### Session 2.1: Define Word Boundary Expansion

**Location**: `src/regexp/parser.rs`

**Tasks**:
1. Add constants for word boundary expansion patterns
2. Add unit test that expansion produces valid AST
3. Verify existing lookaround tests pass

```rust
/// Expansion for ~b (word boundary)
/// Matches: word-to-nonword, nonword-to-word, start-if-word, end-if-word
const WORD_BOUNDARY_EXPANSION: &str =
    "(?:(?<=~w)(?=~W)|(?<=~W)(?=~w)|^(?=~w)|(?<=~w)$)";

/// Expansion for ~B (non-word boundary)
/// Matches: word-to-word, nonword-to-nonword, start-if-nonword, end-if-nonword
const NON_WORD_BOUNDARY_EXPANSION: &str =
    "(?:(?<=~w)(?=~w)|(?<=~W)(?=~W)|^(?=~W)|(?<=~W)$)";
```

**Commit**: `feat(regexp): define word boundary expansion patterns`

### Session 2.2: Parser Integration for `~b` and `~B`

**Location**: `src/regexp/parser.rs` in escape handling

**Tasks**:
1. Add test that `parse_regexp("~bfoo~b")` succeeds
2. Modify escape handling to expand `~b`/`~B` inline
3. Handle the expansion as a non-capturing group

**Key code location**: `check_multi_char_escape()` or `read_atom()` around line 738

```rust
// In read_atom() or similar, when handling ESCAPE:
'b' => {
    // Word boundary - expand to lookaround expression
    return parse_word_boundary_expansion(parse, false);
}
'B' => {
    // Non-word boundary - expand to lookaround expression
    return parse_word_boundary_expansion(parse, true);
}
```

**Commit**: `feat(regexp): parse ~b and ~B word boundaries`

### Session 2.3: Handle Edge Cases

**Tasks**:
1. Test `~b` at start of pattern: `~bword`
2. Test `~b` at end of pattern: `word~b`
3. Test `~B` in middle: `foo~Bbar`
4. Test combined: `~bword~b`
5. Verify UTF-8 handling (word chars include Unicode letters?)

**Decision point**: Should `~w` in word boundary use ASCII-only `[a-zA-Z0-9_]` or Unicode `~p{L}`?
- Recommend: ASCII-only for consistency with existing `~w` definition

**Commit**: `test(regexp): word boundary edge cases`

### Session 2.4: Enable Samples & Integration Tests

**Tasks**:
1. Remove `~b`/`~B` from `should_skip()` in `test_regexp_validity`
2. Update relevant samples in `regexp_samples.rs` to `valid: true`
3. Add match/nomatch test data

**Samples to enable** (grep for `~b` in regexp_samples.rs):
- `~bfoo` patterns
- `~Bfoo` patterns
- Combined patterns

**Commit**: `test(regexp): enable word boundary samples`

### Session 2.5: Performance & Miri Validation

**Tasks**:
1. Run miri tests to verify no UB in lookaround expansion
2. Add benchmark for word boundary patterns
3. Compare performance with/without word boundaries
4. Consider caching expanded patterns if slow

**Commit**: `perf(regexp): validate word boundary performance`

---

## Testing Checklist

### Unit Tests (per feature)
- [ ] Parser accepts new syntax
- [ ] Parser rejects malformed syntax
- [ ] NFA builds correctly
- [ ] Matching works for positive cases
- [ ] Matching rejects negative cases

### Integration Tests
- [ ] `test_regexp_validity` passes with new samples enabled
- [ ] End-to-end through Quamina API works

### Safety Tests
- [ ] `cargo miri test` passes (or miri-minimal variants)
- [ ] No new clippy warnings
- [ ] Fuzz targets cover new code paths (if applicable)

### CI Checklist
- [ ] All existing tests pass
- [ ] New tests pass
- [ ] Miri CI job passes
- [ ] Coverage doesn't decrease

---

## Sample Updates Reference

### Character Class Subtraction Samples

| Sample | Regex | Current | Target | Matches | NoMatches |
|--------|-------|---------|--------|---------|-----------|
| 55 | `[a-d-[b-c]]` | invalid | valid | `a`, `d` | `b`, `c`, `e` |
| 59 | `[a-b-[0-9]]+` | invalid | valid | `ab`, `aab` | `1`, `a1` |
| 60 | `[a-c-[^a-c]]` | invalid | valid | `a`, `b`, `c` | `d` |
| 61 | `[a-z-[^a]]` | invalid | valid | `a` | `b`, `z` |

### Word Boundary Samples

| Sample | Regex | Current | Target | Matches | NoMatches |
|--------|-------|---------|--------|---------|-----------|
| * | `~bfoo~b` | skipped | valid | `foo` | `foobar`, `afoo` |
| * | `~Boo~B` | skipped | valid | `foo` (middle) | `oo` |

---

## Estimated Effort

| Session | Feature | Effort | Cumulative |
|---------|---------|--------|------------|
| 1.1 | `subtract_rune_range` | 2h | 2h |
| 1.2 | Parser `-[` syntax | 3h | 5h |
| 1.3 | Enable samples | 2h | 7h |
| 1.4 | Nested subtraction | 1h | 8h |
| 2.1 | Word boundary expansion | 2h | 10h |
| 2.2 | Parser `~b`/`~B` | 3h | 13h |
| 2.3 | Edge cases | 2h | 15h |
| 2.4 | Enable samples | 2h | 17h |
| 2.5 | Perf validation | 1h | 18h |

**Total**: ~18 hours across 9 focused sessions

---

## References

- `src/regexp/parser.rs:607-735` - XML name char pattern (RuneRange)
- `src/regexp/parser.rs:1200-1280` - `invert_rune_range` implementation
- `src/regexp/parser.rs:758-850` - Lookaround parsing
- `src/tests_operators.rs:2487-2530` - Test skip logic to update
- [Character Class Subtraction](https://www.regular-expressions.info/charclasssubtract.html)
- [Word Boundaries](https://www.regular-expressions.info/wordboundaries.html)
