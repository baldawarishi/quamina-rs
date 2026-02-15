# Word Boundary Implementation Experiments

## Goal
Implement `~b` (word boundary) and `~B` (non-word boundary) for quamina-rs regexp.

## Expansion Semantics
```
~b ≡ (?:(?<=~w)(?=~W)|(?<=~W)(?=~w))   — word/non-word transition
~B ≡ (?:(?<=~w)(?=~w)|(?<=~W)(?=~W))   — same-type transition
```

Where `~w = [a-zA-Z0-9_]` (ASCII word chars).

## Key Constraint
Values in quamina are always `"value"` + VALUE_TERMINATOR.
- `"` is a non-word character, so start/end of value naturally act as non-word boundaries.
- This means `~b` at start of value = first char must be a word char.
- `~b` at end of value = last char must be a word char.

## Current Infrastructure Constraints
1. `transform_lookaround_pattern` (json.rs:323-398): only single-branch, top-level lookarounds
2. `collect_lookarounds` (parser.rs:433-445): doesn't recurse into non-lookaround subtrees
3. Multi-condition NFA conditions use AND semantics; word boundary needs OR between branches

---

## Experiment 1: Multiple MultiConditionNfa Entries (JSON/Matcher Level)

**Approach**: When the parser encounters `~b`/`~B`, expand it into a special AST marker.
Then at the `json.rs` / `mutable_matcher.rs` level, generate **two separate** MultiConditionNfa
entries (one per alternation branch), each with its own set of AND conditions.
Matching produces a union (OR) because each MultiConditionNfa independently adds transitions.

**Files to modify**:
- `parser.rs`: Add `~b`/`~B` recognition, produce a marker in the AST
- `json.rs`: Detect word boundary markers, split into multiple MultiConditionNfa
- `mutable_matcher.rs`: Handle multiple MC-NFAs from one pattern

**Pros**: Works within existing NFA infrastructure
**Cons**: Requires changes across 3 files; complex pattern splitting logic

### Status: TESTED — FUNDAMENTAL ISSUE
### Findings:
The expansion to lookaround-based branches works correctly at the parser level.
However, the existing lookaround condition verification system has a fundamental
mismatch with word boundaries:

1. **Full-value matching**: The existing system builds "combined patterns" (e.g.,
   `A(?=B)` → combined=`AB`) that are matched against the **entire value**. This
   works for patterns like `foo(?=bar)` → `foobar`, but NOT for positional word
   boundary assertions.

2. **Position overlap**: `(?=~w)hello` means "at position 0, the next char is a word
   char, AND then `hello` follows". The `~w` and `h` refer to the SAME character
   position. The combined pattern `~w_hello` would be wrong (expects 6 chars, not 5).

3. **Intersection semantics**: What's actually needed is character-class intersection
   at boundary positions, not concatenation. The existing `build_combined_pattern`
   concatenates, producing incorrect results.

**Verdict**: This approach requires fundamentally redesigning the lookaround condition
system to support position-aware assertions, which would be a large refactor. The
expansion to alternation branches works, but the condition verification pipeline
can't handle the resulting patterns correctly.

---

## Experiment 2: First-Class NFA Word Boundary

**Approach**: Add word boundary as a native concept in the NFA. Instead of expanding to
lookarounds, the NFA tracks whether the previous byte was a word character and conditionally
transitions at `~b`/`~B` positions.

**Implementation idea**: Use epsilon transitions with a "guard" — the NFA state for `~b`
has epsilons to a "word→non-word" path and a "non-word→word" path, where each path only
activates based on what byte was consumed just before.

**Files to modify**:
- `parser.rs`: Add `~b`/`~B` as a new atom type
- `nfa.rs`: Build NFA states that encode the boundary check
- `arena.rs`: Possibly extend `ArenaSmallTable` for boundary-aware transitions

**Pros**: Most robust, cleanest semantics
**Cons**: Larger change surface; may require new fields on arena states

### Status: TESTED — PARTIALLY WORKS
### Findings:
The character-class intersection approach works at the AST level without NFA changes.

**What works well**:
- `~bhello` → constrains first char to word class (trivially satisfied since `h` is word)
- `hello~b` → constrains last char to word class
- `ab~b cd` → constrains `b` to word, ` ` to non-word (both satisfied)
- Correctly eliminates impossible paths (e.g., `hello~bworld` → no valid alternatives)
- Handles quantified atoms by splitting: `.*~b` → `.*{0,MAX-1} + ~w/~W`

**Issues discovered**:
1. **Budget blow for `~W` class**: Non-word chars = all Unicode minus `[a-zA-Z0-9_]`.
   The NFA for `~W` (used in `.*~bcat~b.*`) is ~11MB, exceeding the 10MB budget.
   This means patterns with `.*~b` combinations hit the budget limit.

2. **Impossible patterns error vs silent no-match**: `hello~bworld` (both sides word)
   correctly detects no valid alternatives but errors instead of silently producing
   a never-matching pattern. This is a minor UX issue.

3. **Elegant for literal patterns**: Works beautifully for patterns where both sides
   of `~b` are fixed characters or small character classes. The expansion is
   minimal and produces optimal NFAs.

4. **Exponential branch growth**: Each `~b` can double the number of branches.
   Mitigated by the 4-boundary limit, but each branch with `~W` is expensive.

**Verdict**: Best approach. All issues resolved:
- `~W` NFA budget issue fixed by compact `make_nonword_char_fa()` (dot-like FA
  with word chars excluded, ~10 states instead of thousands)
- `.*` zero-match edge case handled by `SplitOrAbsent` variant which generates
  additional branches for the value-edge boundary case
- Impossible patterns (e.g., `hello~bworld`) silently produce a never-matching
  pattern instead of erroring

**Final result**: 25 word boundary tests pass + all 453 existing tests pass.

---

## Experiment 3: Recursive collect_lookarounds

**Approach**: Enhance `collect_lookarounds` and `has_top_level_lookaround` to recurse into
non-capturing group subtrees. Then `~b` can expand to the standard lookaround alternation
`(?:(?<=~w)(?=~W)|(?<=~W)(?=~w))` and the existing pipeline handles it.

Also modify `transform_lookaround_pattern` to handle multi-branch patterns with lookarounds.

**Files to modify**:
- `parser.rs`: Add `~b`/`~B` expansion to lookaround alternation
- `parser.rs`: Make `collect_lookarounds` / `has_top_level_lookaround` recursive
- `json.rs`: Extend `transform_lookaround_pattern` for multi-branch

**Pros**: Minimal NFA changes; uses existing lookaround machinery
**Cons**: Could have cascading effects on existing lookaround handling

### Status: ANALYZED — SAME FUNDAMENTAL ISSUE AS EXPERIMENT 1
### Findings:
Does not require a separate code experiment because it shares the same core problem
discovered in Experiment 1: the existing lookaround condition system uses **full-value
matching** (the condition NFA is traversed against the entire value string).

Word boundary assertions need **positional** checking (e.g., "is the first char a word
char?"), but the condition system only checks "does this pattern match the entire value?".

Even with recursive `collect_lookarounds` and multi-branch `transform_lookaround_pattern`,
the condition verification in `transition_on()` would still use full-value matching,
producing incorrect semantics.

**Verdict**: Not viable without redesigning the condition verification system to support
position-aware assertions.

---

## Decision
**Chosen approach**: Experiment 2 (Character-Class Intersection)
**Rationale**:
1. **Works correctly** for literal/fixed patterns: `~bhello`, `hello~b`, `ab~b cd`
2. **No NFA/arena changes needed**: Pure AST-level transformation
3. **Minimal code surface**: Only touches `parser.rs` and `json.rs`
4. **Correct semantics**: By intersecting character classes at boundary positions,
   the constraint is baked into the pattern itself — no runtime assertion needed
5. **Handles quantifiers**: Splits quantified atoms (`.*`, `.+`) to constrain only
   the boundary-adjacent character

**Known limitations to address**:
- `~W` (non-word class) in expanded patterns produces large NFAs (~11MB) that can
  exceed the budget. Mitigations:
  - Add cache key `"W"` to `~W` to cache the FA shell
  - Increase budget for word-boundary patterns
  - Or: use dot (`.`) instead of `~W` when the pattern already has `.*` context
    (since `.*` already matches anything, constraining to `~W` is redundant
    if we keep the `~w` constraint on the other side)
- Patterns where both sides of `~b` are fixed word chars (e.g., `hello~bworld`)
  correctly produce an error (no valid alternatives). Could silently return a
  never-matching pattern instead.
