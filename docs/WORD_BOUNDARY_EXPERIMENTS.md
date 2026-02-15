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

### Status: NOT STARTED
### Findings:
(to be filled after experiment)

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

### Status: NOT STARTED
### Findings:
(to be filled after experiment)

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

### Status: NOT STARTED
### Findings:
(to be filled after experiment)

---

## Decision
**Chosen approach**: (to be filled after all experiments)
**Rationale**: (to be filled)
