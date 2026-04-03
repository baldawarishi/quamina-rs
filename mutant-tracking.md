# Mutation Testing Gap Closure — Tracking (Issue #41)

## Completed Sessions (Summary)

### Sessions 1–5 — Done, PRs merged
- **Session 1** (Groups 1+2): Config fix + arena stats/utility tests. 697→697 tests. Excluded kani proofs (13 mutants). Added 10 tests covering `ArenaStats::add`, `estimated_byte_size`, `with_capacity`, etc.
- **Session 2** (Group 3): Arena production logic. 697→706 tests. 9 tests in `tests_operators.rs` covering merge/spinout/traverse/monocase/IPv6/anything-but/clone/epsilon-closure.
- **Session 3** (Group 4 — PR #85): flatten_json.rs UTF-8 encoding. 706→718 tests. 12 tests for 2-byte/3-byte/surrogate-pair/boundary unicode escapes in member names and values.
- **Session 4** (Group 5 — PR #86): json.rs IPv6 CIDR + parse_string. 718→725 tests. 7 tests for IPv6 double-colon, group limits, right-side arithmetic, non-byte-aligned prefix, `\b`/`\f` escapes, unicode BMP, surrogate pair.
- **Session 5** (Group 6 — PR #87): regexp/parser + mutable_matcher + nfa. 725→749 tests. 24 tests covering lookbehind fixed-length validation, word boundary expansion, branch_fixed_length quantifiers, lookbehind integration.

---

## CI Run — 2026-03-31 (run 23783248014, branch: test/mutants-group6-parser-matcher-nfa)

Manual workflow_dispatch run. 8 shards. Shards 1–5 failed (missed mutants). **57 unique missed mutations** found.

---

## Missed Mutations — Full List

### Semantically equivalent — NO test needed

These mutations produce identical observable behavior; document here so future runs don't re-investigate.

| File:Line | Mutation | Reason |
|-----------|----------|--------|
| `flatten_json.rs:711:44` | `\|` → `^` in `read_member_name_with_escapes` | UTF-8 encoding: `0x80 \| x` == `0x80 ^ x` when x ≤ 0x3F (no bit overlap) |
| `flatten_json.rs:722:52` | `\|` → `^` | Same — surrogate pair 4th byte encoding |
| `flatten_json.rs:724:52` | `\|` → `^` | Same — surrogate pair 3rd byte |
| `flatten_json.rs:816:43` | `\|` → `^` in `read_string_with_escapes` | Same — 2-byte UTF-8 2nd byte |
| `flatten_json.rs:827:51` | `\|` → `^` | Same — surrogate 4-byte 1st byte (`0xF0 \|`) |
| `flatten_json.rs:829:51` | `\|` → `^` | Same — surrogate 4-byte 3rd byte |
| `flatten_json.rs:835:43` | `\|` → `^` | Same — 3-byte UTF-8 1st byte (`0xE0 \|`) |
| `flatten_json.rs:837:43` | `\|` → `^` | Same — 3-byte UTF-8 3rd byte |
| `flatten_json.rs:1003:18` | `==` → `!=` in `error` | Error format string comparison only (previously documented) |
| `arena.rs:308:31` | `>` → `>=` in `ArenaStats::add` max_ceilings | When equal, writes same value back — no-op |
| `arena.rs:319:34` | `>` → `>=` in `ArenaStats::add` max_closure_len | Same |
| `arena.rs:529:23` | `>` → `>=` in `StateArena::stats` max_ceilings | Same |
| `arena.rs:537:23` | `>` → `>=` in `StateArena::stats` max_epsilons | Same |
| `arena.rs:975:30` | `closure.len() == 1` → `!= 1` in `traverse_arena_nfa` | Both branches iterate the same 1-element closure; identical FT results |
| `arena.rs:1088:24` | `\|\|` → `&&` in `traverse_arena_dfa_backward` | If `start.is_none()`, `dstep(NONE, byte)` returns NONE immediately; if `val.is_empty()`, loop never executes — both produce no transitions |
| `json.rs:428:19` | `==` → `!=` in `compute_lookbehind_byte_length` | Already documented (Session 4) |
| `nfa.rs:510:16` | delete `!` in `instantiate_shell` | Already documented (Session 5) |
| `nfa.rs:626:20` | `<=` → `>` in surrogate gap | Already documented (Session 5) |
| `mutable_matcher.rs:85:47` | `==` → `!=` in `build_lookbehind_combined_pattern` | Already documented (Session 5) |
| `regexp/parser.rs:1343:9` | delete `\|` arm in `read_atom` | Falls through to `_` arm which does identical "stuck" error — semantically equivalent |

### Mutations that need tests

#### flatten_json.rs (5 non-equivalent mutations)

| Line | Mutation | What breaks | Test to write |
|------|----------|-------------|---------------|
| `367:51` | `&&` → `\|\|` in `read_object` null branch | Captures null value even when `self.skipping > 0` | Event: nested object with a null field under an unmatched key (skipped path). Verify no spurious match. |
| `385:46` | `>` → `<` in `read_object` array branch | `skipping < 0` is always false for usize → never skips arrays | Event: matched field next to an array field in same object. Without skipping, array contents bleed through. |
| `632:46` | `<` → `<=` in `skip_string_value` | Reads one byte past buffer when escape is last char | Event: string value ending with `\\` or `\"` as the last bytes (escape at buffer boundary). |
| `714:43` | `+` → `-` in surrogate pair guard `read_member_name_with_escapes` | Weakens bounds check `index + 5 < len` → `index - 5 < len`; underflow for small indices | Event: field name `\uD83D\uDE00` (😀) near the start of the event buffer where index < 5. |
| `721:65` | `-` → `+` in `(low - 0xDC00)` in `read_member_name_with_escapes` | Wrong codepoint arithmetic → wrong UTF-8 bytes | Pattern uses **literal** UTF-8 emoji in field name; event uses `\uD83D\uDE00` escape. Both must encode to same bytes. Currently tests use escapes on both sides — if both encode wrong, they still match. |
| `819:43` | `+` → `-` in surrogate guard `read_string_with_escapes` | Same as 714 but for string values | Same fix: small-index surrogate pair in a string value. |
| `826:65` | `-` → `+` in `(low - 0xDC00)` in `read_string_with_escapes` | Same as 721 but for string values | Pattern uses **literal** UTF-8 emoji as the matched value; event uses `\uD83D\uDE00` escape. |
| `827:60` | `>>` → `<<` in `full >> 18` in `read_string_with_escapes` | For codepoints ≥ U+40000 (e.g. U+40000 = `\uD8C0\uDC00`), first byte differs. For most emoji (< U+40000), `full >> 18 = 0` and `full << 18 as u8 = 0` — semantically equivalent for common emoji. | Test with a codepoint ≥ U+40000 to distinguish, OR accept as semantically equiv for common emoji. |

**Note on 714/819:** These are `+` → `-` in `self.index + 5 < self.event.len()`. The mutation is hard to hit because: at the time the high surrogate `\u` is processed, `self.index` is typically ≥ 6 (after `{`, `"`, field chars, `"`). Consider documenting as semantically equivalent if the small-index scenario can't happen in practice given JSON structure.

#### arena.rs (~20 mutations)

**Stats / utility** (existing tests are too weak):

| Line | Mutation | Fix needed |
|------|----------|------------|
| `366:9` | `with_capacity` → `Default::default()` | `test_with_capacity`: add `assert!(arena.states.capacity() >= 10)` — currently only checks `len()` and `is_empty()` which both still pass with `Default::default()` |
| `377:13` | `+` → `-` in `estimated_byte_size` | `test_estimated_byte_size`: assert exact value, e.g. after allocating N states, size should equal `N_capacity * size_of::<ArenaFaState>() + closure_cap * size_of::<StateId>()` |
| `377:44` | `*` → `/` in `estimated_byte_size` | Same |
| `379:13` | `+` → `*` in `estimated_byte_size` | Same |
| `379:42` | `*` → `/` in `estimated_byte_size` | Same |
| `379:42` | `*` → `+` in `estimated_byte_size` | Same |

**Merge functions — Case 1 (both spinouts) — code path not reached by current tests:**

Current tests use start states with epsilons pointing TO spinouts. Merge begins with Case 3 (splice) for the start states; spinouts are independently cloned, not merged together. Case 1 is only hit when BOTH states being merged are themselves spinout states. Need to trigger this directly.

| Line | Mutation | Root issue |
|------|----------|------------|
| `1576:67` | `<= 1` → `> 1` in `is_spinout_state` | With `> 1`, only states with 2+ epsilons are spinouts (inverts logic) |
| `1649:23` | `&&` → `\|\|` in Case 1 check | Asymmetric merge incorrectly uses both-spinout path |
| `1666:16` | delete `!` — `!merged.is_none()` | Epsilon of spinout not propagated (pushes NONE instead of valid state) |
| `1697:27` | delete `!` | Same |
| `1697:67` | delete `!` | Same |
| `1722:33` | `==` → `!=` in `spinner_next == spinner_id` | Self-loop detection inverted in asymmetric merge |
| `1746:36` | `==` → `!=` in `spinner_next == spinner_id` | Same |
| `1839:16` | delete `!` | Spinner's epsilon not propagated |
| `1857:24` | `\|\|` → `&&` in Case 3 | Creates splice only when both have epsilons instead of either |

**Test approach:** Write a unit test in `arena.rs` tests that directly constructs two spinout states (not wrapped in start states) and calls `merge_arena_nfas` on them. A spinout state is one where `table.default == state_id`. Construct `arena1` with a bare spinout (self-loop, 1 epsilon to a match state), same for `arena2`, then merge and verify the merged spinout still reaches both match states.

**Clone/remap:**

| Line | Mutation | Root issue |
|------|----------|------------|
| `1941:8` | `!default.is_none()` → `default.is_none()` in `clone_state_into_arena` | When default IS valid (spinout self-loop), it's not cloned → new_table.default = NONE |
| `1996:8` | Same in `remap_nfa_table_recursive` | Same |
| `2089:12` | delete `!` in `merge_nfa_tables_bytewise` epsilons | Table epsilons not propagated through merge |

**Test approach:** Construct an arena with a state that has a non-NONE `default` (e.g. a spinout state) and verify that `clone_state_into_arena` preserves the default. For 2089: construct two arenas each with epsilons on their NFA tables and verify that after merge the epsilon targets survive.

**Monocase:**

| Line | Mutation | Root issue |
|------|----------|------------|
| `3133:21` | `<` → `==` in `build_monocase_ascii_chain` | `byte == alt` is always false when cases differ → always uses `else` branch (swapped table order) |
| `3196:24` | `<` → `<=` in `build_monocase_arena_recursive` | Off-by-one when `orig[0] == alt_bytes[0]` |

Existing monocase unit tests in arena.rs should already cover this. Check why they miss — possibly the test strings don't exercise both orderings (where lowercase < uppercase in ASCII, it's `a`=0x61 > `A`=0x41, so `byte < alt` is `'a' < 'A'` = false, so it always went to `else` already). If `byte` is uppercase and `alt` is lowercase, then `byte < alt` is true. Check if tests cover both orderings.

**IPv6:**

| Line | Mutation | Root issue |
|------|----------|------------|
| `3455:37` | `-` → `/` in `mask = !0u16 << (16 - constrained_bits)` | Arithmetic error: `16 / constrained_bits` instead of `16 - constrained_bits` |
| `3458:46` | `-` → `/` in `(base as u32 + range_size - 1)` | Off-by-one in max range value |
| `3527:21` | `&&` → `\|\|` in full-range check | `min==0 OR max==0xffff` triggers full-range path (wrong for partial ranges) |
| `3601:22` | `>` → `>=` in `build_any_hex_group_arena` | Epsilon at digit_pos=0 allows 0-digit match |

**Test approach:**
- 3455/3458: IPv6 pattern with a prefix that falls in the middle of a group (e.g., /20 which constrains 4 bits of the 2nd group). Verify only the correct range of addresses matches.
- 3527: CIDR with `min_val=0, max_val=0xfffe` (not full range) — should not use the any-hex-group path. Or test that an IPv6 group with range [0,0xfffe] rejects `ffff`.
- 3601: Verify that an IPv6 CIDR requires at least 1 hex digit per group (empty group `""` should not match).

#### regexp/parser.rs (5 non-equivalent mutations)

| Line | Mutation | Root issue | Test to write |
|------|----------|------------|---------------|
| `1253:17` | delete `quant_min` field from `[abc]` atom | `quant_min` defaults to 0, making char class match 0 repetitions (empty string) | Pattern `{"f": {"regexp": "[abc]"}}` should NOT match `""` (empty string) |
| `1285:21` | delete `quant_min` from multi-char escape | Same for `~w`, `~d`, etc. | Pattern with `~w` should not match empty string |
| `1305:21` | delete `quant_min` from `~p{...}` category | Same for Unicode categories | Pattern with `~p{L}` should not match empty string |
| `1715:16` | `>` → `>=` in `add_gap_range` surrogate gap | `end >= SURROGATE_END_CP` changes when post-surrogate range is added | Need a char class or range spanning the exact surrogate boundary, e.g. `[\uDFFF-\uE000]` area — test that chars just outside the surrogate range match correctly |
| `1830:13` | delete `'M'` arm in `read_category` | Without the arm, `initial='M'` uses `valid_details=""` → all `Mn`/`Mc`/`Me` details are rejected | `{"regexp": "~p{Mn}"}` should compile and match combining marks, not error |

#### mutable_matcher.rs (1 mutation)

| Line | Mutation | Root issue | Test to write |
|------|----------|------------|---------------|
| `175:65` | `>` → `<` in `add_transition` | `all_exact.len() > 1` → `< 1` (always false) — bulk string optimization never fires | Add a test with 2+ exact string values: `{"f": ["a", "b", "c"]}`. Verify all three match. The optimization path vs one-by-one path should give same results, but currently no test distinguishes them. May be semantically equivalent if both paths produce same FA. |

**Note on mutable_matcher.rs:175:** The bulk vs one-by-one paths should produce the same FA. This is likely semantically equivalent. Verify by reading `add_string_transitions_bulk` vs individual `add_transition` — if they produce structurally identical NFAs for 2+ exact strings, document as semantically equivalent.

---

## Recommended Session Plan

### Session 6 — flatten_json.rs + arena.rs stats fixes (easiest)
1. Fix `test_with_capacity` to assert capacity is actually reserved
2. Fix `test_estimated_byte_size` to assert exact computed values
3. Add flatten_json test: literal UTF-8 emoji in pattern vs `\uD83D\uDE00` in event (catches 721/826)
4. Add flatten_json test: skipping arrays in nested object (catches 385)
5. Add flatten_json test: null in skipped context (catches 367)
6. Run targeted mutants: `cargo mutants -F 'flatten_json.rs' --line-in-file '367,385,632,721,826' --timeout 120`

### Session 7 — arena.rs merge Case 1 + clone/remap
1. Add unit test directly constructing two spinout StateArenas and merging them (catches 1576/1649/1666/1697/1839)
2. Add unit test for `clone_state_into_arena` preserving non-NONE defaults (catches 1941/1996)
3. Add unit test for `merge_nfa_tables_bytewise` epsilon propagation (catches 2089)
4. Run targeted mutants: `cargo mutants -F 'arena.rs' --line-in-file '1576,1649,1666,1697,1839,1857,1941,1996,2089' --timeout 120`

### Session 8 — arena.rs monocase + IPv6 + parser.rs
1. Fix/add monocase tests checking both byte-orderings for `build_monocase_ascii_chain`
2. Add IPv6 CIDR tests for constrained-bit groups (catches 3455/3458)
3. Add IPv6 test for partial-range groups `min=0, max≠0xffff` (catches 3527)
4. Add regexp/parser tests: char class/multi-char-escape/category don't match empty string (catches 1253/1285/1305)
5. Add `~p{Mn}` pattern test (catches 1830)
6. Check mutable_matcher.rs:175 equivalence — if not, add 2+ exact values test

### Session 9 — Final CI run + triage
- Trigger full CI run, confirm all non-equivalent mutations are caught
- Update this doc with final counts
