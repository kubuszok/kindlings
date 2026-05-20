# Diff Library Comparison: kindlings-diff-derivation vs difflicious vs diffx

## Architecture comparison

### Derivation mechanism

| Aspect | kindlings-diff | difflicious | diffx |
|--------|---------------|-------------|-------|
| Derivation engine | Hearth cross-platform macros | Magnolia 1.x | Magnolia 1.x |
| Scala versions | 2.13 + 3 | 2.13 + 3 | 2.12 + 2.13 + 3 |
| Platforms | JVM + JS + Native | JVM + JS | JVM + JS |
| Type name resolution | `runtimePlainPrint`/`runtimeShortPrint`/`runtimePrettyPrint` — runtime-composable for generics | izumi-reflect `LTag` | Plain `String` (compile-time only) |
| Type name rendering | 4 modes (pretty/plain/simple/short), choice deferred to render time | FQN + short, fixed at derive time | Single string, fixed at derive time |

### DiffResult AST

| Aspect | kindlings-diff | difflicious | diffx |
|--------|---------------|-------------|-------|
| Base type | `sealed abstract class` with lazy name fields | `sealed trait` with `PairType` | `sealed trait` |
| Type names | 4 lazy `String` fields on every node (by-name → lazy val) | `SomeTypeName` (izumi-reflect) | Plain `String name` on container nodes only |
| Leaf values | Pre-rendered `String` | Pre-rendered `String` | Raw typed `T` (needs `Show` at render time) |
| One-sided values | `Edit.Insert`/`Edit.Delete` wrapping `DiffResult` | `PairType.ObtainedOnly`/`ExpectedOnly` on nodes | `DiffResultMissing[T]`/`DiffResultAdditional[T]` |
| String diff | Hierarchical 3-level: `StringChunk`→`WordChunk`→`CharChunk` | None (just `ValueResult.Both`) | Hierarchical but using generic `DiffResult` nodes (loses level info) |
| Collection diff | `Edit[DiffResult]` vector (Myers-aligned) | Flat `Vector[DiffResult]` (index or greedy matched) | `Map[String, DiffResult]` (string-keyed indices) |

### Collection diffing algorithm

| Aspect | kindlings-diff | difflicious | diffx |
|--------|---------------|-------------|-------|
| Algorithm | **Myers O(ND)** with shortest edit script | Greedy O(n·m) matching | Greedy O(n·m) matching |
| Sequence alignment | LCS-based optimal alignment | `zipAll` by index OR `pairBy` function | `ObjectMatcher`-based greedy |
| Pre-optimizations | Common prefix/suffix stripping (Neil Fraser) | None | None |
| Result quality | Minimal edit distance, shows true insertions/deletions/moves | Index-based (misaligns after insert) OR function-based (no optimality guarantee) | Matcher-based (no optimality guarantee) |

### String diffing

| Aspect | kindlings-diff | difflicious | diffx |
|--------|---------------|-------------|-------|
| Algorithm | Myers at each level | None | Myers (via java-diff-utils port) |
| Hierarchy | line → word → char (dedicated AST nodes) | N/A (strings are primitive values) | line → word → char (generic `DiffResult` nodes) |
| Pre-optimizations | Equality check, common prefix/suffix, single edit (Neil Fraser) | N/A | None |
| Post-cleanups | Semantic cleanup, semantic lossless alignment (Neil Fraser) | N/A | None |
| Similarity threshold | Configurable (default 0.5) for word→char drill-down | N/A | 0.5 threshold |
| Dedicated AST | Yes: `StringChunk`, `WordChunk`, `CharChunk` | N/A | No: reuses `DiffResultString`/`StringLine`/`StringWord` (same `DiffResult` base) |

### Rendering

| Aspect | kindlings-diff | difflicious | diffx |
|--------|---------------|-------------|-------|
| Color support | ANSI + plain (configurable) | ANSI via fansi library | ANSI with 4 themes + `NO_COLOR` support |
| Name style | 4 options: Simple, Short, FQN, Pretty (ANSI) | Fixed (short + FQN available) | Fixed |
| Indentation | Configurable: Spaces(n) or Tab | Fixed 2 spaces | Fixed 5 spaces |
| Configuration | `RenderConfig` case class | Hardcoded | `ShowConfig` with per-color functions + `DiffResultTransformer` |
| Skip identical | Not yet | No | Yes via `ShowConfig.skipIdentical` |

## Feature coverage matrix

### Core features

| Feature | kindlings-diff | difflicious | diffx |
|---------|---------------|-------------|-------|
| Case class derivation | ✅ | ✅ | ✅ |
| Sealed trait / enum | ✅ | ✅ | ✅ |
| Primitive types | ✅ | ✅ | ✅ |
| String (structural diff) | ✅ (hierarchical Myers) | ❌ (equality only) | ✅ (hierarchical Myers) |
| Option | ✅ | ✅ (Magnolia-derived) | ✅ |
| Either | ❌ (not yet) | ✅ (Magnolia-derived) | ✅ |
| List/Vector/Seq | ✅ (Myers) | ✅ (zip or pairBy) | ✅ (greedy) |
| Set | ✅ (greedy match) | ✅ (pairBy) | ✅ (greedy) |
| Map | ✅ (key match) | ✅ (key match) | ✅ (matcher) |
| Value types / opaque | ✅ (unwrap) | ✅ (contramap) | ✅ (Magnolia fallback to useEquals) |
| Recursive types | ✅ (cached defs) | ✅ (lazy Magnolia) | ✅ (lazy Magnolia) |
| Null handling | ❌ (not yet) | ❌ | ✅ (`nullGuard`) |

### Configuration features

| Feature | kindlings-diff | difflicious | diffx |
|---------|---------------|-------------|-------|
| Ignore field at path | ❌ (not yet) | ✅ (`ignoreAt(_.field)`) | ✅ (`modify(_.field).ignore`) |
| Replace differ at path | ❌ (not yet) | ✅ (`replace(_.field)(newDiffer)`) | ✅ (`modify(_.field).setTo(d)`) |
| Transform differ at path | ❌ (not yet) | ✅ (`configure(_.field)(f)`) | ✅ (`modify(_.field).using(f)`) |
| Collection pairBy function | ❌ (not yet — uses Myers) | ✅ (`pairBy(_.id)`) | ✅ (`matchByValue(_.id)`) |
| Collection pairBy index | N/A (Myers handles alignment) | ✅ (`pairByIndex`) | ✅ (`matchByKey(identity)`) |
| Approximate numeric | ❌ (not yet) | ❌ | ✅ (`Diff.approximate(epsilon)`) |
| Custom equality | ❌ (not yet) | ✅ (`Differ.useEquals`) | ✅ (`Diff.useEquals`) |
| Path syntax (`.each`, `.subType[T]`) | ❌ (not yet) | ✅ (compile-time macros) | ✅ (compile-time macros) |

### Integration features

| Feature | kindlings-diff | difflicious | diffx |
|---------|---------------|-------------|-------|
| MUnit | ❌ (not yet) | ✅ | ✅ |
| ScalaTest | ❌ (not yet) | ✅ | ✅ (should + must) |
| Weaver | ❌ (not yet) | ✅ | ✅ |
| Specs2 | ❌ (not yet) | ❌ | ✅ |
| utest | ❌ (not yet) | ❌ | ✅ |
| Cats collections | ❌ (not yet) | ✅ (NEL, NEC, NEV, Chain, NEM, NES, Validated) | ✅ (NEL, NEC, NEV, Chain, NEM, NES) |
| Refined types | ❌ (not yet — Hearth handles opaques) | ❌ | ✅ |
| Tagging | ❌ (not yet) | ❌ | ✅ |

## What we do better

1. **Myers diff for collections** — neither difflicious nor diffx uses optimal alignment for sequences. Both use greedy O(n·m) matching. Our Myers O(ND) finds the shortest edit script, producing cleaner diffs when elements are inserted or deleted in the middle of a list.

2. **Neil Fraser pre-optimizations** — common prefix/suffix stripping, single-edit detection, and equality fast-path reduce input size before running Myers. Neither library does this.

3. **Neil Fraser post-cleanups** — semantic cleanup removes trivially small equalities surrounded by large changes. Semantic lossless alignment slides edits to word/line boundaries for more human-readable output. Neither library does this.

4. **Dedicated string diff AST** — our `StringChunk`/`WordChunk`/`CharChunk` types preserve the hierarchy level, allowing renderers to apply different styling per level. diffx reuses generic `DiffResult` nodes (loses level information). difflicious has no string diffing at all.

5. **Lazy type names with 4 print modes** — type names are by-name parameters stored as lazy vals, computed only when rendering accesses a specific mode. Both difflicious and diffx compute type names eagerly at diff time.

6. **Runtime-composable type names** — for `def foo[A: Diff]: Diff[Foo[A]]`, our type names are composed at runtime from the `Diff[A]` instance's names. difflicious uses izumi-reflect (heavy dependency). diffx uses compile-time-only strings (breaks for generic defs).

7. **Cross-platform** — JVM + Scala.js + Scala Native, both Scala 2.13 and 3. difflicious only supports JVM + JS. diffx only JVM + JS (and also still supports 2.12).

## What we're missing (future work)

### High priority

- **Path-based configuration** (`modify(_.field).ignore`, `configure(_.field.each)(f)`) — both libraries have this as a core user-facing API. Without it, users can't customize derivation for specific fields.
- **Either support** — both libraries handle `Either[A, B]` out of the box.
- **Test framework integrations** (MUnit, ScalaTest, Weaver) — both libraries provide `assertNoDiff`-style assertions. This is how most users interact with a diff library.
- **`Diff.useEquals[T]`** — custom equality-based differ for types where structural diff isn't desired.

### Medium priority

- **Approximate numeric comparison** — diffx has `Diff.approximate[T](epsilon)`. Useful for floating-point comparisons in tests.
- **`ShowConfig.skipIdentical`** — diffx can filter out unchanged fields from rendered output, making large diffs more readable.
- **Cats collection integration** — NonEmptyList, NonEmptyVector, Chain, NonEmptyChain, NonEmptyMap, NonEmptySet, Validated.
- **Null handling** — diffx's `nullGuard` pattern. Not critical for Scala-only code but useful for Java interop.
- **`contramap` / `TransformedDiffer`** — difflicious has `ValueDiffer.contramap[S](f: S => T)` for creating differs for newtypes/opaque types without unwrapping.

### Low priority

- **`AlwaysIgnoreDiffer`** — difflicious has `Differ.alwaysIgnore[T]` for permanently ignoring a type regardless of path.
- **Refined/tagging integrations** — diffx has these. Hearth's opaque/value type handling may cover most of these automatically.
- **Multiple color themes** — diffx has dark/light/normal themes plus `DIFFX_COLOR_THEME` env var support.
- **Specs2 / utest integrations** — lower adoption than MUnit/ScalaTest/Weaver.
