# Macro-expansion profile — jsoniter derivation, Hearth 0.3.1-52

Aggregated from the 344 per-derivation `.speedscope.json` flame graphs emitted by
`jsoniterDerivation3/Test/compile` (`-Xmacro-settings:hearth.mioBenchmarkScopes=true`). Self-time =
time a scope is on top of the MIO stack (excludes nested derivations). Total wall ≈ 11.1s across 344
derivations (~32 ms each).

## Top scopes by self-time (% of total wall)

| scope | self ms | % |
|---|---:|---:|
| decoder rule: handle as case class | 698 | 6.3 |
| **encoder rule: handle as map** | **670** | **6.0** |
| **decoder rule: handle as map** | **538** | **4.8** |
| encoder rule: handle as case class | 451 | 4.0 |
| encoder rule: handle as collection | 434 | 3.9 |
| decoder rule: handle as collection | 422 | 3.8 |
| encoder rule: handle as Option | 302 | 2.7 |
| encoder rule: use implicit when available | 261 | 2.3 |
| encoder/decoder: handle as enum | 165 / 135 | 1.5 / 1.2 |
| encoder/decoder: handle as value type | 150 / 133 | 1.3 / 1.2 |
| encoder/decoder: handle as built-in | 105 / 88 | 0.9 / 0.8 |

Almost all the cost is **rule applicability evaluation** (the `IsX` checks run on every field of every
type until one matches), not the actual code generation (`Deriving …` scopes are <1% each).

## Key opportunity: the map rule double-parses `IsCollection`

`IsMap.parse` is implemented as `IsCollection.parse` + an `isInstanceOf[IsMapOf]` check. The map rule
runs **before** the collection rule (required — `Map <: Iterable`). So for any field that is **not** a
map, the chain pays:

1. map rule → `IsMap.unapply` → **full `IsCollection.parse`** (iterates every collection provider,
   doing `=:=`/`baseType` type matching) → not a map → yields;
2. collection rule → **`IsCollection.parse` again**.

`IsCollection.parse` is the expensive part (provider iteration + compiler-reflection type matching),
and it runs twice for every non-map field — which is most fields. This is consistent with the map
rule's outsized self-time (≈11% combined) despite few map-typed fields in the suite.

### Candidate fixes

- **Merge "handle as map" + "handle as collection" into one rule** that calls `IsCollection.parse`
  once and dispatches on `IsMapOf` vs plain `IsCollectionOf`. Halves the `IsCollection.parse` calls
  for every collection/non-collection field. Multi-module (jsoniter, circe, yaml, pureconfig, avro,
  …) but mechanical, and the `IsMap`-before-`IsCollection` ordering is preserved (it becomes a single
  match arm order). Reusable shape could live in `derivation-commons`.
- **Reusable Hearth angle:** `IsMap`/`IsCollection` could expose a combined parse so a caller learns
  "collection, and here's whether it's a map" in one pass. Memoizing parse results by type is *not*
  viable (compiler types lack stable `hashCode`/`equals`, per prior research).

Memoization of type comparisons remains the irreducible floor; the double-parse is the one clearly
removable redundancy. Runtime codegen is already at/above parity with best-in-class libraries (see the
0.3.1-52 ratio scan), so compile-time is where the remaining macro headroom is.
