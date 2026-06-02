# Kindlings: Remaining Gaps & Action Items

Last updated: 2026-06-02. Items marked ~~struck~~ were fixed and removed.

Legend: **P0** = blocks correctness / parity, **P1** = important for migrating users,
**P2** = nice to have / quality.

---

## 1. Cross-Cutting: Testing Methodology

### 1.1 Combinatorial testing matrices (P1)

Required per module: Wrapper x Inner type, Annotation x Type shape,
Annotation x Codec direction, Name collision tests. See bug pattern analysis below.

### 1.2 Upstream bug tracker scan process (P1)

Before releases, review recently-closed issues/PRs in upstream libraries.
Port regression tests even if kindlings doesn't currently have the bug.

---

## 2. avro-derivation

| # | Gap | Priority |
|---|---|---|
| 2.1 | Schema evolution: `@avroAlias`, missing fields from defaults | P1 |
| 2.2 | `@avroFixed` on String fields | P2 |
| 2.3 | `@avroNamespace` on fields (field-level override) | P2 |
| 2.4 | Mutually recursive types (`MutRec1 -> List[MutRec2] -> List[MutRec1]`) | P2 |
| 2.5 | Missing temporal types: `java.sql.Date`, `java.sql.Timestamp`, `OffsetDateTime` | P2 |
| 2.6 | Streaming / container format (`AvroInputStream`/`AvroOutputStream`) | P2 |
| 2.7 | `@avroProp` with JSON values (Jackson `JsonNode`) | P2 |
| 2.8 | `None.type` as standalone type (NULL schema) | P2 |
| 2.9 | Parameterized enums with `val` fields (Scala 3) | P2 |

---

## 3. cats-derivation

| # | Gap | Priority |
|---|---|---|
| 3.1 | Recursive nested HKT: `Search[+A](move, child: Option[Search[A]], variations: List[Search[A]])` | P1 |
| 3.2 | Composition / type alias HKT: `OptList[A] = Option[List[A]]`, `AndChar[A] = (A, Char)` | P1 |
| 3.3 | Automatic derivation mode (`import auto.show.given`) | P2 |
| 3.4 | `strict.semiauto` mode (all transitive instances manual) | P2 |
| 3.5 | Serializable tests for all type classes | P2 |

---

## 4. circe-derivation

| # | Gap | Priority |
|---|---|---|
| 4.1 | Patch decoders (`Decoder[A => A]`) | P2 |
| 4.2 | Incomplete/partial decoders | P2 |
| 4.3 | Programmatic strict-mode field list (structured, not just error string) | P2 |
| 4.4 | Local class derivation test | P2 |

---

## 5. jsoniter-derivation

| # | Gap | Priority |
|---|---|---|
| 5.1 | Default value differences from upstream (documented in JsoniterConfig.scala) | P1 |
| 5.2 | Missing collection types: `LinkedHashMap`, `Queue`, `LazyList`, etc. | P2 |
| 5.3 | `javaEnumValueNameMapper` | P2 |
| 5.4 | `skipNestedOptionValues` (Option[Option[_]]) | P2 |
| 5.5 | `alwaysEmitDiscriminator` | P2 |
| 5.6 | `inlineOneValueClasses` (non-AnyVal single-field) | P2 |
| 5.7 | `IArray[A]` (Scala 3 immutable arrays) | P2 |

---

## 6. tapir-schema-derivation

| # | Gap | Priority |
|---|---|---|
| 6.1 | `.modify(_.path)` post-derivation | P2 |
| 6.2 | Scala 3 union type schemas (`String | Int`) | P2 |

---

## 7. pureconfig-derivation

| # | Gap | Priority |
|---|---|---|
| 7.1 | Value class edge cases (private constructor, double-wrapping) | P2 |
| 7.2 | Candidate key suggestions in error messages | P2 |

---

## 8. scalacheck-derivation

| # | Gap | Priority |
|---|---|---|
| 8.1 | Cogen for recursive types | P2 |
| 8.2 | ADT shrink behavior (case class → case object alternatives) | P2 |

---

## 9. diff-derivation

Independent implementation (not diffx port). Gaps below matter for diffx parity.

| # | Gap | Priority |
|---|---|---|
| 9.1 | Post-derivation modification: `.modify(_.path).ignore` etc. | P1 |
| 9.2 | ObjectMatcher system (matching elements by key) | P1 |
| 9.3 | `Either`/`Tuple` built-in diff | P2 |
| 9.4 | `Diff.approximate[T](epsilon)` | P2 |
| 9.5 | Test framework integrations (scalatest, munit matchers) | P2 |
| 9.6 | String diff edge cases: combined escaping, CharChunk composition | P2 |

---

## 10. cats-tagless-derivation

| # | Gap | Priority |
|---|---|---|
| 10.1 | SemigroupalK, ApplyK (deferred to Hearth 0.4.0) | P1 |
| 10.2 | AOP / Instrumentation (`Instrument`, `Aspect`) | P2 |
| 10.3 | Utility derivations (`const`, `void`, `readerT`) | P2 |

---

## 11. yaml-derivation

| # | Gap | Priority |
|---|---|---|
| 11.1 | Custom YAML tags | P2 |
| 11.2 | `orElse` on decoders | P2 |

---

## 12. Known Bugs (2 pre-existing + 1 limitation)

| Module | Test | Issue |
|---|---|---|
| sconfig | `ConfigWriter.derived[CombOuter]` on Scala 3 | Splice isolation in writer enum rule |
| xml | `KindlingsXmlEncoder.derived[CombOuter]` on Scala 3 | Splice isolation in encoder enum rule |
| jsoniter | `decodingOnly + encodingOnly compile error` | `semiEval` can't evaluate config in `compileErrors()` context; works in real usage |

---

## 13. Bug Pattern Analysis

6 of 10 real bugs came from features tested in isolation but never composed.

| Pattern | Issues | Mitigation |
|---|---|---|
| Combinatorial gap | #120, #78, #80, #79 | Wrapper x Inner matrices |
| Annotation tested one-direction only | #110, #108 | Round-trip tests |
| Upstream fix not ported | #92, #91 | Bug tracker scan |
| Hearth/cross-quotes | #115, #65 | Already mitigated |
| Performance/codegen | #109, #86 | Codegen audits |

---

## Addressed (removed from this document)

- **API: Remove `derive`** — done, all 9 modules use only `derived`
- **Law-based property testing** — LawSpec added with 79 property checks
- **Negative compilation tests** — added across cats-derivation
- **Avro auto defaults, package namespace, byte collections** — implemented
- **cats HKT sealed traits** — Functor/Foldable/Traverse enum rules for IList
- **cats nested type constructors** — Vector[A], Option[A] in Functor/Foldable/Traverse
- **cats Bifunctor/Bifoldable/Bitraverse sealed traits** — Result[A, E] enum rules
- **Order transitivity bug** — fixed with ordinal-based comparison
- **Arbitrary recursion** — Gen.sized + size halving in enum and case class rules
- **Circe splice isolation** — confirmed fixed
- **Circe #120 (Option[DerivedType])** — fixed
- **jsoniter java.time types** — 6 codecs added
- **jsoniter boxed primitives** — 8 codecs added
- **jsoniter Either, BitSet** — implemented
- **jsoniter config conflict validation** — compile-time check implemented
- **jsoniter IntMap/LongMap** — already supported via Hearth's standard collection provider; tests added
- **jsoniter requireDiscriminatorFirst** — already implemented (always strict by design)
- **tapir derivedEnumeration** — implemented
- **Respecting user-provided instances** — tested for Show, Eq, Semigroup
- **Interleaved, CaseClassWOption types** — Functor/Foldable/Traverse derived
- **Def-caching rule coverage** — recursive type tests exist in all modules
- **xml @xmlAttribute on sealed trait subtype** — fixed: FromAttribute now decodes through field helper
- **sconfig strict decoding for sealed traits** — fixed: strip discriminator key before passing to child reader
- **diff-derivation core coverage** — audit shows comprehensive test coverage (DiffResult/DiffRuntime all types tested)
- **diff-derivation Map/Collection/Option inner type derivation** — fixed: recursive derivation via def-caching
- **tapir @default annotation** — already implemented with tests
- **tapir @encodedExample annotation** — already implemented with tests
