# Kindlings: Remaining Gaps & Action Items

Last updated: 2026-06-02. 25 gaps closed.

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
| 2.1 | ~~Schema evolution~~ → decoder uses name-based field access with `@avroAlias` fallback + Scala default values for missing fields | ✅ |
| 2.2 | ~~`@avroFixed` on String fields~~ → intentionally rejected at compile time (Array[Byte] only) | ✅ |
| 2.3 | `@avroNamespace` on fields (field-level override) | P2 |
| 2.4 | ~~Mutually recursive types~~ → verified working via `setHelper`/`getHelper` caching, tests added | ✅ |
| 2.5 | ~~Missing temporal types~~ → `OffsetDateTime` added; `java.sql.Date`/`Timestamp` deferred (JVM-only, low demand) | ✅ |
| 2.6 | Streaming / container format (`AvroInputStream`/`AvroOutputStream`) | P2 |
| 2.7 | ~~`@avroProp` with JSON values~~ → `addSchemaProp`/`addFieldProp` auto-detect JSON strings and parse to structured objects | ✅ |
| 2.9 | ~~Parameterized enums with `val` fields~~ → already fully supported with test coverage | ✅ |

---

## 3. cats-derivation

| # | Gap | Priority |
|---|---|---|
| 3.1 | Recursive nested HKT: `Search[+A](move, child: Option[Search[A]], variations: List[Search[A]])` — needs 4th field classification category (nested+self-recursive) in Functor/Foldable/Traverse; infrastructure exists but composing nested functors with self-recursion is non-trivial | P1 |
| 3.3 | ~~Automatic derivation mode~~ → `import auto.show.given` (Scala 3) / `import auto.show._` (Scala 2) for Show, Eq, Order, Hash, Semigroup, Monoid, Empty, Functor, Contravariant, Foldable, Traverse | ✅ |
| 3.4 | ~~`strict.semiauto` mode~~ → `StrictDerivation` sentinel type; when in implicit scope, UseImplicit rules fail instead of yielding to auto-derivation | ✅ |

---

## 4. circe-derivation

| # | Gap | Priority |
|---|---|---|
| 4.1 | ~~Patch decoders~~ → `KindlingsDecoder.patch[A]` returns `Decoder[A => A]` via JSON merge (requires both Decoder and Encoder) | ✅ |
| 4.2 | ~~Incomplete/partial decoders~~ → works via `Configuration.default.withDefaults` (missing fields use Scala defaults) | ✅ |
| 4.3 | ~~Programmatic strict-mode field list~~ → `KindlingsDecoder.expectedFields: Option[Set[String]]` API added, factory methods support it | ✅ |

---

## 5. jsoniter-derivation

| # | Gap | Priority |
|---|---|---|
| 5.1 | ~~Default value differences from upstream~~ → `JsoniterConfig.jsoniterScalaDefaults` preset added | ✅ |
| 5.2 | ~~Missing collection types~~ → `LinkedHashMap`, `Queue`, `LazyList` verified working (Hearth's generic `Iterable` + `Factory` matching) | ✅ |
| 5.3 | ~~`javaEnumValueNameMapper`~~ → config field + macro wiring through encoder/decoder enum rules | ✅ |
| 5.4 | `skipNestedOptionValues` — config field added; macro wiring blocked by `semiEval` limitation with function-type config fields | P2 |
| 5.5 | `alwaysEmitDiscriminator` — config field added; macro wiring blocked by `semiEval` limitation with function-type config fields | P2 |
| 5.6 | `inlineOneValueClasses` — config field + rule infrastructure added; blocked by `semiEval` not evaluating configs with function fields | P2 |

---

## 6. tapir-schema-derivation

| # | Gap | Priority |
|---|---|---|
| 6.1 | ~~`.modify(_.path)` post-derivation~~ → verified working with tapir's native `Schema.modify`, tests added | ✅ |
| 6.2 | Scala 3 union type schemas — blocked by Hearth cross-quotes error in type printer for union types | P2 |

---

## 7. pureconfig-derivation

| # | Gap | Priority |
|---|---|---|
| 7.1 | ~~Value class edge cases~~ → verified working for WrappedInt, WrappedString as fields; double-wrapping deferred | ✅ |
| 7.2 | ~~Candidate key suggestions in error messages~~ → Levenshtein-based suggestions via `KeyNotFound.candidates` | ✅ |

---

## 8. scalacheck-derivation

| # | Gap | Priority |
|---|---|---|
| 8.1 | ~~Cogen for recursive types~~ → verified working on both Scala 2.13 and 3 | ✅ |
| 8.2 | ~~ADT shrink behavior~~ → `shrinkEnumWithAlternatives` offers case object variants when shrinking case class variants | ✅ |

---

## 9. diff-derivation

Independent implementation (not diffx port). Gaps below matter for diffx parity.

| # | Gap | Priority |
|---|---|---|
| 9.1 | ~~Post-derivation modification~~ → `ignoreField`, `modifyField().ignore` API added. `modifyField().using(customDiff)` deferred (needs field accessor integration) | ✅ |
| 9.2 | ~~ObjectMatcher system~~ → `ObjectMatcher.by(_.key)` + `Diff.seqDiff(elemDiff, matcher)` for key-based collection matching | ✅ |
| 9.4 | ~~`Diff.approximate[T](epsilon)`~~ → `Diff.approximate[T: Numeric](epsilon)` factory method added | ✅ |
| 9.5 | ~~Test framework integrations~~ → `Diff.assertNoDiff` / `Diff.assertDiff` assertion helpers added | ✅ |

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

---

## 12. Known Bugs (1 Hearth-level + 1 limitation)

| Module | Test | Issue |
|---|---|---|
| **all encoder modules** | `*.derived[CaseClassContaining[Option[SealedTrait]]]` on Scala 3 | Hearth-level splice isolation: `toValDefs.use` + `parMatchOn` interaction creates cross-Quotes references when sealed traits are derived recursively inside Option wrappers. Affects sconfig, xml, circe, and others. Requires upstream Hearth fix. |
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
