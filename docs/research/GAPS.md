# Kindlings: Remaining Gaps & Action Items

Last updated: 2026-06-05.

Legend: **P1** = important for migrating users, **P2** = nice to have / quality.

---

## 1. Cross-Cutting: Testing Methodology

### 1.1 Combinatorial testing matrices (P1)

Wrapper x Inner, Annotation x Type shape, Annotation x Codec direction, and Name
collision tests have been added across modules. Remaining: expand matrices to cover
more module/wrapper combinations.

---

## 2. avro-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 2.3 | `@avroNamespace` on fields (field-level override) | P2 | Requires threading namespace override through recursive schema derivation context. |
| 2.6 | Streaming / container format (`AvroInputStream`/`AvroOutputStream`) | P2 | New runtime API, not a derivation gap. |

---

## 3. jsoniter-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 5.4 | `skipNestedOptionValues` | P2 | Blocked: `semiEval` cannot evaluate `JsoniterConfig` with function-type fields. Needs runtime branching. |
| 5.5 | `alwaysEmitDiscriminator` | P2 | Blocked: same `semiEval` limitation + needs parent ADT context threading. |
| 5.6 | `inlineOneValueClasses` | P2 | Blocked: same `semiEval` limitation. Rules exist but never fire. |

---

## 4. tapir-schema-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 6.2 | Scala 3 union type schemas | P2 | No derivation code needed — `Enum.parse` + `SchemaHandleAsEnumRule` handle unions automatically. Blocked on testing: `Expr.summonImplicit` fails in `scala-3/` source files. See `docs/research/hearth-summon-implicit-scala3-only-files.md`. |

---

## 5. cats-tagless-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 10.1 | FunctorK, InvariantK, ContravariantK, SemigroupalK | P1 | **Done.** Case class: both platforms. Trait: Scala 3. |
| 10.1b | ApplyK | P1 | **Done.** Composes FunctorK + SemigroupalK. map2K via compose. |
| 10.1c | Variance-aware trait derivation | P1 | **Done.** FunctorK errors on F-in-params. ContravariantK handles F-in-params. InvariantK handles both positions. |
| 10.1d | Scala 2 trait support | P2 | AnonymousInstance.construct on Scala 2 generates class outside lambda scope. |
| 10.2 | Instrument (AOP) | P2 | **Done.** Wraps F[X] fields/methods in Instrumentation with algebra/method names. |
| 10.3 | Utility derivations (`const`, `void`, `readerT`) | P2 | Deferred — these produce values, not type class instances. Different approach needed. |
| | **Total** | | **66 tests** (25 Scala 2.13, 41 Scala 3) |

---

## 6. yaml-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 11.1 | Custom YAML tags | P2 | Needs investigation of scala-yaml's `Tag` API. |

---

## 7. Known Bugs

| Module | Test | Issue |
|---|---|---|
| tapir | `Expr.summonImplicit` in scala-3/ files | Phantom type witness fails in Scala-3-only compilation units. See `hearth-summon-implicit-scala3-only-files.md`. |
| jsoniter | `decodingOnly + encodingOnly compile error` | `semiEval` can't evaluate config in `compileErrors()` context; works in real usage. |

### Resolved

| Module | Issue | Resolution |
|---|---|---|
| all encoder modules | Splice isolation: `Option[SealedTrait]` on Scala 3 | **Fixed** in Hearth 0.4.0. Verified across circe, jsoniter, ubjson, yaml, fast-show-pretty. |
| all decoder modules | Default values for generic case classes on Scala 3 | **Fixed** in Hearth 0.3.0-94. `BoxWithDefault[Int]` compiles and tests pass. |
