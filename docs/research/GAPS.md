# Kindlings: Remaining Gaps & Action Items

Last updated: 2026-06-06.

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
| 2.3 | `@avroNamespace` on fields | P2 | **Done.** |
| 2.6 | Streaming / container format | P2 | Out of scope (runtime API, not derivation). |

---

## 3. jsoniter-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 5.4 | `skipNestedOptionValues` | P2 | **Done.** |
| 5.5 | `alwaysEmitDiscriminator` | P2 | **Done.** |
| 5.6 | `inlineOneValueClasses` | P2 | **Done.** |

---

## 4. tapir-schema-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 6.2 | Scala 3 union type schemas | P2 | **Done.** 72 Scala 3 tests pass. |

---

## 5. cats-tagless-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 10.1 | FunctorK, InvariantK, ContravariantK, SemigroupalK | P1 | **Done.** |
| 10.1b | ApplyK | P1 | **Done.** |
| 10.1c | Variance-aware trait derivation | P1 | **Done.** |
| 10.1d | Scala 2 trait support | P2 | **Done.** |
| 10.2 | Instrument (AOP) | P2 | **Done.** |
| 10.3 | Utility derivations (`const`, `void`, `readerT`) | P2 | Deferred — produce values, not type class instances. |

---

## 6. yaml-derivation

| # | Gap | Priority | Status |
|---|---|---|---|
| 11.1 | Custom YAML tags | P2 | Encoding blocked on upstream scala-yaml presenter. |

---

## 7. Known Bugs

| Module | Issue | Status |
|---|---|---|
| jsoniter | `decodingOnly + encodingOnly compile error` in `compileErrors()` context | Works in real usage. |
| tapir | Scala 2 tapir-schema tests broken | Pre-existing Hearth issue with Scala 2 type identity. |

### Resolved

| Issue | Resolution |
|---|---|
| Annotation extraction (#283) | **Fixed in Hearth 0.3.1-45** — `annotationTypes` / `annotationsOfType` / `hasAnnotationOfType` / `Annotations.decodedConstructorArguments`. Kindlings already migrated off the workaround (shared `AnnotationSupport` is native-API-backed convenience only). Gap doc deleted 2026-06-15. |
| HKT ctor primitives, first-order (#284) | **Fixed in Hearth 0.3.1-45** — `decompose1/2` + `CtorK1` + summon. `cats-derivation` migrated off the bridges. The higher-order `CtorK1`-argument case (`cats-tagless`) is still open — see `hearth-gap-hkt-ctor-primitives.md`. |
| Splice isolation: `Option[SealedTrait]` on Scala 3 | Fixed in Hearth 0.4.0. |
| Default values for generic case classes on Scala 3 | Fixed in Hearth 0.3.0-94. |
| Scala 2 anonymous instance scope | Fixed in Hearth 0.3.0-96. |
| `Expr.summonImplicit` cross-file type identity | Fixed via scalac setting fallback + `freshConfigType`. |
| `semiEval` inline parameter opacity | Workaround: runtime branching. |
