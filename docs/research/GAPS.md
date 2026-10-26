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
| di-cats | No Hearth `Ctor` for mixed kinds (`Resource[F[_], A]`) — Hearth feedback item 11 | Open in Hearth (no ETA); `isResource`/`resourceUnderlying` recognise `Resource` by FQCN + `decompose2`. |

### Resolved

| Issue | Resolution |
|---|---|
| Annotation extraction (#283) | **Fixed in Hearth 0.3.1-45** — `annotationTypes` / `annotationsOfType` / `hasAnnotationOfType` / `Annotations.decodedConstructorArguments`. Kindlings already migrated off the workaround (shared `AnnotationSupport` is native-API-backed convenience only). Gap doc deleted 2026-06-15. |
| HKT ctor primitives, first-order (#284) | **Fixed in Hearth 0.3.1-45** — `decompose1/2` + `CtorK1` + summon. `cats-derivation` migrated off the bridges. The higher-order `CtorK1`-argument case (`cats-tagless`) is **resolved too** (2026-10-25, Hearth 0.4.3 `UntypedType.dealias`/`typeArguments`/`typeConstructor`/`applyTypeArgs`; `TaglessKBridgeScala{2,3}` deleted) — see `hearth-gap-hkt-ctor-primitives.md`. The Scala 2 entry glue (`Type.Ctor1.of[F]`/`CtorK1.of[Alg]` broken for a macro's own type parameter) is **resolved** by kubuszok/hearth#389: all Scala 2 entry points now call `Type.CtorN.of[F]` / `Type.CtorK1.of[Alg]` — see `hearth-gap-scala2-ctor-of-macro-type-param.md`. |
| `Type.of` bootstrap cycle in extensions (#285) | **Fixed in Hearth 0.3.1-45** — self-referential `implicit val …: Type[A] = Type.of[A]` now works in cross-quotes inside a `StandardMacroExtension`. circe + jsoniter field-config extensions migrated off the bypass (and the per-platform scala-2/scala-3 pairs collapsed into one shared file each). Gap doc deleted 2026-06-15. |
| Generic method return type after `ApplyTypes` (`Nothing` on Scala 2); cross-quotes `Type.of` before `import x.Underlying`; no `isFunction`/`isContextFunction` | **Resolved by kubuszok/hearth#389** (Hearth 0.4.4, verified against `0.4.3-24-gc0c6e43-SNAPSHOT`) — di-cats reads `knownReturning` directly, `preciseDependency` uses the natural import order, di/di-cats use the function predicates. See `hearth-gap-method-generic-return-type.md`, `hearth-gap-cross-quotes-import-order.md`. |
| Hearth feedback 2026-10, items 1-10 (`docs/research/hearth-feedback-2026-10/`) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (to be released as 0.4.4); Kindlings pins that snapshot and dropped every workaround. Item 11 stays open (see Known Bugs). |
| Scala 2 `UntypedType.parents` `Nil` for applied generic classes (`baseClasses` with unresolved type params) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4); di/di-cats detect factories with `!parents.exists(_.isFunction)` again — see `hearth-gap-scala2-parents-generic-classes.md`. |
| Enclosing-method parameters not exposed (feedback item 4) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4, `Environment.enclosingMethodParameters`); `DI.wire` & co. now use them as the innermost scope on Scala 2 and 3 (`WiringSpec` "DI.wire enclosing-method parameters"). |
| `VarArgs.toIterable` silently drops non-literal varargs (feedback item 8) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4, `VarArgs.toIterableOption`); optics uses it — see `hearth-gap-varargs-non-literal.md`. |
| Path-dependent type prefix / type members (feedback item 9) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4, `Type.pseudoCompanionObject`); newtype/neotype integrations use it — see `hearth-gap-type-prefix-and-members.md`. |
| `Ctor2.of[Function1].unapply` on subtypes, Scala 3 (feedback item 7) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4); cats-derivation uses plain `Type.Ctor2.of[Function1]`. |
| `decompose1/2`/`typeArguments` on singleton types (feedback item 3) | **RESOLVED** in Hearth `0.4.3-24-gc0c6e43-SNAPSHOT` (0.4.4); no Kindlings workaround existed (di-cats widens dependencies through `DestructuredExpr`). |
| Splice isolation: `Option[SealedTrait]` on Scala 3 | Fixed in Hearth 0.4.0. |
| Default values for generic case classes on Scala 3 | Fixed in Hearth 0.3.0-94. |
| Scala 2 anonymous instance scope | Fixed in Hearth 0.3.0-96. |
| `Expr.summonImplicit` cross-file type identity | Fixed via scalac setting fallback + `freshConfigType`. |
| `semiEval` inline parameter opacity | Workaround: runtime branching. |
