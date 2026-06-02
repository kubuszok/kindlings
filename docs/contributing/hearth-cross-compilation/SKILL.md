---
name: hearth-cross-compilation
description: >
  Cross-compilation pitfalls for Scala 2.13 + 3 macros with Hearth. Path-dependent types,
  splice isolation, type constructor matching, reification failures, Array ClassTag,
  upcast constraints, phantom types, asInstanceOf semantics, IsMap ordering.
paths:
  - "**/compiletime/**/*.scala"
  - "**/scala-2/**/*.scala"
  - "**/scala-3/**/*.scala"
user-invocable: false
---

# Skill: Cross-Compilation Pitfalls

Live reference for things that bite when writing cross-compiled Kindlings macros. Each
entry in [pitfalls.md](pitfalls.md) describes the failure mode, the platform(s) it affects,
and the workaround.

For higher-level rules around extension loading and `LambdaBuilder` scope, see
[hearth-standard-extensions](../hearth-standard-extensions/SKILL.md) and
[hearth-lambda-builder](../hearth-lambda-builder/SKILL.md).

## Pitfall index

### Splice and quote scoping

| # | Pitfall | Severity | Platforms |
|---|---------|----------|-----------|
| 1 | [Sibling `Expr.splice` isolation](pitfalls.md#1-sibling-exprsplice-isolation) | CRITICAL | Scala 3 |
| 2 | [`Expr.quote { (x: T) => ... }` is supported](pitfalls.md#2-exprquote--x-t-----is-supported) | LOW | Both |
| 3 | [Path-dependent types in `Expr.quote`](pitfalls.md#3-path-dependent-types-in-exprquote) | CRITICAL | Scala 2 |
| 4 | [Macro-internal types leaking into `Expr.quote`](pitfalls.md#4-macro-internal-types-leaking-into-exprquote) | HIGH | Scala 2 |
| 5 | [`Expr.upcast` only widens](pitfalls.md#5-exprupcast-only-widens) | MEDIUM | Both |
| 6 | [`ValDefsCache` wrapping scope](pitfalls.md#6-valdefscache-wrapping-scope) | HIGH | Both |

### Type system details

| # | Pitfall | Severity | Platforms |
|---|---------|----------|-----------|
| 7 | [Macro methods need concrete types](pitfalls.md#7-macro-methods-need-concrete-types) | MEDIUM | Both |
| 8 | [Phantom type-parameter inference](pitfalls.md#8-phantom-type-parameter-inference) | HIGH | Both |
| 9 | [`Type.of[A]` bootstrap cycle in extensions](pitfalls.md#9-typeof-a-bootstrap-cycle-in-extensions) | CRITICAL | Both |
| 10 | [`IsMap` before `IsCollection` ordering](pitfalls.md#10-ismap-before-iscollection-ordering) | HIGH | Both |
| 11 | [`Type.Ctor2.of[Function1].unapply` wrong](pitfalls.md#11-typector2offunction1unapply-wrong) | HIGH | Scala 3 |
| 12 | [`primaryConstructor` strict type checking](pitfalls.md#12-primaryconstructor-strict-type-checking) | HIGH | Both |
| 13 | [`Array` needs `ClassTag`](pitfalls.md#13-array-needs-classtag) | MEDIUM | Both |

### Implicit summoning

| # | Pitfall | Severity | Platforms |
|---|---------|----------|-----------|
| 14 | [`summonExprIgnoring` vs OOM](pitfalls.md#14-summonexprignoring-vs-oom) | CRITICAL | Both |
| 15 | [Newtype aliases in `Expr.quote`](pitfalls.md#15-newtype-aliases-in-exprquote) | HIGH | Scala 2 |
| 16 | [Cross-quotes unused `Type` implicit warnings](pitfalls.md#16-cross-quotes-unused-type-implicit-warnings) | MEDIUM | Both |

### HKT (kind `* -> *`) derivation

| # | Pitfall | Severity | Platforms |
|---|---------|----------|-----------|
| 17 | [HKT type constructor summoning across compilation boundary](pitfalls.md#17-hkt-type-constructor-summoning) | HIGH | Both |
| 18 | [Erased approach for polymorphic type classes](pitfalls.md#18-erased-approach-for-polymorphic-type-classes) | MEDIUM | Scala 2 |
| 19 | [`IsValueType` intercepts single-element named tuples](pitfalls.md#19-isvaluetype-intercepts-named-tuples) | HIGH | Scala 3 |
| 20 | [`MacroExtension` ClassTag erasure](pitfalls.md#20-macroextension-classtag-erasure) | MEDIUM | Both |

### Additional Scala 2 reification pitfalls

| # | Pitfall | Severity | Platforms |
|---|---------|----------|-----------|
| 21 | [`.asInstanceOf` is NOT fully erased for outer types](pitfalls.md#21-asinstanceof-is-not-fully-erased-for-outer-types) | HIGH | JVM |
| 22 | [Scala 2 reification failure with refined types](pitfalls.md#22-scala-2-reification-failure-with-refined-types) | HIGH | Scala 2 |
| 23 | [Helper method pattern for path-dependent types in providers](pitfalls.md#23-helper-method-pattern-for-providers) | MEDIUM | Scala 2 |
| 24 | [Newtype type aliases in `Expr.quote`](pitfalls.md#24-newtype-type-aliases-in-exprquote) | HIGH | Scala 2 |
| 25 | [Macro methods require concrete types at call site](pitfalls.md#25-macro-methods-require-concrete-types) | MEDIUM | Both |
| 26 | [Sibling `Expr.splice` blocks have isolated `Quotes`](pitfalls.md#26-sibling-exprsplice-isolated-quotes) | CRITICAL | Scala 3 |
| 27 | [`ValDefsCache` wrapping scope for multi-function derivation](pitfalls.md#27-valdefscache-wrapping-for-multi-function) | HIGH | Both |
| 28 | [Path-dependent types from `IsMap`/`IsCollection`](pitfalls.md#28-path-dependent-types-from-extractors) | MEDIUM | Scala 3 |
| 29 | [Discriminator-style enum decoding](pitfalls.md#29-discriminator-style-enum-decoding) | MEDIUM | Both |
| 30 | [Built-in type rule is mandatory](pitfalls.md#30-built-in-type-rule-is-mandatory) | HIGH | Both |
| 31 | [`group()` is from `MacroSuite`](pitfalls.md#31-group-is-from-macrosuite) | LOW | Both |
| 32 | [`ProviderResult` is not `Option`](pitfalls.md#32-providerresult-is-not-option) | MEDIUM | Both |
| 33 | [`primaryConstructor` type-checks field expressions strictly](pitfalls.md#33-primaryconstructor-type-checks-strictly) | HIGH | Both |
| 34 | [`parTraverse` and `ValDefsCache` state threading](pitfalls.md#34-partraverse-and-valdefscache-threading) | HIGH | Both |

### Resolved Hearth issues (for context only)

| # | Issue | Status |
|---|-------|--------|
| R1 | `parTraverse` and `ValDefsCache` | Fixed in Hearth 0.2.0-268+ |
| R2 | `loadStandardExtensions` repeated registration | Fixed in Hearth 0.2.0-257 |
| R3 | Cross-quotes `Type.Ctor1` resolution in HKT | Fixed in Hearth 0.2.0-263+ |

## Quick reference: most common pitfalls

The top pitfalls encountered in practice, by frequency:

1. **Path-dependent types in `Expr.quote`** (#3) -- use helper methods with regular type params
2. **Sibling splice isolation** (#1, #26) -- use def-caching, not LambdaBuilder
3. **`summonExprIgnoring` vs OOM** (#14) -- always pass ignored implicits
4. **`IsMap` before `IsCollection`** (#10) -- check map first in pattern match
5. **`Type.Ctor2.of[Function1].unapply`** (#11) -- wrap with `fromUntyped`
6. **`fromUntyped` for ALL SPI-boundary type matching** -- not just Function1; any `Type.Ctor1/Ctor2`
   created in one compilation unit and `unapply`'d in another (via `StandardMacroExtension` SPI)
   needs `fromUntyped` wrapping. Affects: iron/refined integrations, cats collection/map providers,
   cats-derivation contravariant/bifunctor. Pattern: `val MyCtor = Type.Ctor2.fromUntyped[F](Type.Ctor2.of[F].asUntyped)`

See [pitfalls.md](pitfalls.md) for detailed descriptions of all entries.

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture
- [hearth-case-class-rules](../hearth-case-class-rules/SKILL.md) -- case class derivation
- [hearth-enum-rules](../hearth-enum-rules/SKILL.md) -- enum derivation
- [hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md) -- polymorphic derivation
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching (fix for splice isolation)
- [hearth-lambda-builder](../hearth-lambda-builder/SKILL.md) -- LambdaBuilder scope rules
