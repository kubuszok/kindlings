# Hearth Feature Request: `semiEval` partial evaluation for configs with function fields

**Hearth version**: 0.3.0-94+ (0.4.0 branch)
**Affects**: jsoniter-derivation on both Scala 2 and 3
**Type**: Feature request

## Problem

`semiEval` cannot evaluate `JsoniterConfig` at compile time because it contains
function-type fields:

```scala
case class JsoniterConfig(
  fieldNameMapper: String => String = identity,    // ← function field
  adtLeafClassNameMapper: String => String = identity,  // ← function field
  discriminatorFieldName: Option[String] = None,
  skipNestedOptionValues: Boolean = false,
  alwaysEmitDiscriminator: Boolean = false,
  inlineOneValueClasses: Boolean = false,
  // ... more boolean/string fields
)
```

When `semiEval[JsoniterConfig]` encounters function fields (`String => String`), it
fails entirely — it cannot reify lambda values at compile time. This means the ENTIRE
config is unavailable at compile time, even though the non-function fields (booleans,
strings, options) are perfectly evaluable.

## Impact

Three jsoniter-derivation features are blocked:

| Feature | Config field | Why blocked |
|---|---|---|
| `skipNestedOptionValues` | `Boolean` | Rule needs `evaluatedConfig` to decide code path at compile time |
| `alwaysEmitDiscriminator` | `Boolean` | Same — compile-time code path selection |
| `inlineOneValueClasses` | `Boolean` | Rule files exist and are wired in, but `evaluatedConfig` is always `None` |

All three use boolean fields that ARE evaluable — the function fields just prevent
semiEval from succeeding on the whole config.

## Proposed solution: Partial semiEval

Allow `semiEval` to partially evaluate a case class, producing a result where:
- Primitive/String/Option fields that can be evaluated → concrete values
- Function fields that can't be evaluated → `None` / sentinel / left as `Expr`

### Option A: Field-level `semiEval`

```scala
// Instead of:
val config: Option[JsoniterConfig] = semiEval[JsoniterConfig](configExpr)

// Allow:
val skipNested: Option[Boolean] = semiEvalField[JsoniterConfig, Boolean](configExpr, "skipNestedOptionValues")
```

### Option B: Partial config with `Option` wrapping

```scala
// semiEval returns a "partial" result where unevaluable fields are None
case class PartialJsoniterConfig(
  fieldNameMapper: Option[String => String],  // None (can't evaluate)
  skipNestedOptionValues: Option[Boolean],     // Some(false)
  // ...
)
```

### Option C: Skip unevaluable fields

`semiEval` succeeds for the whole config but sets function fields to a default
(e.g., `identity`). The caller knows that function fields may not reflect the
actual runtime value.

## Current workaround attempts

The kindlings codebase currently:
1. Tries `semiEval[JsoniterConfig](configExpr)` → fails, returns `None`
2. Falls back to runtime-only code paths (no compile-time optimization)
3. Rules that depend on `evaluatedConfig` (5.4, 5.5, 5.6) never fire

A kindlings-side workaround would be to add runtime branching:
```scala
if (evaluatedConfig.exists(_.skipNestedOptionValues)) {
  // compile-time optimized path
} else {
  // runtime check: if (config.skipNestedOptionValues) { ... }
}
```

But this doubles the generated code size and loses the compile-time optimization
benefit that semiEval provides.

## Context

Other config types in kindlings (circe `Configuration`, pureconfig `ConfigReaderConfig`,
etc.) don't have function fields and work fine with `semiEval`. The jsoniter config is
the only one with this issue because it bundles field name mappers alongside boolean flags.
