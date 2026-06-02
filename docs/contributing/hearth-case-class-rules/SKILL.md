---
name: hearth-case-class-rules
description: >
  Deriving type classes for case classes: CaseClass.parse, primaryConstructor, field access,
  Expr_?? existential types, decoder construction, nest vs nestInCache, dual-path derivation.
paths:
  - "**/compiletime/rules/*CaseClass*.scala"
  - "**/compiletime/rules/*HandleAsCaseClass*.scala"
user-invocable: false
---

# Skill: Case Class Derivation Rules

Patterns for deriving type classes over case classes using Hearth's `CaseClass.parse` API.
Covers both encoder-style (reading fields) and decoder-style (constructing types).

## Handling case classes (encoder-style)

Use `CaseClass.parse[A]` to introspect case class structure. Returns `ClassViewResult` --
use `.toEither` for skip reasons.

```scala
object HandleAsCaseClassRule extends DerivationRule("handle as case class") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          val name = Expr(Type[A].shortName)
          NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
            case Some(fieldValues) =>
              // derive each field recursively
            case None =>
              // handle zero-field case class
          }
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
    }
}
```

**Key points:**
- `caseClass.caseFieldValuesAt(expr)` extracts typed field values from an existing instance
- Each field value is an existential with `import fieldValue.{Underlying as Field, value as fieldExpr}`
- Use `ctx.nest(fieldExpr)` to recurse into each field
- Use `.parTraverse` for parallel field derivation

## Decoder-style derivation: constructing types

**Reference:** `circe-derivation/DecoderMacrosImpl.scala`

Encoder-style derivation **reads** fields via `caseFieldValuesAt(expr)`. Decoder-style
**constructs** a value from separately decoded fields. See [reference.md](reference.md)
for detailed patterns including:

- Recursive flatMap chain (recommended approach)
- Collect-then-construct with runtime type witness
- `primaryConstructor(Map[String, Expr_??])` vs `construct`
- `Expr_??` and `as_??` existential types
- `LambdaBuilder` for decoder lambdas

### Using `primaryConstructor` directly

`CaseClass` provides two ways to construct instances:

1. **`caseClass.construct[F](makeArgument)`** -- uses `ConstructField[F]` with a dependent
   return type. Has path-dependent type issues on Scala 2.

2. **`caseClass.primaryConstructor(fieldMap: Map[String, Expr_??])`** -- takes a
   `Map[String, Expr_??]` and returns `Either[String, Expr[A]]`. Avoids path-dependent
   types because `Expr_??` is an existential.

**Always prefer `primaryConstructor(fieldMap)` for decoder-style derivation.**

### Key API: `Expr_??` and `as_??`

`Expr_??` is `Existential[Expr]` -- wraps an `Expr[A]` with its `Type[A]` proof, erasing
the concrete type from the outer signature.

```scala
// Wrapping: any Expr[A] with Type[A] in scope can become Expr_??
val existential: Expr_?? = someExpr.as_??

// Consuming: import brings type and value back into scope
import existential.{Underlying as FieldType, value as expr}
// Now: implicit FieldType: Type[FieldType] and expr: Expr[FieldType]
```

Use `Expr_??` whenever you need to store heterogeneously-typed expressions in a collection
(e.g., a field map for `primaryConstructor`).

## Advanced decoder patterns

See [reference.md](reference.md) for detailed documentation of these patterns:

- **`nest` vs `nestInCache` semantics** -- when sub-contexts must hardcode mode-specific
  state vs preserving cached def parameters
- **Dual-path derivation with runtime boolean** -- single cached def handling both
  fail-fast and error-accumulating paths via a `failFast: Boolean` parameter
- **ValDefsCache key collisions** -- when return type is `Any`, the string key must
  disambiguate (use `s"cached-decode-method:${Type[B].prettyPrint}"`)
- **`directDecoderOpt` pattern** -- for types without cached helpers (value types, options,
  collections, maps)

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture, rule pattern, caching
- [hearth-enum-rules](../hearth-enum-rules/SKILL.md) -- enum/sealed trait derivation
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- path-dependent type pitfalls
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching for multi-method instances
- [lambda-builder-when-to-use](../hearth-lambda-builder/SKILL.md) -- when to use LambdaBuilder
