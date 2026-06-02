---
name: hearth-enum-rules
description: >
  Deriving type classes for sealed traits and Scala 3 enums: Enum.parse, Enum.matchOn dispatch,
  singleton handling, discriminator decoding, recursive enum self-reference (var-self trick).
paths:
  - "**/compiletime/rules/*Enum*.scala"
  - "**/compiletime/rules/*HandleAsEnum*.scala"
user-invocable: false
---

# Skill: Enum/Sealed Trait Derivation Rules

Patterns for deriving type classes over sealed traits and Scala 3 enums using Hearth's
`Enum.parse` API.

## Basic enum handling

Use `Enum.parse[A]` and `parMatchOn` for exhaustive case handling. Returns `ClassViewResult`.

```scala
object HandleAsEnumRule extends DerivationRule("handle as enum") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          enumm
            .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
              import matched.{value as enumCaseValue, Underlying as EnumCase}
              Log.namedScope(s"Deriving ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
                deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue))
              }
            }
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
    }
}
```

**Key points:**
- `Enum.parse[A]` detects sealed traits, Scala 3 `enum` declarations, and Java enums
- `parMatchOn` generates an exhaustive pattern match over all cases
- Each `matched` provides the case value and its specific type via existential import
- Case objects (singletons) should be handled by a separate `HandleAsSingletonRule` that
  runs before `HandleAsEnumRule` in encoder-style derivation, or within the enum rule
  itself for decoder-style derivation

## Singleton handling within enums

Enum cases that are singletons (case objects, parameterless enum values) have no fields to
derive. Use `SingletonValue.parse[A]` inside the enum match to detect them:

```scala
enumm
  .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
    import matched.{value as enumCaseValue, Underlying as EnumCase}
    SingletonValue.parse[EnumCase].toEither match {
      case Right(sv) =>
        // Singleton case -- use sv.singletonExpr for the value
        MIO.pure(Expr.quote { Expr.splice(ctx.sb).append(Expr.splice(Expr(Type[EnumCase].shortName))) })
      case Left(_) =>
        // Case class case -- recurse into fields
        deriveResultRecursively[EnumCase](using ctx.nest(enumCaseValue))
    }
  }
```

For decoder-style derivation, singletons return the singleton expression directly:
```scala
case Right(sv) => MIO.pure(Rule.matched(sv.singletonExpr))
```

## Discriminator-style enum decoding in streaming codecs

When implementing a combined codec for a streaming JSON library (like jsoniter-scala), the
discriminator-style enum decoder has a fundamental constraint: after reading the
discriminator field from the JSON object, the remaining fields must be read **from the same
already-opened object**, not as a separate nested object.

```json
// Wrapper-style: each child is a separate object under a key
{"Dog": {"name": "Rex", "breed": "Labrador"}}

// Discriminator-style: discriminator + fields are in the SAME object
{"type": "Dog", "name": "Rex", "breed": "Labrador"}
```

**Solution:** Implement two variants:
1. `decodeCaseClassFields` -- reads a full object (with `{` ... `}`)
2. `decodeCaseClassFieldsInline` -- reads remaining fields from an already-opened object

The enum decoder rule must build **two dispatch functions** -- one for wrapper mode and one
for inline/discriminator mode -- and select based on the config:

```scala
children.parTraverse { case (childName, child) =>
  for {
    wrapper <- deriveChildDecoder[A, ChildType](childName)        // full object
    inline  <- deriveChildDecoderInline[A, ChildType](childName)  // fields only
  } yield (wrapper, inline)
}.flatMap { allDispatchers =>
  for {
    wrapperFn <- buildDispatchLambda(allDispatchers.map(_._1))
    inlineFn  <- buildDispatchLambda(allDispatchers.map(_._2))
  } yield Expr.quote {
    config.discriminatorFieldName match {
      case Some(field) => readWithDiscriminator(reader, field)(Expr.splice(inlineFn))
      case None        => readWrapped(reader)(Expr.splice(wrapperFn))
    }
  }
}
```

This issue does not arise in cursor-based libraries (like circe) where the decoder receives
an `HCursor` that can navigate freely.

**Reference:** `jsoniter-derivation/CodecMacrosImpl.scala` -- `decodeEnumCases`,
`deriveChildDecoderInline`, `decodeCaseClassFieldsInline`.
`jsoniter-derivation/runtime/JsoniterDerivationUtils.scala` -- `readWithDiscriminator`,
`readObjectInline`.

## Var-self trick for recursive enums

When deriving codec/encoder/decoder for a recursive enum like:
```scala
sealed trait Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Lit(value: Int) extends Expr
```

The `Enum.matchOn` generates a pattern match that dispatches to per-case derivation. For
recursive types, the cached def pattern (see [hearth-def-caching](../hearth-def-caching/SKILL.md))
handles the recursion: the forward-declared def is referenced before its body is populated,
and the generated code calls itself by name.

For streaming codecs where the enum decoder must reference the outer codec within case
bodies, use the var-self pattern:

```scala
// The generated code looks like:
var self: JsonValueCodec[MyEnum] = null
self = new JsonValueCodec[MyEnum] {
  def decodeValue(reader: JsonReader, default: MyEnum): MyEnum = {
    // ... matchOn dispatches to case decoders that call self.decodeValue(reader, ...)
  }
}
```

The `var self` forward reference allows the codec instance to reference itself before it
is fully constructed. This is safe because the codec is never called during construction.

## Enum derivation for HKT type classes

For polymorphic type classes (kind `* -> *`), enum derivation follows the same
`Enum.parse` + `matchOn` pattern but operates on erased types (`F[Any]`). Each case
within the enum must be classified the same way as case class fields (direct, invariant,
nested). See [hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md) for details.

Key consideration: sealed trait children that are themselves case classes containing
the type parameter `A` need recursive derivation through the erased approach. Children
that are singletons (case objects) are invariant by definition.

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture, rule pattern
- [hearth-case-class-rules](../hearth-case-class-rules/SKILL.md) -- case class derivation
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- cross-compilation pitfalls
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching for recursive types
- [hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md) -- polymorphic enum derivation
