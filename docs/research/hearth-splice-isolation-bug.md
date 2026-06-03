# Hearth Bug: Splice Isolation with `toValDefs.use` + `parMatchOn` for `Option[SealedTrait]`

## Summary

On Scala 3, deriving an encoder for a case class containing `Option[SealedTrait]`
(e.g., `Option[Shape]` where `Shape` is a sealed trait with case class children)
fails at compile time with a cross-Quotes splice isolation error.

## Reproducer

```scala
sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

case class CombOuter(
  optSealedTrait: Option[Shape],
  // other fields don't matter — just having Option[Shape] is enough
)

// This compiles on Scala 2.13 but fails on Scala 3:
val encoder = KindlingsEncoder.derived[CombOuter]
```

## Error

```
Macro derivation failed with the following errors:
  - Expression created in a splice was used outside of that splice.
    Created in: .../EncoderMacrosImpl.scala:39 at column 25
    Used in: .../KindlingsEncoderCompanionCompat.scala:11 at column 78
    Type: Shape
```

## Root Cause

When `CaseClass[CombOuter]` is parsed and the `Option[Shape]` field is encountered:
1. The Option rule derives the inner encoder for `Shape`
2. `Shape` is a sealed trait, so `Enum.parse[Shape]` runs `parMatchOn`
3. `parMatchOn` creates match cases in parallel MIO branches
4. `toValDefs.use` wraps the outer `Expr.quote { new Encoder[CombOuter] { ... } }`
5. The `Shape` match cases reference `Quotes` from a different splice context

The issue is that `parMatchOn` creates expressions under one `Quotes` instance, but
they get spliced into a different context when `toValDefs.use` wraps the entire
type class instance.

## Affected Modules

Every encoder-style module that derives for `Option[SealedTrait]` on Scala 3:
- circe-derivation (`KindlingsEncoder`)
- sconfig-derivation (`ConfigWriter`)
- xml-derivation (`KindlingsXmlEncoder`)
- pureconfig-derivation (`KindlingsConfigWriter`)
- (potentially any module using `parMatchOn` inside an Option context)

Decoder-style modules are NOT affected (they use different code paths).

## Workaround

None. The type `Option[SealedTrait]` cannot be derived as part of a case class
on Scala 3. Users must provide explicit encoder instances for the outer type.

## Test Cases

- `circe-derivation/.../RoundTripSpec.scala` — `CombOuter` tests (compile on 2.13, fail on 3)
- `pureconfig-derivation/.../KindlingsConfigWriterSpec.scala` — `CombOuter` tests
- `sconfig-derivation/.../ConfigWriterSpec.scala` — `CombOuter` tests

## Expected Fix

The `Quotes` instance used by `parMatchOn` should be the same as the one captured
by the outer `toValDefs.use` context. This likely requires either:
- Threading the outer `Quotes` through `parMatchOn`, or
- Deferring the match case expression creation to the outer splice context
