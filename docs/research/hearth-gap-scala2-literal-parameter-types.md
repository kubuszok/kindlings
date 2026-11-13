# Hearth gap: literal-typed constructor parameters on Scala 2

Found while porting jsoniter-scala's `JsonCodecMakerLiteralTypesSpec` to jsoniter-derivation.

```scala
case class TmpLitI(i: 2)
KindlingsJsonValueCodec.derived[TmpLitI]   // Scala 2.13: type mismatch; found: Int, required: 2
case class TmpLitS(s: "VVV")
KindlingsJsonValueCodec.derived[TmpLitS]   // Scala 2.13: type mismatch; found: String, required: "VVV"
```

The same derivation works on Scala 3, and on Scala 2 the literal types themselves are handled
(`KindlingsJsonValueCodec.derived[2]`, `derived[Map[2, Int]]`, `derived['1']` compile and round-trip). The case class
decoder derives each field's decoder from `Parameter.tpe`; on Scala 2 that type appears to be the *widened* type of a
literal-typed parameter (`Int` instead of `2`), so the decoded `Int` is passed to the constructor expecting `2`.

Suggested fix: `Parameter.tpe` on Scala 2 should keep `ConstantType`s of constructor parameters (as on Scala 3).

Pinned by `Scala2LiteralFieldsGapSpec` (scala-2 test dir); the Scala 3 round-trip lives in `JsoniterScala3ParitySpec`.
