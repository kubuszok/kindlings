# Hearth gap: literal-typed constructor parameters on Scala 2

Status: **RESOLVED** in Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (kubuszok/hearth#401, to be released as 0.4.4). The cause was
not `Parameter.tpe`: quoted lambdas (`Expr.quote { (r: R) => ... }`) and `LambdaBuilder.build` widened a literal
result type (`"VVV"` to `String`), and types read from class files made a `var` of a literal type unassignable
("assignment to non variable", only in incremental builds). `Scala2LiteralFieldsGapSpec` is deleted; the
`ParityLiteralTypes` round trip moved from `JsoniterScala3ParitySpec` to the shared `JsoniterScalaLiteralParitySpec`,
and compiles on Scala 2.13 both after `clean` and when only the spec is recompiled.

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
