package hearth.kindlings.jsoniterderivation

import hearth.MacroSuite

/** Hearth gap (docs/research/hearth-gap-scala2-literal-parameter-types.md): on Scala 2 a constructor parameter of a
  * literal type (`s: "VVV"`) is reported with its widened type, so the derived decoder does not compile. Top-level
  * literal types and literal map keys work. When Hearth fixes it, this check fails: move the round-trip of
  * `JsoniterScala3ParitySpec` "case class with literal types" to the shared `JsoniterScalaLiteralParitySpec`.
  */
final class Scala2LiteralFieldsGapSpec extends MacroSuite {

  test("case classes with literal-typed fields are not derivable on Scala 2 yet") {
    compileErrors(
      "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.ParityLiteralTypes]"
    ).check("KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.ParityLiteralTypes]") // surfaces as "type mismatch" or "assignment to non variable", depending on the codegen order
  }
}
