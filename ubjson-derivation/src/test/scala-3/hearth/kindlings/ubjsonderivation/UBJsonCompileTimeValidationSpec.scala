package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

/** Contradictory configs known at compile time fail the derivation, with jsoniter-scala's messages (the macro evaluated
  * the config: the optimized codegen). Per Scala version only because passing the config explicitly differs.
  */
final class UBJsonCompileTimeValidationSpec extends MacroSuite {

  test("contradictory compile-time configs fail the derivation") {
    compileErrors(
      "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbBigInt](using hearth.kindlings.ubjsonderivation.UBJsonConfig().withRequireDefaultFields.withTransientDefault)"
    ).check("'requireDefaultFields' and 'transientDefault' cannot be 'true' simultaneously")
    compileErrors(
      "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbBigInt](using hearth.kindlings.ubjsonderivation.UBJsonConfig().withTransientEmpty.withRequireCollectionFields)"
    ).check("'requireCollectionFields' and 'transientEmpty' cannot be 'true' simultaneously")
    compileErrors(
      "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[List[hearth.kindlings.ubjsonderivation.UbMixed]](using hearth.kindlings.ubjsonderivation.UBJsonConfig().withDiscriminator(\"type\").withCirceLikeObjectEncoding)"
    ).check("'discriminatorFieldName' should be 'None' when 'circeLikeObjectEncoding' is 'true'")
  }

  test("with a compile-time config, scalaTransientSupport makes scala.transient fields transient (default required)") {
    compileErrors(
      "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbScalaTransientNoDefault](using hearth.kindlings.ubjsonderivation.UBJsonConfig.default.withScalaTransientSupport)"
    ).check("field 'b' of")
  }
}
