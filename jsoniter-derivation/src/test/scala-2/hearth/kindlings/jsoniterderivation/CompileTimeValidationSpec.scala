package hearth.kindlings.jsoniterderivation

import hearth.MacroSuite

/** Compile-time validations of jsoniter-scala (Spec ~2365, ~2731, ~2803, ~3195, ~3223), for compile-time-known configs.
  * Kept in sync with the Scala 3 version of this file: the only difference is the syntax of passing the config.
  */
final class CompileTimeValidationSpec extends MacroSuite {

  group("compile-time validations (jsoniter-scala parity)") {

    test("conflicting config flags") {
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[Int](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withDecodingOnly.withEncodingOnly)"
      ).check("'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[List[hearth.kindlings.jsoniterderivation.ParityAdt]](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withDiscriminator(\"type\").withCirceLikeObjectEncoding)"
      ).check("'discriminatorFieldName' should be 'None' when 'circeLikeObjectEncoding' is 'true'")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.ParityAAA](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withAlwaysEmitDiscriminator)"
      ).check("'discriminatorFieldName' should not be 'None' when 'alwaysEmitDiscriminator' is 'true'")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[Int](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withTransientEmpty.withRequireCollectionFields)"
      ).check("'requireCollectionFields' and 'transientEmpty' cannot be 'true' simultaneously")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[Int](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withTransientDefault.withRequireDefaultFields)"
      ).check("'requireDefaultFields' and 'transientDefault' cannot be 'true' simultaneously")
    }

    test("field name collisions, also with the discriminator") {
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.CollidingFields](hearth.kindlings.jsoniterderivation.JsoniterConfig.default)"
      ).check("Duplicated JSON key(s) defined for")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.CamelFields](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withSnakeCaseFieldNames)"
      ).check("'foo_bar'")
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.CollidingAdt](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withDiscriminator(\"type\"))"
      ).check("Duplicated JSON key(s) defined for")
    }

    test("a leaf derived on its own may have a field named like the discriminator (unless it writes it)") {
      val codec = KindlingsJsonValueCodec.derived[CollidingLeaf](JsoniterConfig.default.withDiscriminator("type"))
      com.github.plokhotnyuk.jsoniter_scala.core.writeToString(CollidingLeaf("x"))(codec) ==> """{"type":"x"}"""
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.CollidingLeaf](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withDiscriminator(\"type\").withAlwaysEmitDiscriminator)"
      ).check("Duplicated JSON key(s) defined for")
    }

    test("duplicated discriminator values after mapping") {
      compileErrors(
        "hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.CollidingNames](hearth.kindlings.jsoniterderivation.JsoniterConfig.default.withAdtLeafClassNameMapper(_ => \"same\"))"
      ).check("Duplicated discriminator defined for ADT base")
    }
  }
}
