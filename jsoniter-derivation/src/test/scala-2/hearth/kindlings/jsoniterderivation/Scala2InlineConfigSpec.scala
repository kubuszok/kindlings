package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** Scala 2: a config argument of `derived(...)` (and of the other entry points) that defines code (a partial-function
  * literal, a lambda) is evaluated once, not duplicated into every place of the generated codec that reads the config.
  */
final class Scala2InlineConfigSpec extends MacroSuite {

  test("a partial-function literal inside the config argument of derived(...)") {
    val c = KindlingsJsonValueCodec.derived[ParityAdt](
      JsoniterConfig.default
        .withDiscriminator("type")
        .withAdtLeafClassNameMapper {
          case "hearth.kindlings.jsoniterderivation.ParityAAA" => "aaa"
          case s                                                => JsoniterConfig.simpleClassName(s)
        }
        .withFieldNameMapper { case "a" => "A" }
    )
    writeToString[ParityAdt](ParityAAA(1))(c) ==> """{"type":"aaa","A":1}"""
    readFromString[ParityAdt]("""{"type":"aaa","A":1}""")(c) ==> ParityAAA(1)
  }

  test("a lambda inside the config argument of derived(...)") {
    val c = KindlingsJsonValueCodec.derived[ParityAAA](JsoniterConfig.default.withFieldNameMapper(_.toUpperCase))
    writeToString(ParityAAA(1))(c) ==> """{"A":1}"""
    readFromString[ParityAAA]("""{"A":1}""")(c) ==> ParityAAA(1)
  }

  test("a partial-function literal inside the config of KindlingsJsonCodec, inline and key codecs") {
    val codec = KindlingsJsonCodec.derived[ParityAAA](JsoniterConfig.default.withFieldNameMapper { case "a" => "x" })
    writeToString(ParityAAA(1))(codec) ==> """{"x":1}"""
    val _ = KindlingsJsonCodec.deriveKeyCodec[Int](JsoniterConfig.default.withFieldNameMapper { case "a" => "x" })
    KindlingsJsonValueCodec.writeToString(ParityAAA(2))(JsoniterConfig.default.withFieldNameMapper { case "a" => "y" }) ==>
      """{"y":2}"""
    KindlingsJsonValueCodec.readFromString[ParityAAA]("""{"y":2}""")(
      JsoniterConfig.default.withFieldNameMapper { case "a" => "y" }
    ) ==> Right(ParityAAA(2))
  }

  test("a config from an object of the same module is a runtime config, also in incremental builds") {
    // Evaluated at compile time, the contradictory flags would fail the compilation; at runtime they throw.
    val error = intercept[IllegalArgumentException](KindlingsJsonValueCodec.derived[Int](SameModuleConfigs.decodingAndEncodingOnly))
    error.getMessage ==> "'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously"
  }
}
