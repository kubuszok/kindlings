package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** Java enums as in jsoniter-scala (Spec ~640, ~784): always JSON strings, named by `javaEnumValueNameMapper`, also as
  * map keys. JVM-only, because the Java enum fixture is a Java source.
  */
final class JavaEnumParitySpec extends MacroSuite {

  group("Java enums (jsoniter-scala parity)") {

    test("are strings whatever the discriminator, also as map keys") {
      val value = JavaEnumHolder(JavaColor.GREEN, Map(JavaColor.RED -> 1))
      val json = """{"c":"GREEN","m":{"RED":1}}"""
      val runtimeCodec = {
        implicit val c: JsoniterConfig = JavaEnumRuntimeConfigs.discriminator
        KindlingsJsonValueCodec.derived[JavaEnumHolder]
      }
      List[JsonValueCodec[JavaEnumHolder]](KindlingsJsonValueCodec.derived[JavaEnumHolder], runtimeCodec).foreach { c =>
        writeToString(value)(c) ==> json
        readFromString[JavaEnumHolder](json)(c) ==> value
        val error = intercept[JsonReaderException](readFromString[JavaEnumHolder]("""{"c":"XXX","m":{}}""")(c))
        assert(error.getMessage.contains("illegal enum value \"XXX\""), error.getMessage)
      }
    }

    test("javaEnumValueNameMapper renames values and map keys, adtLeafClassNameMapper does not apply") {
      val value = JavaEnumHolder(JavaColor.GREEN, Map(JavaColor.RED -> 1))
      val json = """{"c":"green","m":{"red":1}}"""
      val runtimeCodec = {
        implicit val c: JsoniterConfig = JavaEnumRuntimeConfigs.lowerCase
        KindlingsJsonValueCodec.derived[JavaEnumHolder]
      }
      writeToString(value)(runtimeCodec) ==> json
      readFromString[JavaEnumHolder](json)(runtimeCodec) ==> value
    }
  }
}

case class JavaEnumHolder(c: JavaColor, m: Map[JavaColor, Int])

object JavaEnumRuntimeConfigs {
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  @volatile var lowerCase: JsoniterConfig =
    JsoniterConfig.default.withJavaEnumValueNameMapper(_.toLowerCase).withAdtLeafClassNameMapper(_ => "WRONG")
}
