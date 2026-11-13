package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** jsoniter-scala's name mappers (`JsonCodecMaker.enforceCamelCase` etc., Spec ~3678ff), ported as `JsoniterConfig`
  * helpers, and the fully-qualified names passed to `adtLeafClassNameMapper`.
  */
final class JsoniterConfigNameMappersSpec extends MacroSuite {

  group("name mappers (jsoniter-scala parity)") {

    test("upstream test vectors") {
      JsoniterConfig.enforceCamelCase("o_o") ==> "oO"
      JsoniterConfig.enforceCamelCase("o_ooo_") ==> "oOoo"
      JsoniterConfig.enforceCamelCase("OO_OOO_111") ==> "ooOoo111"
      JsoniterConfig.enforceCamelCase("ooo_111") ==> "ooo111"
      JsoniterConfig.enforceCamelCase("o-o") ==> "oO"
      JsoniterConfig.enforceCamelCase("o-ooo-") ==> "oOoo"
      JsoniterConfig.enforceCamelCase("O-OOO-111") ==> "oOoo111"
      JsoniterConfig.enforceCamelCase("OoOo111") ==> "ooOo111"
      JsoniterConfig.enforceCamelCase("OOoo111") ==> "oOoo111"
      JsoniterConfig.enforceCamelCase("OOOOooo") ==> "oooOooo"
      JsoniterConfig.enforceCamelCase("OOOO") ==> "oooo"
      JsoniterConfig.enforceCamelCase("") ==> ""
      JsoniterConfig.enforceCamelCase("o") ==> "o"
      JsoniterConfig.enforceCamelCase("oO") ==> "oO"
      JsoniterConfig.enforceCamelCase("oOoo") ==> "oOoo"
      JsoniterConfig.enforceCamelCase("oooOooo") ==> "oooOooo"
      JsoniterConfig.enforceCamelCase("oooo") ==> "oooo"
      JsoniterConfig.EnforcePascalCase("o_o") ==> "OO"
      JsoniterConfig.EnforcePascalCase("o_ooo_") ==> "OOoo"
      JsoniterConfig.EnforcePascalCase("OO_OOO_111") ==> "OoOoo111"
      JsoniterConfig.EnforcePascalCase("ooo_111") ==> "Ooo111"
      JsoniterConfig.EnforcePascalCase("o-o") ==> "OO"
      JsoniterConfig.EnforcePascalCase("o-ooo-") ==> "OOoo"
      JsoniterConfig.EnforcePascalCase("O-OOO-111") ==> "OOoo111"
      JsoniterConfig.EnforcePascalCase("ooOo111") ==> "OoOo111"
      JsoniterConfig.EnforcePascalCase("oOoo111") ==> "OOoo111"
      JsoniterConfig.EnforcePascalCase("") ==> ""
      JsoniterConfig.EnforcePascalCase("O") ==> "O"
      JsoniterConfig.EnforcePascalCase("Oo") ==> "Oo"
      JsoniterConfig.EnforcePascalCase("OOoo") ==> "OOoo"
      JsoniterConfig.EnforcePascalCase("OOOOooo") ==> "OooOooo"
      JsoniterConfig.EnforcePascalCase("OOOO") ==> "Oooo"
      JsoniterConfig.enforce_snake_case("oO") ==> "o_o"
      JsoniterConfig.enforce_snake_case("oOoo") ==> "o_ooo"
      JsoniterConfig.enforce_snake_case("oOoo111") ==> "o_ooo_111"
      JsoniterConfig.enforce_snake_case("oOoo$") ==> "o_ooo_$"
      JsoniterConfig.enforce_snake_case("Oo") ==> "oo"
      JsoniterConfig.enforce_snake_case("Ooo111") ==> "ooo_111"
      JsoniterConfig.enforce_snake_case("OOOoo111") ==> "oo_ooo_111"
      JsoniterConfig.enforce_snake_case("OOOoo$") ==> "oo_ooo_$"
      JsoniterConfig.enforce_snake_case("o-o") ==> "o_o"
      JsoniterConfig.enforce_snake_case("o-ooo-") ==> "o_ooo_"
      JsoniterConfig.enforce_snake_case("o-ooo111") ==> "o_ooo_111"
      JsoniterConfig.enforce_snake_case("o-ooo-111") ==> "o_ooo_111"
      JsoniterConfig.enforce_snake_case("o-ooo-$") ==> "o_ooo_$"
      JsoniterConfig.enforce_snake_case("o_o") ==> "o_o"
      JsoniterConfig.enforce_snake_case("o_ooo_") ==> "o_ooo_"
      JsoniterConfig.enforce_snake_case("o_ooo111") ==> "o_ooo_111"
      JsoniterConfig.enforce_snake_case("o_ooo_111") ==> "o_ooo_111"
      JsoniterConfig.enforce_snake_case("o_ooo$") ==> "o_ooo_$"
      JsoniterConfig.enforce_snake_case("OOO_OOO") ==> "ooo_ooo"
      JsoniterConfig.enforce_snake_case2("oO") ==> "o_o"
      JsoniterConfig.enforce_snake_case2("oOoo") ==> "o_ooo"
      JsoniterConfig.enforce_snake_case2("oOoo111") ==> "o_ooo111"
      JsoniterConfig.enforce_snake_case2("oOoo$") ==> "o_ooo$"
      JsoniterConfig.enforce_snake_case2("Oo") ==> "oo"
      JsoniterConfig.enforce_snake_case2("Ooo111") ==> "ooo111"
      JsoniterConfig.enforce_snake_case2("OOOoo111") ==> "oo_ooo111"
      JsoniterConfig.enforce_snake_case2("OOOoo$") ==> "oo_ooo$"
      JsoniterConfig.enforce_snake_case2("o-o") ==> "o_o"
      JsoniterConfig.enforce_snake_case2("o-ooo-") ==> "o_ooo_"
      JsoniterConfig.enforce_snake_case2("o-ooo111") ==> "o_ooo111"
      JsoniterConfig.enforce_snake_case2("o-ooo-111") ==> "o_ooo111"
      JsoniterConfig.enforce_snake_case2("o-ooo-$") ==> "o_ooo$"
      JsoniterConfig.enforce_snake_case2("o_o") ==> "o_o"
      JsoniterConfig.enforce_snake_case2("o_ooo_") ==> "o_ooo_"
      JsoniterConfig.enforce_snake_case2("o_ooo111") ==> "o_ooo111"
      JsoniterConfig.enforce_snake_case2("o_ooo_111") ==> "o_ooo111"
      JsoniterConfig.enforce_snake_case2("o_ooo_$") ==> "o_ooo$"
      JsoniterConfig.enforce_snake_case2("OOO_OOO") ==> "ooo_ooo"
      JsoniterConfig.`enforce-kebab-case`("oO") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case`("oOoo") ==> "o-ooo"
      JsoniterConfig.`enforce-kebab-case`("oOoo111") ==> "o-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("oOoo$") ==> "o-ooo-$"
      JsoniterConfig.`enforce-kebab-case`("Oo") ==> "oo"
      JsoniterConfig.`enforce-kebab-case`("Ooo111") ==> "ooo-111"
      JsoniterConfig.`enforce-kebab-case`("OOOoo111") ==> "oo-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("OOOoo$") ==> "oo-ooo-$"
      JsoniterConfig.`enforce-kebab-case`("o_o") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case`("o_ooo_") ==> "o-ooo-"
      JsoniterConfig.`enforce-kebab-case`("o_ooo111") ==> "o-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("o_ooo_111") ==> "o-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("o_ooo_$") ==> "o-ooo-$"
      JsoniterConfig.`enforce-kebab-case`("o-o") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case`("o-ooo-") ==> "o-ooo-"
      JsoniterConfig.`enforce-kebab-case`("o-ooo111") ==> "o-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("o-ooo-111") ==> "o-ooo-111"
      JsoniterConfig.`enforce-kebab-case`("o-ooo$") ==> "o-ooo-$"
      JsoniterConfig.`enforce-kebab-case2`("oO") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case2`("oOoo") ==> "o-ooo"
      JsoniterConfig.`enforce-kebab-case2`("oOoo111") ==> "o-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("oOoo$") ==> "o-ooo$"
      JsoniterConfig.`enforce-kebab-case2`("Oo") ==> "oo"
      JsoniterConfig.`enforce-kebab-case2`("Ooo111") ==> "ooo111"
      JsoniterConfig.`enforce-kebab-case2`("OOOoo111") ==> "oo-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("OOOoo$") ==> "oo-ooo$"
      JsoniterConfig.`enforce-kebab-case2`("o_o") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case2`("o_ooo_") ==> "o-ooo-"
      JsoniterConfig.`enforce-kebab-case2`("o_ooo111") ==> "o-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("o_ooo_111") ==> "o-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("o_ooo_$") ==> "o-ooo$"
      JsoniterConfig.`enforce-kebab-case2`("o-o") ==> "o-o"
      JsoniterConfig.`enforce-kebab-case2`("o-ooo-") ==> "o-ooo-"
      JsoniterConfig.`enforce-kebab-case2`("o-ooo111") ==> "o-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("o-ooo-111") ==> "o-ooo111"
      JsoniterConfig.`enforce-kebab-case2`("o-ooo-$") ==> "o-ooo$"
      JsoniterConfig.simpleClassName("com.github.plohkotnyuk.jsoniter_scala.Test") ==> "Test"
      JsoniterConfig.simpleClassName("JsonCodecMakerSpec.this.Test") ==> "Test"
      JsoniterConfig.simpleClassName(".Test") ==> "Test"
      JsoniterConfig.simpleClassName("Test") ==> "Test"
    }

    test("snake/kebab-case of acronyms and digits") {
      JsoniterConfig.enforce_snake_case("HTTPServer") ==> "http_server"
      JsoniterConfig.enforce_snake_case("field1") ==> "field_1"
      JsoniterConfig.`enforce-kebab-case`("HTTPServer") ==> "http-server"
      JsoniterConfig.screamingSnakeCase("httpServer1") ==> "HTTP_SERVER_1"
      val codec = {
        implicit val c: JsoniterConfig = JsoniterConfigNameMappersSpec.snakeCase
        KindlingsJsonValueCodec.derived[MapperFields]
      }
      writeToString(MapperFields(1, 2))(codec) ==> """{"http_server":1,"field_1":2}"""
      readFromString[MapperFields]("""{"http_server":1,"field_1":2}""")(codec) ==> MapperFields(1, 2)
    }

    test("adtLeafClassNameMapper receives fully-qualified names, simpleClassName is the default") {
      val identityCodec = {
        implicit val c: JsoniterConfig = JsoniterConfigNameMappersSpec.identityLeafNames
        KindlingsJsonValueCodec.derived[List[ParityAdt]]
      }
      val values = List[ParityAdt](ParityAAA(1), ParityDDD)
      val json =
        """[{"type":"hearth.kindlings.jsoniterderivation.ParityAAA","a":1},{"type":"hearth.kindlings.jsoniterderivation.ParityDDD"}]"""
      writeToString(values)(identityCodec) ==> json
      readFromString[List[ParityAdt]](json)(identityCodec) ==> values
      JsoniterConfig.default.adtLeafClassNameMapper("a.b.C") ==> "C"
    }

    test("name mappers may be partial functions: other names stay unchanged (upstream applies `lift`)") {
      val runtimeCodec = {
        implicit val c: JsoniterConfig = JsoniterConfigNameMappersSpec.partialMappersRuntime
        KindlingsJsonValueCodec.derived[List[ParityAdt]]
      }
      val compileTimeCodec = {
        implicit val c: JsoniterConfig = JsoniterConfigNameMappersSpec.partialMappers
        KindlingsJsonValueCodec.derived[List[ParityAdt]]
      }
      val values = List[ParityAdt](ParityAAA(1), ParityDDD)
      val json = """[{"type":"aaa","b":1},{"type":"ParityDDD"}]"""
      List(runtimeCodec, compileTimeCodec).foreach { c =>
        writeToString(values)(c) ==> json
        readFromString[List[ParityAdt]](json)(c) ==> values
      }
      JsoniterConfig.default.withJavaEnumValueNameMapper { case "X" => "y" }.javaEnumValueNameMapper("Z") ==> "Z"
      JsoniterConfig.default.withFieldNameMapper((s: String) => s.toUpperCase).fieldNameMapper("a") ==> "A"
    }
  }
}

object JsoniterConfigNameMappersSpec {
  val partialMappers: JsoniterConfig = JsoniterConfig.default
    .withDiscriminator("type")
    .withFieldNameMapper { case "a" => "b" }
    .withAdtLeafClassNameMapper {
      case "hearth.kindlings.jsoniterderivation.ParityAAA" => "aaa"; case s => JsoniterConfig.simpleClassName(s)
    }
  @volatile var partialMappersRuntime: JsoniterConfig = partialMappers
  val snakeCase: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
  val identityLeafNames: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAdtLeafClassNameMapper(identity)
}
