package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.Node.ScalarNode

final class KindlingsYamlDecoderJvmSpec extends MacroSuite {

  group("KindlingsYamlDecoder (JVM-only)") {

    group("Java enum decoder (enumAsStrings)") {

      test("decode Java enum value from string") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlDecoder.decode[JavaColor](ScalarNode("RED")) ==> Right(JavaColor.RED: JavaColor)
      }

      test("decode all Java enum values from strings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlDecoder.decode[JavaColor](ScalarNode("GREEN")) ==> Right(JavaColor.GREEN: JavaColor)
        KindlingsYamlDecoder.decode[JavaColor](ScalarNode("BLUE")) ==> Right(JavaColor.BLUE: JavaColor)
      }

      test("Java enum with name transform") {
        implicit val config: YamlConfig =
          YamlConfig(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsYamlDecoder.decode[JavaColor](ScalarNode("red")) ==> Right(JavaColor.RED: JavaColor)
      }
    }
  }
}
