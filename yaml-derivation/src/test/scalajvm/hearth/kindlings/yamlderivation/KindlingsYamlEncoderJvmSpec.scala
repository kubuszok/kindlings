package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.Node.ScalarNode

final class KindlingsYamlEncoderJvmSpec extends MacroSuite {

  group("KindlingsYamlEncoder (JVM-only)") {

    group("Java enum (enumAsStrings)") {

      test("encode Java enum value as string") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlEncoder.encode[JavaColor](JavaColor.RED) ==> ScalarNode("RED")
      }

      test("encode all Java enum values as strings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlEncoder.encode[JavaColor](JavaColor.GREEN) ==> ScalarNode("GREEN")
        KindlingsYamlEncoder.encode[JavaColor](JavaColor.BLUE) ==> ScalarNode("BLUE")
      }

      test("Java enum with name transform") {
        implicit val config: YamlConfig =
          YamlConfig(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsYamlEncoder.encode[JavaColor](JavaColor.RED) ==> ScalarNode("red")
      }
    }
  }
}
