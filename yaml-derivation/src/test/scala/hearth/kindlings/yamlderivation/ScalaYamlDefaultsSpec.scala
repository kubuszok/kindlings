package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

/** The default `YamlConfig` behaves as scala-yaml's own derivation (checked against it by `YamlOracleSpec` in
  * upstream-oracle-tests): constructor defaults fill in absent fields, and sealed traits are untagged.
  */
final class ScalaYamlDefaultsSpec extends MacroSuite {

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  group("defaults") {

    test("absent fields with constructor defaults use them") {
      KindlingsYamlDecoder.decode[WithDefaults](mappingOf("name" -> ScalarNode("Alice"))) ==>
        Right(WithDefaults("Alice", 25, true))
    }

    test("withoutUseDefaults: absent fields fail") {
      implicit val config: YamlConfig = YamlConfig.default.withoutUseDefaults
      assert(KindlingsYamlDecoder.decode[WithDefaults](mappingOf("name" -> ScalarNode("Alice"))).isLeft)
    }
  }

  group("sealed traits") {

    test("subtypes are written untagged") {
      KindlingsYamlEncoder.encode[Shape](Circle(5.0)) ==> mappingOf("radius" -> ScalarNode("5.0"))
    }

    test("case objects are written as empty mappings") {
      KindlingsYamlEncoder.encode[CardinalDirection](North) ==> mappingOf()
    }

    test("decoding tries each subtype; the first that decodes wins") {
      KindlingsYamlDecoder.decode[Shape](mappingOf("radius" -> ScalarNode("5.0"))) ==> Right(Circle(5.0): Shape)
      KindlingsYamlDecoder.decode[Shape](mappingOf("width" -> ScalarNode("1"), "height" -> ScalarNode("2"))) ==>
        Right(Rectangle(1.0, 2.0): Shape)
    }

    test("when no subtype decodes, the failure is scala-yaml's") {
      val node = mappingOf("side" -> ScalarNode("1"))
      val Left(error) = KindlingsYamlDecoder.decode[Shape](node): @unchecked
      assert(error.getMessage.contains("Cannot parse"), error.getMessage)
    }

    test("withWrappedSubtypes: Kindlings 0.3.x single-key mappings") {
      implicit val config: YamlConfig = YamlConfig.default.withWrappedSubtypes
      KindlingsYamlEncoder.encode[Shape](Circle(5.0)) ==> mappingOf(
        "Circle" -> mappingOf("radius" -> ScalarNode("5.0"))
      )
      KindlingsYamlDecoder.decode[Shape](mappingOf("Circle" -> mappingOf("radius" -> ScalarNode("5.0")))) ==>
        Right(Circle(5.0): Shape)
    }

    test("kindlings03Defaults restores both previous defaults") {
      YamlConfig.kindlings03Defaults.useDefaults ==> false
      YamlConfig.kindlings03Defaults.wrappedSubtypes ==> true
    }
  }
}
