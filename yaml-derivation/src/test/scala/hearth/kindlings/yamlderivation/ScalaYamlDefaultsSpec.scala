package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{Node, YamlEncoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

/** The default `YamlConfig` behaves as scala-yaml's own derivation (checked against it by `YamlOracleSpec` in
  * upstream-oracle-tests): constructor defaults fill in absent fields, and sealed traits are untagged.
  */
final class ScalaYamlDefaultsSpec extends MacroSuite {

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  // A Double as scala-yaml's own encoder writes it on this platform (`toString`: `5.0` on the JVM, `5` on Scala.js)
  private def double(d: Double): Node = YamlEncoder.forDouble.asNode(d)

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
      KindlingsYamlEncoder.encode[Shape](Circle(5.0)) ==> mappingOf("radius" -> double(5.0))
    }

    test("case objects are written as empty mappings") {
      KindlingsYamlEncoder.encode[CardinalDirection](North) ==> mappingOf()
    }

    test("decoding tries each subtype; the first that decodes wins") {
      KindlingsYamlDecoder.decode[Shape](mappingOf("radius" -> double(5.0))) ==> Right(Circle(5.0): Shape)
      KindlingsYamlDecoder.decode[Shape](mappingOf("width" -> ScalarNode("1"), "height" -> ScalarNode("2"))) ==>
        Right(Rectangle(1.0, 2.0): Shape)
    }

    test("case objects decode from any mapping with scalar keys, as scala-yaml's empty products") {
      // The first subtype tried wins: in declaration order on Scala 3 (as scala-yaml), by name on Scala 2 (the same
      // in every build: Scala 2 class files do not record the declaration order).
      val isScala3 = ScalaVersionSpecific.isScala3
      val first: CardinalDirection = if (isScala3) North else East
      KindlingsYamlDecoder.decode[CardinalDirection](mappingOf()) ==> Right(first)
      KindlingsYamlDecoder.decode[CardinalDirection](mappingOf("any" -> ScalarNode("1"))) ==> Right(first)
      val nonScalarKey: Node = MappingNode(Map((SequenceNode(ScalarNode("k")): Node) -> (ScalarNode("v"): Node)))
      assert(KindlingsYamlDecoder.decode[CardinalDirection](nonScalarKey).isLeft)
      assert(KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("North")).isLeft)
    }

    test("withUntaggedSubtypes restores the default after withWrappedSubtypes") {
      implicit val config: YamlConfig = YamlConfig.default.withWrappedSubtypes.withUntaggedSubtypes
      KindlingsYamlEncoder.encode[Shape](Circle(5.0)) ==> mappingOf("radius" -> double(5.0))
    }

    test("when no subtype decodes, the failure is scala-yaml's") {
      val node = mappingOf("side" -> ScalarNode("1"))
      val Left(error) = KindlingsYamlDecoder.decode[Shape](node): @unchecked
      assert(error.getMessage.contains("Cannot parse"), error.getMessage)
    }

    test("withWrappedSubtypes: Kindlings 0.3.x single-key mappings") {
      implicit val config: YamlConfig = YamlConfig.default.withWrappedSubtypes
      KindlingsYamlEncoder.encode[Shape](Circle(5.0)) ==> mappingOf(
        "Circle" -> mappingOf("radius" -> double(5.0))
      )
      KindlingsYamlDecoder.decode[Shape](mappingOf("Circle" -> mappingOf("radius" -> double(5.0)))) ==>
        Right(Circle(5.0): Shape)
    }

    test("the 0.3.x constructor, apply and copy (kept for binary compatibility) keep wrapped subtypes") {
      @scala.annotation.nowarn("cat=deprecation")
      def configs: List[YamlConfig] = List(
        new YamlConfig(identity, identity, None, false, false),
        YamlConfig(identity[String], identity[String], None, false, false),
        YamlConfig.default.withWrappedSubtypes.copy(identity[String], identity[String], None, false, false)
      )
      configs.foreach { config =>
        config.wrappedSubtypes ==> true
        config.useDefaults ==> false
        config.emptyCollectionsAsFlow ==> false
      }
      YamlConfig.default.copy(useDefaults = false).wrappedSubtypes ==> false
      // the 0.3.x copy keeps the fields it does not take
      @scala.annotation.nowarn("cat=deprecation")
      val copied =
        YamlConfig.default.withEmptyCollectionsAsFlow.copy(identity[String], identity[String], None, false, true)
      copied.emptyCollectionsAsFlow ==> true
    }

    test("kindlings03Defaults restores both previous defaults") {
      YamlConfig.kindlings03Defaults.useDefaults ==> false
      YamlConfig.kindlings03Defaults.wrappedSubtypes ==> true
    }
  }
}
