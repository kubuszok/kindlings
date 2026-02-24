package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{Node, Tag}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

final class YamlScala3Spec extends MacroSuite {

  // On Scala.js, whole-number doubles like 5.0 print as "5" instead of "5.0"
  // See https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
  private def doubleStr(d: Double): String = d.toString

  private def scalarNode(value: String): Node = ScalarNode(value)

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  group("Scala 3 enums") {

    group("encoding") {

      test("enum variant with fields (wrapper-style)") {
        val node = KindlingsYamlEncoder.encode[Fruit](Fruit.Apple(1.5))
        node ==> mappingOf("Apple" -> mappingOf("weight" -> scalarNode(doubleStr(1.5))))
      }

      test("second enum variant (wrapper-style)") {
        val node = KindlingsYamlEncoder.encode[Fruit](Fruit.Banana(20.0))
        node ==> mappingOf("Banana" -> mappingOf("length" -> scalarNode(doubleStr(20.0))))
      }

      test("enum with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val node = KindlingsYamlEncoder.encode[Fruit](Fruit.Banana(20.0))
        node match {
          case MappingNode(mappings, _) =>
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "type" && v == "Banana"
              case _                                    => false
            } ==> true
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "length" && v == doubleStr(20.0)
              case _                                    => false
            } ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }

      test("enum with custom constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = KindlingsYamlEncoder.encode[Fruit](Fruit.Apple(1.5))
        node ==> mappingOf("apple" -> mappingOf("weight" -> scalarNode(doubleStr(1.5))))
      }
    }

    group("decoding") {

      test("enum variant with fields (wrapper-style)") {
        val node = mappingOf("Banana" -> mappingOf("length" -> scalarNode("20.0")))
        KindlingsYamlDecoder.decode[Fruit](node) ==> Right(Fruit.Banana(20.0))
      }

      test("second enum variant (wrapper-style)") {
        val node = mappingOf("Apple" -> mappingOf("weight" -> scalarNode("1.5")))
        KindlingsYamlDecoder.decode[Fruit](node) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val node = mappingOf(
          "type" -> scalarNode("Apple"),
          "weight" -> scalarNode("1.5")
        )
        KindlingsYamlDecoder.decode[Fruit](node) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with custom constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = mappingOf("banana" -> mappingOf("length" -> scalarNode("20.0")))
        KindlingsYamlDecoder.decode[Fruit](node) ==> Right(Fruit.Banana(20.0))
      }
    }
  }

  group("opaque types") {

    test("encode standalone opaque type") {
      import YamlOpaqueTypes.*
      KindlingsYamlEncoder.encode(UserId(42)) ==> scalarNode("42")
    }

    test("decode standalone opaque type") {
      import YamlOpaqueTypes.*
      KindlingsYamlDecoder.decode[UserId](scalarNode("42")) ==> Right(UserId(42))
    }

    test("encode case class with opaque type field") {
      import YamlOpaqueTypes.*
      val node = KindlingsYamlEncoder.encode(YamlUserWithOpaque(UserId(42), "Alice"))
      node ==> mappingOf("id" -> scalarNode("42"), "name" -> scalarNode("Alice"))
    }

    test("decode case class with opaque type field") {
      import YamlOpaqueTypes.*
      val node = mappingOf("id" -> scalarNode("42"), "name" -> scalarNode("Alice"))
      KindlingsYamlDecoder.decode[YamlUserWithOpaque](node) ==> Right(YamlUserWithOpaque(UserId(42), "Alice"))
    }
  }

  group("auto-derivation isolation") {

    group("encoder uses kindlings derivation") {

      test("constructor name transforms are applied") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = KindlingsYamlEncoder.encode[Shape](Circle(5.0))
        node ==> mappingOf("circle" -> mappingOf("radius" -> scalarNode(doubleStr(5.0))))
      }

      test("member name transforms are applied") {
        implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
        val node = KindlingsYamlEncoder.encode(CamelCasePerson("Bob", "Jones"))
        node match {
          case MappingNode(mappings, _) =>
            val keys = mappings.keys.collect { case ScalarNode(k, _) => k }.toSet
            keys.contains("first_name") ==> true
            keys.contains("last_name") ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }
    }

    group("decoder uses kindlings derivation") {

      test("constructor name transforms are applied") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = mappingOf("circle" -> mappingOf("radius" -> scalarNode("5.0")))
        KindlingsYamlDecoder.decode[Shape](node) ==> Right(Circle(5.0): Shape)
      }
    }
  }
}
