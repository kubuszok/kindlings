package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{Node, YamlDecoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

final class KindlingsYamlDecoderSpec extends MacroSuite {

  private def scalarNode(value: String): Node = ScalarNode(value)

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  private def seqOf(nodes: Node*): Node =
    SequenceNode(nodes.toSeq*)

  group("KindlingsYamlDecoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        assertEquals(KindlingsYamlDecoder.decode[Int](scalarNode("42")), Right(42))
      }

      test("String") {
        assertEquals(KindlingsYamlDecoder.decode[String](scalarNode("hello")), Right("hello"))
      }

      test("Boolean") {
        assertEquals(KindlingsYamlDecoder.decode[Boolean](scalarNode("true")), Right(true))
      }

      test("Double") {
        assertEquals(KindlingsYamlDecoder.decode[Double](scalarNode("3.14")), Right(3.14))
      }

      test("Long") {
        assertEquals(KindlingsYamlDecoder.decode[Long](scalarNode("42")), Right(42L))
      }
    }

    group("case classes") {

      test("simple case class") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        assertEquals(KindlingsYamlDecoder.decode[SimplePerson](node), Right(SimplePerson("Alice", 30)))
      }

      test("empty case class") {
        val node = mappingOf()
        assertEquals(KindlingsYamlDecoder.decode[EmptyClass](node), Right(EmptyClass()))
      }

      test("single field case class") {
        val node = mappingOf("value" -> scalarNode("42"))
        assertEquals(KindlingsYamlDecoder.decode[SingleField](node), Right(SingleField(42)))
      }

      test("nested case class (auto-derived)") {
        val node = mappingOf(
          "name" -> scalarNode("Bob"),
          "age" -> scalarNode("25"),
          "address" -> mappingOf(
            "street" -> scalarNode("123 Main St"),
            "city" -> scalarNode("Springfield")
          )
        )
        assertEquals(
          KindlingsYamlDecoder.decode[PersonWithAddress](node),
          Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
        )
      }

      test("case class with List of case classes") {
        val node = mappingOf(
          "name" -> scalarNode("Dev"),
          "members" -> seqOf(
            mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30")),
            mappingOf("name" -> scalarNode("Bob"), "age" -> scalarNode("25"))
          )
        )
        assertEquals(
          KindlingsYamlDecoder.decode[TeamWithMembers](node),
          Right(TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25))))
        )
      }
    }

    group("options") {

      test("Some value") {
        val node = scalarNode("42")
        assertEquals(KindlingsYamlDecoder.decode[Option[Int]](node), Right(Some(42)))
      }

      test("None from null") {
        val node: Node = hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils.nodeNull
        assertEquals(KindlingsYamlDecoder.decode[Option[Int]](node), Right(None))
      }
    }

    group("collections") {

      test("List of ints") {
        val node = seqOf(scalarNode("1"), scalarNode("2"), scalarNode("3"))
        assertEquals(KindlingsYamlDecoder.decode[List[Int]](node), Right(List(1, 2, 3)))
      }

      test("empty list") {
        val node = seqOf()
        assertEquals(KindlingsYamlDecoder.decode[List[Int]](node), Right(List.empty[Int]))
      }

      test("Vector of strings") {
        val node = seqOf(scalarNode("a"), scalarNode("b"))
        assertEquals(KindlingsYamlDecoder.decode[Vector[String]](node), Right(Vector("a", "b")))
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val node = scalarNode("42")
        assertEquals(KindlingsYamlDecoder.decode[WrappedInt](node), Right(WrappedInt(42)))
      }
    }

    group("sealed traits") {

      test("wrapper-style decoding (default)") {
        val node = mappingOf("Circle" -> mappingOf("radius" -> scalarNode("5.0")))
        assertEquals(KindlingsYamlDecoder.decode[Shape](node), Right(Circle(5.0): Shape))
      }

      test("wrapper-style decoding for second case") {
        val node = mappingOf(
          "Rectangle" -> mappingOf(
            "width" -> scalarNode("3.0"),
            "height" -> scalarNode("4.0")
          )
        )
        assertEquals(KindlingsYamlDecoder.decode[Shape](node), Right(Rectangle(3.0, 4.0): Shape))
      }

      test("discriminator-style decoding") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val node = mappingOf(
          "type" -> scalarNode("Dog"),
          "name" -> scalarNode("Rex"),
          "breed" -> scalarNode("Labrador")
        )
        assertEquals(KindlingsYamlDecoder.decode[Animal](node), Right(Dog("Rex", "Labrador"): Animal))
      }

      test("unknown discriminator produces error") {
        val node = mappingOf("Unknown" -> mappingOf())
        val result = KindlingsYamlDecoder.decode[Shape](node)
        assert(result.isLeft)
      }
    }

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = mappingOf("circle" -> mappingOf("radius" -> scalarNode("5.0")))
        assertEquals(KindlingsYamlDecoder.decode[Shape](node), Right(Circle(5.0): Shape))
      }
    }

    group("derive") {

      test("explicit derive returns YamlDecoder") {
        val decoder: YamlDecoder[SimplePerson] = KindlingsYamlDecoder.derive[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        assertEquals(decoder.construct(node)(), Right(SimplePerson("Alice", 30)))
      }

      test("derived provides KindlingsYamlDecoder") {
        val decoder: KindlingsYamlDecoder[SimplePerson] = KindlingsYamlDecoder.derived[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        assertEquals(decoder.construct(node)(), Right(SimplePerson("Alice", 30)))
      }
    }

    group("custom implicit priority") {

      test("user-provided implicit YamlDecoder works with derived") {
        implicit val decoder: YamlDecoder[SimplePerson] = KindlingsYamlDecoder.derived[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        assertEquals(decoder.construct(node)(), Right(SimplePerson("Alice", 30)))
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val node = mappingOf("a" -> scalarNode("1"), "b" -> scalarNode("2"))
        assertEquals(KindlingsYamlDecoder.decode[Map[String, Int]](node), Right(Map("a" -> 1, "b" -> 2)))
      }

      test("empty map") {
        assertEquals(KindlingsYamlDecoder.decode[Map[String, Int]](mappingOf()), Right(Map.empty[String, Int]))
      }
    }

    group("recursive types") {

      test("recursive tree") {
        val node = mappingOf(
          "value" -> scalarNode("1"),
          "children" -> seqOf(
            mappingOf("value" -> scalarNode("2"), "children" -> seqOf()),
            mappingOf(
              "value" -> scalarNode("3"),
              "children" -> seqOf(
                mappingOf("value" -> scalarNode("4"), "children" -> seqOf())
              )
            )
          )
        )
        assertEquals(
          KindlingsYamlDecoder.decode[RecursiveTree](node),
          Right(RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil))))))
        )
      }
    }

    group("error handling") {

      test("missing required field") {
        val node = mappingOf("name" -> scalarNode("Alice"))
        val result = KindlingsYamlDecoder.decode[SimplePerson](node)
        assert(result.isLeft)
      }

      test("wrong type for field") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("not-a-number"))
        val result = KindlingsYamlDecoder.decode[SimplePerson](node)
        assert(result.isLeft)
      }
    }
  }
}
