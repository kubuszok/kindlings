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
        KindlingsYamlDecoder.decode[Int](scalarNode("42")) ==> Right(42)
      }

      test("String") {
        KindlingsYamlDecoder.decode[String](scalarNode("hello")) ==> Right("hello")
      }

      test("Boolean") {
        KindlingsYamlDecoder.decode[Boolean](scalarNode("true")) ==> Right(true)
      }

      test("Double") {
        KindlingsYamlDecoder.decode[Double](scalarNode("3.14")) ==> Right(3.14)
      }

      test("Long") {
        KindlingsYamlDecoder.decode[Long](scalarNode("42")) ==> Right(42L)
      }
    }

    group("case classes") {

      test("simple case class") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        KindlingsYamlDecoder.decode[SimplePerson](node) ==> Right(SimplePerson("Alice", 30))
      }

      test("empty case class") {
        val node = mappingOf()
        KindlingsYamlDecoder.decode[EmptyClass](node) ==> Right(EmptyClass())
      }

      test("single field case class") {
        val node = mappingOf("value" -> scalarNode("42"))
        KindlingsYamlDecoder.decode[SingleField](node) ==> Right(SingleField(42))
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
        KindlingsYamlDecoder.decode[PersonWithAddress](node) ==>
          Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
      }

      test("case class with List of case classes") {
        val node = mappingOf(
          "name" -> scalarNode("Dev"),
          "members" -> seqOf(
            mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30")),
            mappingOf("name" -> scalarNode("Bob"), "age" -> scalarNode("25"))
          )
        )
        KindlingsYamlDecoder.decode[TeamWithMembers](node) ==>
          Right(TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25))))
      }
    }

    group("options") {

      test("Some value") {
        val node = scalarNode("42")
        KindlingsYamlDecoder.decode[Option[Int]](node) ==> Right(Some(42))
      }

      test("None from null") {
        val node: Node = hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils.nodeNull
        KindlingsYamlDecoder.decode[Option[Int]](node) ==> Right(None)
      }
    }

    group("collections") {

      test("List of ints") {
        val node = seqOf(scalarNode("1"), scalarNode("2"), scalarNode("3"))
        KindlingsYamlDecoder.decode[List[Int]](node) ==> Right(List(1, 2, 3))
      }

      test("empty list") {
        val node = seqOf()
        KindlingsYamlDecoder.decode[List[Int]](node) ==> Right(List.empty[Int])
      }

      test("Vector of strings") {
        val node = seqOf(scalarNode("a"), scalarNode("b"))
        KindlingsYamlDecoder.decode[Vector[String]](node) ==> Right(Vector("a", "b"))
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val node = scalarNode("42")
        KindlingsYamlDecoder.decode[WrappedInt](node) ==> Right(WrappedInt(42))
      }
    }

    group("sealed traits") {

      test("wrapper-style decoding (default)") {
        val node = mappingOf("Circle" -> mappingOf("radius" -> scalarNode("5.0")))
        KindlingsYamlDecoder.decode[Shape](node) ==> Right(Circle(5.0): Shape)
      }

      test("wrapper-style decoding for second case") {
        val node = mappingOf(
          "Rectangle" -> mappingOf(
            "width" -> scalarNode("3.0"),
            "height" -> scalarNode("4.0")
          )
        )
        KindlingsYamlDecoder.decode[Shape](node) ==> Right(Rectangle(3.0, 4.0): Shape)
      }

      test("discriminator-style decoding") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val node = mappingOf(
          "type" -> scalarNode("Dog"),
          "name" -> scalarNode("Rex"),
          "breed" -> scalarNode("Labrador")
        )
        KindlingsYamlDecoder.decode[Animal](node) ==> Right(Dog("Rex", "Labrador"): Animal)
      }

      test("unknown discriminator produces error") {
        val node = mappingOf("Unknown" -> mappingOf())
        val result = KindlingsYamlDecoder.decode[Shape](node)
        result.isLeft ==> true
      }
    }

    group("string enum decoding (enumAsStrings)") {

      test("decode case-object-only sealed trait from string") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("North")) ==> Right(North: CardinalDirection)
      }

      test("decode all cases from strings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("South")) ==> Right(South: CardinalDirection)
        KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("East")) ==> Right(East: CardinalDirection)
        KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("West")) ==> Right(West: CardinalDirection)
      }

      test("enum as string with constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("north")) ==> Right(North: CardinalDirection)
      }

      test("non-scalar input fails with enumAsStrings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        val result = KindlingsYamlDecoder.decode[CardinalDirection](mappingOf("North" -> mappingOf()))
        result.isLeft ==> true
      }

      test("unknown string value fails") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        val result = KindlingsYamlDecoder.decode[CardinalDirection](ScalarNode("NorthWest"))
        result.isLeft ==> true
      }
    }

    group("sets") {

      test("Set of ints") {
        val node = seqOf(scalarNode("1"), scalarNode("2"), scalarNode("3"))
        KindlingsYamlDecoder.decode[Set[Int]](node) ==> Right(Set(1, 2, 3))
      }

      test("empty set") {
        KindlingsYamlDecoder.decode[Set[Int]](seqOf()) ==> Right(Set.empty[Int])
      }
    }

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = mappingOf("circle" -> mappingOf("radius" -> scalarNode("5.0")))
        KindlingsYamlDecoder.decode[Shape](node) ==> Right(Circle(5.0): Shape)
      }

      test("snake_case member names") {
        implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
        val node = mappingOf("first_name" -> scalarNode("Alice"), "last_name" -> scalarNode("Smith"))
        KindlingsYamlDecoder.decode[CamelCasePerson](node) ==> Right(CamelCasePerson("Alice", "Smith"))
      }

      test("kebab-case member names") {
        implicit val config: YamlConfig = YamlConfig.default.withKebabCaseMemberNames
        val node = mappingOf("first-name" -> scalarNode("Alice"), "last-name" -> scalarNode("Smith"))
        KindlingsYamlDecoder.decode[CamelCasePerson](node) ==> Right(CamelCasePerson("Alice", "Smith"))
      }
    }

    group("derive") {

      test("explicit derive returns YamlDecoder") {
        val decoder: YamlDecoder[SimplePerson] = KindlingsYamlDecoder.derive[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        decoder.construct(node)() ==> Right(SimplePerson("Alice", 30))
      }

      test("derived provides KindlingsYamlDecoder") {
        val decoder: KindlingsYamlDecoder[SimplePerson] = KindlingsYamlDecoder.derived[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        decoder.construct(node)() ==> Right(SimplePerson("Alice", 30))
      }
    }

    group("custom implicit priority") {

      test("user-provided implicit YamlDecoder works with derived") {
        implicit val decoder: YamlDecoder[SimplePerson] = KindlingsYamlDecoder.derived[SimplePerson]
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        decoder.construct(node)() ==> Right(SimplePerson("Alice", 30))
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val node = mappingOf("a" -> scalarNode("1"), "b" -> scalarNode("2"))
        KindlingsYamlDecoder.decode[Map[String, Int]](node) ==> Right(Map("a" -> 1, "b" -> 2))
      }

      test("empty map") {
        KindlingsYamlDecoder.decode[Map[String, Int]](mappingOf()) ==> Right(Map.empty[String, Int])
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
        KindlingsYamlDecoder.decode[RecursiveTree](node) ==>
          Right(RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil))))))
      }
    }

    group("tuples") {

      test("decode (Int, String) from YAML mapping") {
        val node = mappingOf("_1" -> scalarNode("42"), "_2" -> scalarNode("hello"))
        KindlingsYamlDecoder.decode[(Int, String)](node) ==> Right((42, "hello"))
      }

      test("decode (Int, String, Boolean) from YAML mapping") {
        val node = mappingOf("_1" -> scalarNode("42"), "_2" -> scalarNode("hello"), "_3" -> scalarNode("true"))
        KindlingsYamlDecoder.decode[(Int, String, Boolean)](node) ==> Right((42, "hello", true))
      }
    }

    group("generic case classes") {

      test("Box[Int]") {
        val node = mappingOf("value" -> scalarNode("42"))
        KindlingsYamlDecoder.decode[Box[Int]](node) ==> Right(Box(42))
      }

      test("Pair[String, Int]") {
        val node = mappingOf("first" -> scalarNode("hello"), "second" -> scalarNode("42"))
        KindlingsYamlDecoder.decode[Pair[String, Int]](node) ==> Right(Pair("hello", 42))
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        val node = mappingOf(
          "name" -> scalarNode("Alice"),
          "address" -> mappingOf(
            "street" -> scalarNode("123 Main"),
            "city" -> scalarNode("NYC"),
            "geo" -> mappingOf(
              "lat" -> scalarNode("40.7"),
              "lon" -> scalarNode("-74.0")
            )
          )
        )
        KindlingsYamlDecoder.decode[PersonFull](node) ==>
          Right(PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0))))
      }
    }

    group("type aliases") {

      test("WithAlias decodes type alias field") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        KindlingsYamlDecoder.decode[WithAlias](node) ==> Right(WithAlias("Alice", 30))
      }
    }

    group("combined configuration") {

      test("snake_case + discriminator + constructor transform") {
        implicit val config: YamlConfig = YamlConfig(
          transformMemberNames = YamlConfig.snakeCase,
          transformConstructorNames = _.toLowerCase,
          discriminator = Some("type")
        )
        val node = mappingOf(
          "type" -> scalarNode("dog"),
          "name" -> scalarNode("Rex"),
          "breed" -> scalarNode("Labrador")
        )
        KindlingsYamlDecoder.decode[Animal](node) ==> Right(Dog("Rex", "Labrador"): Animal)
      }
    }

    group("empty class with non-mapping input") {

      test("decode scalar as EmptyClass succeeds (no fields to validate)") {
        val node = scalarNode("42")
        KindlingsYamlDecoder.decode[EmptyClass](node) ==> Right(EmptyClass())
      }
    }

    group("error handling") {

      test("missing required field") {
        val node = mappingOf("name" -> scalarNode("Alice"))
        val result = KindlingsYamlDecoder.decode[SimplePerson](node)
        result.isLeft ==> true
      }

      test("wrong type for field") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("not-a-number"))
        val result = KindlingsYamlDecoder.decode[SimplePerson](node)
        result.isLeft ==> true
      }
    }
  }
}
