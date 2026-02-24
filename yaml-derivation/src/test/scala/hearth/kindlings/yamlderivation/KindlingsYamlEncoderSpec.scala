package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{Node, YamlEncoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])
case class RecursiveTree(value: Int, children: List[RecursiveTree])
final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

case class CamelCasePerson(firstName: String, lastName: String)

final class KindlingsYamlEncoderSpec extends MacroSuite {

  // On Scala.js, whole-number doubles like 5.0 print as "5" instead of "5.0"
  // See https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
  private def doubleStr(d: Double): String = d.toString

  private def scalarNode(value: String): Node = ScalarNode(value)

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  private def seqOf(nodes: Node*): Node =
    SequenceNode(nodes.toSeq*)

  group("KindlingsYamlEncoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        val node = KindlingsYamlEncoder.encode(42)
        node ==> scalarNode("42")
      }

      test("String") {
        val node = KindlingsYamlEncoder.encode("hello")
        node ==> scalarNode("hello")
      }

      test("Boolean") {
        val node = KindlingsYamlEncoder.encode(true)
        node ==> scalarNode("true")
      }

      test("Double") {
        val node = KindlingsYamlEncoder.encode(3.14)
        node ==> scalarNode(doubleStr(3.14))
      }

      test("Long") {
        val node = KindlingsYamlEncoder.encode(42L)
        node ==> scalarNode("42")
      }
    }

    group("case classes") {

      test("simple case class") {
        val node = KindlingsYamlEncoder.encode(SimplePerson("Alice", 30))
        node ==> mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
      }

      test("empty case class") {
        val node = KindlingsYamlEncoder.encode(EmptyClass())
        node ==> mappingOf()
      }

      test("single field case class") {
        val node = KindlingsYamlEncoder.encode(SingleField(42))
        node ==> mappingOf("value" -> scalarNode("42"))
      }

      test("nested case class") {
        val node = KindlingsYamlEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
        node ==> mappingOf(
          "name" -> scalarNode("Bob"),
          "age" -> scalarNode("25"),
          "address" -> mappingOf(
            "street" -> scalarNode("123 Main St"),
            "city" -> scalarNode("Springfield")
          )
        )
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val node = KindlingsYamlEncoder.encode(WrappedInt(42))
        node ==> scalarNode("42")
      }
    }

    group("options") {

      test("Some value") {
        val node = KindlingsYamlEncoder.encode(Option(42))
        node ==> scalarNode("42")
      }

      test("None") {
        val node = KindlingsYamlEncoder.encode(Option.empty[Int])
        node ==> hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils.nodeNull
      }
    }

    group("collections") {

      test("List of ints") {
        val node = KindlingsYamlEncoder.encode(List(1, 2, 3))
        node ==> seqOf(scalarNode("1"), scalarNode("2"), scalarNode("3"))
      }

      test("empty list") {
        val node = KindlingsYamlEncoder.encode(List.empty[Int])
        node ==> seqOf()
      }

      test("Vector of strings") {
        val node = KindlingsYamlEncoder.encode(Vector("a", "b"))
        node ==> seqOf(scalarNode("a"), scalarNode("b"))
      }

      test("List of case classes") {
        val node = KindlingsYamlEncoder.encode(
          TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        )
        node ==> mappingOf(
          "name" -> scalarNode("Dev"),
          "members" -> seqOf(
            mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30")),
            mappingOf("name" -> scalarNode("Bob"), "age" -> scalarNode("25"))
          )
        )
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val node = KindlingsYamlEncoder.encode(Map("a" -> 1))
        node match {
          case MappingNode(mappings, _) =>
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "a" && v == "1"
              case _                                    => false
            } ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }

      test("empty map") {
        val node = KindlingsYamlEncoder.encode(Map.empty[String, Int])
        node ==> mappingOf()
      }
    }

    group("sealed traits") {

      test("wrapper-style encoding (default)") {
        val node = KindlingsYamlEncoder.encode[Shape](Circle(5.0))
        node ==> mappingOf("Circle" -> mappingOf("radius" -> scalarNode(doubleStr(5.0))))
      }

      test("wrapper-style encoding for second case") {
        val node = KindlingsYamlEncoder.encode[Shape](Rectangle(3.0, 4.0))
        node ==> mappingOf(
          "Rectangle" -> mappingOf(
            "width" -> scalarNode(doubleStr(3.0)),
            "height" -> scalarNode(doubleStr(4.0))
          )
        )
      }

      test("discriminator-style encoding") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val node = KindlingsYamlEncoder.encode[Animal](Dog("Rex", "Labrador"))
        node match {
          case MappingNode(mappings, _) =>
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "type" && v == "Dog"
              case _                                    => false
            } ==> true
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "name" && v == "Rex"
              case _                                    => false
            } ==> true
            mappings.exists {
              case (ScalarNode(k, _), ScalarNode(v, _)) => k == "breed" && v == "Labrador"
              case _                                    => false
            } ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }
    }

    group("recursive types") {

      test("recursive tree") {
        val tree = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val node = KindlingsYamlEncoder.encode(tree)
        node ==> mappingOf(
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
      }
    }

    group("sets") {

      test("Set of ints") {
        val node = KindlingsYamlEncoder.encode(Set(1))
        node ==> seqOf(scalarNode("1"))
      }

      test("empty set") {
        val node = KindlingsYamlEncoder.encode(Set.empty[Int])
        node ==> seqOf()
      }
    }

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val node = KindlingsYamlEncoder.encode[Shape](Circle(5.0))
        node ==> mappingOf("circle" -> mappingOf("radius" -> scalarNode(doubleStr(5.0))))
      }

      test("snake_case member names") {
        implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
        val node = KindlingsYamlEncoder.encode(CamelCasePerson("Alice", "Smith"))
        node match {
          case MappingNode(mappings, _) =>
            val keys = mappings.keys.collect { case ScalarNode(k, _) => k }.toSet
            keys.contains("first_name") ==> true
            keys.contains("last_name") ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }

      test("kebab-case member names") {
        implicit val config: YamlConfig = YamlConfig.default.withKebabCaseMemberNames
        val node = KindlingsYamlEncoder.encode(CamelCasePerson("Alice", "Smith"))
        node match {
          case MappingNode(mappings, _) =>
            val keys = mappings.keys.collect { case ScalarNode(k, _) => k }.toSet
            keys.contains("first-name") ==> true
            keys.contains("last-name") ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }

      test("PascalCase member names") {
        implicit val config: YamlConfig = YamlConfig.default.withPascalCaseMemberNames
        val node = KindlingsYamlEncoder.encode(CamelCasePerson("Alice", "Smith"))
        node match {
          case MappingNode(mappings, _) =>
            val keys = mappings.keys.collect { case ScalarNode(k, _) => k }.toSet
            keys.contains("FirstName") ==> true
            keys.contains("LastName") ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }

      test("SCREAMING_SNAKE_CASE member names") {
        implicit val config: YamlConfig = YamlConfig.default.withScreamingSnakeCaseMemberNames
        val node = KindlingsYamlEncoder.encode(CamelCasePerson("Alice", "Smith"))
        node match {
          case MappingNode(mappings, _) =>
            val keys = mappings.keys.collect { case ScalarNode(k, _) => k }.toSet
            keys.contains("FIRST_NAME") ==> true
            keys.contains("LAST_NAME") ==> true
          case other => fail(s"Expected MappingNode but got $other")
        }
      }
    }

    group("derive") {

      test("explicit derive returns YamlEncoder") {
        val encoder: YamlEncoder[SimplePerson] = KindlingsYamlEncoder.derive[SimplePerson]
        val node = encoder.asNode(SimplePerson("Alice", 30))
        node ==> mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
      }

      test("derived provides KindlingsYamlEncoder") {
        val encoder: KindlingsYamlEncoder[SimplePerson] = KindlingsYamlEncoder.derived[SimplePerson]
        val node = encoder.asNode(SimplePerson("Alice", 30))
        node ==> mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
      }
    }

    group("custom implicit priority") {

      test("user-provided YamlEncoder is used over derivation") {
        implicit val customEncoder: YamlEncoder[SingleField] = new YamlEncoder[SingleField] {
          def asNode(obj: SingleField): Node = ScalarNode((obj.value * 10).toString)
        }
        val node = KindlingsYamlEncoder.encode(SingleField(5))
        node ==> scalarNode("50")
      }
    }
  }
}
