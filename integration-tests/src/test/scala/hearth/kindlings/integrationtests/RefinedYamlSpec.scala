package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

final class RefinedYamlSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  private def scalarNode(value: String): Node = ScalarNode(value)

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  group("Refined + YAML") {

    group("encoding") {

      test("case class with refined fields encodes correctly") {
        val person = RefinedPerson(alice, thirty)
        val node = KindlingsYamlEncoder.encode(person)
        node ==> mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
      }
    }

    group("decoding valid") {

      test("case class with refined fields decodes valid YAML") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        val result = KindlingsYamlDecoder.decode[RefinedPerson](node)
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }

    group("decoding invalid") {

      test("refined positive rejects negative") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("-1"))
        val result = KindlingsYamlDecoder.decode[RefinedPerson](node)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val person = RefinedPerson(alice, thirty)
        val node = KindlingsYamlEncoder.encode(person)
        val decoded = KindlingsYamlDecoder.decode[RefinedPerson](node)
        assert(decoded.isRight, s"Expected Right but got $decoded")
        decoded.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }
  }
}
