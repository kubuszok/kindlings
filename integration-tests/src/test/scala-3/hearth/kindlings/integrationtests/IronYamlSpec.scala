package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

final class IronYamlSpec extends MacroSuite {

  private def scalarNode(value: String): Node = ScalarNode(value)

  private def mappingOf(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)

  group("Iron + YAML") {

    group("encoding") {

      test("case class with iron fields encodes correctly") {
        val person = IronPerson("Alice", 30)
        val node = KindlingsYamlEncoder.encode(person)
        node ==> mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
      }
    }

    group("decoding valid") {

      test("case class with iron fields decodes valid YAML") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("30"))
        val result = KindlingsYamlDecoder.decode[IronPerson](node)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("decoding invalid") {

      test("iron positive rejects negative") {
        val node = mappingOf("name" -> scalarNode("Alice"), "age" -> scalarNode("-1"))
        val result = KindlingsYamlDecoder.decode[IronPerson](node)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }
  }
}
