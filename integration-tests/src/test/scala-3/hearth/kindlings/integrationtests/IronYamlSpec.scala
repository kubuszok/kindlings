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

    group("map keys") {

      test("iron String keys (a subtype of String) are validated, not read as a plain String") {
        val valid = mappingOf("values" -> mappingOf("a" -> scalarNode("1")))
        KindlingsYamlDecoder.decode[WithIronKeyMap](valid) ==> Right(
          WithIronKeyMap(Map(("a": String :| Not[Blank]) -> 1))
        )
        val blank = mappingOf("values" -> mappingOf(" " -> scalarNode("1")))
        val result = KindlingsYamlDecoder.decode[WithIronKeyMap](blank)
        assert(result.isLeft, s"Expected Left but got $result")
        KindlingsYamlEncoder.encode(WithIronKeyMap(Map(("a": String :| Not[Blank]) -> 1))) ==> valid
      }

      test("iron Int keys are validated") {
        val result =
          KindlingsYamlDecoder.decode[WithIronIntKeyMap](mappingOf("values" -> mappingOf("-1" -> scalarNode("1"))))
        assert(result.isLeft, s"Expected Left but got $result")
        KindlingsYamlDecoder.decode[WithIronIntKeyMap](mappingOf("values" -> mappingOf("2" -> scalarNode("1")))) ==>
          Right(WithIronIntKeyMap(Map((2: Int :| Positive) -> 1)))
      }
    }
  }
}
