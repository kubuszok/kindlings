package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

final class NeotypeYamlSpec extends MacroSuite {

  private def personNode(email: String, age: String): Node =
    MappingNode(
      Map[Node, Node](
        ScalarNode("email") -> ScalarNode(email),
        ScalarNode("age") -> ScalarNode(age),
        ScalarNode("nickname") -> ScalarNode("ally")
      )
    )

  private val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))

  group("Neotype + YAML") {

    test("case class with neotype fields encodes correctly") {
      KindlingsYamlEncoder.encode(person) ==> personNode("alice@example.com", "30")
    }

    test("case class with neotype fields decodes valid YAML") {
      KindlingsYamlDecoder.decode[NeotypePerson](personNode("alice@example.com", "30")) ==> Right(person)
    }

    test("neotype validation rejects invalid value") {
      val result = KindlingsYamlDecoder.decode[NeotypePerson](personNode("alice@example.com", "-1"))
      assert(result.isLeft, s"Expected Left but got $result")
    }
  }
}
