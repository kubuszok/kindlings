package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

final case class KeyJavaEnums(byColor: Map[JavaColor, Int])

// Java enums are compiled from Java sources, available on the JVM only
final class YamlKeyCodecJvmSpec extends MacroSuite {

  test("Java enum keys, by their names") {
    val value = KeyJavaEnums(Map(JavaColor.RED -> 1, JavaColor.BLUE -> 2))
    val node: Node = MappingNode(
      Map[Node, Node](
        ScalarNode("byColor") -> MappingNode(
          Map[Node, Node](ScalarNode("RED") -> ScalarNode("1"), ScalarNode("BLUE") -> ScalarNode("2"))
        )
      )
    )
    KindlingsYamlEncoder.encode(value) ==> node
    KindlingsYamlDecoder.decode[KeyJavaEnums](node) ==> Right(value)
  }
}
