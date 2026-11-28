package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{ConstructError, Node}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

object YamlKeyOpaques {
  opaque type PlainId = String
  object PlainId {
    def apply(value: String): PlainId = value
    extension (id: PlainId) def value: String = id
  }
  // a subtype of String: must not be read as a plain String (its own instance / constructor is used)
  opaque type BoundedId <: String = String
  object BoundedId {
    def apply(value: String): BoundedId = value
  }
  opaque type CheckedId <: String = String
  object CheckedId {
    def apply(value: String): CheckedId = value
    given YamlKeyDecoder[CheckedId] =
      YamlKeyDecoder.forString.emap(s => if s.nonEmpty && s.forall(_.isDigit) then Right(s) else Left(s"Not an id: $s"))
    given YamlKeyEncoder[CheckedId] = YamlKeyEncoder.instance(id => s"$id")
  }
}
import YamlKeyOpaques.*

final case class KeyOpaques(plain: Map[PlainId, Int], bounded: Map[BoundedId, Int])
final case class KeyChecked(byId: Map[CheckedId, Int])
enum KeyPlanet {
  case Mercury, Venus
}
final case class KeyPlanets(byPlanet: Map[KeyPlanet, Int])

final class YamlKeyCodecScala3Spec extends MacroSuite {

  private def mappingOf(entries: (Node, Node)*): Node = MappingNode(entries.toMap)
  private def str(value: String): Node = ScalarNode(value)

  private def failure[A](result: Either[ConstructError, A]): String = result match {
    case Left(error)  => error.msg.linesIterator.next()
    case Right(value) => s"decoded: $value"
  }

  test("opaque type keys (also bounded by String) without a user-provided key codec") {
    val value = KeyOpaques(Map(PlainId("a") -> 1), Map(BoundedId("b") -> 2))
    val node =
      mappingOf(str("plain") -> mappingOf(str("a") -> str("1")), str("bounded") -> mappingOf(str("b") -> str("2")))
    KindlingsYamlEncoder.encode(value) ==> node
    KindlingsYamlDecoder.decode[KeyOpaques](node) ==> Right(value)
  }

  test("an opaque type bounded by String uses its own key codec (validation), not the String fast path") {
    val node = mappingOf(str("byId") -> mappingOf(str("12") -> str("1")))
    KindlingsYamlDecoder.decode[KeyChecked](node) ==> Right(KeyChecked(Map(CheckedId("12") -> 1)))
    KindlingsYamlEncoder.encode(KeyChecked(Map(CheckedId("12") -> 1))) ==> node
    failure(KindlingsYamlDecoder.decode[KeyChecked](mappingOf(str("byId") -> mappingOf(str("x") -> str("1"))))) ==>
      "Not an id: x"
  }

  test("a user-provided key codec for an opaque type wins") {
    given YamlKeyDecoder[PlainId] = YamlKeyDecoder.forString.map(s => PlainId(s.toUpperCase))
    given YamlKeyEncoder[PlainId] = YamlKeyEncoder.instance(id => s"id-${id.value}")
    val decoder = KindlingsYamlDecoder.derived[KeyOpaques]
    val encoder = KindlingsYamlEncoder.derived[KeyOpaques]
    decoder.construct(mappingOf(str("plain") -> mappingOf(str("a") -> str("1")), str("bounded") -> mappingOf())) ==>
      Right(KeyOpaques(Map(PlainId("A") -> 1), Map.empty))
    encoder.asNode(KeyOpaques(Map(PlainId("a") -> 1), Map.empty)) ==>
      mappingOf(str("plain") -> mappingOf(str("id-a") -> str("1")), str("bounded") -> mappingOf())
  }

  test("Scala 3 enum keys, by their names") {
    val value = KeyPlanets(Map(KeyPlanet.Mercury -> 1, KeyPlanet.Venus -> 2))
    val node = mappingOf(str("byPlanet") -> mappingOf(str("Mercury") -> str("1"), str("Venus") -> str("2")))
    KindlingsYamlEncoder.encode(value) ==> node
    KindlingsYamlDecoder.decode[KeyPlanets](node) ==> Right(value)
    failure(
      KindlingsYamlDecoder.decode[KeyPlanets](mappingOf(str("byPlanet") -> mappingOf(str("Pluto") -> str("1"))))
    ) ==>
      "Unknown key Pluto. Expected one of: Mercury, Venus"
  }
}
