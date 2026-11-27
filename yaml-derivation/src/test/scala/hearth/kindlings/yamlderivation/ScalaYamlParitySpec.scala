package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlEncoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

final case class ParityItem(port: Int)
final case class ParityContainers(
    list: List[ParityItem],
    vector: Vector[ParityItem],
    set: Set[ParityItem],
    map: Map[String, ParityItem]
)
final case class ParityIntKeys(byId: Map[Int, ParityItem])
final case class ParityKey(value: String) extends AnyVal
final case class ParityValueKeys(byKey: Map[ParityKey, ParityItem])
final case class ParityPrivate(name: String, private val secret: Int, flag: Boolean) {
  def reveal: Int = secret
}
object ParityCounter {
  var evaluated = 0
  def next(): String = { evaluated += 1; s"default-$evaluated" }
}
final case class ParityLazyDefault(a: Int, b: String = ParityCounter.next())

/** Behaviour of scala-yaml's own instances that the derived ones keep where scala-yaml has no instance to delegate to
  * (collections, maps and options of derived types). Checked against scala-yaml itself, on Scala 3, by
  * `YamlUpstreamSuitesOracleSpec` in upstream-oracle-tests (scala-yaml's derivation tests: `DecoderSuite`,
  * `ConstructSuite`, `YamlEncoderSuite`); this suite also runs on Scala 2.13 and on Scala.js / Native.
  */
final class ScalaYamlParitySpec extends MacroSuite {

  private def mappingOf(entries: (Node, Node)*): Node = MappingNode(entries.toMap)
  private def str(value: String): Node = ScalarNode(value)
  private val nullNode: Node = YamlEncoder.forOption(YamlEncoder.forInt).asNode(None)
  private def item(port: Int): Node = mappingOf(str("port") -> str(port.toString))

  private def containers(
      list: Node = SequenceNode(),
      vector: Node = SequenceNode(),
      set: Node = SequenceNode(),
      map: Node = mappingOf()
  ): Node = mappingOf(str("list") -> list, str("vector") -> vector, str("set") -> set, str("map") -> map)

  private def failure[A](result: Either[ConstructError, A]): String = result match {
    case Left(error)  => error.msg.linesIterator.next()
    case Right(value) => s"decoded: $value"
  }

  private val decoder: YamlDecoder[ParityContainers] = KindlingsYamlDecoder.derived[ParityContainers]
  private val encoder: YamlEncoder[ParityContainers] = KindlingsYamlEncoder.derived[ParityContainers]

  group("collections and maps of derived types fail as scala-yaml's collection decoders") {

    test("valid nodes decode") {
      decoder.construct(
        containers(
          SequenceNode(item(1)),
          SequenceNode(item(2)),
          SequenceNode(item(3), item(3)),
          mappingOf(str("k") -> item(4))
        )
      ) ==> Right(
        ParityContainers(List(ParityItem(1)), Vector(ParityItem(2)), Set(ParityItem(3)), Map("k" -> ParityItem(4)))
      )
    }

    test("a node of another kind") {
      failure(decoder.construct(containers(list = str("x")))) ==>
        "Could't construct scala.collection.immutable.List from tag:yaml.org,2002:str"
      failure(decoder.construct(containers(vector = item(1)))) ==>
        "Could't construct scala.collection.immutable.Vector from tag:yaml.org,2002:map"
      failure(decoder.construct(containers(set = str("1")))) ==>
        "Could't construct scala.collection.immutable.Set from tag:yaml.org,2002:int"
      failure(decoder.construct(containers(map = SequenceNode(item(1))))) ==>
        "Could't construct scala.collection.immutable.Map from tag:yaml.org,2002:seq"
    }

    test("a null") {
      failure(decoder.construct(containers(list = nullNode))) ==>
        "Could't construct scala.collection.immutable.List from null (tag:yaml.org,2002:null)"
      failure(decoder.construct(containers(map = nullNode))) ==>
        "Could't construct scala.collection.immutable.Map from null (tag:yaml.org,2002:null)"
    }

    test("the first failing item") {
      val broken = mappingOf(str("port") -> str("x"))
      failure(decoder.construct(containers(list = SequenceNode(item(1), broken, str("y"))))) ==>
        "For input string: \"x\""
    }

    test("String keys: a null or a key that is not a scalar fails, as scala-yaml's String decoder") {
      failure(decoder.construct(containers(map = mappingOf(nullNode -> item(1))))) ==>
        "Could't construct java.lang.String from null (tag:yaml.org,2002:null)"
      failure(decoder.construct(containers(map = mappingOf(SequenceNode(str("a")) -> item(1))))) ==>
        "Could't construct java.lang.String from tag:yaml.org,2002:seq"
    }

    test("String keys are written as scala-yaml's String encoder writes them (non-printable characters escaped)") {
      val value = ParityContainers(Nil, Vector.empty, Set.empty, Map("a\u0000b" -> ParityItem(1)))
      encoder.asNode(value) ==> containers(map = mappingOf(YamlEncoder.forString.asNode("a\u0000b") -> item(1)))
      encoder.asNode(value) ==> containers(map = mappingOf(str("a\\u0000b") -> item(1)))
    }
  }

  group("maps with keys that are not strings") {

    test("keys with a scala-yaml instance (Int)") {
      val value = ParityIntKeys(Map(1 -> ParityItem(1), -2 -> ParityItem(2)))
      val node = mappingOf(str("byId") -> mappingOf(str("1") -> item(1), str("-2") -> item(2)))
      KindlingsYamlEncoder.encode[ParityIntKeys](value) ==> node
      KindlingsYamlDecoder.decode[ParityIntKeys](node) ==> Right(value)
      failure(KindlingsYamlDecoder.decode[ParityIntKeys](mappingOf(str("byId") -> mappingOf(str("a") -> item(1))))) ==>
        "For input string: \"a\""
    }

    test("derived keys (a value class)") {
      val value = ParityValueKeys(Map(ParityKey("k") -> ParityItem(1)))
      val node = mappingOf(str("byKey") -> mappingOf(str("k") -> item(1)))
      KindlingsYamlEncoder.encode[ParityValueKeys](value) ==> node
      KindlingsYamlDecoder.decode[ParityValueKeys](node) ==> Right(value)
    }
  }

  group("case classes") {

    test("private constructor fields are written (by position) and read back") {
      val value = ParityPrivate("n", 7, flag = true)
      val node = KindlingsYamlEncoder.encode[ParityPrivate](value)
      node ==> mappingOf(str("name") -> str("n"), str("secret") -> str("7"), str("flag") -> str("true"))
      KindlingsYamlDecoder.decode[ParityPrivate](node).map(_.reveal) ==> Right(7)
    }

    test("a default value is evaluated only when its field is absent, and on every such decode") {
      val decoder: YamlDecoder[ParityLazyDefault] = KindlingsYamlDecoder.derived[ParityLazyDefault]
      ParityCounter.evaluated = 0
      decoder.construct(mappingOf(str("a") -> str("1"), str("b") -> str("given"))) ==>
        Right(ParityLazyDefault(1, "given"))
      ParityCounter.evaluated ==> 0
      decoder.construct(mappingOf(str("a") -> str("1"))) ==> Right(ParityLazyDefault(1, "default-1"))
      decoder.construct(mappingOf(str("a") -> str("1"))) ==> Right(ParityLazyDefault(1, "default-2"))
      // a failing field before it: the later fields are not decoded
      assert(decoder.construct(mappingOf(str("a") -> str("x"))).isLeft)
      ParityCounter.evaluated ==> 2
    }

    test("a default value with a configuration only known at runtime") {
      def derive(config: YamlConfig): YamlDecoder[ParityLazyDefault] = {
        implicit val runtimeConfig: YamlConfig = config
        KindlingsYamlDecoder.derived[ParityLazyDefault]
      }
      ParityCounter.evaluated = 0
      derive(YamlConfig()).construct(mappingOf(str("a") -> str("1"), str("b") -> str("given"))) ==>
        Right(ParityLazyDefault(1, "given"))
      ParityCounter.evaluated ==> 0
      derive(YamlConfig()).construct(mappingOf(str("a") -> str("1"))) ==> Right(ParityLazyDefault(1, "default-1"))
      assert(derive(YamlConfig().withoutUseDefaults).construct(mappingOf(str("a") -> str("1"))).isLeft)
      ParityCounter.evaluated ==> 1
    }
  }
}
