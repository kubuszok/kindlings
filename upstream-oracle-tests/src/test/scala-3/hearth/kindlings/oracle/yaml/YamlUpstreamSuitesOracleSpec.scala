package hearth.kindlings.oracle.yaml

import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder, YamlConfig}
import org.virtuslab.yaml.{Node, YamlCodec, YamlDecoder, YamlEncoder}
import org.virtuslab.yaml.*

// Fixtures of scala-yaml's own suites (core/shared/src/test/scala-3: decoder/DecoderSuite, decoder/DecoderErrorsSuite,
// decoder/DockerYamlDecoderSuite, ConstructSuite, BaseYamlEncoderSuite; core/jvm/src/test: YamlEncoderSuite), top-level
// so both sides derive the same types.

// DecoderSuite "numbers"
final case class YuValueTypes(
    double: Double,
    float: Float,
    long: Long,
    int: Int,
    short: Short,
    byte: Byte,
    boolean: Boolean
)
// DecoderSuite "option"
final case class YuOptionTypes(double: Option[Double], float: Option[Float], int: Option[Int], short: Option[Short])
// DecoderSuite "sequence"
final case class YuSequenceTypes(doubles: List[Double], floats: Seq[Float], ints: Set[Int])
// DecoderSuite "mapping", "mapping-2"
final case class YuMappingTypes(doubles: Map[String, Double], floats: Map[String, List[Float]])
// DecoderSuite "case class"
final case class YuPort(port: Int)
final case class YuSelector(app: String, tier: String)
final case class YuSpec(`type`: String, ports: List[YuPort], selector: YuSelector)
final case class YuConfig(spec: YuSpec)
// DecoderSuite "sequence of mappings"
sealed trait YuAddress
final case class YuNetwork(network: String, port: Int) extends YuAddress
final case class YuLocal(local: String, port: Int) extends YuAddress
final case class YuAddresses(addresses: List[YuAddress])
// DecoderSuite "codec mapping": a user-provided codec (found by both derivations)
final case class YuCapital(value: String)
object YuCapital {
  given YamlCodec[YuCapital] = YamlCodec.make[String].mapInvariant(s => YuCapital(s.toUpperCase))(_.value.toUpperCase)
}
final case class YuCapitals(capitals: List[YuCapital], main: YuCapital, other: Option[YuCapital])
// DecoderSuite "option" (the second one), BaseYamlEncoderSuite "option"
final case class YuFoo(int: Int, string: String)
final case class YuFoos(foos: List[Option[YuFoo]])
final case class YuOptionField(field: Option[String])
// DecoderSuite "issue 222"
final case class YuEdgeCases(
    booleans: List[Boolean],
    integers: List[Int],
    floats: List[Float],
    `also floats`: List[Float],
    `also doubles`: List[Double]
)
// DecoderSuite "issue 281"
final case class YuDescription(description: String)
// DecoderSuite "issue 86"
final case class YuEmptyValues(`single line`: Option[Int] = None, `multi line`: Option[Int] = None, a: String)
// DecoderSuite "issue 120"
final case class YuNonNull(key1: Int, key2: Int)
// DecoderSuite "default parameters ..."
final case class YuDefaultParams(a: Int = 1, b: String = "test", c: Option[Int] = None, d: Double)
object YuCounter {
  var times = 0
  def next(): String = { times += 1; s"test-$times" }
}
final case class YuLazyDefault(a: Int, b: String = YuCounter.next())
// DecoderSuite "decode into Map[Any, Any]", "alias ...", "issue 314": Any is decoded with scala-yaml's YamlDecoder.forAny
final case class YuAnys(value: Any, map: Map[Any, Any], list: List[Any])
// ConstructSuite, YamlEncoderSuite
final case class YuStats(hr: Int, avg: Double, rbi: Int)
enum YuSomeEnum {
  case Foo(value: Int)
  case Bar(price: Double)
}
// BaseYamlEncoderSuite
final case class YuData(int: Int, double: Double)
final case class YuSeqOfData(data: Seq[YuData], nested: Seq[Seq[Int]], chars: Map[String, Char], booleans: Set[Boolean])
final case class YuNestedAB(a: Int, b: String)
final case class YuMappingOfMappings(first: YuNestedAB, second: YuNestedAB)
final case class YuWeb(build: String, ports: List[String], volumes: List[String])
final case class YuRedis(image: String)
final case class YuServices(web: YuWeb, redis: YuRedis)
final case class YuCompose(version: String, services: YuServices)
final case class YuStrings(plain: String, nonPrintable: String, char: Char)

// Beyond scala-yaml's suites: what its derivation does for shapes its tests do not cover.

// more than 4 fields: scala-yaml builds the mapping with `.toMap`
final case class YuWide(f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int)
// collections, options and maps of derived types (no given instance for the element on the Kindlings side)
final case class YuContainers(
    list: List[YuPort],
    seq: Seq[YuPort],
    set: Set[YuPort],
    option: Option[YuPort],
    map: Map[String, YuPort]
)
final case class YuIntKeys(byId: Map[Int, YuPort])
// user-provided String / Int instances: scala-yaml's forMap uses them for keys too (it has no key type class), Kindlings
// reads and writes keys with key codecs (YamlKeyDecoder / YamlKeyEncoder)
final case class YuUserKeys(name: String, ports: Map[String, YuPort], counts: Map[String, Int], byId: Map[Int, YuPort])
final case class YuBigs(int: BigInt, decimal: BigDecimal)
final case class YuGeneric[A](value: A, items: List[A] = Nil, label: String = "generic")
final case class YuTree(value: Int, children: List[YuTree])
final case class YuPrivate(name: String, private val secret: Int) {
  def reveal: Int = secret
}
final case class YuWrapper(value: Int) extends AnyVal
final case class YuWrapped(id: YuWrapper)

/** scala-yaml's own derivation tests, run through Kindlings (default `YamlConfig`) and scala-yaml's `derived`, and
  * compared: encoded nodes (values, tags, order of the keys), printed YAML, decoded values and the complete failures
  * (message with the position, `expected`).
  *
  * Each case lists the *exact* differences it expects; each one must be documented (see
  * `docs/user-guide/feature-parity.md`, yaml-derivation).
  */
final class YamlUpstreamSuitesOracleSpec extends MacroSuite {

  private object K {
    given YamlConfig = YamlConfig.default
    private def codec[A](using d: KindlingsYamlDecoder[A], e: KindlingsYamlEncoder[A]): YamlCodec[A] =
      YamlCodec.make(d, e)
    val valueTypes: YamlCodec[YuValueTypes] = codec
    val optionTypes: YamlDecoder[YuOptionTypes] = KindlingsYamlDecoder.derived
    val sequenceTypes: YamlCodec[YuSequenceTypes] = codec
    val mappingTypes: YamlCodec[YuMappingTypes] = codec
    val config: YamlCodec[YuConfig] = codec
    val addresses: YamlCodec[YuAddresses] = codec
    val capitals: YamlCodec[YuCapitals] = codec
    val foos: YamlCodec[YuFoos] = codec
    val optionField: YamlCodec[YuOptionField] = codec
    val edgeCases: YamlCodec[YuEdgeCases] = codec
    val description: YamlCodec[YuDescription] = codec
    val emptyValues: YamlCodec[YuEmptyValues] = codec
    val nonNull: YamlDecoder[YuNonNull] = KindlingsYamlDecoder.derived
    val defaultParams: YamlCodec[YuDefaultParams] = codec
    val lazyDefault: YamlCodec[YuLazyDefault] = codec
    val anys: YamlDecoder[YuAnys] = KindlingsYamlDecoder.derived
    val stats: YamlCodec[YuStats] = codec
    val someEnum: YamlCodec[YuSomeEnum] = codec
    val seqOfData: YamlEncoder[YuSeqOfData] = KindlingsYamlEncoder.derived
    val mappingOfMappings: YamlCodec[YuMappingOfMappings] = codec
    val compose: YamlCodec[YuCompose] = codec
    val strings: YamlEncoder[YuStrings] = KindlingsYamlEncoder.derived
    val wide: YamlCodec[YuWide] = codec
    val containers: YamlCodec[YuContainers] = codec
    val intKeys: YamlCodec[YuIntKeys] = codec
    val bigs: YamlDecoder[YuBigs] = KindlingsYamlDecoder.derived
    val generic: YamlCodec[YuGeneric[Int]] = codec
    val tree: YamlCodec[YuTree] = codec
    val priv: YamlCodec[YuPrivate] = codec
    val wrapped: YamlCodec[YuWrapped] = codec
    val tuple: YamlCodec[(Int, String)] = codec
    val either: YamlCodec[Either[YuPort, YuRedis]] = codec
  }

  private object U {
    val valueTypes: YamlCodec[YuValueTypes] = YamlCodec.derived
    val optionTypes: YamlDecoder[YuOptionTypes] = YamlDecoder.derived
    val sequenceTypes: YamlCodec[YuSequenceTypes] = YamlCodec.derived
    val mappingTypes: YamlCodec[YuMappingTypes] = YamlCodec.derived
    given port: YamlCodec[YuPort] = YamlCodec.derived
    given selector: YamlCodec[YuSelector] = YamlCodec.derived
    given spec: YamlCodec[YuSpec] = YamlCodec.derived
    val config: YamlCodec[YuConfig] = YamlCodec.derived
    given address: YamlCodec[YuAddress] = YamlCodec.derived
    val addresses: YamlCodec[YuAddresses] = YamlCodec.derived
    val capitals: YamlCodec[YuCapitals] = YamlCodec.derived
    given foo: YamlCodec[YuFoo] = YamlCodec.derived
    val foos: YamlCodec[YuFoos] = YamlCodec.derived
    val optionField: YamlCodec[YuOptionField] = YamlCodec.derived
    val edgeCases: YamlCodec[YuEdgeCases] = YamlCodec.derived
    val description: YamlCodec[YuDescription] = YamlCodec.derived
    val emptyValues: YamlCodec[YuEmptyValues] = YamlCodec.derived
    val nonNull: YamlDecoder[YuNonNull] = YamlDecoder.derived
    val defaultParams: YamlCodec[YuDefaultParams] = YamlCodec.derived
    val lazyDefault: YamlCodec[YuLazyDefault] = YamlCodec.derived
    val anys: YamlDecoder[YuAnys] = YamlDecoder.derived
    val stats: YamlCodec[YuStats] = YamlCodec.derived
    val someEnum: YamlCodec[YuSomeEnum] = YamlCodec.derived
    given data: YamlCodec[YuData] = YamlCodec.derived
    val seqOfData: YamlEncoder[YuSeqOfData] = YamlEncoder.derived
    given nestedAB: YamlCodec[YuNestedAB] = YamlCodec.derived
    val mappingOfMappings: YamlCodec[YuMappingOfMappings] = YamlCodec.derived
    given web: YamlCodec[YuWeb] = YamlCodec.derived
    given redis: YamlCodec[YuRedis] = YamlCodec.derived
    given services: YamlCodec[YuServices] = YamlCodec.derived
    val compose: YamlCodec[YuCompose] = YamlCodec.derived
    val strings: YamlEncoder[YuStrings] = YamlEncoder.derived
    val wide: YamlCodec[YuWide] = YamlCodec.derived
    val containers: YamlCodec[YuContainers] = YamlCodec.derived
    val intKeys: YamlCodec[YuIntKeys] = YamlCodec.derived
    val bigs: YamlDecoder[YuBigs] = YamlDecoder.derived
    val generic: YamlCodec[YuGeneric[Int]] = YamlCodec.derived
    val priv: YamlCodec[YuPrivate] = YamlCodec.derived
    val tuple: YamlCodec[(Int, String)] = YamlCodec.derived
    val either: YamlCodec[Either[YuPort, YuRedis]] = YamlCodec.derived
  }

  // A user-provided YamlDecoder / YamlEncoder for String and Int in scope of both derivations.
  private object UserKeyInstances {
    given upperCase: YamlDecoder[String] = YamlDecoder.forString.mapError { s =>
      if s.isEmpty then Left(ConstructError.from("empty key")) else Right(s.toUpperCase)
    }
    given prefixed: YamlEncoder[String] = YamlEncoder.forString.mapContra[String]("k-" + _)
    given plusOne: YamlDecoder[Int] = YamlDecoder.forInt.map(_ + 1)
    given timesTen: YamlEncoder[Int] = YamlEncoder.forInt.mapContra[Int](_ * 10)
  }
  private object KUserKeys {
    import UserKeyInstances.given
    given YamlConfig = YamlConfig.default
    val userKeys: YamlCodec[YuUserKeys] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YuUserKeys], KindlingsYamlEncoder.derived[YuUserKeys])
  }
  private object UUserKeys {
    import UserKeyInstances.given
    given port: YamlCodec[YuPort] = YamlCodec.derived
    val userKeys: YamlCodec[YuUserKeys] = YamlCodec.derived
  }

  // --- helpers -------------------------------------------------------------------------------------------------------

  // keys in the order of the mapping: it is the order in which the YAML is printed
  private def toScala(node: Node): Any = node match {
    case s: Node.ScalarNode   => s"${s.value} (${s.tag})"
    case s: Node.SequenceNode => (s.tag.toString, s.nodes.toList.map(toScala))
    case m: Node.MappingNode  =>
      (
        m.tag.toString,
        scala.collection.immutable.ListMap(m.mappings.toSeq.map { case (k, v) => toScala(k).toString -> toScala(v) }*)
      )
  }

  private def compareEncoded[A](kindlings: YamlEncoder[A], upstream: YamlEncoder[A])(values: A*)(
      expectedDifferences: String*
  ): Unit = {
    val differences = values.toList.flatMap { a =>
      val nodes = Oracle.diff(toScala(kindlings.asNode(a)), toScala(upstream.asNode(a))).map(d => s"$a: $d")
      val (k, u) = (kindlings.asNode(a).asYaml, upstream.asNode(a).asYaml)
      nodes ++ (if nodes.isEmpty && k != u then List(s"$a: printed kindlings=$k upstream=$u") else Nil)
    }
    assertEquals(
      differences,
      expectedDifferences.toList,
      differences.mkString("Kindlings vs scala-yaml encoding:\n", "\n", "")
    )
  }

  // NaN != NaN
  private def show(value: Any): String = value.toString

  private def decode[A](decoder: YamlDecoder[A], yaml: String): String =
    yaml.asNode.flatMap(decoder.construct(_)) match {
      case Right(value)                => s"Right(${show(value)})"
      case Left(error: ConstructError) => s"Left(${error.msg} / expected=${error.expected})"
      case Left(error)                 => s"Left(${error.msg})"
    }

  private def compareDecoded[A](kindlings: YamlDecoder[A], upstream: YamlDecoder[A])(inputs: String*)(
      expectedDifferences: String*
  ): Unit = {
    val differences = inputs.toList.flatMap { yaml =>
      val k = decode(kindlings, yaml)
      val u = decode(upstream, yaml)
      if k == u then Nil else List(s"${yaml.replace("\n", "\\n")}: kindlings=$k upstream=$u")
    }
    assertEquals(
      differences.map(_.replace("\n", "\\n")),
      expectedDifferences.toList,
      differences.mkString("Kindlings vs scala-yaml decoding:\n", "\n", "")
    )
  }

  private def assertDecodes[A](decoder: YamlDecoder[A], yaml: String, expected: A): Unit =
    assertEquals(yaml.asNode.flatMap(decoder.construct(_)), Right(expected))

  group("yaml oracle: scala-yaml's DecoderSuite") {

    test("numbers") {
      val yaml =
        s"""double: ${Double.MaxValue}
           |float:  ${Float.MinValue}
           |long:   ${Long.MaxValue}
           |int:    ${Int.MinValue}
           |short:  ${Short.MaxValue}
           |byte:   ${Byte.MinValue}
           |boolean: false
           |""".stripMargin
      assertDecodes(
        K.valueTypes,
        yaml,
        YuValueTypes(Double.MaxValue, Float.MinValue, Long.MaxValue, Int.MinValue, Short.MaxValue, Byte.MinValue, false)
      )
      compareDecoded(K.valueTypes, U.valueTypes)(
        yaml,
        // out of range, not a number, octal/hexadecimal/underscores, a null and a sequence for a number
        "double: 1\nfloat: 1\nlong: 1\nint: 1\nshort: 40000\nbyte: 1\nboolean: false",
        "double: 1\nfloat: 1\nlong: 1\nint: 1\nshort: 1\nbyte: 200\nboolean: false",
        "double: 1\nfloat: 1\nlong: 1\nint: 3000000000\nshort: 1\nbyte: 1\nboolean: false",
        "double: x\nfloat: 1\nlong: 1\nint: 1\nshort: 1\nbyte: 1\nboolean: false",
        "double: 1\nfloat: y\nlong: 1\nint: 1\nshort: 1\nbyte: 1\nboolean: false",
        "double: 1\nfloat: 1\nlong: 1.5\nint: 1\nshort: 1\nbyte: 1\nboolean: false",
        "double: 1_000.5\nfloat: 1_0\nlong: 0x10\nint: 0o17\nshort: 1_000\nbyte: 0x7F\nboolean: True",
        "double: 1\nfloat: 1\nlong: 1\nint: 1\nshort: 1\nbyte: 1\nboolean: yes",
        "double: 1\nfloat: 1\nlong: 1\nint: 1\nshort: 1\nbyte: 1\nboolean: 1",
        "double: 1\nfloat: 1\nlong: 1\nint: null\nshort: 1\nbyte: 1\nboolean: false",
        "double: 1\nfloat: 1\nlong: 1\nint: [1]\nshort: 1\nbyte: 1\nboolean: false",
        "double: 1\nfloat: 1\nlong: 1\nint: {a: 1}\nshort: 1\nbyte: 1\nboolean: false"
      )()
      compareEncoded(K.valueTypes, U.valueTypes)(
        YuValueTypes(
          Double.MaxValue,
          Float.MinValue,
          Long.MaxValue,
          Int.MinValue,
          Short.MaxValue,
          Byte.MinValue,
          false
        ),
        YuValueTypes(Double.NaN, Float.PositiveInfinity, 0L, 0, 0, 0, true)
      )()
    }

    test("option") {
      val yaml =
        s"""double: ${Double.MaxValue}
           |float: null
           |int:
           |""".stripMargin
      assertDecodes(K.optionTypes, yaml, YuOptionTypes(Some(Double.MaxValue), None, None, None))
      compareDecoded(K.optionTypes, U.optionTypes)(
        yaml,
        "{}",
        "double: ~\nfloat: Null\nint: NULL\nshort: !!null\n",
        "double: x",
        "short: [1]"
      )()
    }

    test("sequence") {
      val flow = "doubles: ['1.0', '2.0']\nfloats: ['1.0', '2.0']\nints: ['1', '2']\n"
      val block = "doubles:\n  - 1.0\n  - 2.0\nfloats:\n  - 1.0\n  - 2.0\nints: \n  - 1 \n  - 2\n"
      val expected = YuSequenceTypes(List(1.0, 2.0), Seq(1.0f, 2.0f), Set(1, 2))
      assertDecodes(K.sequenceTypes, flow, expected)
      assertDecodes(K.sequenceTypes, block, expected)
      compareDecoded(K.sequenceTypes, U.sequenceTypes)(
        flow,
        block,
        "doubles: 1\nfloats: []\nints: []",
        "doubles: []\nfloats: {a: 1}\nints: []",
        "doubles: []\nfloats: []\nints: null",
        "doubles: []\nfloats: []\nints: [1, x, y]"
      )()
      compareEncoded(K.sequenceTypes, U.sequenceTypes)(expected, YuSequenceTypes(Nil, Nil, Set.empty))()
    }

    test("mapping, mapping-2") {
      val block = "doubles:\n  double1: 1\n  double2: 2\nfloats: \n  floats: \n    - 3.0\n    - 4.0\n"
      val flow = "doubles: { double1: 1.0, double2: 2.0 }\nfloats: \n  floats: [ '3.0', '4.0' ]\n"
      val expected = YuMappingTypes(Map("double1" -> 1.0, "double2" -> 2.0), Map("floats" -> List(3.0f, 4.0f)))
      assertDecodes(K.mappingTypes, block, expected)
      assertDecodes(K.mappingTypes, flow, expected)
      compareDecoded(K.mappingTypes, U.mappingTypes)(
        block,
        flow,
        "doubles: [1]\nfloats: {}",
        "doubles: {a: x}\nfloats: {}",
        "doubles: {}\nfloats: {a: 1}",
        "doubles: {}\nfloats: null"
      )()
      compareEncoded(K.mappingTypes, U.mappingTypes)(expected, YuMappingTypes(Map.empty, Map.empty))()
    }

    test("case class") {
      val yaml =
        s"""spec:
           |  # comment
           |  type: NodePort
           |  # to delete
           |  ports:
           |  - port: 80
           |  selector:
           |    app: guestbook
           |    tier: frontend
           |""".stripMargin
      val expected = YuConfig(YuSpec("NodePort", List(YuPort(80)), YuSelector("guestbook", "frontend")))
      assertDecodes(K.config, yaml, expected)
      compareDecoded(K.config, U.config)(
        yaml,
        "spec: {type: T, ports: [], selector: {app: a}}",
        "spec: {type: T, ports: [{port: x}], selector: {app: a, tier: t}}",
        "spec: {type: T, ports: [{}], selector: {app: a, tier: t}}",
        "spec: {type: T, ports: {port: 1}, selector: {app: a, tier: t}}",
        "spec: {type: T, ports: 1, selector: {app: a, tier: t}}",
        "spec: {type: T, ports: null, selector: {app: a, tier: t}}",
        "spec: {type: T, ports: [1], selector: {app: a, tier: t}}",
        "spec: {type: T, ports: [], selector: s}",
        "spec: {type: T, ports: [], selector: null}",
        "spec: null",
        "spec: [1]",
        // the first failing field, in the order of the fields
        "spec: {selector: 1, ports: 2}"
      )()
      compareEncoded(K.config, U.config)(expected)()
    }

    test("sequence of mappings") {
      val yaml = "addresses:\n  - port: 80\n    local: localhost\n  - port: 81\n    network: 127.0.0.1\n"
      assertDecodes(K.addresses, yaml, YuAddresses(List(YuLocal("localhost", 80), YuNetwork("127.0.0.1", 81))))
      compareDecoded(K.addresses, U.addresses)(
        yaml,
        "addresses: [{port: 1}]",
        "addresses: [{port: 1, local: l, network: n}]",
        "addresses: [x]",
        "addresses: [null]"
      )()
      compareEncoded(K.addresses, U.addresses)(
        YuAddresses(List(YuLocal("localhost", 80), YuNetwork("127.0.0.1", 81)))
      )()
    }

    test("codec mapping: a user-provided codec is used") {
      val value = YuCapitals(List(YuCapital("hello"), YuCapital("world")), YuCapital("main"), Some(YuCapital("o")))
      compareEncoded(K.capitals, U.capitals)(value, value.copy(other = None))()
      compareDecoded(K.capitals, U.capitals)(
        "capitals: [hello, world]\nmain: main\nother: o",
        "capitals: [hello, world]\nmain: main",
        "capitals: [hello, [1]]\nmain: main",
        "capitals: [hello]\nmain: {a: 1}"
      )()
      assertDecodes(
        K.capitals,
        "capitals: [hello, world]\nmain: main",
        YuCapitals(List(YuCapital("HELLO"), YuCapital("WORLD")), YuCapital("MAIN"), None)
      )
    }

    test("option of a case class in a list") {
      val yaml = "foos:\n  - int: 1\n    string: \"1\"\n  - !!null\n"
      assertDecodes(K.foos, yaml, YuFoos(List(Some(YuFoo(1, "1")), None)))
      compareDecoded(K.foos, U.foos)(yaml, "foos: [~, {int: 1}]", "foos: [x]", "foos: [[1]]")()
      compareEncoded(K.foos, U.foos)(YuFoos(List(Some(YuFoo(1, "1")), None)))()
    }

    test("issue 222 - parse edge cases of booleans floats doubles and integers") {
      val yaml = """booleans: [ true, True, false, FALSE ]
                   |integers: [ 0, 0o7, 0x3A, -19 ]
                   |floats: [
                   |  0., -0.0, .5, +12e03, -2E+05 ]
                   |also floats: [
                   |  .inf, -.Inf, +.INF, .NAN, .nan, .NaN]
                   |also doubles: [
                   |  .inf, -.Inf, +.INF, .NAN, .nan, .NaN]""".stripMargin
      compareDecoded(K.edgeCases, U.edgeCases)(yaml)()
      assertEquals(
        decode(K.edgeCases, yaml),
        "Right(YuEdgeCases(List(true, true, false, false),List(0, 7, 58, -19),List(0.0, -0.0, 0.5, 12000.0, -200000.0)," +
          "List(Infinity, -Infinity, Infinity, NaN, NaN, NaN),List(Infinity, -Infinity, Infinity, NaN, NaN, NaN)))"
      )
    }

    test("issue 281 - parse multiline string") {
      val yaml = "description: |-\n  Hi\n  my name\n  is John"
      assertDecodes(K.description, yaml, YuDescription("Hi\nmy name\nis John"))
      compareDecoded(K.description, U.description)(yaml, "description: 'hello\\there'", "description: [a]")()
    }

    test("issue 86 - parsing key with empty value") {
      val yamls = List(
        "{ \"single line\", a: b}",
        "{ \"multi\n  line\", a: b}",
        "{ \"single line\": 23, a: c}",
        "{ \"multi\n  line\": 42, a: c}"
      )
      compareDecoded(K.emptyValues, U.emptyValues)(yamls*)()
      assertDecodes(K.emptyValues, yamls(2), YuEmptyValues(Some(23), None, "c"))
      assertDecodes(K.emptyValues, yamls(3), YuEmptyValues(None, Some(42), "c"))
    }

    test("issue 120 - fail conversion of !!null to non-optional types") {
      compareDecoded(K.nonNull, U.nonNull)("key1: 1\nkey2: !!null\n", "key1: ~\nkey2: 2", "key1: 1\nkey2: ")()
      assert(decode(K.nonNull, "key1: 1\nkey2: !!null\n").contains("Could't construct int from null"))
    }

    test("default parameters for case classes can be used when decoding") {
      assertDecodes(K.defaultParams, "d: 1.0", YuDefaultParams(1, "test", None, 1.0))
      compareDecoded(K.defaultParams, U.defaultParams)("d: 1.0", "a: 2\nd: 1.0", "a: x\nd: 1.0", "{}", "c: 1\nd: 2")()
      compareEncoded(K.defaultParams, U.defaultParams)(
        YuDefaultParams(d = 1.0),
        YuDefaultParams(2, "b", Some(3), 4.0)
      )()
    }

    test("default parameters are evaluated lazily, on every use, and not when the field is provided") {
      def uses(decoder: YamlDecoder[YuLazyDefault]): List[String] = {
        YuCounter.times = 0
        List("a: 1", "a: 1", "a: 1\nb: from yaml", "a: 1", "a: x", "b: y", "a: 1")
          .map(decode(decoder, _)) :+ s"evaluated ${YuCounter.times} times"
      }
      val kindlings = uses(K.lazyDefault)
      val upstream = uses(U.lazyDefault)
      assertEquals(kindlings.take(6), upstream.take(6))
      assertEquals(
        kindlings.take(4),
        List(
          "Right(YuLazyDefault(1,test-1))",
          "Right(YuLazyDefault(1,test-2))",
          "Right(YuLazyDefault(1,from yaml))",
          "Right(YuLazyDefault(1,test-3))"
        )
      )
      // Documented difference (feature-parity.md, yaml-derivation): Kindlings stops at the first failing field, scala-yaml
      // decodes every field and then reports the first failure - so for `a: x` it also evaluated the default of `b`.
      assertEquals(kindlings.drop(6), List("Right(YuLazyDefault(1,test-4))", "evaluated 4 times"))
      assertEquals(upstream.drop(6), List("Right(YuLazyDefault(1,test-5))", "evaluated 5 times"))
    }

    test("Any: aliases, Map[Any, Any], precise doubles, non-finite floats") {
      compareDecoded(K.anys, U.anys)(
        "value: &a 5\nmap: {123: 321, string: aezakmi, true: false, 5.5: 55.55}\nlist: [*a, 0.018256052173961423, .inf]",
        "value: {seq1: &a [1, 2], seq2: *a}\nmap: {}\nlist: []",
        "value: [a, 1, 1.5, true, ~]\nmap: {k: [1]}\nlist: [{a: 1}]",
        "value: !Custom 5\nmap: {}\nlist: []",
        "value: 1\nmap: [1]\nlist: []"
      )()
    }
  }

  group("yaml oracle: scala-yaml's ConstructSuite, DecoderErrorsSuite, DockerYamlDecoderSuite") {

    test("derive construct for case class, for sealed trait") {
      val stats = Node.MappingNode(
        Node.ScalarNode("hr") -> Node.ScalarNode("65"),
        Node.ScalarNode("avg") -> Node.ScalarNode("0.278"),
        Node.ScalarNode("rbi") -> Node.ScalarNode("147")
      )
      assertEquals(K.stats.construct(stats), Right(YuStats(65, 0.278, 147)))
      assertEquals(K.stats.construct(stats), U.stats.construct(stats))
      val foo = Node.MappingNode(Node.ScalarNode("value") -> Node.ScalarNode("65"))
      val bar = Node.MappingNode(Node.ScalarNode("price") -> Node.ScalarNode("65.997"))
      assertEquals(K.someEnum.construct(foo), Right(YuSomeEnum.Foo(65)))
      assertEquals(K.someEnum.construct(bar), Right(YuSomeEnum.Bar(65.997)))
      compareDecoded(K.someEnum, U.someEnum)("value: 65", "price: 65.997", "value: x", "other: 1", "x", "[1]", "~")()
      compareEncoded(K.someEnum, U.someEnum)(YuSomeEnum.Foo(1), YuSomeEnum.Bar(1.5))()
    }

    test("key must be scalar node") {
      val node = Node.MappingNode(
        Node.MappingNode(Node.ScalarNode("key") -> Node.ScalarNode("value")) ->
          Node.MappingNode(Node.ScalarNode("key2") -> Node.ScalarNode("value2"))
      )
      assertEquals(K.stats.construct(node), U.stats.construct(node))
      assertEquals(
        K.stats.construct(node),
        Left(
          ConstructError.from(
            "Parameter of a class must be a scalar value",
            Node.MappingNode(Node.ScalarNode("key") -> Node.ScalarNode("value"))
          )
        )
      )
    }

    test("fail for invalid int: the message has the position") {
      val yaml = "hr: 1\navg: 1.5\nrbi: xxx\n"
      compareDecoded(K.stats, U.stats)(yaml, "hr: 1\navg: 1.5", "hr: 1\navg: 1.5\nrbi: ", "- 1", "text", "~")()
      assertEquals(
        yaml.asNode.flatMap(K.stats.construct(_)).left.map(_.msg),
        Left("""|For input string: "xxx"
                |at 2:5, expected Int
                |rbi: xxx
                |     ^ """.stripMargin)
      )
    }

    test("derive construct for docker compose file") {
      val yaml =
        s"""version: "3.9"
           |services:
           |  web:
           |    build: .
           |    volumes:
           |      - .:/code
           |      - logvolume01:/var/log
           |    ports:
           |      - "5000:5000"
           |  redis:
           |    image: "redis:alpine"
           |""".stripMargin
      val expected = YuCompose(
        "3.9",
        YuServices(YuWeb(".", List("5000:5000"), List(".:/code", "logvolume01:/var/log")), YuRedis("redis:alpine"))
      )
      assertDecodes(K.compose, yaml, expected)
      compareDecoded(K.compose, U.compose)(yaml, "version: 1\nservices: {web: {build: b, ports: [], volumes: []}}")()
      // BaseYamlEncoderSuite "complex kubernetes mapping"
      compareEncoded(K.compose, U.compose)(expected)()
      assertEquals(
        K.compose.asNode(expected).asYaml,
        "version: 3.9\nservices: \n  web: \n    build: .\n    ports: \n      - 5000:5000\n    volumes: \n" +
          "      - .:/code\n      - logvolume01:/var/log\n  redis: \n    image: redis:alpine\n"
      )
    }
  }

  group("yaml oracle: scala-yaml's YamlEncoderSuite") {

    test("case class (mapping), mapping of mappings") {
      compareEncoded(K.stats, U.stats)(YuStats(1, 1.0, 1))()
      assertEquals(K.stats.asNode(YuStats(1, 1.0, 1)).asYaml, "hr: 1\navg: 1.0\nrbi: 1\n")
      compareEncoded(K.mappingOfMappings, U.mappingOfMappings)(
        YuMappingOfMappings(YuNestedAB(1, "one"), YuNestedAB(2, "two"))
      )()
    }

    test("sequence of mappings, sequence of sequences, map of chars, set of boolean") {
      compareEncoded(K.seqOfData, U.seqOfData)(
        YuSeqOfData(
          Seq(YuData(1, 1.997), YuData(2, 2.997)),
          Seq(Seq(1, 2), Seq(3, 4)),
          Map("1" -> 'a', "2" -> 'b', "3" -> 'c'),
          Set(true, false)
        ),
        YuSeqOfData(Nil, Seq(Nil), Map.empty, Set.empty)
      )()
    }

    test("option") {
      compareEncoded(K.optionField, U.optionField)(YuOptionField(Some("some")), YuOptionField(None))()
      assertNoDiff(K.optionField.asNode(YuOptionField(None)).asYaml, "field: !!null")
      compareDecoded(K.optionField, U.optionField)("field: some", "field: !!null\n", "field: ", "{}", "field: [1]")()
    }

    test("strings: plain values, non-printable characters, chars") {
      compareEncoded(K.strings, U.strings)(
        YuStrings("aezakmi", Char.MinValue.toString + Char.MaxValue.toString + "\u0085\t\n\u007F", 'c'),
        YuStrings("", "a: b # c", '\u0000')
      )()
    }
  }

  group("yaml oracle: scala-yaml's derivation beyond its suites") {

    test("more than 4 fields: the order of the keys") {
      compareEncoded(K.wide, U.wide)(YuWide(1, 2, 3, 4, 5, 6, 7))()
      compareDecoded(K.wide, U.wide)("f1: 1\nf2: 2\nf3: 3\nf4: 4\nf5: 5\nf6: 6\nf7: 7", "f7: 7\nf1: x\nf2: y")()
    }

    test("collections, options and maps of derived types") {
      val value = YuContainers(List(YuPort(1)), Seq(YuPort(2)), Set(YuPort(3)), Some(YuPort(4)), Map("k" -> YuPort(5)))
      compareEncoded(K.containers, U.containers)(value, YuContainers(Nil, Nil, Set.empty, None, Map.empty))()
      def yaml(list: String = "[]", seq: String = "[]", set: String = "[]", option: String = "~", map: String = "{}") =
        s"list: $list\nseq: $seq\nset: $set\noption: $option\nmap: $map"
      compareDecoded(K.containers, U.containers)(
        yaml("[{port: 1}]", "[{port: 2}]", "[{port: 3}, {port: 3}]", "{port: 4}", "{k: {port: 5}}"),
        yaml(),
        "list: []\nseq: []\nset: []\nmap: {}",
        yaml(list = "x"),
        yaml(list = "{port: 1}"),
        yaml(list = "~"),
        yaml(list = "[{port: x}, {port: y}]"),
        yaml(seq = "x"),
        yaml(seq = "~"),
        yaml(set = "{a: 1}"),
        yaml(set = "~"),
        yaml(option = "x"),
        yaml(option = "[1]"),
        yaml(map = "x"),
        yaml(map = "[1]"),
        yaml(map = "~"),
        yaml(map = "{k: 1}"),
        yaml(map = "{[1, 2]: {port: 1}}"),
        yaml(map = "{1: {port: 1}, true: {port: 2}, ~: {port: 3}}")
      )()
    }

    test("big numbers") {
      compareDecoded(K.bigs, U.bigs)(
        "int: 123456789012345678901234567890\ndecimal: 1.5e400",
        "int: 0o17\ndecimal: 1_000.5",
        "int: 1_000\ndecimal: 1",
        "int: 0x10\ndecimal: 1",
        "int: x\ndecimal: 1",
        "int: 1\ndecimal: y",
        "int: ~\ndecimal: 1",
        "int: 1\ndecimal: [1]"
      )()
    }

    test("generic case class with defaults") {
      compareEncoded(K.generic, U.generic)(YuGeneric(1), YuGeneric(1, List(2), "l"))()
      compareDecoded(K.generic, U.generic)("value: 1", "value: 1\nitems: [2]\nlabel: l", "items: [2]", "value: x")()
    }

    test("recursive case class: Kindlings only") {
      // scala-yaml: `given YamlCodec[YuTree] = YamlCodec.derived` (or `derives YamlCodec`) never finishes initializing -
      // the derived encoder summons the instances of the fields in its constructor, i.e. itself, while the lazy given
      // is being initialized (the thread waits for its own initialization) - so there is nothing to compare with.
      val value = YuTree(1, List(YuTree(2, Nil), YuTree(3, List(YuTree(4, Nil)))))
      assertEquals(K.tree.construct(K.tree.asNode(value)), Right(value))
      assertDecodes(K.tree, "value: 1\nchildren: [{value: 2, children: []}]", YuTree(1, List(YuTree(2, Nil))))
      assert(decode(K.tree, "value: 1\nchildren: [{value: 2}]").startsWith("Left(Key children doesn't exist"))
    }

    test("private constructor fields are written and read") {
      compareEncoded(K.priv, U.priv)(YuPrivate("n", 1))()
      compareDecoded(K.priv, U.priv)("name: n\nsecret: 1", "name: n")()
    }

    test("tuples and Either are derived as the case classes they are") {
      compareEncoded(K.tuple, U.tuple)((1, "a"))()
      compareDecoded(K.tuple, U.tuple)("_1: 1\n_2: a", "_1: 1", "[1, a]")()
      compareEncoded(K.either, U.either)(Left(YuPort(1)), Right(YuRedis("r")))()
      compareDecoded(K.either, U.either)("value: {port: 1}", "value: {image: r}", "value: 1", "port: 1")()
    }

    test("value classes: scala-yaml cannot derive them (no Mirror), Kindlings unwraps them") {
      assert(compileErrors("YamlCodec.derived[YuWrapper]").contains("it is a value class"))
      assertEquals(K.wrapped.asNode(YuWrapped(YuWrapper(1))).asYaml, "id: 1\n")
      assertDecodes(K.wrapped, "id: 1", YuWrapped(YuWrapper(1)))
    }

    test("map keys: scala-yaml uses YamlDecoder[K] / YamlEncoder[K], Kindlings key codecs (documented difference)") {
      val yaml = "name: n\nports: {a: {port: 1}}\ncounts: {c: 3}\nbyId: {4: {port: 5}}"
      // the fields and values take the user-provided instances on both sides
      assertDecodes(
        UUserKeys.userKeys,
        yaml,
        YuUserKeys("N", Map("A" -> YuPort(2)), Map("C" -> 4), Map(5 -> YuPort(6)))
      )
      // Kindlings: keys with the built-in key codecs (a user-provided YamlKeyDecoder / YamlKeyEncoder would be used)
      assertDecodes(
        KUserKeys.userKeys,
        yaml,
        YuUserKeys("N", Map("a" -> YuPort(2)), Map("c" -> 4), Map(4 -> YuPort(6)))
      )
      val value = YuUserKeys("n", Map("a" -> YuPort(1)), Map("c" -> 3), Map(4 -> YuPort(5)))
      assertEquals(
        UUserKeys.userKeys.asNode(value).asYaml,
        "name: k-n\nports: \n  k-a: \n    port: 10\ncounts: \n  k-c: 30\nbyId: \n  40: \n    port: 50\n"
      )
      assertEquals(
        KUserKeys.userKeys.asNode(value).asYaml,
        "name: k-n\nports: \n  a: \n    port: 10\ncounts: \n  c: 30\nbyId: \n  4: \n    port: 50\n"
      )
      // the keys that both reject are rejected with the same failures
      compareDecoded(KUserKeys.userKeys, UUserKeys.userKeys)(
        "name: n\nports: {~: {port: 1}}\ncounts: {}\nbyId: {}",
        "name: n\nports: {[1]: {port: 1}}\ncounts: {}\nbyId: {}",
        "name: n\nports: {a: {port: x}}\ncounts: {}\nbyId: {}",
        "name: n\nports: {}\ncounts: {}\nbyId: {x: {port: 1}}"
      )()
    }

    test("maps with keys that are not strings, of derived values") {
      compareEncoded(K.intKeys, U.intKeys)(YuIntKeys(Map(1 -> YuPort(1), -2 -> YuPort(2))), YuIntKeys(Map.empty))()
      compareDecoded(K.intKeys, U.intKeys)(
        "byId: {1: {port: 1}, 0x10: {port: 2}}",
        "byId: {}",
        "byId: {a: {port: 1}}",
        "byId: {~: {port: 1}}",
        "byId: {[1]: {port: 1}}",
        "byId: {1: {port: x}}",
        "byId: {a: {port: x}}",
        "byId: [1]",
        "byId: ~"
      )()
    }
  }
}
