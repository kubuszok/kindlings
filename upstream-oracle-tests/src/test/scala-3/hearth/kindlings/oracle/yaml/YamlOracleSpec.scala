package hearth.kindlings.oracle.yaml

import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder, YamlConfig}
import org.virtuslab.yaml.{Node, YamlCodec, YamlDecoder, YamlEncoder}
import org.virtuslab.yaml.*

final case class YmPerson(name: String, age: Int, nickname: Option[String])
final case class YmDefaults(host: String = "localhost", port: Int = 8080, tags: List[String] = Nil)
final case class YmCollections(list: List[Int], set: Set[String], map: Map[String, Int], seq: Seq[Double])
final case class YmNested(owner: YmPerson, others: List[YmPerson], byName: Map[String, YmPerson])
final case class YmPrimitives(b: Boolean, l: Long, d: Double, f: Float, s: String)

sealed trait YmShape
final case class YmCircle(radius: Double) extends YmShape
final case class YmRectangle(width: Double, height: Double) extends YmShape

// Overlapping subtypes, declared out of alphabetical order: the order in which they are attempted decides the result.
sealed trait YmSource
final case class YmUrl(url: String) extends YmSource
final case class YmFile(path: String) extends YmSource
final case class YmAny(url: String, path: Option[String]) extends YmSource
case object YmNoSource extends YmSource

enum YmFruit {
  case Apple(weight: Double)
  case Banana(length: Double)
  case Unknown
}

final case class YmBasket(fruits: List[YmFruit], source: YmSource)

final case class YmEmpties(
    list: List[Int],
    map: Map[String, Int],
    nested: List[List[Int]],
    mapOfLists: Map[String, List[Int]],
    after: String
)

/** Kindlings' YAML codecs (default `YamlConfig`, which matches scala-yaml) vs scala-yaml's own derivation
  * (`YamlCodec.derived`, Scala 3 only), compared on the YAML nodes encoded and on the values decoded (valid and invalid
  * inputs).
  *
  * Each case lists the *exact* differences it expects; each one must be documented (see
  * `docs/user-guide/feature-parity.md`, yaml-derivation). A difference that appears or disappears fails the test.
  */
final class YamlOracleSpec extends MacroSuite {

  private object KindlingsDefault {
    given YamlConfig = YamlConfig.default
    val person: YamlCodec[YmPerson] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmPerson], KindlingsYamlEncoder.derived[YmPerson])
    val defaults: YamlCodec[YmDefaults] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmDefaults], KindlingsYamlEncoder.derived[YmDefaults])
    val collections: YamlCodec[YmCollections] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmCollections], KindlingsYamlEncoder.derived[YmCollections])
    val nested: YamlCodec[YmNested] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmNested], KindlingsYamlEncoder.derived[YmNested])
    val primitives: YamlCodec[YmPrimitives] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmPrimitives], KindlingsYamlEncoder.derived[YmPrimitives])
    val shape: YamlCodec[YmShape] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmShape], KindlingsYamlEncoder.derived[YmShape])
    val source: YamlCodec[YmSource] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmSource], KindlingsYamlEncoder.derived[YmSource])
    val fruit: YamlCodec[YmFruit] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmFruit], KindlingsYamlEncoder.derived[YmFruit])
    val basket: YamlCodec[YmBasket] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmBasket], KindlingsYamlEncoder.derived[YmBasket])
    val empties: YamlCodec[YmEmpties] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmEmpties], KindlingsYamlEncoder.derived[YmEmpties])
  }

  /** Kindlings' opt-ins that intentionally differ from scala-yaml (the defaults of Kindlings 0.3.x). */
  private object KindlingsOptIns {
    given YamlConfig = YamlConfig.default.withoutUseDefaults.withWrappedSubtypes
    val defaults: YamlCodec[YmDefaults] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmDefaults], KindlingsYamlEncoder.derived[YmDefaults])
    val shape: YamlCodec[YmShape] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmShape], KindlingsYamlEncoder.derived[YmShape])
  }

  private object Upstream {
    given person: YamlCodec[YmPerson] = YamlCodec.derived
    val defaults: YamlCodec[YmDefaults] = YamlCodec.derived
    val collections: YamlCodec[YmCollections] = YamlCodec.derived
    val nested: YamlCodec[YmNested] = YamlCodec.derived
    val primitives: YamlCodec[YmPrimitives] = YamlCodec.derived
    val shape: YamlCodec[YmShape] = YamlCodec.derived
    val source: YamlCodec[YmSource] = YamlCodec.derived
    given fruit: YamlCodec[YmFruit] = YamlCodec.derived
    given sourceGiven: YamlCodec[YmSource] = source
    val basket: YamlCodec[YmBasket] = YamlCodec.derived
    val empties: YamlCodec[YmEmpties] = YamlCodec.derived
  }

  // --- helpers -------------------------------------------------------------------------------------------------------

  private def toScala(node: Node): Any = node match {
    case s: Node.ScalarNode   => s"${s.value} (${s.tag})"
    case s: Node.SequenceNode => (s.tag.toString, s.nodes.toList.map(toScala))
    case m: Node.MappingNode  =>
      (
        m.tag.toString,
        scala.collection.immutable.TreeMap(m.mappings.toSeq.map { case (k, v) => toScala(k).toString -> toScala(v) }*)
      )
  }

  private def compareEncoded[A](kindlings: YamlEncoder[A], upstream: YamlEncoder[A])(values: A*)(
      expectedDifferences: String*
  ): Unit = {
    val differences = values.toList.flatMap { a =>
      Oracle.diff(toScala(kindlings.asNode(a)), toScala(upstream.asNode(a))).map(d => s"$a: $d")
    }
    assertEquals(
      differences,
      expectedDifferences.toList,
      differences.mkString("Kindlings vs scala-yaml encoding:\n", "\n", "")
    )
  }

  private def decode[A](decoder: YamlDecoder[A], yaml: String): String =
    yaml.asNode.flatMap(decoder.construct(_)) match {
      case Right(value) => s"Right($value)"
      case Left(error)  => s"Left(${error.msg.linesIterator.next()})"
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
      differences,
      expectedDifferences.toList,
      differences.mkString("Kindlings vs scala-yaml decoding:\n", "\n", "")
    )
  }

  private val K = KindlingsDefault
  private val U = Upstream

  group("yaml oracle (scala-yaml derives)") {

    test("case class with an Option field") {
      compareEncoded(K.person, U.person)(YmPerson("Alice", 30, Some("Al")), YmPerson("Bob", 1, None))()
      compareDecoded(K.person, U.person)(
        "name: Alice\nage: 30\nnickname: Al",
        "name: Alice\nage: 30",
        "name: Alice\nage: 30\nnickname: null",
        "name: Alice\nage: 30\nextra: 1",
        "name: Alice",
        "name: Alice\nage: thirty",
        "- 1\n- 2",
        "hello"
      )()
    }

    test("primitives") {
      compareEncoded(K.primitives, U.primitives)(YmPrimitives(true, 1L, 1.5, 2.5f, "s"))()
      compareDecoded(K.primitives, U.primitives)(
        "b: true\nl: 1\nd: 1.5\nf: 2.5\ns: s",
        "b: yes\nl: 1\nd: 1\nf: 2\ns: 1"
      )()
    }

    test("default values") {
      compareEncoded(K.defaults, U.defaults)(YmDefaults(), YmDefaults("h", 1, List("a")))()
      compareDecoded(K.defaults, U.defaults)("host: h\nport: 1\ntags: [a]", "host: h", "{}")()
      // Opt-in difference (feature-parity.md, yaml-derivation): `withoutUseDefaults` rejects absent fields
      compareDecoded(KindlingsOptIns.defaults, U.defaults)("host: h\nport: 1\ntags: [a]", "host: h", "{}")(
        "host: h: kindlings=Left(Key port doesn't exist in parsed document) upstream=Right(YmDefaults(h,8080,List()))",
        "{}: kindlings=Left(Key host doesn't exist in parsed document) upstream=Right(YmDefaults(localhost,8080,List()))"
      )
    }

    test("collections and maps") {
      compareEncoded(K.collections, U.collections)(
        YmCollections(List(1, 2), Set("a"), Map("k" -> 1), Seq(1.5)),
        YmCollections(Nil, Set.empty, Map.empty, Seq.empty)
      )()
      compareDecoded(K.collections, U.collections)(
        "list: [1, 2]\nset: [a, b]\nmap: {k: 1}\nseq: [1.5]",
        "list: [1, x]\nset: []\nmap: {}\nseq: []",
        "list: 1\nset: []\nmap: {}\nseq: []"
      )()
    }

    test("nested case classes") {
      compareEncoded(K.nested, U.nested)(
        YmNested(YmPerson("A", 1, None), List(YmPerson("B", 2, Some("b"))), Map("c" -> YmPerson("C", 3, None)))
      )()
      compareDecoded(K.nested, U.nested)(
        "owner: {name: A, age: 1}\nothers: [{name: B, age: 2}]\nbyName: {c: {name: C, age: 3}}",
        "owner: {name: A}\nothers: []\nbyName: {}"
      )()
    }

    test("sealed traits: untagged, decoded by trying each subtype") {
      compareEncoded(K.shape, U.shape)(YmCircle(1.5), YmRectangle(1, 2))()
      compareDecoded(K.shape, U.shape)(
        "radius: 1.5",
        "width: 1\nheight: 2",
        "YmCircle:\n  radius: 1.5",
        "side: 1",
        "- 1",
        "text"
      )()
    }

    test("sealed traits: overlapping subtypes and case objects are attempted in declaration order") {
      compareEncoded(K.source, U.source)(YmUrl("u"), YmFile("p"), YmAny("u", Some("p")), YmNoSource)()
      compareDecoded(K.source, U.source)(
        "url: u",
        "path: p",
        "url: u\npath: p",
        "{}",
        "other: 1",
        "- 1"
      )()
    }

    test("Scala 3 enums with fields and singleton cases, nested in a product") {
      compareEncoded(K.fruit, U.fruit)(YmFruit.Apple(1), YmFruit.Banana(2), YmFruit.Unknown)()
      compareDecoded(K.fruit, U.fruit)("weight: 1", "length: 2", "{}", "color: red", "Apple: {weight: 1}")()
      compareEncoded(K.basket, U.basket)(YmBasket(List(YmFruit.Apple(1), YmFruit.Unknown), YmFile("p")))()
      compareDecoded(K.basket, U.basket)(
        "fruits: [{weight: 1}, {}]\nsource: {path: p}",
        "fruits: [1]\nsource: {path: p}"
      )()
    }

    test("empty collections and maps: printed YAML and print-then-decode") {
      val values = List(
        YmEmpties(Nil, Map.empty, Nil, Map.empty, "x"),
        YmEmpties(Nil, Map.empty, List(Nil, List(1)), Map("k" -> Nil), "x")
      )
      compareEncoded(K.empties, U.empties)(values*)()
      // Printed with scala-yaml's own printer (`Node#asYaml`) in both cases, from the same nodes: an empty sequence or
      // mapping is printed as an empty value (`list: `), which reads back as null, so the printed YAML does not decode
      // back - in scala-yaml just as in Kindlings (documented in feature-parity.md, yaml-derivation).
      val nullList = "Left(Could't construct scala.collection.immutable.List from null (tag:yaml.org,2002:null))"
      values.foreach { value =>
        val kindlingsYaml = KindlingsYamlEncoder.toYamlString(value)
        val upstreamYaml = U.empties.asNode(value).asYaml
        assertEquals(kindlingsYaml, upstreamYaml)
        assert(!upstreamYaml.contains("[]") && !upstreamYaml.contains("{}"), upstreamYaml)
        assertEquals(decode(K.empties, kindlingsYaml), nullList)
        assertEquals(decode(U.empties, upstreamYaml), nullList)
      }
      // the flow style `[]` / `{}` (e.g. written by hand) decodes in both
      compareDecoded(K.empties, U.empties)("list: []\nmap: {}\nnested: [[], [1]]\nmapOfLists: {k: []}\nafter: x")()
      assertEquals(
        decode(K.empties, "list: []\nmap: {}\nnested: [[], [1]]\nmapOfLists: {k: []}\nafter: x"),
        "Right(YmEmpties(List(),Map(),List(List(), List(1)),Map(k -> List()),x))"
      )
    }

    test("withWrappedSubtypes (opt-in): Kindlings' single-key mapping") {
      // Opt-in difference (feature-parity.md, yaml-derivation): each subtype is wrapped in a single-key mapping
      val str = "CoreSchemaTag(tag:yaml.org,2002:str)"
      val float = "CoreSchemaTag(tag:yaml.org,2002:float)"
      val map = "CoreSchemaTag(tag:yaml.org,2002:map)"
      compareEncoded(KindlingsOptIns.shape, U.shape)(YmCircle(1.5), YmRectangle(1, 2))(
        s"YmCircle(1.5): $$._2[YmCircle ($str)]: only in kindlings: ($map,TreeMap(radius ($str) -> 1.5 ($float)))",
        s"YmCircle(1.5): $$._2[radius ($str)]: only in upstream: 1.5 ($float)",
        s"YmRectangle(1.0,2.0): $$._2[YmRectangle ($str)]: only in kindlings: " +
          s"($map,TreeMap(height ($str) -> 2.0 ($float), width ($str) -> 1.0 ($float)))",
        s"YmRectangle(1.0,2.0): $$._2[height ($str)]: only in upstream: 2.0 ($float)",
        s"YmRectangle(1.0,2.0): $$._2[width ($str)]: only in upstream: 1.0 ($float)"
      )
      // (failures compared as failures only: the messages of these two different strategies are unrelated)
      def outcome(decoder: YamlDecoder[YmShape], yaml: String): String =
        if decode(decoder, yaml).startsWith("Left") then "Left" else decode(decoder, yaml)
      val differences = List("radius: 1.5", "width: 1\nheight: 2", "YmCircle:\n  radius: 1.5", "side: 1").collect {
        case yaml if outcome(KindlingsOptIns.shape, yaml) != outcome(U.shape, yaml) =>
          s"$yaml: kindlings=${outcome(KindlingsOptIns.shape, yaml)} upstream=${outcome(U.shape, yaml)}"
      }
      assertEquals(
        differences,
        List(
          "radius: 1.5: kindlings=Left upstream=Right(YmCircle(1.5))",
          "width: 1\nheight: 2: kindlings=Left upstream=Right(YmRectangle(1.0,2.0))",
          "YmCircle:\n  radius: 1.5: kindlings=Right(YmCircle(1.5)) upstream=Left"
        )
      )
    }
  }
}
