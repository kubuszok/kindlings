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

/** Kindlings' YAML codecs (default `YamlConfig`) vs scala-yaml's own derivation (`YamlCodec.derived`, Scala 3 only),
  * compared on the YAML nodes encoded and on the values decoded (valid and invalid inputs).
  *
  * Each case lists the *exact* differences it expects; each one must be documented (see
  * `docs/user-guide/feature-parity.md`, yaml-derivation). A difference that appears or disappears fails the test.
  */
final class YamlOracleSpec extends MacroSuite {

  private object KindlingsDefault {
    given YamlConfig = YamlConfig.default
    val person: YamlCodec[YmPerson] = YamlCodec.make(KindlingsYamlDecoder.derived[YmPerson], KindlingsYamlEncoder.derived[YmPerson])
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
  }

  private object KindlingsWithDefaults {
    given YamlConfig = YamlConfig.default.withUseDefaults
    val defaults: YamlCodec[YmDefaults] =
      YamlCodec.make(KindlingsYamlDecoder.derived[YmDefaults], KindlingsYamlEncoder.derived[YmDefaults])
  }

  private object Upstream {
    given person: YamlCodec[YmPerson] = YamlCodec.derived
    val defaults: YamlCodec[YmDefaults] = YamlCodec.derived
    val collections: YamlCodec[YmCollections] = YamlCodec.derived
    val nested: YamlCodec[YmNested] = YamlCodec.derived
    val primitives: YamlCodec[YmPrimitives] = YamlCodec.derived
    val shape: YamlCodec[YmShape] = YamlCodec.derived
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
    assertEquals(differences, expectedDifferences.toList, differences.mkString("Kindlings vs scala-yaml encoding:\n", "\n", ""))
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
      if (k == u) Nil else List(s"${yaml.replace("\n", "\\n")}: kindlings=$k upstream=$u")
    }
    assertEquals(differences, expectedDifferences.toList, differences.mkString("Kindlings vs scala-yaml decoding:\n", "\n", ""))
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
      // Documented difference (feature-parity.md, yaml-derivation "Default value differences"): scala-yaml always uses
      // the constructor defaults of absent fields; Kindlings only with `withUseDefaults` (compared below: no difference).
      compareDecoded(K.defaults, U.defaults)("host: h\nport: 1\ntags: [a]", "host: h", "{}")(
        "host: h: kindlings=Left(Key port doesn't exist in parsed document) upstream=Right(YmDefaults(h,8080,List()))",
        "{}: kindlings=Left(Key host doesn't exist in parsed document) upstream=Right(YmDefaults(localhost,8080,List()))"
      )
      compareDecoded(KindlingsWithDefaults.defaults, U.defaults)("host: h\nport: 1\ntags: [a]", "host: h", "{}")()
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

    test("sealed traits") {
      // Documented difference (feature-parity.md, yaml-derivation "Sealed trait handling"): scala-yaml writes a subtype
      // without any type information and decodes by trying every subtype; Kindlings wraps the subtype in a single-key
      // mapping (or adds a discriminator with `withDiscriminator`).
      val str = "CoreSchemaTag(tag:yaml.org,2002:str)"
      val float = "CoreSchemaTag(tag:yaml.org,2002:float)"
      val map = "CoreSchemaTag(tag:yaml.org,2002:map)"
      compareEncoded(K.shape, U.shape)(YmCircle(1.5), YmRectangle(1, 2))(
        s"YmCircle(1.5): $$._2[YmCircle ($str)]: only in kindlings: ($map,TreeMap(radius ($str) -> 1.5 ($float)))",
        s"YmCircle(1.5): $$._2[radius ($str)]: only in upstream: 1.5 ($float)",
        s"YmRectangle(1.0,2.0): $$._2[YmRectangle ($str)]: only in kindlings: " +
          s"($map,TreeMap(height ($str) -> 2.0 ($float), width ($str) -> 1.0 ($float)))",
        s"YmRectangle(1.0,2.0): $$._2[height ($str)]: only in upstream: 2.0 ($float)",
        s"YmRectangle(1.0,2.0): $$._2[width ($str)]: only in upstream: 1.0 ($float)"
      )
      // (failures compared as failures only: the messages of these two different strategies are unrelated)
      def outcome(decoder: YamlDecoder[YmShape], yaml: String): String =
        if (decode(decoder, yaml).startsWith("Left")) "Left" else decode(decoder, yaml)
      val differences = List("radius: 1.5", "width: 1\nheight: 2", "YmCircle:\n  radius: 1.5", "side: 1").collect {
        case yaml if outcome(K.shape, yaml) != outcome(U.shape, yaml) =>
          s"$yaml: kindlings=${outcome(K.shape, yaml)} upstream=${outcome(U.shape, yaml)}"
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
