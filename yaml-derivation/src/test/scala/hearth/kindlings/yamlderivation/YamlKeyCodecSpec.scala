package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlEncoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

final case class KeyItem(port: Int)
final case class KeyUser(name: String, byName: Map[String, KeyItem], byId: Map[Int, KeyItem])
// maps whose values have a scala-yaml instance (scala-yaml's forMap would be found for the whole map)
final case class KeyCounts(byName: Map[String, Int], byId: Map[Int, Int])
final case class KeyBuiltIns(
    chars: Map[Char, Int],
    booleans: Map[Boolean, Int],
    bytes: Map[Byte, Int],
    shorts: Map[Short, Int],
    ints: Map[Int, Int],
    longs: Map[Long, Int],
    floats: Map[Float, Int],
    doubles: Map[Double, Int],
    bigInts: Map[BigInt, Int],
    bigDecimals: Map[BigDecimal, Int],
    uuids: Map[java.util.UUID, Int]
)
final case class KeyName(value: String) extends AnyVal
final case class KeyNumber(value: Int) extends AnyVal
final case class KeyWrappers(byName: Map[KeyName, KeyItem], byNumber: Map[KeyNumber, Int])
final case class KeyValidated(value: String) extends AnyVal
object KeyValidated {
  implicit val keyDecoder: YamlKeyDecoder[KeyValidated] =
    YamlKeyDecoder.forString.emap(s => if (s.startsWith("id-")) Right(KeyValidated(s)) else Left(s"Invalid id: $s"))
}
final case class KeyValidatedMap(byId: Map[KeyValidated, Int])
sealed trait KeyColor
object KeyColor {
  case object LightRed extends KeyColor
  case object DarkBlue extends KeyColor
}
final case class KeyEnums(colors: Map[KeyColor, Int], directions: Map[CardinalDirection, Int])
final case class KeyEnumerations(colors: Map[ScalaColor.Value, Int])
final case class KeyPoint(x: Int, y: Int)
final case class KeyComplex(byPoint: Map[KeyPoint, String])
final case class KeyFunctions(byFunction: Map[Int => Int, Int])

/** Map keys are read and written with key codecs (`YamlKeyDecoder` / `YamlKeyEncoder`: user-provided, built-in or
  * derived), not with scala-yaml's `YamlDecoder[K]` / `YamlEncoder[K]` as scala-yaml's `forMap` does (a documented
  * difference). Complex (non-scalar) keys without a key codec still use their `YamlDecoder` / `YamlEncoder`.
  */
final class YamlKeyCodecSpec extends MacroSuite {

  private def mappingOf(entries: (Node, Node)*): Node = MappingNode(entries.toMap)
  private def str(value: String): Node = ScalarNode(value)
  private def item(port: Int): Node = mappingOf(str("port") -> str(port.toString))
  private val nullNode: Node = YamlEncoder.forOption(YamlEncoder.forInt).asNode(None)

  private def failure[A](result: Either[ConstructError, A]): String = result match {
    case Left(error)  => error.msg.linesIterator.next()
    case Right(value) => s"decoded: $value"
  }

  group("user-provided instances") {

    test("a YamlDecoder / YamlEncoder for the key type in scope is not used for keys, only for fields and values") {
      implicit val upperCase: YamlDecoder[String] = YamlDecoder.forString.map(_.toUpperCase)
      implicit val prefixed: YamlEncoder[String] = YamlEncoder.forString.mapContra[String]("k-" + _)
      implicit val plusOne: YamlDecoder[Int] = YamlDecoder.forInt.map(_ + 1)
      implicit val timesTen: YamlEncoder[Int] = YamlEncoder.forInt.mapContra[Int](_ * 10)
      val decoder = KindlingsYamlDecoder.derived[KeyUser]
      val encoder = KindlingsYamlEncoder.derived[KeyUser]
      val node = mappingOf(
        str("name") -> str("n"),
        str("byName") -> mappingOf(str("a") -> item(1)),
        str("byId") -> mappingOf(str("1") -> item(2))
      )
      decoder.construct(node) ==> Right(KeyUser("N", Map("a" -> KeyItem(2)), Map(1 -> KeyItem(3))))
      encoder.asNode(KeyUser("n", Map("a" -> KeyItem(1)), Map(2 -> KeyItem(3)))) ==> mappingOf(
        str("name") -> str("k-n"),
        str("byName") -> mappingOf(str("a") -> mappingOf(str("port") -> str("10"))),
        str("byId") -> mappingOf(str("2") -> mappingOf(str("port") -> str("30")))
      )
      val counts =
        mappingOf(str("byName") -> mappingOf(str("a") -> str("1")), str("byId") -> mappingOf(str("2") -> str("3")))
      KindlingsYamlDecoder.derived[KeyCounts].construct(counts) ==> Right(KeyCounts(Map("a" -> 2), Map(2 -> 4)))
      KindlingsYamlEncoder.derived[KeyCounts].asNode(KeyCounts(Map("a" -> 1), Map(2 -> 3))) ==>
        mappingOf(str("byName") -> mappingOf(str("a") -> str("10")), str("byId") -> mappingOf(str("2") -> str("30")))
    }

    test("a YamlKeyDecoder / YamlKeyEncoder in scope wins, also for String keys") {
      implicit val upperCase: YamlKeyDecoder[String] = YamlKeyDecoder.forString.map(_.toUpperCase)
      implicit val prefixed: YamlKeyEncoder[String] = YamlKeyEncoder.forString.contramap[String]("k-" + _)
      implicit val plusOne: YamlKeyDecoder[Int] = YamlKeyDecoder.forInt.map(_ + 1)
      implicit val timesTen: YamlKeyEncoder[Int] = YamlKeyEncoder.forInt.contramap[Int](_ * 10)
      val decoder = KindlingsYamlDecoder.derived[KeyUser]
      val encoder = KindlingsYamlEncoder.derived[KeyUser]
      val node = mappingOf(
        str("name") -> str("n"),
        str("byName") -> mappingOf(str("a") -> item(1)),
        str("byId") -> mappingOf(str("1") -> item(2))
      )
      decoder.construct(node) ==> Right(KeyUser("n", Map("A" -> KeyItem(1)), Map(2 -> KeyItem(2))))
      encoder.asNode(KeyUser("n", Map("a" -> KeyItem(1)), Map(2 -> KeyItem(3)))) ==> mappingOf(
        str("name") -> str("n"),
        str("byName") -> mappingOf(str("k-a") -> item(1)),
        str("byId") -> mappingOf(str("20") -> item(3))
      )
      val counts =
        mappingOf(str("byName") -> mappingOf(str("a") -> str("1")), str("byId") -> mappingOf(str("2") -> str("3")))
      KindlingsYamlDecoder.derived[KeyCounts].construct(counts) ==> Right(KeyCounts(Map("A" -> 1), Map(3 -> 3)))
      KindlingsYamlEncoder.derived[KeyCounts].asNode(KeyCounts(Map("a" -> 1), Map(2 -> 3))) ==>
        mappingOf(str("byName") -> mappingOf(str("k-a") -> str("1")), str("byId") -> mappingOf(str("20") -> str("3")))
    }

    test("a failing user-provided key decoder fails the map, at the key") {
      implicit val nonEmpty: YamlKeyDecoder[String] =
        YamlKeyDecoder.forString.emap(s => if (s == "bad") Left("bad key") else Right(s))
      val decoder = KindlingsYamlDecoder.derived[KeyUser]
      failure(
        decoder.construct(
          mappingOf(
            str("name") -> str("n"),
            str("byName") -> mappingOf(str("bad") -> item(1)),
            str("byId") -> mappingOf()
          )
        )
      ) ==> "bad key"
      // a null or non-scalar key is rejected before the key decoder is called
      failure(
        decoder.construct(
          mappingOf(
            str("name") -> str("n"),
            str("byName") -> mappingOf(nullNode -> item(1)),
            str("byId") -> mappingOf()
          )
        )
      ) ==> "Could't construct java.lang.String from null (tag:yaml.org,2002:null)"
    }
  }

  group("built-in key codecs") {

    test("String, Char, Boolean, numbers, BigInt, BigDecimal, UUID round-trip") {
      val uuid = java.util.UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
      val value = KeyBuiltIns(
        chars = Map('a' -> 1),
        booleans = Map(true -> 2, false -> 3),
        bytes = Map(4.toByte -> 4),
        shorts = Map(5.toShort -> 5),
        ints = Map(-6 -> 6),
        longs = Map(7000000000L -> 7),
        floats = Map(1.5f -> 8),
        doubles = Map(2.25 -> 9),
        bigInts = Map(BigInt("123456789012345678901234567890") -> 10),
        bigDecimals = Map(BigDecimal("1.5") -> 11),
        uuids = Map(uuid -> 12)
      )
      val node = KindlingsYamlEncoder.encode(value)
      node ==> mappingOf(
        str("chars") -> mappingOf(str("a") -> str("1")),
        str("booleans") -> mappingOf(str("true") -> str("2"), str("false") -> str("3")),
        str("bytes") -> mappingOf(str("4") -> str("4")),
        str("shorts") -> mappingOf(str("5") -> str("5")),
        str("ints") -> mappingOf(str("-6") -> str("6")),
        str("longs") -> mappingOf(str("7000000000") -> str("7")),
        str("floats") -> mappingOf(str("1.5") -> str("8")),
        str("doubles") -> mappingOf(str("2.25") -> str("9")),
        str("bigInts") -> mappingOf(str("123456789012345678901234567890") -> str("10")),
        str("bigDecimals") -> mappingOf(str("1.5") -> str("11")),
        str("uuids") -> mappingOf(str(uuid.toString) -> str("12"))
      )
      KindlingsYamlDecoder.decode[KeyBuiltIns](node) ==> Right(value)
    }

    test("numbers are read as scala-yaml reads them (hexadecimal, octal, underscores) and fail as it does") {
      val decoder = KindlingsYamlDecoder.derived[KeyUser]
      def byId(keys: (Node, Node)*) =
        decoder.construct(
          mappingOf(str("name") -> str("n"), str("byName") -> mappingOf(), str("byId") -> mappingOf(keys*))
        )
      byId(str("0x10") -> item(1), str("0o17") -> item(2), str("1_000") -> item(3)).map(_.byId) ==>
        Right(Map(16 -> KeyItem(1), 15 -> KeyItem(2), 1000 -> KeyItem(3)))
      failure(byId(str("a") -> item(1))) ==> "For input string: \"a\""
      failure(byId(nullNode -> item(1))) ==> "Could't construct int from null (tag:yaml.org,2002:null)"
    }

    test("Char and UUID keys that do not parse") {
      failure(
        KindlingsYamlDecoder.decode[Map[Char, Int]](mappingOf(str("ab") -> str("1")))
      ) ==> "Cannot parse ab as Char"
      failure(
        KindlingsYamlDecoder.decode[Map[java.util.UUID, Int]](mappingOf(str("x") -> str("1")))
      ) ==> "Invalid UUID string: x"
    }
  }

  group("derived key codecs") {

    test("value classes over a type with a key codec") {
      val value = KeyWrappers(Map(KeyName("a") -> KeyItem(1)), Map(KeyNumber(2) -> 3))
      val node = mappingOf(
        str("byName") -> mappingOf(str("a") -> item(1)),
        str("byNumber") -> mappingOf(str("2") -> str("3"))
      )
      KindlingsYamlEncoder.encode(value) ==> node
      KindlingsYamlDecoder.decode[KeyWrappers](node) ==> Right(value)
      failure(
        KindlingsYamlDecoder.decode[KeyWrappers](
          mappingOf(str("byName") -> mappingOf(), str("byNumber") -> mappingOf(str("x") -> str("3")))
        )
      ) ==> "For input string: \"x\""
    }

    test("a key decoder that validates: a rejected key is a decoding failure") {
      val node = mappingOf(str("byId") -> mappingOf(str("id-1") -> str("1")))
      KindlingsYamlDecoder.decode[KeyValidatedMap](node) ==> Right(KeyValidatedMap(Map(KeyValidated("id-1") -> 1)))
      failure(
        KindlingsYamlDecoder.decode[KeyValidatedMap](mappingOf(str("byId") -> mappingOf(str("x") -> str("1"))))
      ) ==> "Invalid id: x"
      // with the position of the key
      val error = org.virtuslab.yaml
        .parseYaml("byId:\n  x: 1")
        .flatMap(KindlingsYamlDecoder.derived[KeyValidatedMap].construct(_))
      assert(error.left.exists(_.msg.contains("at 1:2")), error.toString)
    }

    test("enums of case objects, by their names") {
      val value = KeyEnums(Map(KeyColor.LightRed -> 1, KeyColor.DarkBlue -> 2), Map(North -> 3))
      val node = mappingOf(
        str("colors") -> mappingOf(str("LightRed") -> str("1"), str("DarkBlue") -> str("2")),
        str("directions") -> mappingOf(str("North") -> str("3"))
      )
      KindlingsYamlEncoder.encode(value) ==> node
      KindlingsYamlDecoder.decode[KeyEnums](node) ==> Right(value)
      failure(
        KindlingsYamlDecoder.decode[KeyEnums](
          mappingOf(str("colors") -> mappingOf(str("Green") -> str("1")), str("directions") -> mappingOf())
        )
      ) ==> "Unknown key Green. Expected one of: DarkBlue, LightRed"
    }

    test("enums of case objects, by their configured names") {
      implicit val config: YamlConfig = YamlConfig().withKebabCaseConstructorNames
      val value = KeyEnums(Map(KeyColor.LightRed -> 1), Map(South -> 2))
      val node = mappingOf(
        str("colors") -> mappingOf(str("light-red") -> str("1")),
        str("directions") -> mappingOf(str("south") -> str("2"))
      )
      KindlingsYamlEncoder.encode(value) ==> node
      KindlingsYamlDecoder.decode[KeyEnums](node) ==> Right(value)
    }

    test("scala.Enumeration values, by their names") {
      val value = KeyEnumerations(Map(ScalaColor.Red -> 1, ScalaColor.Blue -> 2))
      val node = mappingOf(str("colors") -> mappingOf(str("Red") -> str("1"), str("Blue") -> str("2")))
      KindlingsYamlEncoder.encode(value) ==> node
      KindlingsYamlDecoder.decode[KeyEnumerations](node) ==> Right(value)
    }
  }

  group("complex keys") {

    test("a case class key without a key codec is written and read as a node") {
      val value = KeyComplex(Map(KeyPoint(1, 2) -> "a"))
      val node =
        mappingOf(str("byPoint") -> mappingOf(mappingOf(str("x") -> str("1"), str("y") -> str("2")) -> str("a")))
      KindlingsYamlEncoder.encode(value) ==> node
      KindlingsYamlDecoder.decode[KeyComplex](node) ==> Right(value)
    }

    test("a user-provided YamlDecoder / YamlEncoder for a key type without a key codec is used") {
      implicit val pointDecoder: YamlDecoder[KeyPoint] = YamlDecoder.forString.map { s =>
        val Array(x, y) = s.split(',')
        KeyPoint(x.toInt, y.toInt)
      }
      implicit val pointEncoder: YamlEncoder[KeyPoint] =
        YamlEncoder.forString.mapContra[KeyPoint](p => s"${p.x},${p.y}")
      val value = KeyComplex(Map(KeyPoint(1, 2) -> "a"))
      val node = mappingOf(str("byPoint") -> mappingOf(str("1,2") -> str("a")))
      KindlingsYamlEncoder.derived[KeyComplex].asNode(value) ==> node
      KindlingsYamlDecoder.derived[KeyComplex].construct(node) ==> Right(value)
    }

    test("a key type with neither a key codec nor a YamlDecoder / YamlEncoder does not compile") {
      compileErrors(
        """
        import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KeyFunctions}
        KindlingsYamlDecoder.derived[KeyFunctions]
        """
      ).check("for the map's keys: none in scope and none built in or derivable")
      compileErrors(
        """
        import hearth.kindlings.yamlderivation.{KindlingsYamlEncoder, KeyFunctions}
        KindlingsYamlEncoder.derived[KeyFunctions]
        """
      ).check("for the map's keys: none in scope and none built in or derivable")
    }
  }
}
