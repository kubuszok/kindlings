package hearth.kindlings.oracle.jsoniter

import java.math.RoundingMode
import java.time.*
import java.util.UUID
import scala.collection.immutable.BitSet

/** Values to encode and JSON inputs to decode (valid and invalid ones) for one fixture type. */
final case class JoSamples[A](values: Seq[A], inputs: Seq[String])

object JoSamples {

  private def variants(valid: String)(replacements: (String, String)*): Seq[String] =
    valid +: replacements.map { case (from, to) =>
      assert(valid.contains(from), s"'$from' is not in $valid")
      valid.replace(from, to)
    }

  private val commonInvalid = Seq("null", "[]", "{}", "1", "\"x\"", "{", "{\"a\":1,}", "")

  val person: JoSamples[JOPerson] = JoSamples(
    Seq(JOPerson("Ann", 30, Some("a@b.c"), List("x")), JOPerson("Bob", 0, None, Nil)),
    variants("""{"name":"Ann","age":30,"email":"a@b.c","tags":["x"]}""")(
      """"name":"Ann",""" -> "",
      """"name":"Ann"""" -> """"name":null""",
      """"age":30""" -> """"age":null""",
      """"age":30""" -> """"age":"30"""",
      """"age":30""" -> """"age":30.5""",
      """"age":30""" -> """"age":3000000000""",
      """"age":30""" -> """"age":-0""",
      """"age":30""" -> """"age":1e2""",
      """"email":"a@b.c"""" -> """"email":null""",
      """"email":"a@b.c"""" -> """"email":1""",
      """"tags":["x"]""" -> """"tags":null""",
      """"tags":["x"]""" -> """"tags":"x"""",
      """"tags":["x"]""" -> """"tags":[null]""",
      ""","email":"a@b.c","tags":["x"]""" -> "",
      """"tags":["x"]""" -> """"tags":["x"],"extra":{"a":[1,{"b":null}]}""",
      """"tags":["x"]""" -> """"tags":["x"],"name":"Bob"""",
      """"tags":["x"]""" -> """"tags":["x"],"age":31""",
      """"tags":["x"]""" -> """"tags":["x"],"email":null""",
      """"name":"Ann"""" -> """"name":"Ann\n""""
    ) ++ commonInvalid
  )

  val defaults: JoSamples[JODefaults] = JoSamples(
    Seq(JODefaults(), JODefaults(2, "e", Nil, None, Map.empty), JODefaults(l = List(5), o = Some(4))),
    Seq(
      """{}""",
      """{"i":2}""",
      """{"i":null}""",
      """{"s":null}""",
      """{"l":null}""",
      """{"o":null}""",
      """{"m":null}""",
      """{"l":[],"m":{}}""",
      """{"l":[7],"m":{"z":2}}""",
      """{"o":5}""",
      """{"i":1,"i":2}""",
      """{"i":"x"}""",
      """{"x":1}""",
      """{"i":1,"s":"d","l":null,"o":null,"m":null}""",
      """{"i":1,"s":null,"l":[1],"o":1,"m":{}}""",
      """{"i":1,"s":"x","l":[],"o":1,"m":{}}""",
      """{"i":null,"s":"x","l":[],"o":1,"m":{}}"""
    ) ++ commonInvalid
  )

  val emptyable: JoSamples[JOEmptyable] = JoSamples(
    Seq(
      JOEmptyable(Nil, Vector.empty, Map.empty, Set.empty, None),
      JOEmptyable(List(1), Vector("a"), Map("k" -> 1), Set(1, 2, 3), Some(1))
    ),
    Seq(
      """{}""",
      """{"l":null,"v":null,"m":null,"s":null,"o":null}""",
      """{"l":[],"v":[],"m":{},"s":[]}""",
      """{"l":[1,2,3],"s":[1,2,3],"m":{"a":1,"b":2,"c":3}}""",
      """{"s":[1,1,1]}""",
      """{"s":[1,2]}""",
      """{"m":{"a":1,"a":2}}""",
      """{"m":{"a":1,"b":2}}""",
      """{"m":[["a",1]]}""",
      """{"l":[1],"l":[2]}""",
      """{"o":1,"o":null}"""
    )
  )

  val nestedOption: JoSamples[JONestedOption] = JoSamples(
    Seq(
      JONestedOption(None, None),
      JONestedOption(Some(None), Some("x")),
      JONestedOption(Some(Some(1)), None)
    ),
    Seq(
      """{}""",
      """{"oo":null}""",
      """{"oo":1}""",
      """{"oo":{"type":"Some","value":1}}""",
      """{"oo":{"type":"None"}}""",
      """{"oo":"None"}""",
      """{"oo":{"Some":{"value":1}}}""",
      """{"oo":{"type":"Some","value":null}}""",
      """{"oo":{"type":"Some","value":{"type":"Some","value":1}}}""",
      """{"o":null}"""
    )
  )

  val primitives: JoSamples[JOPrimitives] = JoSamples(
    Seq(
      JOPrimitives(1, 2, 3, 4L, 1.5f, 2.5, 'c', z = true, "s"),
      JOPrimitives(
        Byte.MaxValue,
        Short.MinValue,
        Int.MinValue,
        Long.MaxValue,
        Float.MaxValue,
        Double.MinPositiveValue,
        '\u0001',
        z = false,
        "\"quoted\"\n "
      ),
      JOPrimitives(0, 0, 0, 0L, Float.NaN, Double.PositiveInfinity, 'x', z = true, "")
    ),
    variants("""{"b":1,"s":2,"i":3,"l":4,"f":1.5,"d":2.5,"c":"c","z":true,"str":"s"}""")(
      """"b":1""" -> """"b":128""",
      """"b":1""" -> """"b":"1"""",
      """"s":2""" -> """"s":32768""",
      """"l":4""" -> """"l":9223372036854775808""",
      """"l":4""" -> """"l":4.0""",
      """"f":1.5""" -> """"f":1e39""",
      """"f":1.5""" -> """"f":"1.5"""",
      """"d":2.5""" -> """"d":1e400""",
      """"d":2.5""" -> """"d":NaN""",
      """"d":2.5""" -> """"d":"NaN"""",
      """"c":"c"""" -> """"c":"ab"""",
      """"c":"c"""" -> """"c":""""",
      """"c":"c"""" -> """"c":99""",
      """"z":true""" -> """"z":"true"""",
      """"z":true""" -> """"z":1""",
      """"str":"s"""" -> """"str":1""",
      """"str":"s"""" -> "\"str\":\"\\ud800\""
    )
  )

  val boxed: JoSamples[JOBoxed] = JoSamples(
    Seq(JOBoxed(1, 2L, 3.0, true)),
    Seq(
      """{"i":1,"l":2,"d":3.0,"z":true}""",
      """{"i":null,"l":null,"d":null,"z":null}""",
      """{}"""
    )
  )

  val big: JoSamples[JOBig] = JoSamples(
    Seq(JOBig(BigDecimal("123.456"), BigInt("12345678901234567890")), JOBig(BigDecimal("1E+10"), BigInt(-1))),
    variants("""{"bd":1.5,"bi":1}""")(
      """"bd":1.5""" -> """"bd":1.23456789""",
      """"bd":1.5""" -> """"bd":1.234""",
      """"bd":1.5""" -> """"bd":12345""",
      """"bd":1.5""" -> """"bd":1e10""",
      """"bd":1.5""" -> """"bd":1E-7000""",
      """"bd":1.5""" -> """"bd":"1.5"""",
      """"bd":1.5""" -> (""""bd":""" + ("9" * 400)),
      """"bi":1""" -> """"bi":12345""",
      """"bi":1""" -> """"bi":1.5""",
      """"bi":1""" -> """"bi":1e3""",
      """"bi":1""" -> (""""bi":""" + ("9" * 400))
    )
  )

  val wrapped: JoSamples[JOWrapped] = JoSamples(
    Seq(JOWrapped(JOId(1), List(JOId(2)), Some(JOId(3))), JOWrapped(JOId(0), Nil, None)),
    Seq(
      """{"id":1}""",
      """{"id":"1"}""",
      """{"id":1,"ids":["2"],"maybe":"3"}""",
      """{"id":{"value":1}}""",
      """{}"""
    )
  )

  private val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")

  val maps: JoSamples[JOMaps] = JoSamples(
    Seq(
      JOMaps(Map(1 -> "a", 2 -> "b"), Map(3L -> true), Map(uuid -> 1), Map(JOId(7) -> "x"), Map("k" -> List(1))),
      JOMaps(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
    ),
    Seq(
      """{"byInt":{"x":"a"}}""",
      """{"byInt":{"1.5":"a"}}""",
      """{"byInt":{"1":"a","1":"b"}}""",
      """{"byInt":{"1":"a","2":"b","3":"c"}}""",
      """{"byInt":[[1,"a"]]}""",
      """{"byInt":[[1,"a"],[2,"b"],[3,"c"]]}""",
      """{"byLong":{"3":"true"}}""",
      """{"byUuid":{"not-a-uuid":1}}""",
      """{"byId":{"7":"x"}}""",
      """{"byId":[[7,"x"]]}""",
      """{"byString":{"k":null}}"""
    )
  )

  val collections: JoSamples[JOCollections] = JoSamples(
    Seq(
      JOCollections(Seq(1, 2), Vector("a"), Set(1), List(Some(1), None), List(List(1), Nil), BitSet(1, 5, 100)),
      JOCollections(Nil, Vector.empty, Set.empty, Nil, Nil, BitSet.empty)
    ),
    Seq(
      """{"bits":[1,2000]}""",
      """{"bits":[9,10]}""",
      """{"bits":[-1]}""",
      """{"bits":[1,1]}""",
      """{"bits":["1"]}""",
      """{"list":[null,1]}""",
      """{"nested":[null]}""",
      """{"set":[1,1]}"""
    )
  )

  val nonEmpty: JoSamples[JONonEmpty] = JoSamples(
    Seq(JONonEmpty(::(1, List(2)), Nil), JONonEmpty(::(1, Nil), List(1), ::(2, List(3)))),
    Seq(
      """{"head":[1]}""",
      """{"head":[]}""",
      """{"head":null}""",
      """{"rest":[]}""",
      """{"head":[1],"withDefault":[]}""",
      """{"head":[1],"withDefault":null}""",
      """{"head":[1],"withDefault":"x"}""",
      """{"head":[1],"withDefault":[5,6]}""",
      """{"head":[1,"x"]}""",
      """{"head":[1}""",
      """{}"""
    )
  )

  val tuples: JoSamples[JOTuples] = JoSamples(
    Seq(JOTuples((1, "a"), (true, 1.5, List(1)))),
    Seq(
      """{"pair":[1],"triple":[true,1.5,[]]}""",
      """{"pair":[1,"a","x"],"triple":[true,1.5,[]]}""",
      """{"pair":{"_1":1,"_2":"a"},"triple":[true,1.5,[]]}""",
      """{"pair":null,"triple":[true,1.5,[]]}""",
      """{"triple":[true,1.5,[]]}"""
    )
  )

  val time: JoSamples[JOTime] = JoSamples(
    Seq(
      JOTime(
        Instant.parse("2020-01-01T00:00:00Z"),
        LocalDate.of(2020, 1, 2),
        LocalTime.of(3, 4, 5),
        LocalDateTime.of(2020, 1, 2, 3, 4, 5, 6000),
        OffsetDateTime.of(2020, 1, 2, 3, 4, 5, 0, ZoneOffset.ofHours(2)),
        ZonedDateTime.of(2020, 1, 2, 3, 4, 5, 0, ZoneId.of("Europe/Warsaw")),
        Duration.ofSeconds(90),
        Period.of(1, 2, 3),
        uuid
      )
    ),
    variants(
      """{"instant":"2020-01-01T00:00:00Z","date":"2020-01-02","time":"03:04:05","dateTime":"2020-01-02T03:04:05","offset":"2020-01-02T03:04:05+02:00","zoned":"2020-01-02T03:04:05+01:00[Europe/Warsaw]","duration":"PT1M30S","period":"P1Y2M3D","uuid":"123e4567-e89b-12d3-a456-426614174000"}"""
    )(
      """"2020-01-02"""" -> """"2020-13-01"""",
      """"2020-01-01T00:00:00Z"""" -> """"2020-01-01T00:00:00"""",
      """"PT1M30S"""" -> """"90"""",
      """"2020-01-02"""" -> """20200102""",
      """"123e4567-e89b-12d3-a456-426614174000"""" -> """"123e4567"""",
      """"03:04:05"""" -> """"3:04""""
    )
  )

  val either: JoSamples[JOEither] = JoSamples(
    Seq(JOEither(Left("x")), JOEither(Right(1))),
    Seq(
      """{"e":{"type":"Middle"}}""",
      """{"e":{"Left":{"value":"x"}}}""",
      """{"e":"x"}""",
      """{"e":{"type":"Right","value":"x"}}"""
    )
  )

  private val shapeInputs = Seq(
    """{"type":"JOCircle","radius":1}""",
    """{"radius":1,"type":"JOCircle"}""",
    """{"type":"JOCircle"}""",
    """{"type":"JOCircle","radius":1,"type":"JORect"}""",
    """{"type":"JOCircle","radius":1,"extra":1}""",
    """{"type":"JOCircle","radius":1,"radius":2}""",
    """{"type":"Unknown"}""",
    """{"type":1}""",
    """{"type":null}""",
    """{"type":"JOEmpty"}""",
    """{"type":"JOEmpty","extra":1}""",
    """{"type":"sq","side":1}""",
    """{"type":"JOSquare","side":1}""",
    """{"type":"jo_circle","radius":1}""",
    """{"type":"JO_CIRCLE","radius":1}""",
    """{"kind":"JOCircle","radius":1}""",
    """{"kind":"JOEmpty"}""",
    """"JOEmpty"""",
    """"jo_empty"""",
    """"JOCircle"""",
    """{"JOCircle":{"radius":1}}""",
    """{"JOCircle":{"radius":1},"JORect":{}}""",
    """{"JOEmpty":{}}""",
    """{"JOEmpty":{"x":1}}""",
    """{"sq":{"side":1}}""",
    """{"Unknown":{}}""",
    """{}"""
  ) ++ commonInvalid

  val shape: JoSamples[JOShape] = JoSamples(Seq(JOCircle(1.0), JORect(1, 2), JOEmpty, JOSquare(3)), shapeInputs)

  val circle: JoSamples[JOCircle] = JoSamples(
    Seq(JOCircle(2)),
    Seq(
      """{"radius":2}""",
      """{"type":"JOCircle","radius":2}""",
      """{"radius":2,"type":"JOCircle"}""",
      """{"type":"JORect","radius":2}"""
    )
  )

  val drawing: JoSamples[JODrawing] = JoSamples(
    Seq(JODrawing(List(JOCircle(1), JOEmpty), JORect(1, 2))),
    Seq(
      """{"shapes":[],"main":"JOEmpty"}""",
      """{"shapes":[],"main":{"type":"JOEmpty"}}""",
      """{"main":{"type":"JOCircle","radius":1}}""",
      """{"shapes":[{"kind":"JOEmpty"}],"main":{"kind":"JOCircle","radius":1}}""",
      """{"shapes":[null],"main":{"type":"JOEmpty"}}""",
      """{"shapes":[]}"""
    )
  )

  val shapeHolder: JoSamples[JOShapeHolder] = JoSamples(
    Seq(JOShapeHolder(), JOShapeHolder(JOCircle(1), JOBlue, Left("x"))),
    Seq(
      """{}""",
      """{"shape":null,"colour":null,"either":null}""",
      """{"shape":"x"}""",
      """{"shape":1}""",
      """{"colour":1}""",
      """{"colour":{}}""",
      """{"either":1}""",
      """{"either":"x"}"""
    )
  )

  val colour: JoSamples[JOColour] = JoSamples(
    Seq(JORed, JOBlue),
    Seq(
      """"JORed"""",
      """"blue"""",
      """"JOBlue"""",
      """"jo_red"""",
      """"Purple"""",
      """{"type":"JORed"}""",
      """{"type":"blue"}""",
      """{"JORed":{}}""",
      """{"blue":{}}"""
    ) ++ commonInvalid
  )

  val enums: JoSamples[JOEnums] = JoSamples(
    Seq(JOEnums(JOLevel.High, RoundingMode.HALF_UP, JOGreen)),
    variants("""{"level":"High","rounding":"HALF_UP","colour":{"type":"JOGreen"}}""")(
      """"High"""" -> """1""",
      """"High"""" -> """"1"""",
      """"High"""" -> """"Medium"""",
      """"High"""" -> """7""",
      """"HALF_UP"""" -> """"half_up"""",
      """"HALF_UP"""" -> """"HALF_SIDEWAYS"""",
      """"HALF_UP"""" -> """4""",
      """{"type":"JOGreen"}""" -> """"JOGreen"""",
      """{"type":"JOGreen"}""" -> """{"JOGreen":{}}"""
    )
  )

  val annotated: JoSamples[JOAnnotated] = JoSamples(
    Seq(JOAnnotated("n", 5, 7, Some(8L), List(BigDecimal("1.5"))), JOAnnotated("m", 0, 1, None, Nil)),
    Seq(
      """{"full_name":"n","count":"7"}""",
      """{"full_name":"n","count":7}""",
      """{"name":"n","count":"7"}""",
      """{"full_name":"n","count":"7","cache":3}""",
      """{"full_name":"n","count":"7","maybe":"8","values":["1.5"]}""",
      """{"full_name":"n","count":"7","maybe":8}""",
      """{"full_name":"n","count":"7","values":[1.5]}""",
      """{"full_name":"n","count":"x"}"""
    )
  )

  val camel: JoSamples[JOCamel] = JoSamples(
    Seq(JOCamel("a", "b", "c", 1)),
    Seq(
      """{"firstName":"a","lastName":"b","httpServer":"c","field1":1}""",
      """{"first_name":"a","last_name":"b","http_server":"c","field_1":1}""",
      """{"first_name":"a","last_name":"b","http_server":"c","field1":1}""",
      """{"first-name":"a","last-name":"b","http-server":"c","field-1":1}""",
      """{"FirstName":"a","LastName":"b","HttpServer":"c","Field1":1}""",
      """{"given":"a","lastName":"b","httpServer":"c","field1":1}"""
    )
  )

  val user: JoSamples[JOUser] = JoSamples(
    Seq(JOUser(JOName("n"), Some(JOName("m"))), JOUser(JOName("o"), None)),
    Seq(
      """{"name":"n","alias":"m"}""",
      """{"name":{"value":"n"},"alias":{"value":"m"}}""",
      """{"name":null}""",
      """{}"""
    )
  )

  val nameMap: JoSamples[JONameMap] = JoSamples(
    Seq(JONameMap(Map(JOName("k") -> 1))),
    Seq("""{"byName":{"k":1}}""", """{"byName":{"k":1,"k":2}}""")
  )

  val scalaTransient: JoSamples[JOScalaTransient] = JoSamples(
    Seq(JOScalaTransient("n", 9), JOScalaTransient("m")),
    Seq("""{"name":"n","cache":3}""", """{"name":"n"}""")
  )

  val stringy: JoSamples[JOStringy] = JoSamples(
    Seq(JOStringy(1, List(2L))),
    Seq("""{"i":1,"l":[2]}""", """{"i":"1","l":["2"]}""")
  )

  val listInt: JoSamples[List[Int]] = JoSamples(
    Seq(List(1, 2), Nil),
    Seq("[1,2]", """["1","2"]""", """[1,"2"]""", "null", "{}", "[null]")
  )

  val setInt: JoSamples[Set[Int]] = JoSamples(
    Seq(Set(1, 2, 3)),
    Seq("[1,2]", "[1,1]", (1 to 1024).mkString("[", ",", "]"), (1 to 1025).mkString("[", ",", "]"), "null")
  )

  val mapIntString: JoSamples[Map[Int, String]] = JoSamples(
    Seq(Map(1 -> "a"), Map.empty),
    Seq(
      """{"1":"a"}""",
      """{"1":"a","1":"b"}""",
      (1 to 1024).map(i => s""""$i":"v"""").mkString("{", ",", "}"),
      (1 to 1025).map(i => s""""$i":"v"""").mkString("{", ",", "}"),
      """[[1,"a"]]""",
      """{"x":"a"}""",
      """{"1":1}""",
      "null"
    )
  )

  val optionInt: JoSamples[Option[Int]] = JoSamples(Seq(Some(1), None), Seq("1", "\"1\"", "null", "[]"))

  val tupleTop: JoSamples[(Int, Boolean)] = JoSamples(
    Seq((1, true)),
    Seq("[1,true]", """["1","true"]""", "[1]", "null")
  )

  val literal: JoSamples[JOLiteral] = JoSamples(
    Seq(JOLiteral("VVV", 2, true)),
    Seq(
      """{"s":"VVV","i":2,"b":true}""",
      """{"s":"VV","i":2,"b":true}""",
      """{"s":"VVV","i":3,"b":true}""",
      """{"s":"VVV","i":2,"b":false}""",
      """{"s":"VVV","i":2}"""
    )
  )

  val tree: JoSamples[JOTree] = JoSamples(
    Seq(JOTree(1, List(JOTree(2, Nil)))),
    Seq(
      """{"value":1}""",
      """{"value":1,"children":[{"value":2,"children":null}]}""",
      """{"value":1,"children":[null]}"""
    )
  )

  val expr: JoSamples[JOExpr] = JoSamples(
    Seq(JOAdd(JOLit(1), JOAdd(JOLit(2), JOLit(3)))),
    Seq(
      """{"type":"JOAdd","left":{"type":"JOLit","value":1}}""",
      """{"type":"JOAdd","left":{"type":"JOLit","value":1},"right":null}"""
    )
  )
}
