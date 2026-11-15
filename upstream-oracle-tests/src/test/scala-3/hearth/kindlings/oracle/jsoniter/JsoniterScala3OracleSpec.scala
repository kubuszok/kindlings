package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{named as uNamed, CodecMakerConfig, JsonCodecMaker}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.annotations.named as kNamed
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}

enum JO3Colour {
  case Red, Green
  @kNamed("blue") @uNamed("blue") case Blue
}

enum JO3Shape {
  case Circle(radius: Double)
  case Empty
}

final case class JO3Holder(
    colour: JO3Colour,
    shape: JO3Shape,
    nick: String | Null,
    pair: (a: Int, b: String),
    generic: Int *: String *: EmptyTuple,
    optional: (x: Option[Int], y: List[Int])
)

/** Scala 3-only shapes of `JsoniterOracleSpec`: Scala 3 enums, nullable `T | Null` fields (`transientNull`), named
  * tuples and generic tuples.
  */
final class JsoniterScala3OracleSpec extends MacroSuite {

  private object RuntimeConfigs {
    @volatile var defaults: JsoniterConfig = JsoniterConfig.jsoniterScalaDefaults
    @volatile var noDiscriminator: JsoniterConfig =
      JsoniterConfig.jsoniterScalaDefaults.withDiscriminatorFieldName(None)
    @volatile var transientNull: JsoniterConfig = JsoniterConfig.jsoniterScalaDefaults.withTransientNull
  }

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig ?=> JsonValueCodec[A]): JsonValueCodec[A] =
    derive(using config)

  private val defaults = JoCodecs[JO3Holder](
    KindlingsJsonValueCodec.derived[JO3Holder](using JsoniterConfig.jsoniterScalaDefaults),
    runtime(RuntimeConfigs.defaults)(KindlingsJsonValueCodec.derived[JO3Holder]),
    JsonCodecMaker.make[JO3Holder]
  )

  private val noDiscriminator = JoCodecs[JO3Holder](
    KindlingsJsonValueCodec.derived[JO3Holder](using
      JsoniterConfig.jsoniterScalaDefaults.withDiscriminatorFieldName(None)
    ),
    runtime(RuntimeConfigs.noDiscriminator)(KindlingsJsonValueCodec.derived[JO3Holder]),
    JsonCodecMaker.make[JO3Holder](CodecMakerConfig.withDiscriminatorFieldName(None))
  )

  private val transientNull = JoCodecs[JO3Holder](
    KindlingsJsonValueCodec.derived[JO3Holder](using JsoniterConfig.jsoniterScalaDefaults.withTransientNull),
    runtime(RuntimeConfigs.transientNull)(KindlingsJsonValueCodec.derived[JO3Holder]),
    JsonCodecMaker.make[JO3Holder](CodecMakerConfig.withTransientNull(true))
  )

  private val values = Seq(
    JO3Holder(JO3Colour.Red, JO3Shape.Circle(1), "n", (1, "x"), 2 *: "y" *: EmptyTuple, (Some(1), List(2))),
    JO3Holder(JO3Colour.Blue, JO3Shape.Empty, null, (0, ""), 0 *: "" *: EmptyTuple, (None, Nil))
  )

  private val valid =
    """{"colour":"Red","shape":{"type":"Circle","radius":1},"nick":"n","pair":{"a":1,"b":"x"},"generic":[2,"y"],"optional":{"x":1,"y":[2]}}"""

  private val inputs = Seq(
    valid,
    valid.replace(""""nick":"n",""", ""),
    valid.replace(""""nick":"n"""", """"nick":null"""),
    valid.replace(""""Red"""", """"blue""""),
    valid.replace(""""Red"""", """"Blue""""),
    valid.replace(""""Red"""", """"Purple""""),
    valid.replace(""""Red"""", """{"type":"Red"}"""),
    valid.replace("""{"type":"Circle","radius":1}""", """"Empty""""),
    valid.replace("""{"type":"Circle","radius":1}""", """{"type":"Empty"}"""),
    valid.replace("""{"type":"Circle","radius":1}""", """{"Circle":{"radius":1}}"""),
    valid.replace("""{"a":1,"b":"x"}""", """{"a":1}"""),
    valid.replace("""{"a":1,"b":"x"}""", """[1,"x"]"""),
    valid.replace("""[2,"y"]""", """[2]"""),
    valid.replace("""[2,"y"]""", """[2,"y",3]"""),
    valid.replace("""{"x":1,"y":[2]}""", """{}"""),
    valid.replace("""{"x":1,"y":[2]}""", """{"x":null,"y":null}"""),
    valid.replace("""{"x":1,"y":[2]}""", """{"x":1,"x":2}"""),
    valid.replace("""{"x":1,"y":[2]}""", """{"x":1,"z":2}""")
  )

  private def check(codecs: JoCodecs[JO3Holder])(expectedDifferences: String*): Unit = {
    val differences = JsoniterOracle.differences(codecs, values, inputs)
    assertEquals(differences, expectedDifferences.toList, differences.mkString("\n"))
  }

  // Documented difference (feature-parity, jsoniter-derivation "Design decisions"): a `T | Null` field holding `null`
  // is written as `null` without `transientNull`; jsoniter-scala's codec throws a `NullPointerException` then.
  private def nullNullableField(colour: String, shape: String): String =
    s"""encode JO3Holder(Blue,Empty,null,(0,),(0,),(None,List())): kindlings={"colour":$colour,"shape":$shape,"nick":null,"pair":{"a":0,"b":""},"generic":[0,""],"optional":{}} """ +
      """upstream=<threw java.lang.NullPointerException: Cannot invoke "String.length()" because "x" is null>"""

  group("Scala 3 shapes") {
    test("jsoniterScalaDefaults vs make")(
      check(defaults)(nullNullableField("""{"type":"blue"}""", """{"type":"Empty"}"""))
    )
    test("without a discriminator")(
      check(noDiscriminator)(
        nullNullableField(""""blue"""", """"Empty""""),
        // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case
        // object (here a parameterless enum case) is also read from `{"Leaf":{}}`, so an object is read as such.
        """decode {"colour":{"type":"Red"},"shape":{"type":"Circle","radius":1...(141 chars): """ +
          """kindlings=rejected: illegal discriminator, offset: 0x00000011 upstream=rejected: expected '"', offset: 0x0000000a"""
      )
    )
    test("transientNull")(check(transientNull)())

    test("recursive tuples behind local type aliases") {
      type RecursiveTuple = (Int, Option[RecursiveTuple])
      type RecursiveNamedTuple = (value: Int, next: Option[RecursiveNamedTuple])
      val tuples = JoCodecs[RecursiveTuple](
        KindlingsJsonValueCodec.derived[RecursiveTuple](using JsoniterConfig.jsoniterScalaDefaults),
        runtime(RuntimeConfigs.defaults)(KindlingsJsonValueCodec.derived[RecursiveTuple]),
        JsonCodecMaker.make[RecursiveTuple](CodecMakerConfig.withAllowRecursiveTypes(true))
      )
      val named = JoCodecs[RecursiveNamedTuple](
        KindlingsJsonValueCodec.derived[RecursiveNamedTuple](using JsoniterConfig.jsoniterScalaDefaults),
        runtime(RuntimeConfigs.defaults)(KindlingsJsonValueCodec.derived[RecursiveNamedTuple]),
        JsonCodecMaker.make[RecursiveNamedTuple](CodecMakerConfig.withAllowRecursiveTypes(true))
      )
      val tupleDiffs =
        JsoniterOracle.differences(tuples, Seq((1, Some((2, None)))), Seq("[1,[2,null]]", "[1]", "[1,[2]]"))
      val namedDiffs = JsoniterOracle.differences(
        named,
        Seq((1, Some((2, None)))),
        Seq("""{"value":1,"next":{"value":2}}""", """{"next":null}""", """{"value":1,"next":{}}""")
      )
      assertEquals(tupleDiffs ++ namedDiffs, Nil, (tupleDiffs ++ namedDiffs).mkString("\n"))
    }
  }
}
