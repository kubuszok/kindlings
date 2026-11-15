package hearth.kindlings.oracle.jsoniterjson

import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.{Json as KJson, JsonNumber as KNumber, JsonObject as KObject}
import hearth.kindlings.jsoniterjson.codec.JsonCodec
import io.circe.{Json as CJson, JsonNumber as CNumber, JsonObject as CObject}

import scala.util.Try

/** A JSON value built with the same calls on both sides (`kindlings-jsoniter-json` and circe). */
sealed trait JjBuild
object JjBuild {
  final case class Str(s: String) extends JjBuild
  final case class IntN(n: Int) extends JjBuild
  final case class LongN(n: Long) extends JjBuild
  final case class DoubleN(n: Double) extends JjBuild
  final case class FloatN(n: Float) extends JjBuild
  final case class BigDec(n: BigDecimal) extends JjBuild
  final case class BigI(n: BigInt) extends JjBuild
  final case class Bool(b: Boolean) extends JjBuild
  case object Null extends JjBuild
  final case class Arr(values: List[JjBuild]) extends JjBuild
  final case class Obj(fields: List[(String, JjBuild)]) extends JjBuild
  def arr(values: JjBuild*): Arr = Arr(values.toList)
  def obj(fields: (String, JjBuild)*): Obj = Obj(fields.toList)

  def kindlings(b: JjBuild): KJson = b match {
    case Str(s)      => KJson.fromString(s)
    case IntN(n)     => KJson.fromInt(n)
    case LongN(n)    => KJson.fromLong(n)
    case DoubleN(n)  => KJson.fromDouble(n).getOrElse(KJson.Null)
    case FloatN(n)   => KJson.fromFloat(n).getOrElse(KJson.Null)
    case BigDec(n)   => KJson.fromBigDecimal(n)
    case BigI(n)     => KJson.fromBigInt(n)
    case Bool(v)     => KJson.fromBoolean(v)
    case Null        => KJson.Null
    case Arr(values) => KJson.fromValues(values.map(kindlings))
    case Obj(fields) => KJson.fromJsonObject(KObject.fromIterable(fields.map { case (k, v) => k -> kindlings(v) }))
  }

  def circe(b: JjBuild): CJson = b match {
    case Str(s)      => CJson.fromString(s)
    case IntN(n)     => CJson.fromInt(n)
    case LongN(n)    => CJson.fromLong(n)
    case DoubleN(n)  => CJson.fromDouble(n).getOrElse(CJson.Null)
    case FloatN(n)   => CJson.fromFloat(n).getOrElse(CJson.Null)
    case BigDec(n)   => CJson.fromBigDecimal(n)
    case BigI(n)     => CJson.fromBigInt(n)
    case Bool(v)     => CJson.fromBoolean(v)
    case Null        => CJson.Null
    case Arr(values) => CJson.fromValues(values.map(circe))
    case Obj(fields) => CJson.fromFields(fields.map { case (k, v) => k -> circe(v) })
  }
}

/** `kindlings-jsoniter-json` vs circe's JSON model read and written by jsoniter-scala-circe (the library it replaces,
  * see `docs/user-guide/jsoniter-json.md`): parsing, printing, numbers, and the `Json`/`JsonNumber`/`JsonObject` API it
  * mirrors.
  */
final class JsoniterJsonOracleSpec extends MacroSuite {

  private val kCodec: JsonValueCodec[KJson] = JsonCodec.jsonValueCodec
  private val cCodec: JsonValueCodec[CJson] = JsoniterScalaCodec.jsonC3c

  /** A neutral rendering keeping key order, duplicate keys and the number text. */
  private def showK(j: KJson): String = j.fold(
    "null",
    _.toString,
    n => s"n:${n.toString}",
    s => s"s:${CJson.fromString(s).noSpaces}",
    vs => vs.map(showK).mkString("[", ",", "]"),
    o => o.fields.map { case (k, v) => s"$k=${showK(v)}" }.mkString("{", ",", "}")
  )
  private def showC(j: CJson): String = j.fold(
    "null",
    _.toString,
    n => s"n:${n.toString}",
    s => s"s:${CJson.fromString(s).noSpaces}",
    vs => vs.map(showC).mkString("[", ",", "]"),
    o => o.toList.map { case (k, v) => s"$k=${showC(v)}" }.mkString("{", ",", "}")
  )

  private def outcome[A](a: => A)(show: A => String): String =
    Try(a).fold(e => s"fail ${e.getClass.getSimpleName}", show)

  private def check(differences: List[String], expected: List[String]): Unit =
    assertEquals(differences, expected, s"Kindlings vs circe differences:\n${differences.mkString("\n")}")

  // Documented (jsoniter-json guide "Differences from circe", feature-parity.md jsoniter-json): a `JsonObject` is an
  // ordered list of fields that keeps duplicate keys (`apply` returns the last value), while circe's keeps one value
  // per key (the last, at the first key's position).

  private def nested(depth: Int): String = ("[" * depth) + ("]" * depth)

  private val parseInputs: List[String] = List(
    // structure
    """{"b":1,"a":[true,false,null],"c":{}}""",
    """{"a":1,"a":2}""",
    """{"a":1,"b":2,"a":3}""",
    """ [ 1 , 2 ] """,
    nested(128),
    nested(129),
    nested(10000),
    // numbers
    "0",
    "-0",
    "-0.0",
    "0.0",
    "1.0",
    "1.50",
    "1e3",
    "1E+3",
    "1.5e1",
    "-1.5E-3",
    "123456789012345678901234567890",
    "9223372036854775807",
    "9223372036854775808",
    "-9223372036854775809",
    "0.1",
    "1e400",
    "1e-400",
    "1" + ("0" * 400),
    "0." + ("1" * 400),
    "1e2147483648",
    // strings
    "\"\\u0000\\n\\t\\\"\\\\/\"",
    "\"\\u00e9\\ud83d\\ude00\"",
    "\"\u2028\"",
    "\"\\ud800\"",
    // invalid
    "01",
    "1.",
    ".5",
    "+1",
    "NaN",
    "Infinity",
    "-",
    "1e",
    "[1,]",
    """{"a":1,}""",
    """{"a"}""",
    "nul",
    "",
    "1 2",
    "\"unterminated",
    "{'a':1}"
  )

  test("parsing (readFromString): accepted inputs, structure, key order, duplicate keys, number text") {
    val differences = parseInputs.flatMap { input =>
      val k = outcome(readFromString[KJson](input)(kCodec))(showK)
      val c = outcome(readFromString[CJson](input)(cCodec))(showC)
      if (k == c) Nil else List(s"parse ${input.take(40)}: kindlings=${k.take(200)} upstream=${c.take(200)}")
    }
    check(
      differences,
      List( // duplicate keys are kept (documented, see the comment above `nested`)
        "parse {\"a\":1,\"a\":2}: kindlings={a=n:1,a=n:2} upstream={a=n:2}",
        "parse {\"a\":1,\"b\":2,\"a\":3}: kindlings={a=n:1,b=n:2,a=n:3} upstream={a=n:3,b=n:2}"
      )
    )
  }

  test("parse then print (writeToString)") {
    val differences = parseInputs.flatMap { input =>
      val k = outcome(writeToString(readFromString[KJson](input)(kCodec))(kCodec))(identity)
      val c = outcome(writeToString(readFromString[CJson](input)(cCodec))(cCodec))(identity)
      if (k == c) Nil else List(s"round trip ${input.take(40)}: kindlings=${k.take(200)} upstream=${c.take(200)}")
    }
    check(
      differences,
      List( // duplicate keys are kept (documented, see the comment above `nested`)
        "round trip {\"a\":1,\"a\":2}: kindlings={\"a\":1,\"a\":2} upstream={\"a\":2}",
        "round trip {\"a\":1,\"b\":2,\"a\":3}: kindlings={\"a\":1,\"b\":2,\"a\":3} upstream={\"a\":3,\"b\":2}"
      )
    )
  }

  private val built: List[JjBuild] = {
    import JjBuild.*
    List(
      Str("plain"),
      Str("\u0000\u001f\"\\/\b\f\n\r\t"),
      Str("\u00e9\u2028\u2029\ud83d\ude00</script>"),
      IntN(Int.MinValue),
      LongN(Long.MaxValue),
      DoubleN(1.0),
      DoubleN(-0.0),
      DoubleN(0.1 + 0.2),
      DoubleN(1e10),
      DoubleN(1e-7),
      DoubleN(1.0e300),
      DoubleN(Double.NaN),
      FloatN(1.1f),
      FloatN(3.0e38f),
      BigDec(BigDecimal("1e1000")),
      BigDec(BigDecimal("-0.0")),
      BigDec(BigDecimal("1.50")),
      BigI(BigInt("123456789012345678901234567890")),
      Bool(true),
      Null,
      arr(),
      obj(),
      obj("b" -> IntN(1), "a" -> arr(Null, obj("x" -> Str("y")))),
      obj("a" -> IntN(1), "a" -> IntN(2))
    )
  }

  test("printing (writeToString) of values built with the same calls") {
    val differences = built.flatMap { b =>
      val k = outcome(writeToString(JjBuild.kindlings(b))(kCodec))(identity)
      val c = outcome(writeToString(JjBuild.circe(b))(cCodec))(identity)
      if (k == c) Nil else List(s"print $b: kindlings=$k upstream=$c")
    }
    check(
      differences,
      List( // duplicate keys are kept (documented, see the comment above `nested`)
        "print Obj(List((a,IntN(1)), (a,IntN(2)))): kindlings={\"a\":1,\"a\":2} upstream={\"a\":2}"
      )
    )
  }

  private val numberTexts: List[String] = List(
    "0",
    "-0",
    "-0.0",
    "1",
    "-546",
    "-546.0",
    "1e3",
    "1E3",
    "1.5e1",
    "1500e-2",
    "5.5",
    "2147483647",
    "2147483648",
    "-2147483649",
    "9223372036854775807",
    "9223372036854775808",
    "123456789012345678901234567890",
    "1e19",
    "1e400",
    "1e-400",
    "-1e400",
    "1e2147483648",
    "1e-2147483649",
    "0.1",
    "1.7976931348623157E308",
    "3.4028235e38",
    "3.5e38",
    // not JSON numbers
    "01",
    "-01",
    "1.",
    ".5",
    "+1",
    "1e",
    "1e+",
    "0x10",
    "NaN",
    "Infinity",
    "-Infinity",
    " 1",
    "1 ",
    "1_000",
    "",
    "-"
  )

  /** `BigDecimal`s compared by value (`0.0` and `0`, `1.50` and `1.5` are the same number). */
  private def value(bd: Option[BigDecimal]): String = bd.map(_.underlying.stripTrailingZeros.toString).toString

  private def conversionsK(n: KNumber): String =
    s"int=${n.toInt} long=${n.toLong} bigInt=${n.toBigInt} bigDecimal=${value(n.toBigDecimal)} double=${n.toDouble} float=${n.toFloat}"
  private def conversionsC(n: CNumber): String =
    s"int=${n.toInt} long=${n.toLong} bigInt=${n.toBigInt} bigDecimal=${value(n.toBigDecimal)} double=${Some(n.toDouble)
        .filterNot(d => d.isNaN || d.isInfinite)} float=${Some(n.toFloat).filterNot(f => f.isNaN || f.isInfinite)}"

  test("JsonNumber.fromString (validation) and the numeric conversions") {
    val differences = numberTexts.flatMap { s =>
      val k = outcome(KNumber.fromString(s))(_.fold("rejected")(conversionsK))
      val c = outcome(CNumber.fromString(s))(_.fold("rejected")(conversionsC))
      if (k == c) Nil else List(s"fromString '$s': kindlings=$k upstream=$c")
    }
    check(
      differences,
      // Documented (jsoniter-json guide "Differences from circe"): only JSON numbers are accepted, since a number is
      // written as its text; circe's `fromString` also accepts leading zeros and a leading `.` (and then writes JSON
      // that is not valid).
      List(
        "fromString '01': kindlings=rejected upstream=int=Some(1) long=Some(1) bigInt=Some(1) bigDecimal=Some(1) double=Some(1.0) float=Some(1.0)",
        "fromString '-01': kindlings=rejected upstream=int=Some(-1) long=Some(-1) bigInt=Some(-1) bigDecimal=Some(-1) double=Some(-1.0) float=Some(-1.0)",
        "fromString '.5': kindlings=rejected upstream=int=None long=None bigInt=None bigDecimal=Some(0.5) double=Some(0.5) float=Some(0.5)"
      )
    )
  }

  test("numeric conversions of parsed numbers") {
    val differences = numberTexts.flatMap { s =>
      val k = outcome(readFromString[KJson](s)(kCodec))(_.asNumber.fold("not a number")(conversionsK))
      val c = outcome(readFromString[CJson](s)(cCodec))(_.asNumber.fold("not a number")(conversionsC))
      if (k == c) Nil else List(s"parsed '$s': kindlings=$k upstream=$c")
    }
    check(differences, Nil)
  }

  test("numbers from Double/Float: NaN and infinities rejected, text") {
    val doubles = List(1.0, -0.0, 0.5, 1e21, 1e-7, Double.NaN, Double.PositiveInfinity, Double.MinPositiveValue)
    val floats = List(1.0f, -0.0f, 0.1f, Float.NaN, Float.NegativeInfinity)
    val differences =
      doubles.flatMap { d =>
        val k = KNumber.fromDouble(d).map(conversionsK)
        val c = CJson.fromDouble(d).flatMap(_.asNumber).map(conversionsC)
        if (k == c) Nil else List(s"fromDouble $d: kindlings=$k upstream=$c")
      } ++ floats.flatMap { f =>
        val k = KNumber.fromFloat(f).map(conversionsK)
        val c = CJson.fromFloat(f).flatMap(_.asNumber).map(conversionsC)
        if (k == c) Nil else List(s"fromFloat $f: kindlings=$k upstream=$c")
      }
    check(
      differences,
      // Documented (jsoniter-json guide "Differences from circe"): the number keeps the text `-0.0`, so `toDouble` is
      // `-0.0`; circe's `JsonFloat` converts a negative zero to `0.0` (but its `JsonDouble` keeps `-0.0`).
      List(
        "fromFloat -0.0: kindlings=Some(int=Some(0) long=Some(0) bigInt=Some(0) bigDecimal=Some(0) double=Some(-0.0) float=Some(-0.0)) upstream=Some(int=Some(0) long=Some(0) bigInt=Some(0) bigDecimal=Some(0) double=Some(0.0) float=Some(-0.0))"
      )
    )
  }

  test("equality") {
    def parsed(s: String): (KJson, CJson) = (readFromString[KJson](s)(kCodec), readFromString[CJson](s)(cCodec))
    val pairs = List(
      "1" -> "1.0",
      "1" -> "1e0",
      "100" -> "1e2",
      "0" -> "-0",
      "0.10" -> "0.1",
      """{"a":1,"b":2}""" -> """{"b":2,"a":1}""",
      """{"a":1}""" -> """{"a":1,"a":1}""",
      "[1,2]" -> "[2,1]"
    )
    val differences = pairs.flatMap { case (a, b) =>
      val (ka, ca) = parsed(a)
      val (kb, cb) = parsed(b)
      if ((ka == kb) == (ca == cb)) Nil else List(s"$a == $b: kindlings=${ka == kb} upstream=${ca == cb}")
    }
    check(
      differences,
      // Documented (jsoniter-json guide "Differences from circe"): `Json` is a plain ADT with structural equality, so
      // numbers are equal when their texts are and objects when their (ordered) fields are; circe compares numbers by
      // value and objects as maps.
      List(
        "1 == 1.0: kindlings=false upstream=true",
        "100 == 1e2: kindlings=false upstream=true",
        "0.10 == 0.1: kindlings=false upstream=true",
        "{\"a\":1,\"b\":2} == {\"b\":2,\"a\":1}: kindlings=false upstream=true",
        "{\"a\":1} == {\"a\":1,\"a\":1}: kindlings=false upstream=true"
      )
    )
  }

  test("JsonObject operations") {
    val k0 = KObject("a" -> KJson.fromInt(1), "b" -> KJson.fromInt(2))
    val c0 = CObject("a" -> CJson.fromInt(1), "b" -> CJson.fromInt(2))
    val kd = KObject.fromIterable(List("a" -> KJson.fromInt(1), "b" -> KJson.fromInt(2), "a" -> KJson.fromInt(3)))
    val cd = CObject.fromIterable(List("a" -> CJson.fromInt(1), "b" -> CJson.fromInt(2), "a" -> CJson.fromInt(3)))
    def k(o: KObject): String = showK(KJson.fromJsonObject(o))
    def c(o: CObject): String = showC(CJson.fromJsonObject(o))
    val results = List(
      ("add new key", k(k0.add("c", KJson.fromInt(3))), c(c0.add("c", CJson.fromInt(3)))),
      ("add existing key", k(k0.add("a", KJson.fromInt(9))), c(c0.add("a", CJson.fromInt(9)))),
      ("remove", k(k0.remove("a")), c(c0.remove("a"))),
      ("remove missing", k(k0.remove("z")), c(c0.remove("z"))),
      ("apply", k0("b").map(showK).toString, c0("b").map(showC).toString),
      ("apply missing", k0("z").toString, c0("z").toString),
      ("keys", k0.keys.toList.toString, c0.keys.toList.toString),
      ("values", k0.values.map(showK).toList.toString, c0.values.map(showC).toList.toString),
      ("size", k0.size.toString, c0.size.toString),
      ("isEmpty", KObject.empty.isEmpty.toString, CObject.empty.isEmpty.toString),
      ("nonEmpty", k0.nonEmpty.toString, c0.nonEmpty.toString),
      (
        "toMap",
        k0.toMap.map { case (key, v) => key -> showK(v) }.toString,
        c0.toMap.map { case (key, v) =>
          key -> showC(v)
        }.toString
      ),
      ("mapValues", k(k0.mapValues(_ => KJson.True)), c(c0.mapValues(_ => CJson.True))),
      ("fromIterable with a duplicate key", k(kd), c(cd)),
      ("duplicate key: apply", kd("a").map(showK).toString, cd("a").map(showC).toString),
      ("duplicate key: size", kd.size.toString, cd.size.toString),
      (
        "duplicate key: printed",
        writeToString(KJson.fromJsonObject(kd))(kCodec),
        writeToString(CJson.fromJsonObject(cd))(cCodec)
      ),
      (
        "Json.obj with a duplicate key",
        showK(KJson.obj("a" -> KJson.True, "a" -> KJson.False)),
        showC(
          CJson.obj("a" -> CJson.True, "a" -> CJson.False)
        )
      )
    )
    val differences = results.collect { case (what, kr, cr) if kr != cr => s"$what: kindlings=$kr upstream=$cr" }
    check(
      differences,
      List( // duplicate keys are kept (documented, see the comment above `nested`): `add` appends (circe replaces the value of an existing key in place)
        "add existing key: kindlings={a=n:1,b=n:2,a=n:9} upstream={a=n:9,b=n:2}",
        "fromIterable with a duplicate key: kindlings={a=n:1,b=n:2,a=n:3} upstream={a=n:3,b=n:2}",
        "duplicate key: size: kindlings=3 upstream=2",
        "duplicate key: printed: kindlings={\"a\":1,\"b\":2,\"a\":3} upstream={\"a\":3,\"b\":2}",
        "Json.obj with a duplicate key: kindlings={a=true,a=false} upstream={a=false}"
      )
    )
  }

  test("Json accessors and fold") {
    val values = List[JjBuild](
      JjBuild.Null,
      JjBuild.Bool(true),
      JjBuild.IntN(1),
      JjBuild.Str("s"),
      JjBuild.arr(JjBuild.Null),
      JjBuild.obj("a" -> JjBuild.Null)
    )
    val differences = values.flatMap { b =>
      val kj = JjBuild.kindlings(b)
      val cj = JjBuild.circe(b)
      val k = List(kj.isNull, kj.isBoolean, kj.isNumber, kj.isString, kj.isArray, kj.isObject).toString +
        List(kj.asBoolean, kj.asNumber.map(_.toString), kj.asString, kj.asArray.map(_.size), kj.asObject.map(_.size)) +
        kj.fold("n", _ => "b", _ => "num", _ => "s", _ => "a", _ => "o")
      val c = List(cj.isNull, cj.isBoolean, cj.isNumber, cj.isString, cj.isArray, cj.isObject).toString +
        List(cj.asBoolean, cj.asNumber.map(_.toString), cj.asString, cj.asArray.map(_.size), cj.asObject.map(_.size)) +
        cj.fold("n", _ => "b", _ => "num", _ => "s", _ => "a", _ => "o")
      if (k == c) Nil else List(s"$b: kindlings=$k upstream=$c")
    }
    check(differences, Nil)
  }
}
