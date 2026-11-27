package hearth.kindlings.oracle.jsoniterjson

import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReader, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.Json as KJson
import hearth.kindlings.jsoniterjson.codec.JsonCodec
import io.circe.Json as CJson

import scala.util.Try

/** The cases of jsoniter-scala-circe's own `JsoniterScalaCodecSpec` (the default codec and every option of
  * `JsoniterScalaCodec.jsonCodec`), run through `kindlings-jsoniter-json`'s `JsonCodec` and through upstream: the JSON
  * written after reading, and the messages of the failures.
  */
final class JsoniterJsonCodecOracleSpec extends MacroSuite {

  /** The failure's message up to the position (`expected '}' or ',', offset: 0x..., buf: ...`). */
  private def outcome(a: => String): String =
    Try(a).fold(e => s"fail ${e.getClass.getSimpleName}: ${String.valueOf(e.getMessage).takeWhile(_ != ',')}", identity)

  private def roundTrips(
      inputs: List[String],
      k: JsonValueCodec[KJson],
      c: JsonValueCodec[CJson]
  ): List[String] = inputs.flatMap { input =>
    val kOut = outcome(writeToString(readFromString[KJson](input)(k))(k))
    val cOut = outcome(writeToString(readFromString[CJson](input)(c))(c))
    if (kOut == cOut) Nil else List(s"${input.take(60)}: kindlings=${kOut.take(200)} upstream=${cOut.take(200)}")
  }

  private def check(differences: List[String], expected: List[String] = Nil): Unit =
    assertEquals(
      differences,
      expected,
      s"Kindlings vs jsoniter-scala-circe differences:\n${differences.mkString("\n")}"
    )

  private val pi = "3.14159265358979323846264338327950288419716939937510"
  private val longDecimal = "0." + "1234567890" * 30 + "123456" // 307 digits, including the leading zero

  // "deserialize json", "serialize json", "not deserialize invalid json", "not deserialize deeply nested json",
  // "deserialize negative zeros keeping the sign like circe does", "deserialize decimal numbers exactly like circe
  // does", "deserialize a single big number", "deserialize big numbers"
  private val inputs: List[String] = List(
    """{"n":null,"s":"VVV","n1":1.0,"n2":2,"b":true,"a":[null,"WWW",[],{},false],"o":{"a":[]}}""",
    """{"s":"VVV","n1":1.0,"n2":2,"b":true,"a":[null,"WWW",[],{},false],"o":{"a":[]}}""",
    """{"n":null[]""",
    """[null{}""",
    """[{"n":""" * 64 + "[]" + "}]" * 64,
    """{"n":[""" * 64 + "{}" + "]}" * 64,
    """[{"n":""" * 63 + "[]" + "}]" * 63,
    "-0",
    "-0.0",
    "-0e0",
    "-0.00E+5",
    "[-0]",
    "{\"n\":-0.0}",
    "0",
    "0.0",
    "-1",
    "-0.5",
    "-1e-5",
    pi,
    longDecimal,
    s"[$longDecimal]",
    longDecimal + "8",
    "1e-6178",
    "1e-6177",
    "1e6178",
    "9223372036854775808",
    "[9223372036854775807,-9223372036854775808,9223372036854775809,9999999999999999999]",
    """{"s":1,"n1":1.0,"n2":4503599627370497,"a":[null,"WWW",[],{}],"o":{"a":4e+297}}""",
    "[1.23456789,123456789,1e-9]",
    "1.234567890",
    "1234567890",
    "1e-10",
    "0.30000000000000004",
    "1e300",
    "1.0E300",
    "-0.0E0",
    "[1.50,100,0.0000001,9007199254740993,-9007199254740993.0]",
    "[4503599627370495,4503599627370496,-4503599627370496,-4503599627370497,0.30000000000000004,1.0E300,1e-300]",
    """{"b":{"z":1,"a":null},"a":[{"d":null,"c":2}],"":0}""",
    // not JSON values
    "x",
    "tru",
    "+1",
    ".5",
    ""
  )

  test("the default codec: JsonCodec.jsonValueCodec vs JsoniterScalaCodec.jsonC3c") {
    check(roundTrips(inputs, JsonCodec.jsonValueCodec, JsoniterScalaCodec.jsonC3c))
    check(roundTrips(inputs, JsonCodec.jsonCodec(), JsoniterScalaCodec.jsonCodec()))
  }

  // "allow customization for number parsing"
  test("numberParser: a custom parser (numbers read as doubles)") {
    val k = JsonCodec.jsonCodec(numberParser = in => KJson.fromDouble(in.readDouble()).getOrElse(KJson.Null))
    val c = JsoniterScalaCodec.jsonCodec(numberParser = in => CJson.fromDoubleOrNull(in.readDouble()))
    check(roundTrips(inputs, k, c))
  }

  // "allow customization of the math context and limits for number parsing"
  test("numberParser(mc, scaleLimit, digitsLimit): rounding and limits") {
    check(
      roundTrips(
        inputs,
        JsonCodec.jsonCodec(numberParser = JsonCodec.numberParser(mc = JsonReader.bigDecimalMathContext)),
        JsoniterScalaCodec.jsonCodec(numberParser =
          io.circe.JsoniterScalaCodec.numberParser(mc = JsonReader.bigDecimalMathContext)
        )
      ) ++ roundTrips(
        inputs,
        JsonCodec.jsonCodec(numberParser = JsonCodec.numberParser(scaleLimit = 10, digitsLimit = 10)),
        JsoniterScalaCodec.jsonCodec(numberParser =
          io.circe.JsoniterScalaCodec.numberParser(scaleLimit = 10, digitsLimit = 10)
        )
      )
    )
  }

  // "allow customization for number serialization"
  test("numberSerializer = jsCompatibleNumberSerializer: parsed numbers") {
    check(
      roundTrips(
        inputs,
        JsonCodec.jsonCodec(numberSerializer = JsonCodec.jsCompatibleNumberSerializer),
        JsoniterScalaCodec.jsonCodec(numberSerializer = io.circe.JsoniterScalaCodec.jsCompatibleNumberSerializer)
      ),
      Nil
    )
  }

  test("numberSerializer = jsCompatibleNumberSerializer: numbers built with the same calls") {
    import JjBuild.*
    val k = JsonCodec.jsonCodec(numberSerializer = JsonCodec.jsCompatibleNumberSerializer)
    val c = JsoniterScalaCodec.jsonCodec(numberSerializer = io.circe.JsoniterScalaCodec.jsCompatibleNumberSerializer)
    val built: List[JjBuild] = List(
      FloatN(3.1415927f),
      FloatN(3.0e38f),
      FloatN(1.0e-45f),
      DoubleN(3.141592653589793),
      DoubleN(0.1 + 0.2),
      DoubleN(1.0e300),
      DoubleN(-1.0e-300),
      DoubleN(Double.MinPositiveValue),
      DoubleN(Double.MaxValue),
      DoubleN(9007199254740993.0),
      DoubleN(-0.0),
      IntN(Int.MinValue),
      LongN(4503599627370495L),
      LongN(4503599627370496L),
      LongN(-4503599627370496L),
      LongN(-4503599627370497L),
      LongN(Long.MaxValue),
      LongN(Long.MinValue),
      BigI(BigInt("123456789012345678901234567890")),
      BigI(BigInt(42)),
      BigDec(BigDecimal("3.141592653589793")),
      BigDec(BigDecimal(pi)),
      BigDec(BigDecimal("4e+297")),
      BigDec(BigDecimal("1e-256")),
      BigDec(BigDecimal("1e-257")),
      BigDec(BigDecimal("1e256")),
      BigDec(BigDecimal("1e257")),
      BigDec(BigDecimal("1.50")),
      BigDec(BigDecimal("0.000001")),
      BigDec(BigDecimal("0.0000001")),
      BigDec(BigDecimal("0.0000000")),
      BigDec(BigDecimal("-1.5")),
      BigDec(BigDecimal("100.00")),
      BigDec(BigDecimal("123456789012345.6")),
      BigDec(BigDecimal("12345678901234.5")),
      BigDec(BigDecimal("0.000000123456789")),
      NumText("1.50"),
      NumText("100"),
      NumText("-0"),
      NumText("-0.0"),
      NumText("0.00"),
      NumText("1e2"),
      NumText("0.30000000000000004"),
      NumText("4e+297"),
      NumText("1e3000000000"),
      NumText("123456789012345678"),
      NumText("3.141592653589793"),
      NumText("1.0E300"),
      NumText("0.0000001"),
      NumText("4503599627370496"),
      NumText("-4503599627370496"),
      arr(IntN(1), obj("a" -> LongN(Long.MaxValue)))
    )
    check(built.flatMap { b =>
      val kOut = outcome(writeToString(JjBuild.kindlings(b))(k))
      val cOut = outcome(writeToString(JjBuild.circe(b))(c))
      if (kOut == cOut) Nil else List(s"$b: kindlings=$kOut upstream=$cOut")
    })
  }

  // "allow customization for key sorting during serialization"
  test("sortKeys") {
    check(roundTrips(inputs, JsonCodec.jsonCodec(sortKeys = true), JsoniterScalaCodec.jsonCodec(sortKeys = true)))
  }

  // "allow filtering for key-value serialization"
  test("doSerialize: fields with null values are not written") {
    check(
      roundTrips(
        inputs,
        JsonCodec.jsonCodec(doSerialize = _ != KJson.Null),
        JsoniterScalaCodec.jsonCodec(doSerialize = _ ne CJson.Null)
      ) ++ roundTrips(
        inputs,
        JsonCodec.jsonCodec(doSerialize = _ != KJson.Null, sortKeys = true),
        JsoniterScalaCodec.jsonCodec(doSerialize = _ ne CJson.Null, sortKeys = true)
      )
    )
  }

  // "not deserialize deeply nested json" with another limit
  test("maxDepth") {
    check(roundTrips(inputs, JsonCodec.jsonCodec(maxDepth = 3), JsoniterScalaCodec.jsonCodec(maxDepth = 3)))
  }

  // "not serialize invalid json", "not serialize deeply nested json", "serialize numbers"
  test("writing: a lone surrogate, values nested too deeply, numbers") {
    import JjBuild.*
    def nest(depth: Int, leaf: JjBuild): JjBuild = if (depth == 0) leaf else arr(obj("n" -> nest(depth - 1, leaf)))
    val built: List[JjBuild] = List(
      Str("\ud800"),
      nest(64, arr()),
      nest(63, arr()),
      arr(FloatN(1.0f), DoubleN(2.0), LongN(3L), BigDec(BigDecimal("777e+777"))),
      NumText("1.50"),
      NumText("100"),
      NumText("-0"),
      NumText("-0.0"),
      NumText("0.00"),
      NumText("1e2"),
      NumText("0.30000000000000004"),
      NumText("4e+297"),
      NumText("1e3000000000"),
      NumText("123456789012345678"),
      NumText("3.141592653589793"),
      NumText("1.0E300"),
      NumText("0.0000001"),
      NumText("4503599627370496"),
      NumText("-4503599627370496"),
      DoubleN(0.1 + 0.2),
      DoubleN(1.0e300),
      DoubleN(-0.0),
      FloatN(3.0e38f),
      BigDec(BigDecimal("0.0000001")),
      BigDec(BigDecimal("1.50")),
      BigI(BigInt("123456789012345678901234567890"))
    )
    check(built.flatMap { b =>
      val kOut = outcome(writeToString(JjBuild.kindlings(b))(JsonCodec.jsonValueCodec))
      val cOut = outcome(writeToString(JjBuild.circe(b))(JsoniterScalaCodec.jsonC3c))
      if (kOut == cOut) Nil else List(s"${b.toString.take(60)}: kindlings=${kOut.take(200)} upstream=${cOut.take(200)}")
    })
  }
}
