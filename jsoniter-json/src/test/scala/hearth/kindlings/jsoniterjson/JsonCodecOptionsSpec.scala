package hearth.kindlings.jsoniterjson

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReader, JsonReaderException}
import hearth.kindlings.jsoniterjson.codec.JsonCodec
import hearth.kindlings.jsoniterjson.codec.JsonCodec.{jsonCodec, jsonValueCodec}
import hearth.Suite

/** The cases of jsoniter-scala-circe's `JsoniterScalaCodecSpec` (jsoniter-scala 2.42.3), with its expectations. */
final class JsonCodecOptionsSpec extends Suite {

  private def messageOf(body: => Any): String =
    try { val _ = body; "no failure" }
    catch { case e: Throwable => String.valueOf(e.getMessage) }

  private def number(text: String): Json = Json.Num(JsonNumber.fromString(text).get)

  test("not deserialize invalid json") {
    assert(messageOf(readFromString[Json]("""{"n":null[]""")).contains("expected '}' or ','"))
    assert(messageOf(readFromString[Json]("""[null{}""")).contains("expected ']' or ','"))
    assert(messageOf(readFromString[Json]("x")).contains("expected JSON value"))
  }

  test("not deserialize deeply nested json") {
    val jsonStr1 = """[{"n":""" * 64 + "[]" + "}]" * 64
    val jsonStr2 = """{"n":[""" * 64 + "{}" + "]}" * 64
    assert(messageOf(readFromString[Json](jsonStr1)).contains("depth limit exceeded"))
    assert(messageOf(readFromString[Json](jsonStr2)).contains("depth limit exceeded"))
  }

  test("allow customization for number parsing") {
    val codec = jsonCodec(numberParser = in => Json.fromDouble(in.readDouble()).getOrElse(Json.Null))
    val jsonStr = """{"n":null,"s":"VVV","n1":1.0,"n2":2,"a":[null,"WWW",[],{}],"o":{"a":[]}}"""
    writeToString(readFromString[Json](jsonStr)(codec)) ==>
      """{"n":null,"s":"VVV","n1":1.0,"n2":2.0,"a":[null,"WWW",[],{}],"o":{"a":[]}}"""
  }

  test("serialize json") {
    val jsonStr = """{"s":"VVV","n1":1.0,"n2":2,"b":true,"a":[null,"WWW",[],{},false],"o":{"a":[]}}"""
    writeToString(readFromString[Json](jsonStr)) ==> jsonStr
  }

  test("allow customization for number serialization") {
    val codec = jsonCodec(numberSerializer = JsonCodec.jsCompatibleNumberSerializer)
    val jsonStr = """{"s":1,"n1":1.0,"n2":4503599627370497,"a":[null,"WWW",[],{}],"o":{"a":4e+297}}"""
    val json = readFromString[Json](jsonStr)
    writeToString(json)(codec) ==>
      """{"s":1,"n1":1.0,"n2":"4503599627370497","a":[null,"WWW",[],{}],"o":{"a":"4E+297"}}"""
    writeToString(Json.fromFloat(3.1415927f).get)(codec) ==> "3.1415927"
    writeToString(Json.fromDouble(3.141592653589793).get)(codec) ==> "3.141592653589793"
    writeToString(number("3.141592653589793"))(codec) ==> "3.141592653589793"
    writeToString(number("3.14159265358979323846264338327950288419716939937510"))(codec) ==>
      """"3.14159265358979323846264338327950288419716939937510""""
    writeToString(number("4e+297"))(codec) ==> """"4E+297""""
    // parsed numbers are integers or exact decimals, whatever their spelling
    writeToString(readFromString[Json]("[0.30000000000000004,1e300,1.0E300,-0.0,-0,1.50,100]"))(codec) ==>
      """["0.30000000000000004","1E+300","1.0E+300",0,0,1.50,100]"""
    writeToString(readFromString[Json]("[4503599627370495,4503599627370496,-4503599627370496,-4503599627370497]"))(
      codec
    ) ==> """[4503599627370495,"4503599627370496",-4503599627370496,"-4503599627370497"]"""
    // numbers made from their text: as their decimal value
    writeToString(Json.arr(number("1.50"), number("100"), number("-0.0"), number("0.00"), number("1e2")))(codec) ==>
      "[1.50,100,0,0,1E+2]"
    // numbers made from a Double or a Float are always JSON numbers
    writeToString(Json.fromDouble(0.1 + 0.2).get)(codec) ==> "0.30000000000000004"
    writeToString(Json.fromDouble(1.0e300).get)(codec) ==> "1.0E300"
    writeToString(Json.fromDouble(-0.0).get)(codec) ==> "-0.0"
    writeToString(Json.fromFloat(3.0e38f).get)(codec) ==> "3.0E38"
    // integers: [-2^52, 2^52) as numbers
    writeToString(Json.arr(Json.fromLong(4503599627370495L), Json.fromLong(4503599627370496L)))(codec) ==>
      """[4503599627370495,"4503599627370496"]"""
    writeToString(Json.arr(Json.fromLong(-4503599627370496L), Json.fromLong(-4503599627370497L)))(codec) ==>
      """[-4503599627370496,"-4503599627370497"]"""
    writeToString(Json.fromLong(Long.MinValue))(codec) ==> "\"-9223372036854775808\""
    writeToString(Json.fromBigInt(BigInt("123456789012345678901234567890")))(codec) ==>
      "\"123456789012345678901234567890\""
    // an exponent too large for a BigDecimal: the text, as a string
    writeToString(number("1e3000000000"))(codec) ==> "\"1e3000000000\""
  }

  test("allow customization for key sorting during serialization") {
    val codec = jsonCodec(sortKeys = true)
    val jsonStr = """{"s":1,"n1":1.0,"n2":4503599627370497,"a":[null,"WWW",[],{}],"o":{"a":4e+297}}"""
    writeToString(readFromString[Json](jsonStr))(codec) ==>
      """{"a":[null,"WWW",[],{}],"n1":1.0,"n2":4503599627370497,"o":{"a":4E+297},"s":1}"""
    // nested objects are sorted too; duplicate keys (kept by JsonObject) stay in their order
    writeToString(readFromString[Json]("""{"b":{"z":1,"a":2},"a":1,"b":3,"a":2}"""))(codec) ==>
      """{"a":1,"a":2,"b":{"a":2,"z":1},"b":3}"""
  }

  test("not serialize invalid json") {
    assert(messageOf(writeToString(Json.fromString("\ud800"))).contains("illegal char sequence of surrogate pair"))
  }

  test("not serialize deeply nested json") {
    def nest(depth: Int, leaf: Json): Json =
      if (depth == 0) leaf else Json.arr(Json.obj("n" -> nest(depth - 1, leaf)))
    assert(messageOf(writeToString(nest(64, Json.arr()))).contains("depth limit exceeded"))
    assert(messageOf(writeToString(nest(64, Json.obj()))).contains("depth limit exceeded"))
    writeToString(nest(1, Json.arr()))(jsonCodec(maxDepth = 3)) ==> """[{"n":[]}]"""
    assert(messageOf(writeToString(nest(2, Json.arr()))(jsonCodec(maxDepth = 3))).contains("depth limit exceeded"))
    assert(messageOf(readFromString[Json]("[[[[]]]]")(jsonCodec(maxDepth = 3))).contains("depth limit exceeded"))
  }

  test("allow filtering for key-value serialization") {
    val codec = jsonCodec(doSerialize = _ != Json.Null)
    val jsonStr = """{"n":null,"s":"VVV","n1":1.0,"n2":2,"a":[null,"WWW",[],{}],"o":{"a":[]}}"""
    val jsonStrExpected = """{"s":"VVV","n1":1.0,"n2":2,"a":[null,"WWW",[],{}],"o":{"a":[]}}"""
    writeToString(readFromString[Json](jsonStr)(codec))(codec) ==> jsonStrExpected
    writeToString(readFromString[Json](jsonStr))(jsonCodec(doSerialize = _ != Json.Null, sortKeys = true)) ==>
      """{"a":[null,"WWW",[],{}],"n1":1.0,"n2":2,"o":{"a":[]},"s":"VVV"}"""
  }

  test("deserialize negative zeros keeping the sign like circe does") {
    Seq("-0", "-0.0", "-0e0").foreach { jsonStr =>
      1.0 / readFromString[Json](jsonStr).asNumber.get.toDouble.get ==> Double.NegativeInfinity
    }
    readFromString[Json]("[-0]") ==> Json.arr(number("-0"))
    readFromString[Json]("{\"n\":-0.0}") ==> Json.obj("n" -> number("-0.0"))
    Seq("0", "0.0", "-1", "-0.5", "-1e-5").foreach { jsonStr =>
      assert(1.0 / readFromString[Json](jsonStr).asNumber.get.toDouble.get != Double.NegativeInfinity)
    }
    writeToString(readFromString[Json]("-0")) ==> "-0"
    writeToString(readFromString[Json]("-0.0")) ==> "-0.0"
  }

  test("deserialize decimal numbers exactly like circe does") {
    val pi = "3.14159265358979323846264338327950288419716939937510"
    readFromString[Json](pi) ==> number(pi)
    writeToString(readFromString[Json](pi)) ==> pi
    val longDecimal = "0." + "1234567890" * 30 + "123456" // 307 digits, including the leading zero
    writeToString(readFromString[Json](longDecimal)) ==> longDecimal
    readFromString[Json](s"[$longDecimal]") ==> Json.arr(number(longDecimal))
    assert(messageOf(readFromString[Json](longDecimal + "8")).contains("value exceeds limit for number of digits"))
    assert(messageOf(readFromString[Json]("1e-6178")).contains("value exceeds limit for scale"))
    intercept[JsonReaderException](readFromString[Json]("1e-6178"))
  }

  test("allow customization of the math context and limits for number parsing") {
    val roundingCodec = jsonCodec(numberParser = JsonCodec.numberParser(mc = JsonReader.bigDecimalMathContext))
    writeToString(
      readFromString[Json]("3.14159265358979323846264338327950288419716939937510")(roundingCodec)
    ) ==> "3.141592653589793238462643383279503"
    writeToString(readFromString[Json]("[-0.0]")(roundingCodec)) ==> "[-0.0]"
    val limitingCodec = jsonCodec(numberParser = JsonCodec.numberParser(scaleLimit = 10, digitsLimit = 10))
    writeToString(readFromString[Json]("[1.23456789,123456789,1e-9]")(limitingCodec)) ==>
      "[1.23456789,123456789,1E-9]"
    assert(
      messageOf(readFromString[Json]("1.234567890")(limitingCodec)).contains("value exceeds limit for number of digits")
    )
    assert(
      messageOf(readFromString[Json]("1234567890")(limitingCodec)).contains("value exceeds limit for number of digits")
    )
    assert(messageOf(readFromString[Json]("1e-10")(limitingCodec)).contains("value exceeds limit for scale"))
  }

  test("deserialize a single big number") {
    readFromString[Json]("9223372036854775808") ==> Json.fromBigInt(BigInt("9223372036854775808"))
  }

  test("deserialize big numbers") {
    val jsonStr = "[9223372036854775807,-9223372036854775808,9223372036854775809,9999999999999999999]"
    readFromString[Json](jsonStr) ==> Json.arr(
      Json.fromBigInt(BigInt("9223372036854775807")),
      Json.fromBigInt(BigInt("-9223372036854775808")),
      Json.fromBigInt(BigInt("9223372036854775809")),
      Json.fromBigInt(BigInt("9999999999999999999"))
    )
  }

  test("serialize numbers") {
    val json = Json.arr(
      Json.fromFloat(1.0f).get,
      Json.fromDouble(2.0).get,
      Json.fromLong(3L),
      Json.fromBigDecimal(BigDecimal("777e+777")),
      number("999e+999")
    )
    writeToString(json) ==> "[1.0,2.0,3,7.77E+779,999e+999]"
  }

  test("jsonCodec() with the defaults behaves like jsonValueCodec") {
    val jsonStr = """{"b":null,"a":[1,-0.0,1e3,{"z":1,"y":2}]}"""
    writeToString(readFromString[Json](jsonStr)(jsonCodec()))(jsonCodec()) ==>
      writeToString(readFromString[Json](jsonStr))
  }
}
