package hearth.kindlings.jsoniterjson

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.*
import munit.FunSuite

final class JsonCodecSpec extends FunSuite {

  test("round-trip: null") {
    val json: Json = Json.Null
    val str = writeToString(json)
    assertEquals(str, "null")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: boolean true") {
    val json: Json = Json.True
    val str = writeToString(json)
    assertEquals(str, "true")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: boolean false") {
    val json: Json = Json.False
    val str = writeToString(json)
    assertEquals(str, "false")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: string") {
    val json: Json = Json.fromString("hello")
    val str = writeToString(json)
    assertEquals(str, "\"hello\"")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: integer number") {
    val json: Json = Json.fromInt(42)
    val str = writeToString(json)
    assertEquals(str, "42")
    val decoded = readFromString[Json](str)
    assertEquals(decoded.asNumber.flatMap(_.toInt), Some(42))
  }

  test("round-trip: long number") {
    val json: Json = Json.fromLong(9999999999L)
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    assertEquals(decoded.asNumber.flatMap(_.toLong), Some(9999999999L))
  }

  test("round-trip: decimal number") {
    val json: Json = Json.fromBigDecimal(BigDecimal("3.14"))
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    assertEquals(decoded.asNumber.flatMap(_.toBigDecimal), Some(BigDecimal("3.14")))
  }

  test("round-trip: empty array") {
    val json: Json = Json.arr()
    val str = writeToString(json)
    assertEquals(str, "[]")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: array of mixed values") {
    val json: Json = Json.arr(Json.fromInt(1), Json.fromString("two"), Json.True, Json.Null)
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    assertEquals(decoded.asArray.map(_.size), Some(4))
  }

  test("round-trip: empty object") {
    val json: Json = Json.obj()
    val str = writeToString(json)
    assertEquals(str, "{}")
    assertEquals(readFromString[Json](str), json)
  }

  test("round-trip: simple object") {
    val json: Json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    assertEquals(decoded.asObject.flatMap(_("name")), Some(Json.fromString("Alice")))
    assertEquals(decoded.asObject.flatMap(_("age")).flatMap(_.asNumber).flatMap(_.toInt), Some(30))
  }

  test("round-trip: nested structure") {
    val json: Json = Json.obj(
      "users" -> Json.arr(
        Json.obj("name" -> Json.fromString("Alice"), "active" -> Json.True),
        Json.obj("name" -> Json.fromString("Bob"), "active" -> Json.False)
      )
    )
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    val users = decoded.asObject.flatMap(_("users")).flatMap(_.asArray)
    assertEquals(users.map(_.size), Some(2))
  }

  test("AST: type checks") {
    assert(Json.Null.isNull)
    assert(!Json.Null.isBoolean)
    assert(Json.True.isBoolean)
    assert(Json.fromInt(1).isNumber)
    assert(Json.fromString("x").isString)
    assert(Json.arr().isArray)
    assert(Json.obj().isObject)
  }

  test("AST: fold") {
    val json: Json = Json.fromInt(42)
    val result = json.fold(
      onNull = "null",
      onBoolean = b => s"bool:$b",
      onNumber = n => s"num:${n.value}",
      onString = s => s"str:$s",
      onArray = a => s"arr:${a.size}",
      onObject = o => s"obj:${o.size}"
    )
    assertEquals(result, "num:42")
  }

  test("JsonObject: apply") {
    val obj = JsonObject("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
    assertEquals(obj("a"), Some(Json.fromInt(1)))
    assertEquals(obj("c"), None)
  }

  test("JsonObject: add and remove") {
    val obj = JsonObject.empty.add("x", Json.fromInt(1))
    assertEquals(obj("x"), Some(Json.fromInt(1)))
    val removed = obj.remove("x")
    assertEquals(removed("x"), None)
  }

  test("JsonObject: preserves insertion order") {
    val obj = JsonObject("z" -> Json.Null, "a" -> Json.Null, "m" -> Json.Null)
    assertEquals(obj.keys, Vector("z", "a", "m"))
  }

  test("JsonNumber: fromInt roundtrip") {
    val n = JsonNumber.fromInt(42)
    assertEquals(n.toInt, Some(42))
    assertEquals(n.toLong, Some(42L))
  }

  test("JsonNumber: fromString validation") {
    assert(JsonNumber.fromString("123").isDefined)
    assert(JsonNumber.fromString("3.14").isDefined)
    assert(JsonNumber.fromString("not-a-number").isEmpty)
  }

  test("JsonNumber: precision preservation") {
    val n = JsonNumber.fromBigDecimal(BigDecimal("12345678901234567890.12345678901234567890"))
    assertEquals(n.toBigDecimal, Some(BigDecimal("12345678901234567890.12345678901234567890")))
  }
}
