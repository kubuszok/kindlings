package hearth.kindlings.jsoniterjson

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.*
import hearth.Suite

final class JsonCodecSpec extends Suite {

  test("round-trip: null") {
    val json: Json = Json.Null
    val str = writeToString(json)
    str ==> "null"
    readFromString[Json](str) ==> json
  }

  test("round-trip: boolean true") {
    val json: Json = Json.True
    val str = writeToString(json)
    str ==> "true"
    readFromString[Json](str) ==> json
  }

  test("round-trip: boolean false") {
    val json: Json = Json.False
    val str = writeToString(json)
    str ==> "false"
    readFromString[Json](str) ==> json
  }

  test("round-trip: string") {
    val json: Json = Json.fromString("hello")
    val str = writeToString(json)
    str ==> "\"hello\""
    readFromString[Json](str) ==> json
  }

  test("round-trip: integer number") {
    val json: Json = Json.fromInt(42)
    val str = writeToString(json)
    str ==> "42"
    val decoded = readFromString[Json](str)
    decoded.asNumber.flatMap(_.toInt) ==> Some(42)
  }

  test("round-trip: long number") {
    val json: Json = Json.fromLong(9999999999L)
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    decoded.asNumber.flatMap(_.toLong) ==> Some(9999999999L)
  }

  test("round-trip: decimal number") {
    val json: Json = Json.fromBigDecimal(BigDecimal("3.14"))
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    decoded.asNumber.flatMap(_.toBigDecimal) ==> Some(BigDecimal("3.14"))
  }

  test("round-trip: empty array") {
    val json: Json = Json.arr()
    val str = writeToString(json)
    str ==> "[]"
    readFromString[Json](str) ==> json
  }

  test("round-trip: array of mixed values") {
    val json: Json = Json.arr(Json.fromInt(1), Json.fromString("two"), Json.True, Json.Null)
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    decoded.asArray.map(_.size) ==> Some(4)
  }

  test("round-trip: empty object") {
    val json: Json = Json.obj()
    val str = writeToString(json)
    str ==> "{}"
    readFromString[Json](str) ==> json
  }

  test("round-trip: simple object") {
    val json: Json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
    val str = writeToString(json)
    val decoded = readFromString[Json](str)
    decoded.asObject.flatMap(_("name")) ==> Some(Json.fromString("Alice"))
    decoded.asObject.flatMap(_("age")).flatMap(_.asNumber).flatMap(_.toInt) ==> Some(30)
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
    users.map(_.size) ==> Some(2)
  }

  test("AST: type checks") {
    Json.Null.isNull ==> true
    Json.Null.isBoolean ==> false
    Json.True.isBoolean ==> true
    Json.fromInt(1).isNumber ==> true
    Json.fromString("x").isString ==> true
    Json.arr().isArray ==> true
    Json.obj().isObject ==> true
  }

  test("AST: fold") {
    val json: Json = Json.fromInt(42)
    json.fold(
      onNull = "null",
      onBoolean = b => s"bool:$b",
      onNumber = n => s"num:${n.value}",
      onString = s => s"str:$s",
      onArray = a => s"arr:${a.size}",
      onObject = o => s"obj:${o.size}"
    ) ==> "num:42"
  }

  test("JsonObject: apply") {
    val obj = JsonObject("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
    obj("a") ==> Some(Json.fromInt(1))
    obj("c") ==> None
  }

  test("JsonObject: add and remove") {
    val obj = JsonObject.empty.add("x", Json.fromInt(1))
    obj("x") ==> Some(Json.fromInt(1))
    val removed = obj.remove("x")
    removed("x") ==> None
  }

  test("JsonObject: preserves insertion order") {
    val obj = JsonObject("z" -> Json.Null, "a" -> Json.Null, "m" -> Json.Null)
    obj.keys ==> Vector("z", "a", "m")
  }

  test("JsonNumber: fromInt roundtrip") {
    val n = JsonNumber.fromInt(42)
    n.toInt ==> Some(42)
    n.toLong ==> Some(42L)
  }

  test("JsonNumber: fromString validation") {
    JsonNumber.fromString("123").isDefined ==> true
    JsonNumber.fromString("3.14").isDefined ==> true
    JsonNumber.fromString("not-a-number").isEmpty ==> true
  }

  test("JsonNumber: precision preservation") {
    val n = JsonNumber.fromBigDecimal(BigDecimal("12345678901234567890.12345678901234567890"))
    n.toBigDecimal ==> Some(BigDecimal("12345678901234567890.12345678901234567890"))
  }
}
