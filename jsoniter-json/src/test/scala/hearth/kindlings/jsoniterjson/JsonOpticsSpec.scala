package hearth.kindlings.jsoniterjson

import hearth.kindlings.jsoniterjson.optics.{JsonOptic, JsonPath}
import munit.FunSuite

final class JsonOpticsSpec extends FunSuite {

  val sampleJson: Json = Json.obj(
    "name" -> Json.fromString("Alice"),
    "age" -> Json.fromInt(30),
    "address" -> Json.obj(
      "street" -> Json.fromString("123 Main St"),
      "city" -> Json.fromString("Springfield")
    ),
    "scores" -> Json.arr(Json.fromInt(95), Json.fromInt(87), Json.fromInt(92))
  )

  test("field: get existing field") {
    val result = JsonOptic.field("name").get(sampleJson)
    assertEquals(result, Some(Json.fromString("Alice")))
  }

  test("field: get missing field") {
    val result = JsonOptic.field("missing").get(sampleJson)
    assertEquals(result, None)
  }

  test("field: modify field") {
    val modified = JsonOptic.field("name").set(Json.fromString("Bob"))(sampleJson)
    assertEquals(JsonOptic.field("name").get(modified), Some(Json.fromString("Bob")))
  }

  test("index: get element") {
    val scores = JsonOptic.field("scores").get(sampleJson).get
    val result = JsonOptic.index(0).get(scores)
    assertEquals(result, Some(Json.fromInt(95)))
  }

  test("index: get out of bounds") {
    val scores = JsonOptic.field("scores").get(sampleJson).get
    val result = JsonOptic.index(99).get(scores)
    assertEquals(result, None)
  }

  test("index: modify element") {
    val scores = JsonOptic.field("scores").get(sampleJson).get
    val modified = JsonOptic.index(1).set(Json.fromInt(100))(scores)
    assertEquals(JsonOptic.index(1).get(modified), Some(Json.fromInt(100)))
  }

  test("composed: field then field") {
    val optic = JsonOptic.field("address").andThen(JsonOptic.field("city"))
    assertEquals(optic.get(sampleJson), Some(Json.fromString("Springfield")))
  }

  test("composed: field then index") {
    val optic = JsonOptic.field("scores").andThen(JsonOptic.index(2))
    assertEquals(optic.get(sampleJson), Some(Json.fromInt(92)))
  }

  test("composed: modify deeply nested") {
    val optic = JsonOptic.field("address").andThen(JsonOptic.field("city"))
    val modified = optic.set(Json.fromString("Shelbyville"))(sampleJson)
    assertEquals(optic.get(modified), Some(Json.fromString("Shelbyville")))
  }

  test("each: getAll from array") {
    val scores = JsonOptic.field("scores").get(sampleJson).get
    val all = JsonOptic.each.getAll(scores)
    assertEquals(all.size, 3)
  }

  test("each: modify all elements") {
    val scores = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    val modified = JsonOptic.each.modify(_ => Json.fromInt(0))(scores)
    assertEquals(modified, Json.arr(Json.fromInt(0), Json.fromInt(0), Json.fromInt(0)))
  }

  test("JsonPath: chained field access") {
    val result = JsonPath.root.field("address").field("street").get(sampleJson)
    assertEquals(result, Some(Json.fromString("123 Main St")))
  }

  test("JsonPath: field then index access") {
    val result = JsonPath.root.field("scores").index(0).get(sampleJson)
    assertEquals(result, Some(Json.fromInt(95)))
  }

  test("JsonPath: modify via path") {
    val modified = JsonPath.root.field("address").field("city").set(Json.fromString("Capital City"))(sampleJson)
    val result = JsonPath.root.field("address").field("city").get(modified)
    assertEquals(result, Some(Json.fromString("Capital City")))
  }

  test("JsonPath: get from root") {
    val result = JsonPath.root.get(sampleJson)
    assertEquals(result, Some(sampleJson))
  }
}
