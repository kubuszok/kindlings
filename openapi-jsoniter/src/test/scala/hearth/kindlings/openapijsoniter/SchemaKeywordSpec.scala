package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec._

import scala.collection.immutable.ListMap

/** Encode-string + round-trip assertions for the JSON-Schema keywords that the existing specs leave untouched:
  * `pattern`, `multipleOf`, `allOf/anyOf/oneOf/not`, `if/then/else`, `prefixItems/contains/unevaluatedItems`,
  * `propertyNames`, `dependentRequired/dependentSchemas`, `$vocabulary/$defs`, `maxLength/minLength`,
  * `maxItems/minItems/maxContains/minContains`, `maxProperties/minProperties`, `patternProperties`,
  * `additionalProperties` (object vs boolean), `unevaluatedProperties`, `readOnly/writeOnly/deprecated`,
  * `$anchor/$dynamicAnchor/$dynamicRef/$comment/$id`. Cross-platform (no circe/tapir).
  */
final class SchemaKeywordSpec extends MacroSuite {

  import OpenApiJsoniter.circe._

  private def enc(s: Schema): String = writeToString(s)(schemaCodec)
  private def roundTrip(s: Schema): Unit = {
    val json = enc(s)
    readFromString[Schema](json)(schemaCodec) ==> s
  }

  private val strSchema = Schema(`type` = Some(List(SchemaType.String)))
  private val intSchema = Schema(`type` = Some(List(SchemaType.Integer)))

  test("pattern") {
    val s = Schema(`type` = Some(List(SchemaType.String)), pattern = Some(Pattern("^[a-z]+$")))
    enc(s) ==> """{"type":"string","pattern":"^[a-z]+$"}"""
    roundTrip(s)
  }

  test("multipleOf") {
    val s = Schema(`type` = Some(List(SchemaType.Number)), multipleOf = Some(BigDecimal("2.5")))
    enc(s) ==> """{"type":"number","multipleOf":2.5}"""
    roundTrip(s)
  }

  test("allOf / anyOf / oneOf / not") {
    val s = Schema(
      allOf = List(Schema($ref = Some("#/A")), Schema($ref = Some("#/B"))),
      anyOf = List(strSchema, intSchema),
      oneOf = List(Schema($ref = Some("#/C"))),
      not = Some(strSchema)
    )
    roundTrip(s)
    val json = enc(s)
    json.contains("\"allOf\"") ==> true
    json.contains("\"not\"") ==> true
  }

  test("if / then / else") {
    val s = Schema(
      `if` = Some(Schema(`type` = Some(List(SchemaType.String)))),
      `then` = Some(intSchema),
      `else` = Some(strSchema)
    )
    val json = enc(s)
    json.contains("\"if\"") ==> true
    json.contains("\"then\"") ==> true
    json.contains("\"else\"") ==> true
    roundTrip(s)
  }

  test("prefixItems / contains / unevaluatedItems / maxContains / minContains") {
    val s = Schema(
      `type` = Some(List(SchemaType.Array)),
      prefixItems = Some(List(strSchema, intSchema)),
      contains = Some(strSchema),
      unevaluatedItems = Some(intSchema),
      maxContains = Some(3),
      minContains = Some(1)
    )
    roundTrip(s)
    enc(s).contains("\"prefixItems\"") ==> true
  }

  test("propertyNames / patternProperties / additionalProperties (object) / unevaluatedProperties") {
    val s = Schema(
      `type` = Some(List(SchemaType.Object)),
      propertyNames = Some(Schema(pattern = Some(Pattern("^x-")))),
      patternProperties = ListMap(Pattern("^[0-9]+$") -> intSchema),
      additionalProperties = Some(strSchema),
      unevaluatedProperties = Some(Schema(SchemaType.Boolean))
    )
    roundTrip(s)
    enc(s).contains("\"patternProperties\"") ==> true
    enc(s).contains("\"propertyNames\"") ==> true
  }

  test("additionalProperties boolean form (AnySchema)") {
    val sFalse = Schema(`type` = Some(List(SchemaType.Object)), additionalProperties = Some(AnySchema.Nothing))
    enc(sFalse) ==> """{"type":"object","additionalProperties":false}"""
    roundTrip(sFalse)
    val sTrue = Schema(`type` = Some(List(SchemaType.Object)), additionalProperties = Some(AnySchema.Anything))
    enc(sTrue) ==> """{"type":"object","additionalProperties":true}"""
    roundTrip(sTrue)
  }

  test("dependentRequired / dependentSchemas") {
    val s = Schema(
      `type` = Some(List(SchemaType.Object)),
      dependentRequired = ListMap("credit_card" -> List("billing_address")),
      dependentSchemas = ListMap("credit_card" -> Schema(required = List("billing_address")))
    )
    roundTrip(s)
    enc(s).contains("\"dependentRequired\"") ==> true
    enc(s).contains("\"dependentSchemas\"") ==> true
  }

  test("$vocabulary / $defs encode") {
    val s = Schema(
      $schema = Some("https://json-schema.org/draft/2020-12/schema"),
      $vocabulary = Some(ListMap("https://json-schema.org/draft/2020-12/vocab/core" -> true)),
      $defs = Some(ListMap("Address" -> Schema(`type` = Some(List(SchemaType.Object)))))
    )
    roundTrip(s)
    val json = enc(s)
    json.contains("\"$vocabulary\"") ==> true
    json.contains("\"$defs\"") ==> true
  }

  test("string assertions: maxLength / minLength") {
    val s = Schema(`type` = Some(List(SchemaType.String)), maxLength = Some(10), minLength = Some(2))
    enc(s) ==> """{"type":"string","maxLength":10,"minLength":2}"""
    roundTrip(s)
  }

  test("array assertions: maxItems / minItems / uniqueItems / items") {
    val s = Schema(
      `type` = Some(List(SchemaType.Array)),
      items = Some(strSchema),
      maxItems = Some(5),
      minItems = Some(1),
      uniqueItems = Some(true)
    )
    roundTrip(s)
  }

  test("object assertions: maxProperties / minProperties") {
    val s = Schema(`type` = Some(List(SchemaType.Object)), maxProperties = Some(10), minProperties = Some(1))
    enc(s) ==> """{"type":"object","maxProperties":10,"minProperties":1}"""
    roundTrip(s)
  }

  test("annotations: readOnly / writeOnly / deprecated / default") {
    val s = Schema(
      `type` = Some(List(SchemaType.String)),
      readOnly = Some(true),
      writeOnly = Some(false),
      deprecated = Some(true),
      default = Some(ExampleSingleValue("x"))
    )
    roundTrip(s)
    val json = enc(s)
    json.contains("\"readOnly\"") ==> true
    json.contains("\"writeOnly\"") ==> true
    json.contains("\"deprecated\"") ==> true
    json.contains("\"default\"") ==> true
  }

  test("identifier keywords: $id / $anchor / $dynamicAnchor / $dynamicRef / $comment") {
    val s = Schema(
      $id = Some("https://example.com/schema"),
      $anchor = Some("anchor1"),
      $dynamicAnchor = Some("dyn1"),
      $dynamicRef = Some("#dyn1"),
      $comment = Some("a comment")
    )
    roundTrip(s)
    val json = enc(s)
    json.contains("\"$id\"") ==> true
    json.contains("\"$anchor\"") ==> true
    json.contains("\"$dynamicAnchor\"") ==> true
    json.contains("\"$dynamicRef\"") ==> true
    json.contains("\"$comment\"") ==> true
  }

  test("title / format / minimum-as-exclusiveMinimum-false combination") {
    val s = Schema(
      `type` = Some(List(SchemaType.Integer)),
      title = Some("an int"),
      format = Some("int32"),
      minimum = Some(BigDecimal(0))
    )
    enc(s) ==> """{"title":"an int","type":"integer","format":"int32","minimum":0}"""
    roundTrip(s)
  }
}
