package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import sttp.apispec._
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap
import scala.io.Source

/** Port of sttp-apispec's `openapi-circe` `threeone.EncoderTest`: build a model and assert its encoding equals the
  * golden resource (with sorted keys, as sttp's `spaces2SortKeys`). These are the authoritative checks for the OAS 3.0
  * vs 3.1 translations (const<->enum, type:[t,null]<->nullable, exclusive bound boolean<->numeric, examples<->example,
  * nullable $ref anyOf<->allOf+nullable).
  */
final class EncoderGoldenJvmSpec extends MacroSuite {

  private def sortKeys(json: Json): Json = json match {
    case Json.Obj(obj) =>
      Json.Obj(JsonObject(obj.fields.map { case (k, v) => (k, sortKeys(v)) }.sortBy(_._1)))
    case Json.Arr(vs) => Json.Arr(vs.map(sortKeys))
    case other        => other
  }

  private def loadAst(path: String): Json = {
    val src = Source.fromInputStream(getClass.getResourceAsStream(path), "UTF-8")
    try readFromString[Json](src.mkString)(jsonValueCodec)
    finally src.close()
  }

  private def assertEncodes(model: OpenAPI, path: String)(implicit codec: JsonValueCodec[OpenAPI]): Unit = {
    val ours = readFromString[Json](writeToString(model)(codec))(jsonValueCodec)
    sortKeys(ours) ==> sortKeys(loadAst(path))
  }

  private def schemaComponent(desc: String)(schema: Schema): (String, Schema) =
    desc -> schema.copy(description = Some(desc))

  private val fullSchemaOpenApi = {
    val components = Components(
      schemas = ListMap(
        schemaComponent("type 'null'")(Schema(SchemaType.Null)),
        schemaComponent("nullable string")(Schema(SchemaType.String, SchemaType.Null)),
        schemaComponent("nullable reference")(Schema.referenceTo("#/components/schemas/", "Foo").nullable),
        schemaComponent("nullable enum")(Schema(`enum` = Some(List("a", "b", null).map(ExampleSingleValue(_))))),
        schemaComponent("single example")(
          Schema(SchemaType.String).copy(examples = Some(List(ExampleSingleValue("exampleValue"))))
        ),
        schemaComponent("multi valued example")(
          Schema(SchemaType.Array).copy(examples = Some(List(ExampleMultipleValue(List("ex1", "ex1")))))
        ),
        schemaComponent("object with example")(
          Schema(SchemaType.Object).copy(examples = Some(List(ExampleSingleValue("""{"a": 1, "b": null}"""))))
        ),
        schemaComponent("min/max")(Schema(minimum = Some(BigDecimal(10)), maximum = Some(BigDecimal(20)))),
        schemaComponent("exclusive min/max")(
          Schema(exclusiveMinimum = Some(BigDecimal(10)), exclusiveMaximum = Some(BigDecimal(20)))
        ),
        schemaComponent("exclusiveMinimum false")(Schema(minimum = Some(BigDecimal(10)))),
        schemaComponent("array")(Schema(SchemaType.Array).copy(items = Some(Schema(SchemaType.String)))),
        schemaComponent("array with unique items")(Schema(SchemaType.Array).copy(uniqueItems = Some(true))),
        schemaComponent("const")(Schema(const = Some("const1").map(ExampleSingleValue(_)))),
        schemaComponent("enum")(Schema(`enum` = Some(List("enum1", "enum2").map(ExampleSingleValue(_)))))
      )
    )
    OpenAPI(info = Info(title = "API", version = "1.0.0"), components = Some(components))
  }

  test("full 3.1 schema") {
    import OpenApiJsoniter.circe._
    val schemas31 = ListMap(
      schemaComponent("multiple examples")(
        Schema(SchemaType.String).copy(examples = Some(List("ex1", "ex2").map(ExampleSingleValue(_))))
      )
    )
    val model = fullSchemaOpenApi.copy(
      components = fullSchemaOpenApi.components.map(c => c.copy(schemas = c.schemas ++ schemas31))
    )
    assertEncodes(model, "/spec/3.1/schema.json")
  }

  test("full 3.0 schema") {
    import OpenApiJsoniter.circe_openapi_3_0_3._
    assertEncodes(fullSchemaOpenApi.copy(openapi = "3.0.1"), "/spec/3.0/schema.json")
  }

  test("replace const by single enum value in 3.0 schema") {
    import OpenApiJsoniter.circe_openapi_3_0_3._
    val components = Components(
      schemas = ListMap(
        schemaComponent("const and enum")(
          Schema(
            const = Some("const1").map(ExampleSingleValue(_)),
            `enum` = Some(List("enum1", "enum2").map(ExampleSingleValue(_)))
          )
        )
      )
    )
    assertEncodes(fullSchemaOpenApi.copy(openapi = "3.0.1", components = Some(components)), "/spec/3.0/const_and_enum.json")
  }
}
