package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import sttp.apispec.*
import sttp.apispec.openapi.*

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

  private val fullSchemaOpenApi = HandBuiltDocs.fullSchemaOpenApi

  test("full 3.1 schema") {
    import OpenApiJsoniter.circe.*
    assertEncodes(HandBuiltDocs.fullSchemaOpenApi31, "/spec/3.1/schema.json")
  }

  test("full 3.0 schema") {
    import OpenApiJsoniter.circe_openapi_3_0_3.*
    assertEncodes(HandBuiltDocs.fullSchemaOpenApi30, "/spec/3.0/schema.json")
  }

  test("replace const by single enum value in 3.0 schema") {
    import OpenApiJsoniter.circe_openapi_3_0_3.*
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
    assertEncodes(
      fullSchemaOpenApi.copy(openapi = "3.0.1", components = Some(components)),
      "/spec/3.0/const_and_enum.json"
    )
  }
}
