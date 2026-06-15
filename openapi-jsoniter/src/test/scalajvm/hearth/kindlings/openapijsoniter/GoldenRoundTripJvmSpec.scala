package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import sttp.apispec.{AnySchema, Schema, SecurityScheme}
import sttp.apispec.openapi.{Components, OpenAPI}

import scala.collection.immutable.ListMap

import scala.io.Source

/** Golden-fixture tests driven by the real sttp-apispec resource JSON files. Resource IO is JVM-only, hence this lives
  * in `scalajvm/`.
  *
  * Two flavours, matching how sttp-apispec itself tests:
  *   - decode -> encode -> compare (sorted keys) for fixtures that are encoder fixpoints (petstore, the 3.1 schema,
  *     callbacks, the boolean AnySchema spec).
  *   - decode-only for fixtures that carry non-model keys or use lossy `Any`-typed values (self-describing schema,
  *     recursive `$defs`).
  */
final class GoldenRoundTripJvmSpec extends MacroSuite {

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

  private def roundTrip[A](path: String)(implicit codec: JsonValueCodec[A]): Unit = {
    val original = loadAst(path)
    val originalStr = writeToString(original)(jsonValueCodec)
    val model = readFromString[A](originalStr)(codec)
    val reencoded = readFromString[Json](writeToString(model)(codec))(jsonValueCodec)
    sortKeys(reencoded) ==> sortKeys(original)
  }

  group("OpenAPI 3.1 encoder fixpoints") {
    import OpenApiJsoniter.circe._
    test("basic petstore")(roundTrip[OpenAPI]("/petstore/basic-petstore.json"))
    test("header petstore")(roundTrip[OpenAPI]("/petstore/header-petstore.json"))
    test("full 3.1 schema")(roundTrip[OpenAPI]("/spec/3.1/schema.json"))
    test("any and nothing (boolean)")(roundTrip[OpenAPI]("/spec/3.1/any_and_nothing1.json"))
  }

  group("OpenAPI 3.0.3 encoder fixpoints") {
    import OpenApiJsoniter.circe_openapi_3_0_3._
    test("callbacks")(roundTrip[OpenAPI]("/callbacks/callbacks.json"))
  }

  group("AnySchema object encoding") {
    import OpenApiJsoniter.circeObjectAnySchema._
    test("any and nothing (object)")(roundTrip[OpenAPI]("/spec/3.1/any_and_nothing2.json"))
  }

  group("security schemes") {
    import OpenApiJsoniter.circe._
    test("with scopes")(roundTrip[SecurityScheme]("/securityScheme/security-scheme-with-scopes.json"))
    test("with empty scopes")(roundTrip[SecurityScheme]("/securityScheme/security-scheme-with-empty-scopes.json"))
  }

  group("decode-only fixtures") {
    import OpenApiJsoniter.circe._

    test("self-describing schema decodes") {
      val json = loadAst("/self-describing-schema.json")
      val model = readFromString[Schema](writeToString(json)(jsonValueCodec))(schemaCodec)
      model.description ==> Some("Meta-schema for self-describing JSON schema")
    }

    test("extending-recursive schema decodes with $defs") {
      val json = loadAst("/extending-recursive.json")
      val model = readFromString[Schema](writeToString(json)(jsonValueCodec))(schemaCodec)
      model.$defs.exists(_.contains("address")) ==> true
    }
  }

  // Ported from sttp-apispec openapi-circe DecoderTest.
  group("decode semantics (ported from sttp-apispec)") {
    import OpenApiJsoniter.circe._

    def decodeOpenAPI(path: String): OpenAPI =
      readFromString[OpenAPI](writeToString(loadAst(path))(jsonValueCodec))(openAPICodec)
    def decodeSecurityScheme(path: String): SecurityScheme =
      readFromString[SecurityScheme](writeToString(loadAst(path))(jsonValueCodec))(securitySchemeCodec)

    test("petstore deserialize") {
      decodeOpenAPI("/petstore/basic-petstore.json").info.description ==>
        Some("This is a sample server for a pet store.")
    }

    test("any/nothing schema (boolean form)") {
      val schemas = decodeOpenAPI("/spec/3.1/any_and_nothing1.json").components.getOrElse(Components.Empty).schemas
      schemas("anything_boolean") ==> AnySchema.Anything
      schemas("nothing_boolean") ==> AnySchema.Nothing
    }

    test("any/nothing schema (object form)") {
      val schemas = decodeOpenAPI("/spec/3.1/any_and_nothing2.json").components.getOrElse(Components.Empty).schemas
      schemas("anything_object") ==> AnySchema.Anything
      schemas("nothing_object") ==> AnySchema.Nothing
    }

    test("all 3.1 schema types decode (15 schemas)") {
      decodeOpenAPI("/spec/3.1/schema.json").components.getOrElse(Components.Empty).schemas.size ==> 15
    }

    test("all 3.0 schema types decode (14 schemas)") {
      decodeOpenAPI("/spec/3.0/schema.json").components.getOrElse(Components.Empty).schemas.size ==> 14
    }

    test("security scheme with scopes") {
      val ss = decodeSecurityScheme("/securityScheme/security-scheme-with-scopes.json")
      ss.flows.flatMap(_.clientCredentials.map(_.tokenUrl)) ==> Some(Some("openapi-circe-token"))
      ss.flows.flatMap(_.clientCredentials.map(_.scopes)) ==> Some(ListMap("example" -> "description"))
    }

    test("security scheme with empty scopes") {
      val ss = decodeSecurityScheme("/securityScheme/security-scheme-with-empty-scopes.json")
      ss.flows.flatMap(_.clientCredentials.map(_.scopes)) ==> Some(ListMap.empty[String, String])
    }

    test("callbacks decode") {
      val openapi =
        readFromString[OpenAPI](writeToString(loadAst("/callbacks/callbacks.json"))(jsonValueCodec))(
          OpenApiJsoniter.circe_openapi_3_0_3.openAPICodec
        )
      val callback = openapi.paths
        .pathItems("/pets")
        .post
        .flatMap(_.callbacks.get("onPetStatusChange"))
        .get
      callback.toOption.get.pathItems.contains("{$request.body#/callbackUrl}") ==> true
    }
  }
}
