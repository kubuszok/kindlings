package hearth.kindlings.tapiropenapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import io.circe.generic.auto.*
import sttp.apispec.openapi.{Info, OpenAPI}
import sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*

/** The module's headline promise, exercised end-to-end through [[TapirOpenApi]]: drive tapir's `OpenAPIDocsInterpreter`
  * (via the bridge) to produce an `sttp.apispec.openapi.OpenAPI`, serialise it with OUR jsoniter codec, and assert the
  * bytes equal circe's `encoderOpenAPI(doc).noSpaces` AND that it round-trips back to an equal model. Scenarios ported
  * from tapir's `VerifyYaml*Test` suites.
  *
  * `openapi-circe` (the cross-check baseline) is a JVM-only test dep, hence `scalajvm/`; the `TapirOpenApi` bridge
  * itself is compiled on JVM + Scala.js.
  */
final class TapirInteropJvmSpec extends MacroSuite {

  private object circe extends SttpOpenAPICirceEncoders

  private def ast(s: String): Json = readFromString[Json](s)(jsonValueCodec)

  /** Assert the bridge's serialisation equals circe's, byte-for-byte, and round-trips to the same model. */
  private def crossCheckAndRoundTrip(doc: OpenAPI): Unit = {
    import OpenApiJsoniter.openapi_3_1.openAPICodec
    val ours = TapirOpenApi.toJson(doc)
    ast(ours) ==> ast(circe.encoderOpenAPI(doc).noSpaces)
    readFromString[OpenAPI](ours)(openAPICodec) ==> doc
  }

  test("GET with a query parameter") {
    val e = endpoint.get
      .in("items")
      .in(query[String]("color"))
      .in(query[Option[Int]]("limit"))
      .out(stringBody)
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(e, Info("Query API", "1.0")))
  }

  test("one-call `endpointsToJson` matches the two-step build-then-serialise path") {
    val e = endpoint.get.in("ping").out(stringBody)
    val info = Info("Ping API", "1.0")
    TapirOpenApi.endpointToJson(e, info) ==> TapirOpenApi.toJson(TapirOpenApi.toOpenAPI(e, info))
    TapirOpenApi.endpointsToJson(List(e), info) ==> TapirOpenApi.toJson(TapirOpenApi.toOpenAPI(List(e), info))
  }

  test("POST with a JSON body and JSON output") {
    val e = endpoint.post
      .in("fruits")
      .in(jsonBody[TapirInteropJvmSpec.FruitAmount])
      .out(jsonBody[TapirInteropJvmSpec.FruitAmount])
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(e, Info("Fruit API", "1.0")))
  }

  test("oneOf output (sealed trait coproduct)") {
    import TapirInteropJvmSpec.*
    implicit val entitySchema: Schema[Entity] =
      Schema.oneOfUsingField[Entity, String](_.kind, identity)(
        "person" -> Schema.derived[Person],
        "org" -> Schema.derived[Organization]
      )
    val e = endpoint.get.in("entities").out(jsonBody[Entity])
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(e, Info("Entity API", "1.0")))
  }

  test("secured endpoint (bearer + apiKey)") {
    val e1 = endpoint.securityIn(auth.bearer[String]()).in("secure1" / path[String]).out(stringBody)
    val e2 = endpoint.securityIn(auth.apiKey(header[String]("X-Api-Key"))).in("secure2").out(stringBody)
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(List(e1, e2), Info("Secure API", "1.0")))
  }

  // Note: tapir's `multipartBody[T]` derivation macro hits an upstream splice-scope bug on Scala 3
  // (MultipartCodecMacros), unrelated to our codec; the multipart `Encoding` encoder branch is instead exercised
  // by HandBuiltDocs.fullBranchDoc in the circe cross-check.

  test("multiple endpoints with shared schema components") {
    import TapirInteropJvmSpec.*
    val list = endpoint.get.in("fruits").out(jsonBody[List[FruitAmount]])
    val create = endpoint.post.in("fruits").in(jsonBody[FruitAmount]).out(jsonBody[FruitAmount])
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(List(list, create), Info("Fruits", "1.0")))
  }

  test("path with multiple segments, header input, status output") {
    val e = endpoint.get
      .in("api" / "v1" / "users" / path[Int]("userId"))
      .in(header[String]("X-Trace"))
      .out(jsonBody[TapirInteropJvmSpec.FruitAmount])
      .errorOut(stringBody)
    crossCheckAndRoundTrip(TapirOpenApi.toOpenAPI(e, Info("Users API", "1.0")))
  }
}

object TapirInteropJvmSpec {
  final case class FruitAmount(fruit: String, amount: Int)

  sealed trait Entity { def kind: String }
  final case class Person(name: String, age: Int) extends Entity { def kind: String = "person" }
  final case class Organization(orgName: String) extends Entity { def kind: String = "org" }
}
