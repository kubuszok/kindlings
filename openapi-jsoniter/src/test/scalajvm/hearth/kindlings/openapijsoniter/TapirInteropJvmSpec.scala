package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import io.circe.generic.auto._
import sttp.apispec.openapi.{Info, OpenAPI}
import sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
import sttp.tapir._
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._

/** The module's headline promise: drive tapir's `OpenAPIDocsInterpreter` to produce an `sttp.apispec.openapi.OpenAPI`,
  * serialise it with OUR jsoniter codec, and assert the bytes equal circe's `encoderOpenAPI(doc).noSpaces` AND that it
  * round-trips back to an equal model. Scenarios ported from tapir's `VerifyYaml*Test` suites.
  *
  * tapir + openapi-circe are JVM-only test deps, hence `scalajvm/`.
  */
final class TapirInteropJvmSpec extends MacroSuite {

  private object circe extends SttpOpenAPICirceEncoders

  private def ast(s: String): Json = readFromString[Json](s)(jsonValueCodec)

  /** Assert our serialisation equals circe's, byte-for-byte, and round-trips to the same model. */
  private def crossCheckAndRoundTrip(doc: OpenAPI): Unit = {
    import OpenApiJsoniter.circe._
    val ours = writeToString(doc)(openAPICodec)
    ast(ours) ==> ast(circe.encoderOpenAPI(doc).noSpaces)
    readFromString[OpenAPI](ours)(openAPICodec) ==> doc
  }

  private val interpreter = OpenAPIDocsInterpreter()

  test("GET with a query parameter") {
    val e = endpoint.get
      .in("items")
      .in(query[String]("color"))
      .in(query[Option[Int]]("limit"))
      .out(stringBody)
    crossCheckAndRoundTrip(interpreter.toOpenAPI(e, Info("Query API", "1.0")))
  }

  test("POST with a JSON body and JSON output") {
    val e = endpoint.post
      .in("fruits")
      .in(jsonBody[TapirInteropJvmSpec.FruitAmount])
      .out(jsonBody[TapirInteropJvmSpec.FruitAmount])
    crossCheckAndRoundTrip(interpreter.toOpenAPI(e, Info("Fruit API", "1.0")))
  }

  test("oneOf output (sealed trait coproduct)") {
    import TapirInteropJvmSpec._
    implicit val entitySchema: Schema[Entity] =
      Schema.oneOfUsingField[Entity, String](_.kind, identity)("person" -> Schema.derived[Person], "org" -> Schema.derived[Organization])
    val e = endpoint.get.in("entities").out(jsonBody[Entity])
    crossCheckAndRoundTrip(interpreter.toOpenAPI(e, Info("Entity API", "1.0")))
  }

  test("secured endpoint (bearer + apiKey)") {
    val e1 = endpoint.securityIn(auth.bearer[String]()).in("secure1" / path[String]).out(stringBody)
    val e2 = endpoint.securityIn(auth.apiKey(header[String]("X-Api-Key"))).in("secure2").out(stringBody)
    crossCheckAndRoundTrip(interpreter.toOpenAPI(List(e1, e2), Info("Secure API", "1.0")))
  }

  // Note: tapir's `multipartBody[T]` derivation macro hits an upstream splice-scope bug on Scala 3
  // (MultipartCodecMacros), unrelated to our codec; the multipart `Encoding` encoder branch is instead exercised
  // by HandBuiltDocs.fullBranchDoc in the circe cross-check.

  test("multiple endpoints with shared schema components") {
    import TapirInteropJvmSpec._
    val list = endpoint.get.in("fruits").out(jsonBody[List[FruitAmount]])
    val create = endpoint.post.in("fruits").in(jsonBody[FruitAmount]).out(jsonBody[FruitAmount])
    crossCheckAndRoundTrip(interpreter.toOpenAPI(List(list, create), Info("Fruits", "1.0")))
  }

  test("path with multiple segments, header input, status output") {
    val e = endpoint.get
      .in("api" / "v1" / "users" / path[Int]("userId"))
      .in(header[String]("X-Trace"))
      .out(jsonBody[TapirInteropJvmSpec.FruitAmount])
      .errorOut(stringBody)
    crossCheckAndRoundTrip(interpreter.toOpenAPI(e, Info("Users API", "1.0")))
  }
}

object TapirInteropJvmSpec {
  final case class FruitAmount(fruit: String, amount: Int)

  sealed trait Entity { def kind: String }
  final case class Person(name: String, age: Int) extends Entity { def kind: String = "person" }
  final case class Organization(orgName: String) extends Entity { def kind: String = "org" }
}
