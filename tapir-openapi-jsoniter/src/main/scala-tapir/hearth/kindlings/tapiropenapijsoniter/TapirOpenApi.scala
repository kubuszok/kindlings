package hearth.kindlings.tapiropenapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import sttp.apispec.openapi.{Info, OpenAPI}
import sttp.tapir.AnyEndpoint
import sttp.tapir.docs.openapi.{OpenAPIDocsInterpreter, OpenAPIDocsOptions}

/** The module's headline feature: turn tapir endpoints into an OpenAPI document and serialize it to JSON **without
  * pulling in circe** — unlike `sttp-apispec`'s `openapi-circe` (which `tapir-openapi-docs` users normally reach for),
  * serialization here goes through this module's hand-written jsoniter codecs.
  *
  * The pipeline is `tapir endpoints -> sttp.apispec.openapi.OpenAPI -> JSON`: the first arrow is tapir's own
  * [[OpenAPIDocsInterpreter]] (re-exposed here for convenience), the second is [[OpenApiJsoniter]]'s circe-free codec.
  * Compiled on JVM + Scala.js only (where `tapir-openapi-docs` is published); the underlying codecs remain
  * cross-platform (including Scala Native).
  */
object TapirOpenApi {

  private val interpreter: OpenAPIDocsInterpreter = OpenAPIDocsInterpreter()

  /** Build the OpenAPI 3.1 model from a single endpoint (delegates to tapir's interpreter). */
  def toOpenAPI(endpoint: AnyEndpoint, info: Info): OpenAPI =
    interpreter.toOpenAPI(endpoint, info)

  /** Build the OpenAPI 3.1 model from a single endpoint, given a title and version. */
  def toOpenAPI(endpoint: AnyEndpoint, title: String, version: String): OpenAPI =
    interpreter.toOpenAPI(endpoint, title, version)

  /** Build the OpenAPI 3.1 model from several endpoints (delegates to tapir's interpreter). */
  def toOpenAPI(endpoints: Iterable[AnyEndpoint], info: Info): OpenAPI =
    interpreter.toOpenAPI(endpoints, info)

  /** Build the OpenAPI 3.1 model from several endpoints, given a title and version. */
  def toOpenAPI(endpoints: Iterable[AnyEndpoint], title: String, version: String): OpenAPI =
    interpreter.toOpenAPI(endpoints, title, version)

  /** Serialize an OpenAPI model to JSON via this module's jsoniter codec (OpenAPI 3.1, circe-free). */
  def toJson(doc: OpenAPI): String =
    writeToString(doc)(OpenApiJsoniter.circe.openAPICodec)

  /** Serialize an OpenAPI model to JSON via this module's jsoniter codec, using the OpenAPI 3.0.3 translation. */
  def toJson30(doc: OpenAPI): String =
    writeToString(doc)(OpenApiJsoniter.circe_openapi_3_0_3.openAPICodec)

  /** One call: tapir endpoint -> OpenAPI 3.1 JSON, with no circe on the classpath. */
  def endpointToJson(endpoint: AnyEndpoint, info: Info): String =
    toJson(toOpenAPI(endpoint, info))

  /** One call: tapir endpoints -> OpenAPI 3.1 JSON, with no circe on the classpath. */
  def endpointsToJson(endpoints: Iterable[AnyEndpoint], info: Info): String =
    toJson(toOpenAPI(endpoints, info))

  /** Use a custom [[OpenAPIDocsInterpreter]] (e.g. with non-default [[OpenAPIDocsOptions]]) when building the model. */
  def withOptions(options: OpenAPIDocsOptions): TapirOpenApiInterpreter =
    new TapirOpenApiInterpreter(OpenAPIDocsInterpreter(options))
}

/** A [[TapirOpenApi]]-style facade bound to a specific [[OpenAPIDocsInterpreter]] (see [[TapirOpenApi.withOptions]]). */
final class TapirOpenApiInterpreter(interpreter: OpenAPIDocsInterpreter) {

  def toOpenAPI(endpoint: AnyEndpoint, info: Info): OpenAPI = interpreter.toOpenAPI(endpoint, info)
  def toOpenAPI(endpoints: Iterable[AnyEndpoint], info: Info): OpenAPI = interpreter.toOpenAPI(endpoints, info)

  def endpointToJson(endpoint: AnyEndpoint, info: Info): String = TapirOpenApi.toJson(toOpenAPI(endpoint, info))
  def endpointsToJson(endpoints: Iterable[AnyEndpoint], info: Info): String =
    TapirOpenApi.toJson(toOpenAPI(endpoints, info))
}
