package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import sttp.apispec.openapi.OpenAPI
import java.util.concurrent.TimeUnit

/** Encode/decode throughput of an OpenAPI document with `kindlings-tapir-openapi-jsoniter` (jsoniter, circe-free) vs.
  * `sttp-apispec`'s own `openapi-circe` codecs — the baseline this module is meant to replace. The document is
  * generated once from tapir endpoints via the module's `TapirOpenApi` bridge.
  */
object OpenApiBenchData {
  import sttp.tapir.*
  import sttp.apispec.openapi.Info
  import hearth.kindlings.tapiropenapijsoniter.TapirOpenApi

  private val endpoints: List[AnyEndpoint] = List(
    endpoint.get.in("users" / path[Int]("id")).in(query[Option[String]]("filter")).out(stringBody),
    endpoint.post.in("users").in(header[String]("X-Token")).out(stringBody).errorOut(stringBody),
    endpoint.get.in("health").out(stringBody),
    endpoint.delete.in("users" / path[Int]("id")).out(stringBody),
    endpoint.put.in("config").in(query[String]("key")).in(query[String]("value")).out(stringBody)
  )

  val doc: OpenAPI = TapirOpenApi.toOpenAPI(endpoints, Info("Bench API", "1.0"))
  val json: String = TapirOpenApi.toJson(doc)
}

/** kindlings-tapir-openapi-jsoniter — jsoniter codecs, no circe. */
object KindlingsOpenApiInstances {
  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import hearth.kindlings.tapiropenapijsoniter.OpenApiJsoniter

  implicit private val codec: JsonValueCodec[OpenAPI] = OpenApiJsoniter.circe.openAPICodec

  def encode(doc: OpenAPI): String = writeToString(doc)
  def decode(json: String): OpenAPI = readFromString[OpenAPI](json)
}

/** sttp-apispec `openapi-circe` baseline. */
object OriginalOpenApiCirceInstances
    extends sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
    with sttp.apispec.openapi.circe.SttpOpenAPICirceDecoders {

  def encode(doc: OpenAPI): String = encoderOpenAPI(doc).noSpaces
  def decode(json: String): OpenAPI =
    io.circe.parser.decode[OpenAPI](json)(openAPIDecoder).fold(throw _, identity)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class TapirOpenApiJsoniterBenchmark {

  @Benchmark def kindlingsEncode(): String = KindlingsOpenApiInstances.encode(OpenApiBenchData.doc)
  @Benchmark def circeEncode(): String = OriginalOpenApiCirceInstances.encode(OpenApiBenchData.doc)

  @Benchmark def kindlingsDecode(): OpenAPI = KindlingsOpenApiInstances.decode(OpenApiBenchData.json)
  @Benchmark def circeDecode(): OpenAPI = OriginalOpenApiCirceInstances.decode(OpenApiBenchData.json)
}
