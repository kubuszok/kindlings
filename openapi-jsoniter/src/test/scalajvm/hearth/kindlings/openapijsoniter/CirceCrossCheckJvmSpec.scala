package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec

/** Strongest fidelity guarantee: our jsoniter output for a representative document must equal `openapi-circe`'s output
  * (compared via our AST so the comparison is independent of the printer, but order-sensitive). Depends on
  * `openapi-circe`, which is published only for the JVM — hence `scalajvm/`.
  */
final class CirceCrossCheckJvmSpec extends MacroSuite {

  test("byte-for-byte matches openapi-circe output (3.1)") {
    val doc = SampleDocs.sampleDoc

    val ours = writeToString(doc)(OpenApiJsoniter.circe.openAPICodec)
    val circeStr = sttp.apispec.openapi.circe.encoderOpenAPI(doc).noSpaces

    val oursAst = readFromString[Json](ours)(jsonValueCodec)
    val circeAst = readFromString[Json](circeStr)(jsonValueCodec)
    oursAst ==> circeAst
  }
}
