package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

final class OpenApiCodecSpec extends MacroSuite {

  import OpenApiJsoniter.circe._

  private val sampleDoc: OpenAPI = SampleDocs.sampleDoc

  group("OpenAPI codec (3.1)") {

    test("round-trips a representative document") {
      val json = writeToString(sampleDoc)(openAPICodec)
      val decoded = readFromString[OpenAPI](json)(openAPICodec)
      decoded ==> sampleDoc
    }

    test("empty security requirement encodes as {} inside an operation") {
      val json = writeToString(sampleDoc)(openAPICodec)
      json.contains("\"security\":[{\"apiKey\":[]}]") ==> true
    }
  }

  group("ReferenceOr") {
    test("Left encodes as a $ref object") {
      val ref: ReferenceOr[Response] = Left(Reference("#/x", summary = Some("s")))
      val json = writeToString(Components(responses = ListMap("r" -> ref)))(codecFor[Components](null))
      json.contains("\"$ref\":\"#/x\"") ==> true
      json.contains("\"summary\":\"s\"") ==> true
    }
  }
}
