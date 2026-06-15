package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec._
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

/** OAS 3.0 translation edge cases and container-level behaviours that the existing `Oas30Spec` does not cover.
  * Cross-platform (no circe/tapir).
  */
final class Oas30EdgesSpec extends MacroSuite {

  private val codec30 = OpenApiJsoniter.circe_openapi_3_0_3.schemaCodec

  private def enc30(s: Schema): String = writeToString(s)(codec30)
  private def dec30(s: String): Schema = readFromString[Schema](s)(codec30)

  group("nullable only applies to a 2-element anyOf [ref, null]") {
    test("anyOf with a non-empty (3-element) list is NOT collapsed to allOf+nullable") {
      val s = Schema(anyOf = List(
        Schema($ref = Some("#/A")),
        Schema($ref = Some("#/B")),
        Schema(`type` = Some(List(SchemaType.Null)))
      ))
      val json = enc30(s)
      // Not the nullable-$ref special case: stays as anyOf, no top-level "nullable".
      json.contains("\"anyOf\"") ==> true
      json.contains("\"nullable\":true") ==> false
    }

    test("anyOf [non-ref, null] is NOT collapsed (only $ref schemas trigger the translation)") {
      val s = Schema(anyOf = List(
        Schema(`type` = Some(List(SchemaType.String))),
        Schema(`type` = Some(List(SchemaType.Null)))
      ))
      val json = enc30(s)
      json.contains("\"anyOf\"") ==> true
      json.contains("\"allOf\"") ==> false
    }

    test("the exact [ref, null] case IS collapsed (regression guard)") {
      val s = Schema(anyOf = List(Schema($ref = Some("#/Foo")), Schema(`type` = Some(List(SchemaType.Null)))))
      enc30(s) ==> """{"nullable":true,"allOf":[{"$ref":"#/Foo"}]}"""
    }
  }

  group("multi-value examples in 3.0") {
    test("examples with multiple elements keeps `examples` (only a single-element list becomes `example`)") {
      val s = Schema(
        `type` = Some(List(SchemaType.String)),
        examples = Some(List(ExampleSingleValue("a"), ExampleSingleValue("b")))
      )
      val json = enc30(s)
      // More than one example -> cannot collapse to `example`; circe keeps the `examples` array.
      json.contains("\"examples\"") ==> true
      json.contains("\"example\":") ==> false
    }

    test("single-element examples list collapses to `example` in 3.0") {
      val s = Schema(`type` = Some(List(SchemaType.String)), examples = Some(List(ExampleSingleValue("only"))))
      enc30(s).contains("\"example\":\"only\"") ==> true
    }
  }

  group("circeObjectAnySchema in 3.0 mode") {
    test("AnySchema.Anything encodes as {} (object) and Nothing as {\"not\":{}}") {
      writeToString(AnySchema.Anything: SchemaLike)(OpenApiJsoniter.circeObjectAnySchema.schemaLikeCodec) ==> "{}"
      writeToString(AnySchema.Nothing: SchemaLike)(OpenApiJsoniter.circeObjectAnySchema.schemaLikeCodec) ==> """{"not":{}}"""
    }
    test("object-any + 3.0 flag combined still emits object form") {
      val codec = OpenApiJsoniter.custom(openApi30Flag = true, anyEncoding = AnySchema.Encoding.Object).schemaLikeCodec
      writeToString(AnySchema.Anything: SchemaLike)(codec) ==> "{}"
      writeToString(AnySchema.Nothing: SchemaLike)(codec) ==> """{"not":{}}"""
    }
  }

  group("ReferenceOr round-trips preserve summary/description") {
    import OpenApiJsoniter.circe._
    test("Left(Reference) with summary + description round-trips") {
      val ref: ReferenceOr[Response] = Left(Reference("#/x", summary = Some("s"), description = Some("d")))
      val components = Components(responses = ListMap("r" -> ref))
      val codec = codecFor[Components](null)
      val json = writeToString(components)(codec)
      json.contains("\"$ref\":\"#/x\"") ==> true
      json.contains("\"summary\":\"s\"") ==> true
      json.contains("\"description\":\"d\"") ==> true
      readFromString[Components](json)(codec) ==> components
    }
  }

  group("extensions on containers that call dropNullsExpand") {
    import OpenApiJsoniter.circe._
    // Every such container hoists x-* extensions; verify the hoist for a representative spread of them.
    test("Info / Contact / License / Server / Tag / Components / Response / Operation / PathItem extensions hoist") {
      val doc = OpenAPI(
        info = Info("API", "1.0",
          contact = Some(Contact(name = Some("J"), extensions = ListMap("x-c" -> ExtensionValue("1")))),
          license = Some(License("L", None, extensions = ListMap("x-l" -> ExtensionValue("1")))),
          extensions = ListMap("x-info" -> ExtensionValue("1"))
        ),
        servers = List(Server(url = "u", extensions = ListMap("x-srv" -> ExtensionValue("1")))),
        tags = List(Tag(name = "t", extensions = ListMap("x-tag" -> ExtensionValue("1")))),
        components = Some(Components(extensions = ListMap("x-cmp" -> ExtensionValue("1")))),
        extensions = ListMap("x-root" -> ExtensionValue("1"))
      ).addPathItem("/p", PathItem(
        extensions = ListMap("x-pi" -> ExtensionValue("1")),
        get = Some(Operation(
          responses = Responses(ListMap(ResponsesCodeKey(200) -> Right(
            Response(description = "ok", extensions = ListMap("x-resp" -> ExtensionValue("1")))
          ))),
          extensions = ListMap("x-op" -> ExtensionValue("1"))
        ))
      ))
      val json = writeToString(doc)(openAPICodec)
      List("x-c", "x-l", "x-info", "x-srv", "x-tag", "x-cmp", "x-root", "x-pi", "x-resp", "x-op")
        .foreach(k => json.contains("\"" + k + "\"") ==> true)
      // None should appear nested under an "extensions" object key.
      json.contains("\"extensions\"") ==> false
    }
  }

  group("non-JSON extension value falls back to a JSON string") {
    import OpenApiJsoniter.circe._
    test("an extension value that is not valid JSON is emitted as a JSON string") {
      val schema = Schema(
        `type` = Some(List(SchemaType.String)),
        extensions = ListMap("x-raw" -> ExtensionValue("not valid json {"))
      )
      val json = writeToString(schema)(schemaCodec)
      json.contains("\"x-raw\":\"not valid json {\"") ==> true
      // Faithful to circe: the decoder stores ExtensionValue values as their JSON-string form (spaces2), so an
      // un-parseable raw string is re-quoted on decode; the re-encoded bytes are nonetheless stable.
      val decoded = readFromString[Schema](json)(schemaCodec)
      decoded.extensions("x-raw") ==> ExtensionValue("\"not valid json {\"")
      writeToString(decoded)(schemaCodec) ==> json
    }
    test("a valid-JSON extension value is parsed and emitted as JSON (not a string)") {
      val schema = Schema(
        `type` = Some(List(SchemaType.String)),
        extensions = ListMap("x-obj" -> ExtensionValue("""{"k":1}"""))
      )
      val json = writeToString(schema)(schemaCodec)
      json.contains("\"x-obj\":{\"k\":1}") ==> true
    }
  }

  group("3.0 type:[t,null] decode reverses into a type array (regression with description)") {
    test("nullable string with extra keywords decodes to a 2-element type list") {
      val decoded = dec30("""{"type":"string","nullable":true,"maxLength":5}""")
      decoded.`type` ==> Some(List(SchemaType.String, SchemaType.Null))
      decoded.maxLength ==> Some(5)
    }
  }
}
