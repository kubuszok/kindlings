package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec.*
import sttp.apispec.openapi.*

import scala.collection.immutable.ListMap

/** Value-based encode + round-trip tests for the model types that no fixture instantiates: `RequestBody`, `Encoding`,
  * `Link`, `Example`, `ServerVariable`, the full `Parameter` (styles/content/examples), the full `Header`, and
  * `ExternalDocumentation`. Uses only the sttp-apispec model (no circe/tapir), so it runs on JVM + JS + Native.
  *
  * Round-trip fixtures avoid the two *faithful-to-circe* lossy paths so equality holds:
  *   - `Any`-typed example values: a JSON-string example decodes to its parsed AST, not the original string. We use
  *     plain-string example values (which round-trip to themselves) for equality checks.
  *   - `ServerVariable` / `Link` extensions: circe's decoders (which we faithfully mirror) do NOT collect `x-*` keys
  *     back for these two types, so their extensions are intentionally dropped on decode. Those types are checked with
  *     extension-free fixtures for round-trip, and with extensions present only for the encode-string assertion.
  */
final class ValueRoundTripSpec extends MacroSuite {

  import OpenApiJsoniter.circe.*

  private def encDoc(doc: OpenAPI): String = writeToString(doc)(openAPICodec)
  private def roundTripOpenAPI(doc: OpenAPI): Unit =
    readFromString[OpenAPI](encDoc(doc))(openAPICodec) ==> doc

  private val strSchema = Schema(`type` = Some(List(SchemaType.String)))
  private val intSchema = Schema(`type` = Some(List(SchemaType.Integer)))

  // Round-trip-safe building blocks (plain-string example values, extensions only where they round-trip).
  private val rtExample = Example(
    summary = Some("an example"),
    description = Some("desc"),
    value = Some(ExampleSingleValue("plain-value")),
    externalValue = Some("https://ex.com/v"),
    extensions = ListMap("x-ex" -> ExtensionValue("true"))
  )
  private val rtHeader = Header(
    description = Some("rate limit"),
    required = Some(true),
    deprecated = Some(false),
    style = Some(ParameterStyle.Simple),
    explode = Some(false),
    schema = Some(intSchema),
    example = Some(ExampleSingleValue("sample")),
    examples = ListMap("ex" -> Right(rtExample))
  )
  private val rtEncoding = Encoding(
    contentType = Some("application/json"),
    headers = ListMap("X-Rate" -> Right(rtHeader)),
    style = Some(ParameterStyle.Form),
    explode = Some(true),
    allowReserved = Some(false),
    extensions = ListMap("x-enc" -> ExtensionValue("1"))
  )
  private val rtRequestBody = RequestBody(
    description = Some("the body"),
    content = ListMap(
      "multipart/form-data" -> MediaType(
        schema = Some(Schema(`type` = Some(List(SchemaType.Object)))),
        encoding = ListMap("file" -> rtEncoding)
      )
    ),
    required = Some(true),
    extensions = ListMap("x-rb" -> ExtensionValue("1"))
  )
  private val rtParameter = Parameter(
    name = "filter",
    in = ParameterIn.Query,
    description = Some("a filter"),
    required = Some(false),
    deprecated = Some(true),
    allowEmptyValue = Some(true),
    style = Some(ParameterStyle.DeepObject),
    explode = Some(true),
    allowReserved = Some(true),
    schema = Some(strSchema),
    example = Some(ExampleSingleValue("foo")),
    examples = ListMap("ex" -> Right(rtExample)),
    content = ListMap("application/json" -> MediaType(schema = Some(strSchema))),
    extensions = ListMap("x-p" -> ExtensionValue("1"))
  )
  // ServerVariable / Link: extension-free so round-trip equality holds (circe drops their x-* on decode).
  private val rtServerVariable = ServerVariable(
    `enum` = Some(List("8443", "443")),
    default = "8443",
    description = Some("port"),
    extensions = ListMap.empty
  )
  private val rtLink = Link(
    operationId = Some("getUser"),
    parameters = ListMap("userId" -> "$response.body#/id"),
    description = Some("the user link"),
    server = Some(Server(url = "https://api.example.com")),
    extensions = ListMap.empty
  )
  private val rtExternalDocs = ExternalDocumentation(
    url = "https://docs.example.com",
    description = Some("more info"),
    extensions = ListMap("x-doc" -> ExtensionValue("1"))
  )

  group("never-instantiated model types round-trip through a document") {

    test("full Parameter (every style/content/examples) via Components.parameters") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        components = Some(Components(parameters = ListMap("P" -> Right(rtParameter))))
      )
      val json = encDoc(doc)
      json.contains("\"deepObject\"") ==> true
      json.contains("\"x-p\":1") ==> true
      roundTripOpenAPI(doc)
    }

    test("RequestBody + multipart Encoding via Operation.requestBody") {
      val doc = OpenAPI(info = Info("API", "1.0")).addPathItem(
        "/x",
        PathItem(post = Some(Operation(requestBody = Some(Right(rtRequestBody)))))
      )
      val json = encDoc(doc)
      json.contains("\"encoding\"") ==> true
      json.contains("\"x-rb\":1") ==> true
      roundTripOpenAPI(doc)
    }

    test("full Header via Components.headers") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        components = Some(Components(headers = ListMap("H" -> Right(rtHeader))))
      )
      encDoc(doc).contains("\"rate limit\"") ==> true
      roundTripOpenAPI(doc)
    }

    test("Link via Components.links") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        components = Some(Components(links = ListMap("L" -> Right(rtLink))))
      )
      encDoc(doc).contains("\"getUser\"") ==> true
      roundTripOpenAPI(doc)
    }

    test("Example via Components.examples") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        components = Some(Components(examples = ListMap("Ex" -> Right(rtExample))))
      )
      encDoc(doc).contains("\"externalValue\"") ==> true
      roundTripOpenAPI(doc)
    }

    test("ServerVariable via Server.variables") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        servers = List(Server(url = "https://{port}.x.com", variables = Some(ListMap("port" -> rtServerVariable))))
      )
      encDoc(doc).contains("\"default\":\"8443\"") ==> true
      roundTripOpenAPI(doc)
    }

    test("ExternalDocumentation via Tag + Schema") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        tags = List(Tag(name = "t", externalDocs = Some(rtExternalDocs))),
        components = Some(
          Components(schemas =
            ListMap(
              "S" -> Schema(`type` = Some(List(SchemaType.Object)), externalDocs = Some(rtExternalDocs))
            )
          )
        )
      )
      val json = encDoc(doc)
      json.contains("\"https://docs.example.com\"") ==> true
      // url must precede description (matches circe / case-class declaration order).
      json.indexOf("\"url\"") < json.indexOf("\"description\":\"more info\"") ==> true
      roundTripOpenAPI(doc)
    }

    test("standalone Encoding via MediaType.encoding") {
      val doc = OpenAPI(info = Info("API", "1.0")).addPathItem(
        "/u",
        PathItem(post =
          Some(
            Operation(requestBody =
              Some(
                Right(
                  RequestBody(content =
                    ListMap(
                      "multipart/form-data" -> MediaType(encoding = ListMap("file" -> rtEncoding))
                    )
                  )
                )
              )
            )
          )
        )
      )
      encDoc(doc).contains("\"contentType\":\"application/json\"") ==> true
      roundTripOpenAPI(doc)
    }
  }

  group("encode-string assertions for extension hoisting (faithful lossy round-trip)") {
    test("ServerVariable extensions ARE emitted (even though circe drops them on decode)") {
      val sv = HandBuiltDocs.serverVariable // carries x-sv
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        servers = List(Server(url = "https://{port}.x.com", variables = Some(ListMap("port" -> sv))))
      )
      encDoc(doc).contains("\"x-sv\":\"a\"") ==> true
    }
    test("Link extensions ARE emitted (even though circe drops them on decode)") {
      val doc = OpenAPI(info = Info("API", "1.0")).copy(
        components = Some(Components(links = ListMap("L" -> Right(HandBuiltDocs.link)))) // carries x-link
      )
      encDoc(doc).contains("\"x-link\":42") ==> true
    }
  }

  group("every ParameterStyle round-trips") {
    HandBuiltDocs.allParameterStyles.foreach { style =>
      test(s"ParameterStyle.${style.value}") {
        val p = Parameter(name = "p", in = ParameterIn.Query, style = Some(style), schema = Some(strSchema))
        val doc = OpenAPI(info = Info("API", "1.0")).copy(
          components = Some(Components(parameters = ListMap("P" -> Right(p))))
        )
        encDoc(doc).contains(s""""style":"${style.value}"""") ==> true
        roundTripOpenAPI(doc)
      }
    }
  }

  group("every SecurityScheme flavor round-trips") {
    HandBuiltDocs.securitySchemes.zipWithIndex.foreach { case (ss, i) =>
      test(s"SecurityScheme ${ss.`type`} #$i") {
        val json = writeToString(ss)(securitySchemeCodec)
        readFromString[SecurityScheme](json)(securitySchemeCodec) ==> ss
      }
    }
  }

  group("range Responses keys") {
    test("5XX range key encodes as \"5XX\" and round-trips") {
      val responses = Responses(
        ListMap(
          ResponsesCodeKey(200) -> Right(Response(description = "ok")),
          ResponsesRangeKey(5) -> Right(Response(description = "server error")),
          ResponsesDefaultKey -> Right(Response(description = "default"))
        )
      )
      val doc =
        OpenAPI(info = Info("API", "1.0")).addPathItem("/x", PathItem(get = Some(Operation(responses = responses))))
      val json = encDoc(doc)
      json.contains("\"5XX\"") ==> true
      readFromString[OpenAPI](json)(openAPICodec) ==> doc
    }
  }
}
