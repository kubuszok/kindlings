package hearth.kindlings.openapijsoniter

import sttp.apispec._
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

/** Hand-built model fixtures that exercise encoder/decoder branches not reached by the golden resource fixtures:
  * every `SecurityScheme` flavor, every `ParameterStyle`, range `Responses` keys, multipart `Encoding`, the
  * never-instantiated model types (`RequestBody`, `Encoding`, `Link`, `Example`, `ServerVariable`, full `Parameter`,
  * full `Header`, `ExternalDocumentation`).
  *
  * Lives in the shared test dir (uses only the sttp-apispec model, no circe/tapir) so the value round-trip specs can
  * reuse it on all platforms; the JVM-only cross-check reuses it too.
  */
object HandBuiltDocs {

  // --- every SecurityScheme flavor ---
  val ssApiKey: SecurityScheme =
    SecurityScheme(`type` = "apiKey", name = Some("api_key"), in = Some("header"))
  val ssHttpBasic: SecurityScheme =
    SecurityScheme(`type` = "http", scheme = Some("basic"))
  val ssHttpBearer: SecurityScheme =
    SecurityScheme(`type` = "http", scheme = Some("bearer"), bearerFormat = Some("JWT"))
  val ssOpenIdConnect: SecurityScheme =
    SecurityScheme(`type` = "openIdConnect", openIdConnectUrl = Some("https://example.com/.well-known/openid-configuration"))
  val ssMutualTLS: SecurityScheme =
    SecurityScheme(`type` = "mutualTLS", description = Some("mTLS"))
  val ssOauth2: SecurityScheme = SecurityScheme(
    `type` = "oauth2",
    flows = Some(
      OAuthFlows(
        `implicit` = Some(OAuthFlow(
          authorizationUrl = Some("https://example.com/auth"),
          scopes = ListMap("read" -> "read access")
        )),
        password = Some(OAuthFlow(
          tokenUrl = Some("https://example.com/token"),
          scopes = ListMap.empty
        )),
        clientCredentials = Some(OAuthFlow(
          tokenUrl = Some("https://example.com/token"),
          refreshUrl = Some("https://example.com/refresh"),
          scopes = ListMap("admin" -> "admin access")
        )),
        authorizationCode = Some(OAuthFlow(
          authorizationUrl = Some("https://example.com/auth"),
          tokenUrl = Some("https://example.com/token"),
          scopes = ListMap("write" -> "write access")
        ))
      )
    )
  )

  val securitySchemes: List[SecurityScheme] =
    List(ssApiKey, ssHttpBasic, ssHttpBearer, ssOpenIdConnect, ssMutualTLS, ssOauth2)

  // --- all 7 ParameterStyles ---
  val allParameterStyles: List[ParameterStyle] = List(
    ParameterStyle.Simple,
    ParameterStyle.Form,
    ParameterStyle.Matrix,
    ParameterStyle.Label,
    ParameterStyle.SpaceDelimited,
    ParameterStyle.PipeDelimited,
    ParameterStyle.DeepObject
  )

  // --- never-instantiated model types ---
  val externalDocs: ExternalDocumentation = ExternalDocumentation(
    url = "https://docs.example.com",
    description = Some("Find more info here"),
    extensions = ListMap("x-doc" -> ExtensionValue("1"))
  )

  val serverVariable: ServerVariable = ServerVariable(
    `enum` = Some(List("8443", "443")),
    default = "8443",
    description = Some("port"),
    extensions = ListMap("x-sv" -> ExtensionValue("\"a\""))
  )

  val example: Example = Example(
    summary = Some("an example"),
    description = Some("desc"),
    value = Some(ExampleSingleValue("""{"a":1}""")),
    extensions = ListMap("x-ex" -> ExtensionValue("true"))
  )

  val link: Link = Link(
    operationId = Some("getUser"),
    parameters = ListMap("userId" -> "$response.body#/id"),
    description = Some("the user link"),
    server = Some(Server(url = "https://api.example.com")),
    extensions = ListMap("x-link" -> ExtensionValue("42"))
  )

  val header: Header = Header(
    description = Some("rate limit"),
    required = Some(true),
    deprecated = Some(false),
    style = Some(ParameterStyle.Simple),
    explode = Some(false),
    schema = Some(Schema(`type` = Some(List(SchemaType.Integer)))),
    example = Some(ExampleSingleValue("100")),
    examples = ListMap("ex" -> Right(example))
  )

  val encoding: Encoding = Encoding(
    contentType = Some("application/json"),
    headers = ListMap("X-Rate" -> Right(header)),
    style = Some(ParameterStyle.Form),
    explode = Some(true),
    allowReserved = Some(false),
    extensions = ListMap("x-enc" -> ExtensionValue("1"))
  )

  val requestBody: RequestBody = RequestBody(
    description = Some("the body"),
    content = ListMap(
      "multipart/form-data" -> MediaType(
        schema = Some(Schema(`type` = Some(List(SchemaType.Object)))),
        encoding = ListMap("file" -> encoding)
      )
    ),
    required = Some(true),
    extensions = ListMap("x-rb" -> ExtensionValue("1"))
  )

  val fullParameter: Parameter = Parameter(
    name = "filter",
    in = ParameterIn.Query,
    description = Some("a filter"),
    required = Some(false),
    deprecated = Some(true),
    allowEmptyValue = Some(true),
    style = Some(ParameterStyle.DeepObject),
    explode = Some(true),
    allowReserved = Some(true),
    schema = Some(Schema(`type` = Some(List(SchemaType.String)))),
    example = Some(ExampleSingleValue("foo")),
    examples = ListMap("ex" -> Right(example)),
    content = ListMap("application/json" -> MediaType(schema = Some(Schema(`type` = Some(List(SchemaType.String)))))),
    extensions = ListMap("x-p" -> ExtensionValue("1"))
  )

  // Parameters with every style, so all 7 ParameterStyle encodings are exercised.
  val parametersWithEveryStyle: List[Parameter] =
    allParameterStyles.zipWithIndex.map { case (st, i) =>
      Parameter(
        name = s"p$i",
        in = ParameterIn.Query,
        style = Some(st),
        schema = Some(Schema(`type` = Some(List(SchemaType.String))))
      )
    }

  /** A document touching: all SecurityScheme flavors, all ParameterStyles, range Responses keys (5XX), multipart
    * Encoding, RequestBody, Link, Example, Header, ServerVariable, ExternalDocumentation, extension overrides.
    */
  val fullBranchDoc: OpenAPI = OpenAPI(
    openapi = "3.1.0",
    info = Info(title = "Branch API", version = "1.0.0"),
    servers = List(
      Server(
        url = "https://{port}.example.com",
        variables = Some(ListMap("port" -> serverVariable))
      )
    ),
    tags = List(Tag(name = "t", externalDocs = Some(externalDocs))),
    paths = Paths(
      pathItems = ListMap(
        "/things" -> PathItem(
          parameters = parametersWithEveryStyle.map(Right(_)),
          post = Some(
            Operation(
              operationId = Some("createThing"),
              parameters = List(Right(fullParameter)),
              requestBody = Some(Right(requestBody)),
              responses = Responses(
                ListMap(
                  ResponsesCodeKey(200) -> Right(
                    Response(
                      description = "ok",
                      headers = ListMap("X-Rate" -> Right(header)),
                      links = ListMap("self" -> Right(link)),
                      content = ListMap("application/json" -> MediaType(
                        schema = Some(Schema(`type` = Some(List(SchemaType.Object)))),
                        examples = ListMap("ex" -> Right(example))
                      ))
                    )
                  ),
                  ResponsesRangeKey(5) -> Right(Response(description = "server error"))
                )
              )
            )
          )
        )
      )
    ),
    components = Some(
      Components(
        schemas = ListMap("Boxed" -> Schema(`type` = Some(List(SchemaType.Object)), externalDocs = Some(externalDocs))),
        examples = ListMap("Ex" -> Right(example)),
        requestBodies = ListMap("Body" -> Right(requestBody)),
        headers = ListMap("H" -> Right(header)),
        links = ListMap("L" -> Right(link)),
        parameters = ListMap("P" -> Right(fullParameter)),
        securitySchemes = ListMap(
          "apiKey" -> Right(ssApiKey),
          "basic" -> Right(ssHttpBasic),
          "bearer" -> Right(ssHttpBearer),
          "oidc" -> Right(ssOpenIdConnect),
          "mtls" -> Right(ssMutualTLS),
          "oauth2" -> Right(ssOauth2)
        )
      )
    )
  )

  private def schemaComponent(desc: String)(schema: Schema): (String, Schema) =
    desc -> schema.copy(description = Some(desc))

  /** The in-memory `OpenAPI` whose encoding equals `/spec/3.1/schema.json` (and, in 3.0 mode, `/spec/3.0/schema.json`)
    * — ported from sttp-apispec's own `threeone.EncoderTest`. Built from native Scala values (not our `Json` AST), so
    * it can be cross-checked against circe's encoder directly. Shared by the golden encoder spec and the cross-check.
    */
  val fullSchemaOpenApi: OpenAPI = {
    val components = Components(
      schemas = ListMap(
        schemaComponent("type 'null'")(Schema(SchemaType.Null)),
        schemaComponent("nullable string")(Schema(SchemaType.String, SchemaType.Null)),
        schemaComponent("nullable reference")(Schema.referenceTo("#/components/schemas/", "Foo").nullable),
        schemaComponent("nullable enum")(Schema(`enum` = Some(List("a", "b", null).map(ExampleSingleValue(_))))),
        schemaComponent("single example")(
          Schema(SchemaType.String).copy(examples = Some(List(ExampleSingleValue("exampleValue"))))
        ),
        schemaComponent("multi valued example")(
          Schema(SchemaType.Array).copy(examples = Some(List(ExampleMultipleValue(List("ex1", "ex1")))))
        ),
        schemaComponent("object with example")(
          Schema(SchemaType.Object).copy(examples = Some(List(ExampleSingleValue("""{"a": 1, "b": null}"""))))
        ),
        schemaComponent("min/max")(Schema(minimum = Some(BigDecimal(10)), maximum = Some(BigDecimal(20)))),
        schemaComponent("exclusive min/max")(
          Schema(exclusiveMinimum = Some(BigDecimal(10)), exclusiveMaximum = Some(BigDecimal(20)))
        ),
        schemaComponent("exclusiveMinimum false")(Schema(minimum = Some(BigDecimal(10)))),
        schemaComponent("array")(Schema(SchemaType.Array).copy(items = Some(Schema(SchemaType.String)))),
        schemaComponent("array with unique items")(Schema(SchemaType.Array).copy(uniqueItems = Some(true))),
        schemaComponent("const")(Schema(const = Some("const1").map(ExampleSingleValue(_)))),
        schemaComponent("enum")(Schema(`enum` = Some(List("enum1", "enum2").map(ExampleSingleValue(_)))))
      )
    )
    OpenAPI(info = Info(title = "API", version = "1.0.0"), components = Some(components))
  }

  /** The 3.1-only variant (adds a "multiple examples" schema), matching `/spec/3.1/schema.json`. */
  val fullSchemaOpenApi31: OpenAPI = {
    val schemas31 = ListMap(
      schemaComponent("multiple examples")(
        Schema(SchemaType.String).copy(examples = Some(List("ex1", "ex2").map(ExampleSingleValue(_))))
      )
    )
    fullSchemaOpenApi.copy(
      components = fullSchemaOpenApi.components.map(c => c.copy(schemas = c.schemas ++ schemas31))
    )
  }

  /** The 3.0 variant matching `/spec/3.0/schema.json`. */
  val fullSchemaOpenApi30: OpenAPI = fullSchemaOpenApi.copy(openapi = "3.0.1")
}
