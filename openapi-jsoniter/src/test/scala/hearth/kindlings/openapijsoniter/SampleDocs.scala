package hearth.kindlings.openapijsoniter

import sttp.apispec._
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

/** Shared model fixtures used by both cross-platform specs and the JVM-only circe cross-check. */
object SampleDocs {

  val sampleDoc: OpenAPI = OpenAPI(
    openapi = "3.1.0",
    info = Info(
      title = "Sample API",
      version = "1.0.0",
      description = Some("A sample API"),
      contact = Some(Contact(name = Some("Jane"), email = Some("jane@example.com"))),
      license = Some(License("Apache 2.0", Some("https://apache.org/licenses/LICENSE-2.0"))),
      extensions = ListMap("x-info-ext" -> ExtensionValue("\"hello\""))
    ),
    servers = List(Server(url = "https://api.example.com", description = Some("prod"))),
    tags = List(Tag(name = "pets", description = Some("Pet operations"))),
    paths = Paths(
      pathItems = ListMap(
        "/pets" -> PathItem(
          get = Some(
            Operation(
              operationId = Some("listPets"),
              summary = Some("List pets"),
              parameters = List(
                Right(
                  Parameter(
                    name = "limit",
                    in = ParameterIn.Query,
                    required = Some(false),
                    schema = Some(Schema(`type` = Some(List(SchemaType.Integer))))
                  )
                )
              ),
              responses = Responses(
                ListMap(
                  ResponsesCodeKey(200) -> Right(
                    Response(
                      description = "ok",
                      content = ListMap(
                        "application/json" -> MediaType(schema = Some(Schema($ref = Some("#/components/schemas/Pet"))))
                      )
                    )
                  ),
                  ResponsesDefaultKey -> Right(Response(description = "error"))
                )
              ),
              security = List(ListMap("apiKey" -> Vector.empty[String]))
            )
          )
        )
      ),
      extensions = ListMap("x-path-ext" -> ExtensionValue("42"))
    ),
    components = Some(
      Components(
        schemas = ListMap(
          "Pet" -> Schema(
            `type` = Some(List(SchemaType.Object)),
            properties = ListMap(
              "id" -> Schema(`type` = Some(List(SchemaType.Integer)), format = Some("int64")),
              "name" -> Schema(`type` = Some(List(SchemaType.String)))
            ),
            required = List("id", "name")
          )
        ),
        securitySchemes = ListMap(
          "apiKey" -> Right(SecurityScheme(`type` = "apiKey", name = Some("api_key"), in = Some("header")))
        )
      )
    ),
    extensions = ListMap("x-root" -> ExtensionValue("true"))
  )
}
