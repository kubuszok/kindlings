package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import io.circe.Encoder as CirceEncoder
import sttp.apispec.{
  AnySchema,
  ExampleSingleValue,
  ExtensionValue,
  Pattern,
  Schema,
  SchemaLike,
  SchemaType,
  SecurityScheme
}
import sttp.apispec.openapi.OpenAPI

import scala.collection.immutable.ListMap
import scala.io.Source

/** Strongest fidelity guarantee: for every golden fixture (and a hand-built document exercising the rarely-used encoder
  * branches) our jsoniter output must equal `openapi-circe`'s circe output, compared byte-for-byte.
  *
  * Depends on `openapi-circe` / `jsonschema-circe`, published only for the JVM — hence `scalajvm/`. The comparison
  * decodes BOTH outputs back into our `Json` AST (so it is independent of how each printer escapes/spaces) and asserts
  * AST equality, which is order-sensitive — exactly the fidelity we want.
  */
final class CirceCrossCheckJvmSpec extends MacroSuite {

  // --- circe variants, mirroring OpenApiJsoniter.{circe, circe_openapi_3_0_3, circeObjectAnySchema} ---
  private object circe31 extends sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders
  private object circe303 extends sttp.apispec.openapi.circe.SttpOpenAPI3_0_3CirceEncoders {
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  }
  private object circeObjAny extends sttp.apispec.openapi.circe.SttpOpenAPICirceEncoders {
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Object
  }

  private def loadAst(path: String): Json = {
    val src = Source.fromInputStream(getClass.getResourceAsStream(path), "UTF-8")
    try readFromString[Json](src.mkString)(jsonValueCodec)
    finally src.close()
  }

  private def ast(s: String): Json = readFromString[Json](s)(jsonValueCodec)

  /** Decode a fixture into the model with OUR decoder, then assert our encoder == circe's encoder, byte-for-byte. */
  private def crossCheckModel[A](path: String)(ourCodec: JsonValueCodec[A])(circeEnc: CirceEncoder[A]): Unit = {
    val originalStr = writeToString(loadAst(path))(jsonValueCodec)
    val model = readFromString[A](originalStr)(ourCodec)
    ast(writeToString(model)(ourCodec)) ==> ast(circeEnc(model).noSpaces)
  }

  /** Same, but for an in-memory model value (no resource). */
  private def crossCheckValue[A](model: A)(ourCodec: JsonValueCodec[A])(circeEnc: CirceEncoder[A]): Unit =
    ast(writeToString(model)(ourCodec)) ==> ast(circeEnc(model).noSpaces)

  group("OpenAPI 3.1 fixtures cross-checked against openapi-circe") {
    val ours = OpenApiJsoniter.circe
    // Note: fixtures that carry `Any`-typed example/extension values (e.g. /spec/3.1/schema.json's "object with example")
    // are NOT comparable through circe's encoder once decoded by us — our decoder stores them as our `Json` AST, which
    // circe's encoder serialises via its `.toString` fallback. Those models are cross-checked from in-memory native
    // values below (and by the golden-fixpoint EncoderGoldenJvmSpec). Fixtures without `Any`-typed values use the
    // decode-then-cross-check path here.
    def check(path: String): Unit = crossCheckModel[OpenAPI](path)(ours.openAPICodec)(circe31.encoderOpenAPI)
    test("basic petstore")(check("/petstore/basic-petstore.json"))
    test("header petstore")(check("/petstore/header-petstore.json"))
    test("any and nothing (boolean)")(check("/spec/3.1/any_and_nothing1.json"))
    test("sample doc (in-memory)")(
      crossCheckValue[OpenAPI](SampleDocs.sampleDoc)(ours.openAPICodec)(circe31.encoderOpenAPI)
    )
    test("full 3.1 schema (in-memory native values)")(
      crossCheckValue[OpenAPI](HandBuiltDocs.fullSchemaOpenApi31)(ours.openAPICodec)(circe31.encoderOpenAPI)
    )
  }

  group("OpenAPI 3.0.3 fixtures cross-checked against circe_openapi_3_0_3") {
    val ours = OpenApiJsoniter.circe_openapi_3_0_3
    def check(path: String): Unit = crossCheckModel[OpenAPI](path)(ours.openAPICodec)(circe303.encoderOpenAPI)
    test("callbacks")(check("/callbacks/callbacks.json"))
    test("3.0 schema (in-memory native values)")(
      crossCheckValue[OpenAPI](HandBuiltDocs.fullSchemaOpenApi30)(ours.openAPICodec)(circe303.encoderOpenAPI)
    )
    test("3.0 const_and_enum")(check("/spec/3.0/const_and_enum.json"))
  }

  group("AnySchema object-encoding cross-checked against circe (object encoding)") {
    val ours = OpenApiJsoniter.circeObjectAnySchema
    def check(path: String): Unit = crossCheckModel[OpenAPI](path)(ours.openAPICodec)(circeObjAny.encoderOpenAPI)
    test("any and nothing (object)")(check("/spec/3.1/any_and_nothing2.json"))
  }

  group("Schema fixtures cross-checked against jsonschema-circe") {
    val ours = OpenApiJsoniter.circe
    def check(path: String): Unit = crossCheckModel[Schema](path)(ours.schemaCodec)(sttp.apispec.circe.encoderSchema)
    test("self-describing schema")(check("/self-describing-schema.json"))
    test("extending-recursive schema")(check("/extending-recursive.json"))
  }

  group("SecurityScheme fixtures cross-checked against circe") {
    val ours = OpenApiJsoniter.circe
    def check(path: String): Unit =
      crossCheckModel[SecurityScheme](path)(ours.securitySchemeCodec)(circe31.encoderSecurityScheme)
    test("security scheme with scopes")(check("/securityScheme/security-scheme-with-scopes.json"))
    test("security scheme with empty scopes")(check("/securityScheme/security-scheme-with-empty-scopes.json"))
  }

  // --- Hand-built documents exercising encoder branches not covered by the fixtures ---
  group("hand-built documents exercising rarely-used encoder branches (3.1)") {
    val ours = OpenApiJsoniter.circe
    val schemaLikeCodec: JsonValueCodec[SchemaLike] = ours.schemaLikeCodec
    def checkSchema(s: SchemaLike): Unit =
      crossCheckValue[SchemaLike](s)(schemaLikeCodec)(sttp.apispec.circe.encoderSchemaLike)

    test("numeric ExampleSingleValue subtypes (incl. Float/Double NaN/Infinity, BigInt, null)") {
      // All branches of encoderExampleSingleValue, via Schema.enum which carries List[ExampleValue].
      val schema = Schema(`enum` =
        Some(
          List[Any](
            42.toShort,
            42L,
            3.5f,
            2.5d,
            BigDecimal("1.23"),
            BigInt("99999999999999999999"),
            Float.NaN,
            Float.PositiveInfinity,
            Double.NegativeInfinity,
            true,
            null,
            "str"
          ).map(ExampleSingleValue(_))
        )
      )
      checkSchema(schema)
    }

    test("extension key overriding a model field") {
      // An extension whose key collides with a real field ("type") must override on encode (expandExtensions).
      val schema = Schema(
        `type` = Some(List(SchemaType.String)),
        extensions = ListMap("type" -> ExtensionValue("\"overridden\""))
      )
      checkSchema(schema)
    }

    test("non-JSON extension value falls back to a JSON string") {
      val schema = Schema(
        `type` = Some(List(SchemaType.String)),
        extensions = ListMap("x-raw" -> ExtensionValue("not valid json {"))
      )
      checkSchema(schema)
    }

    test("all 7 SchemaType values (incl. Boolean/Number)") {
      val schema = Schema(`type` =
        Some(
          List(
            SchemaType.Boolean,
            SchemaType.Object,
            SchemaType.Array,
            SchemaType.Number,
            SchemaType.String,
            SchemaType.Integer,
            SchemaType.Null
          )
        )
      )
      checkSchema(schema)
    }

    test("pattern / patternProperties / additionalProperties (object and boolean form)") {
      val objAp = Schema(
        `type` = Some(List(SchemaType.Object)),
        pattern = Some(Pattern("^a.*z$")),
        patternProperties = ListMap(Pattern("^x-") -> Schema(`type` = Some(List(SchemaType.String)))),
        additionalProperties = Some(Schema(`type` = Some(List(SchemaType.Integer))))
      )
      checkSchema(objAp)
      val boolAp = Schema(`type` = Some(List(SchemaType.Object)), additionalProperties = Some(AnySchema.Nothing))
      checkSchema(boolAp)
    }

    test("full document: all SecurityScheme flavors, all ParameterStyles, range Responses keys, multipart Encoding") {
      crossCheckValue[OpenAPI](HandBuiltDocs.fullBranchDoc)(ours.openAPICodec)(circe31.encoderOpenAPI)
    }

    test("every SecurityScheme flavor individually") {
      HandBuiltDocs.securitySchemes.foreach { ss =>
        crossCheckValue[SecurityScheme](ss)(ours.securitySchemeCodec)(circe31.encoderSecurityScheme)
      }
    }
  }
}
