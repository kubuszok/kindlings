package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import sttp.apispec.*
import sttp.apispec.openapi.OpenAPI

/** Circe-free jsoniter-scala codecs for the sttp-apispec OpenAPI + JSON-Schema model.
  *
  * The byte-level encoding reproduces `sttp-apispec`'s circe codecs exactly. Two variants are provided, mirroring
  * sttp-apispec's `circe` / `circe_openapi_3_0_3` entry points:
  *
  *   - [[circe]] — OpenAPI 3.1 (JSON Schema draft 2020-12 semantics)
  *   - [[circe_openapi_3_0_3]] — OpenAPI 3.0.3 (the `nullable`, `exclusiveMinimum/Maximum`-as-boolean, `example`,
  *     `enum`-instead-of-`const` translations)
  *
  * Each variant exposes the underlying [[internal.Encoder]] / [[internal.Decoder]] type classes as implicits (so model
  * types compose) plus convenience [[JsonValueCodec]]s for the top-level [[OpenAPI]] and [[Schema]] models.
  */
object OpenApiJsoniter {

  /** OpenAPI 3.1 codecs (default). */
  object circe extends internal.OpenApiCodecs {
    override def openApi30: Boolean = false
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  }

  /** OpenAPI 3.0.3 codecs. */
  object circe_openapi_3_0_3 extends internal.OpenApiCodecs {
    override def openApi30: Boolean = true
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  }

  /** OpenAPI 3.1 codecs that encode [[AnySchema]] as objects (`{}` / `{"not":{}}`) rather than booleans. */
  object circeObjectAnySchema extends internal.OpenApiCodecs {
    override def openApi30: Boolean = false
    override def anyObjectEncoding: AnySchema.Encoding = AnySchema.Encoding.Object
  }

  /** Builds a custom codec set, mirroring sttp-apispec's overridable encoder traits (e.g. to choose a different
    * [[AnySchema.Encoding]] or to flip the OAS-3.0 behaviour explicitly).
    */
  def custom(
      openApi30Flag: Boolean = false,
      anyEncoding: AnySchema.Encoding = AnySchema.Encoding.Boolean
  ): internal.OpenApiCodecs =
    new internal.OpenApiCodecs {
      override def openApi30: Boolean = openApi30Flag
      override def anyObjectEncoding: AnySchema.Encoding = anyEncoding
    }
}

package internal {

  /** Bundles the full encoder + decoder set and exposes JSON byte codecs. Mixed into the public `circe` /
    * `circe_openapi_3_0_3` objects.
    */
  trait OpenApiCodecs extends OpenApiEncoders with OpenApiDecoders {

    /** Builds a [[JsonValueCodec]] from an [[Encoder]] / [[Decoder]] pair by going through the kindlings [[Json]] AST
      * and the hand-written `JsonValueCodec[Json]`.
      */
    final def codecFor[A](nullVal: A)(implicit enc: Encoder[A], dec: Decoder[A]): JsonValueCodec[A] =
      new JsonValueCodec[A] {
        def nullValue: A = nullVal
        def encodeValue(x: A, out: JsonWriter): Unit = jsonValueCodec.encodeValue(enc(x), out)
        def decodeValue(in: JsonReader, default: A): A = {
          val json = jsonValueCodec.decodeValue(in, Json.Null)
          dec(json) match {
            case Right(a)  => a
            case Left(err) => in.decodeError(err.message)
          }
        }
      }

    implicit lazy val openAPICodec: JsonValueCodec[OpenAPI] = codecFor[OpenAPI](null)
    implicit lazy val schemaCodec: JsonValueCodec[Schema] = codecFor[Schema](null)
    implicit lazy val schemaLikeCodec: JsonValueCodec[SchemaLike] = codecFor[SchemaLike](null)
    implicit lazy val securitySchemeCodec: JsonValueCodec[SecurityScheme] = codecFor[SecurityScheme](null)
  }
}
