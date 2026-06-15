package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import sttp.apispec.*

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

/** Port of `sttp.apispec.internal.JsonSchemaCirceEncoders` onto the kindlings [[Json]] AST.
  *
  * Field order, OAS 3.0 vs 3.1 toggles, `dropNulls`, extension expansion, and the hand-written `Any`-typed example /
  * extension encoders all match circe exactly.
  */
private[openapijsoniter] trait JsonSchemaEncoders {

  def anyObjectEncoding: AnySchema.Encoding

  def openApi30: Boolean = false

  // --- `:=` syntax, building ordered object entries exactly like circe ---
  final protected class KeyOps(private val key: String) {
    def :=[A](value: A)(implicit e: Encoder[A]): (String, Json) = key -> e(value)
  }
  implicit final protected def keyOps(key: String): KeyOps = new KeyOps(key)

  final protected def jsonObj(entries: Vector[(String, Json)]): Json =
    Json.Obj(JsonObject(entries))

  // --- ExtensionValue: parse the raw string, falling back to a JSON string ---
  implicit val extensionValue: Encoder[ExtensionValue] =
    Encoder.instance(e => JsonAstOps.parse(e.value).getOrElse(Json.fromString(e.value)))

  // note: strict val-s, order matters (forward references resolved by val ordering)
  implicit val encoderExampleSingleValue: Encoder[ExampleSingleValue] = Encoder.instance {
    // Values produced by decoding arbitrary JSON are stored as our `Json` AST; pass them through verbatim so that
    // decode -> encode round-trips faithfully (circe stores its own `Json` here for the same reason).
    case ExampleSingleValue(json: Json)        => json
    case ExampleSingleValue(value: String)     => JsonAstOps.parse(value).getOrElse(Json.fromString(value))
    case ExampleSingleValue(value: Int)        => Json.fromInt(value)
    case ExampleSingleValue(value: Short)      => Json.fromInt(value.toInt)
    case ExampleSingleValue(value: Long)       => Json.fromLong(value)
    case ExampleSingleValue(value: Float)      => JsonNumbers.fromFloatOrString(value)
    case ExampleSingleValue(value: Double)     => JsonNumbers.fromDoubleOrString(value)
    case ExampleSingleValue(value: Boolean)    => Json.fromBoolean(value)
    case ExampleSingleValue(value: BigDecimal) => Json.fromBigDecimal(value)
    case ExampleSingleValue(value: BigInt)     => Json.fromBigInt(value)
    case ExampleSingleValue(null)              => Json.Null
    case ExampleSingleValue(value)             => Json.fromString(value.toString)
  }

  implicit val encoderMultipleExampleValue: Encoder[ExampleMultipleValue] =
    Encoder.instance(e => Json.fromValues(e.values.map(v => encoderExampleSingleValue(ExampleSingleValue(v)))))

  implicit val encoderExampleValue: Encoder[ExampleValue] = Encoder.instance {
    case e: ExampleMultipleValue => encoderMultipleExampleValue(e)
    case e: ExampleSingleValue   => encoderExampleSingleValue(e)
  }

  implicit val encoderSchemaType: Encoder[SchemaType] = Encoder.instance(t => Json.fromString(t.value))

  implicit val encoderKeyPattern: KeyEncoder[Pattern] = KeyEncoder.encodeKeyString.contramap(_.value)
  implicit val encoderPattern: Encoder[Pattern] = Encoder.encodeString.contramap(_.value)

  implicit val encoderDiscriminator: Encoder[Discriminator] = Encoder.instance { d =>
    Json.Obj(
      JsonAstOps.dropNulls(
        JsonObject(
          Vector(
            "propertyName" := d.propertyName,
            "mapping" := d.mapping
          )
        )
      )
    )
  }

  implicit val encoderExternalDocumentation: Encoder[ExternalDocumentation] = Encoder.instance { e =>
    Json.Obj(
      JsonAstOps.dropNullsExpandExtensions(
        JsonObject(
          Vector(
            "url" := e.url,
            "description" := e.description,
            "extensions" := e.extensions
          )
        )
      )
    )
  }

  implicit val encoderAnySchema: Encoder[AnySchema] = Encoder.instance {
    case AnySchema.Anything =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object  => Json.obj()
        case AnySchema.Encoding.Boolean => Json.True
      }
    case AnySchema.Nothing =>
      anyObjectEncoding match {
        case AnySchema.Encoding.Object  => Json.obj("not" -> Json.obj())
        case AnySchema.Encoding.Boolean => Json.False
      }
  }

  implicit val encoderSchema: Encoder[Schema] = Encoder.instance { s =>
    val nullSchema = Schema(`type` = Some(List(SchemaType.Null)))

    val enumAndConstFields =
      if (openApi30 && s.const.isDefined)
        Vector("enum" := s.const.map(List(_)), "const" := (None: Option[ExampleValue]))
      else
        Vector("enum" := s.`enum`, "const" := s.const)

    val wrappedNullableRef30: Option[Schema] = s.anyOf match {
      case List(refSchema: Schema, `nullSchema`) if refSchema.$ref.isDefined && openApi30 => Some(refSchema)
      case _                                                                              => None
    }

    val typeAndNullable = s.`type` match {
      case Some(List(tpe))                               => Vector("type" := tpe)
      case Some(List(tpe, SchemaType.Null)) if openApi30 => Vector("type" := tpe, "nullable" := true)
      case None if wrappedNullableRef30.isDefined        => Vector("nullable" := true)
      case t                                             => Vector("type" := t)
    }

    val minFields = (s.minimum, s.exclusiveMinimum) match {
      case (None, Some(min)) if openApi30 => Vector("minimum" := min, "exclusiveMinimum" := true)
      case _                              => Vector("minimum" := s.minimum, "exclusiveMinimum" := s.exclusiveMinimum)
    }

    val maxFields = (s.maximum, s.exclusiveMaximum) match {
      case (None, Some(max)) if openApi30 => Vector("maximum" := max, "exclusiveMaximum" := true)
      case _                              => Vector("maximum" := s.maximum, "exclusiveMaximum" := s.exclusiveMaximum)
    }

    val exampleFields = s.examples match {
      case Some(List(example)) if openApi30 => Vector("example" := example)
      case _                                => Vector("examples" := s.examples)
    }

    val entries =
      Vector(
        "$schema" := s.$schema,
        "$vocabulary" := s.$vocabulary,
        "$id" := s.$id,
        "$anchor" := s.$anchor,
        "$dynamicAnchor" := s.$dynamicAnchor,
        "$ref" := s.$ref,
        "$dynamicRef" := s.$dynamicRef,
        "$comment" := s.$comment,
        "$defs" := s.$defs,
        "title" := s.title,
        "description" := s.description,
        "default" := s.default,
        "deprecated" := s.deprecated,
        "readOnly" := s.readOnly,
        "writeOnly" := s.writeOnly
      ) ++ exampleFields ++ typeAndNullable ++ enumAndConstFields ++ Vector(
        "format" := s.format,
        "allOf" := wrappedNullableRef30.map(List[SchemaLike](_)).getOrElse(s.allOf),
        "anyOf" := (if (wrappedNullableRef30.isDefined) Nil else s.anyOf),
        "oneOf" := s.oneOf,
        "not" := s.not,
        "if" := s.`if`,
        "then" := s.`then`,
        "else" := s.`else`,
        "dependentSchemas" := s.dependentSchemas,
        "multipleOf" := s.multipleOf
      ) ++ minFields ++ maxFields ++ Vector(
        "maxLength" := s.maxLength,
        "minLength" := s.minLength,
        "pattern" := s.pattern,
        "maxItems" := s.maxItems,
        "minItems" := s.minItems,
        "uniqueItems" := s.uniqueItems,
        "maxContains" := s.maxContains,
        "minContains" := s.minContains,
        "prefixItems" := s.prefixItems,
        "items" := s.items,
        "contains" := s.contains,
        "unevaluatedItems" := s.unevaluatedItems,
        "maxProperties" := s.maxProperties,
        "minProperties" := s.minProperties,
        "required" := s.required,
        "dependentRequired" := s.dependentRequired,
        "discriminator" := s.discriminator,
        "properties" := s.properties,
        "patternProperties" := s.patternProperties,
        "additionalProperties" := s.additionalProperties,
        "propertyNames" := s.propertyNames,
        "unevaluatedProperties" := s.unevaluatedProperties,
        "externalDocs" := s.externalDocs,
        "extensions" := s.extensions
      )

    Json.Obj(JsonAstOps.dropNullsExpandExtensions(JsonObject(entries)))
  }

  implicit val encoderSchemaLike: Encoder[SchemaLike] = Encoder.instance {
    case s: AnySchema => encoderAnySchema(s)
    case s: Schema    => encoderSchema(s)
  }

  // --- ListMap encoding: empty -> null (dropped), unless nullWhenEmpty=false (emit `{}`) ---
  implicit def encodeListMap[K, V](implicit k: KeyEncoder[K], v: Encoder[V]): Encoder[ListMap[K, V]] =
    doEncodeListMap(nullWhenEmpty = true)

  protected def doEncodeListMap[K, V](
      nullWhenEmpty: Boolean
  )(implicit k: KeyEncoder[K], v: Encoder[V]): Encoder[ListMap[K, V]] = Encoder.instance {
    case m if m.isEmpty && nullWhenEmpty => Json.Null
    case m => Json.Obj(JsonObject(m.iterator.map { case (key, value) => k(key) -> v(value) }.toVector))
  }
}
