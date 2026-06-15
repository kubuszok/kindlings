package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import sttp.apispec._

import scala.collection.immutable.ListMap

/** Port of `sttp.apispec.internal.JsonSchemaCirceDecoders` onto the kindlings [[Json]] AST.
  *
  * Reproduces the OAS 3.0/3.1 normalization (`adjustType`/`adjustMaximum`/`adjustMinimum`/`adjustExample`/
  * `translateDefinitionsTo$def`), the `nullable` post-processing, and extension stripping (`withExtensions`).
  */
private[openapijsoniter] trait JsonSchemaDecoders {

  implicit val decodeSchemaType: Decoder[SchemaType] = Decoder.decodeString.emap {
    case SchemaType.Integer.value => Right(SchemaType.Integer)
    case SchemaType.Boolean.value => Right(SchemaType.Boolean)
    case SchemaType.String.value  => Right(SchemaType.String)
    case SchemaType.Number.value  => Right(SchemaType.Number)
    case SchemaType.Array.value   => Right(SchemaType.Array)
    case SchemaType.Object.value  => Right(SchemaType.Object)
    case SchemaType.Null.value    => Right(SchemaType.Null)
    case err                      => Left(s"$err is an unknown schema type")
  }

  implicit val decodePatternKey: KeyDecoder[Pattern] = KeyDecoder.decodeKeyString.map(Pattern.apply)
  implicit val decodePattern: Decoder[Pattern] = Decoder.decodeString.map(Pattern.apply)

  implicit val discriminatorDecoder: Decoder[Discriminator] = Decoder.fromCursor { c =>
    for {
      propertyName <- c.get[String]("propertyName")
      mapping <- c.get[Option[ListMap[String, String]]]("mapping")
    } yield Discriminator(propertyName, mapping)
  }

  implicit val exampleSingleValueDecoder: Decoder[ExampleSingleValue] =
    Decoder.decodeJson.map(json => json.asString.map(ExampleSingleValue(_)).getOrElse(ExampleSingleValue(json)))

  implicit val exampleMultipleValueDecoder: Decoder[ExampleMultipleValue] =
    Decoder.decodeList[Json].map { json =>
      val listString = json.flatMap(_.asString)
      if (listString.nonEmpty) ExampleMultipleValue(listString)
      else ExampleMultipleValue(json)
    }

  implicit val exampleValueDecoder: Decoder[ExampleValue] =
    exampleMultipleValueDecoder.widen[ExampleValue].or(exampleSingleValueDecoder.widen[ExampleValue])

  implicit val extensionValueDecoder: Decoder[ExtensionValue] =
    Decoder.decodeJson.map(j => ExtensionValue(JsonAstOps.spaces2(j)))

  implicit val extensionsDecoder: Decoder[ListMap[String, ExtensionValue]] =
    Decoder.decodeListMap[String, ExtensionValue].map(_.filter(_._1.startsWith("x-")))

  implicit val externalDocumentationDecoder: Decoder[ExternalDocumentation] =
    withExtensions(Decoder.fromCursor { c =>
      for {
        url <- c.get[String]("url")
        description <- c.get[Option[String]]("description")
        extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
      } yield ExternalDocumentation(url, description, extensions)
    })

  // Per-field decoders used inside the Schema decoder where circe relaxes the shape:
  // missing list/map keys default to empty rather than failing.
  private def listMapDecoder[A](implicit d: Decoder[A]): Decoder[ListMap[String, A]] =
    Decoder.decodeOption(Decoder.decodeListMap[String, A]).map(_.getOrElse(ListMap.empty))
  private def listPatternMapDecoder[A](implicit d: Decoder[A]): Decoder[ListMap[Pattern, A]] =
    Decoder.decodeOption(Decoder.decodeListMap[Pattern, A]).map(_.getOrElse(ListMap.empty))
  private val listDependentFieldsDecoder: Decoder[ListMap[String, List[String]]] =
    Decoder.decodeOption(Decoder.decodeListMap[String, List[String]]).map(_.getOrElse(ListMap.empty))
  private def listDecoder[A](implicit d: Decoder[A]): Decoder[List[A]] =
    Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

  implicit val schemaDecoder: Decoder[Schema] = {
    implicit def lm[A: Decoder]: Decoder[ListMap[String, A]] = listMapDecoder[A]
    implicit def lpm[A: Decoder]: Decoder[ListMap[Pattern, A]] = listPatternMapDecoder[A]
    implicit val ldf: Decoder[ListMap[String, List[String]]] = listDependentFieldsDecoder
    implicit def ll[A: Decoder]: Decoder[List[A]] = listDecoder[A]

    val derived: Decoder[Schema] = Decoder.fromCursor { c =>
      for {
        $schema <- c.get[Option[String]]("$schema")
        $vocabulary <- c.get[Option[ListMap[String, Boolean]]]("$vocabulary")
        $id <- c.get[Option[String]]("$id")
        $anchor <- c.get[Option[String]]("$anchor")
        $dynamicAnchor <- c.get[Option[String]]("$dynamicAnchor")
        $ref <- c.get[Option[String]]("$ref")
        $dynamicRef <- c.get[Option[String]]("$dynamicRef")
        $comment <- c.get[Option[String]]("$comment")
        $defs <- c.get[Option[ListMap[String, SchemaLike]]]("$defs")
        title <- c.get[Option[String]]("title")
        description <- c.get[Option[String]]("description")
        default <- c.get[Option[ExampleValue]]("default")
        deprecated <- c.get[Option[Boolean]]("deprecated")
        readOnly <- c.get[Option[Boolean]]("readOnly")
        writeOnly <- c.get[Option[Boolean]]("writeOnly")
        examples <- c.get[Option[List[ExampleValue]]]("examples")
        tpe <- c.get[Option[List[SchemaType]]]("type")
        enum_ <- c.get[Option[List[ExampleValue]]]("enum")
        const <- c.get[Option[ExampleValue]]("const")
        format <- c.get[Option[String]]("format")
        allOf <- c.getOrElse[List[SchemaLike]]("allOf")(List.empty)
        anyOf <- c.getOrElse[List[SchemaLike]]("anyOf")(List.empty)
        oneOf <- c.getOrElse[List[SchemaLike]]("oneOf")(List.empty)
        not <- c.get[Option[SchemaLike]]("not")
        if_ <- c.get[Option[SchemaLike]]("if")
        then_ <- c.get[Option[SchemaLike]]("then")
        else_ <- c.get[Option[SchemaLike]]("else")
        dependentSchemas <- c.getOrElse[ListMap[String, SchemaLike]]("dependentSchemas")(ListMap.empty)
        multipleOf <- c.get[Option[BigDecimal]]("multipleOf")
        minimum <- c.get[Option[BigDecimal]]("minimum")
        exclusiveMinimum <- c.get[Option[BigDecimal]]("exclusiveMinimum")
        maximum <- c.get[Option[BigDecimal]]("maximum")
        exclusiveMaximum <- c.get[Option[BigDecimal]]("exclusiveMaximum")
        maxLength <- c.get[Option[Int]]("maxLength")
        minLength <- c.get[Option[Int]]("minLength")
        pattern <- c.get[Option[Pattern]]("pattern")
        maxItems <- c.get[Option[Int]]("maxItems")
        minItems <- c.get[Option[Int]]("minItems")
        uniqueItems <- c.get[Option[Boolean]]("uniqueItems")
        maxContains <- c.get[Option[Int]]("maxContains")
        minContains <- c.get[Option[Int]]("minContains")
        prefixItems <- c.get[Option[List[SchemaLike]]]("prefixItems")
        items <- c.get[Option[SchemaLike]]("items")
        contains <- c.get[Option[SchemaLike]]("contains")
        unevaluatedItems <- c.get[Option[SchemaLike]]("unevaluatedItems")
        maxProperties <- c.get[Option[Int]]("maxProperties")
        minProperties <- c.get[Option[Int]]("minProperties")
        required <- c.getOrElse[List[String]]("required")(List.empty)
        dependentRequired <- c.getOrElse[ListMap[String, List[String]]]("dependentRequired")(ListMap.empty)
        discriminator <- c.get[Option[Discriminator]]("discriminator")
        properties <- c.getOrElse[ListMap[String, SchemaLike]]("properties")(ListMap.empty)
        patternProperties <- c.getOrElse[ListMap[Pattern, SchemaLike]]("patternProperties")(ListMap.empty)
        additionalProperties <- c.get[Option[SchemaLike]]("additionalProperties")
        propertyNames <- c.get[Option[SchemaLike]]("propertyNames")
        unevaluatedProperties <- c.get[Option[SchemaLike]]("unevaluatedProperties")
        externalDocs <- c.get[Option[ExternalDocumentation]]("externalDocs")
        extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
      } yield Schema(
        $schema, $vocabulary, $id, $anchor, $dynamicAnchor, $ref, $dynamicRef, $comment, $defs,
        title, description, default, deprecated, readOnly, writeOnly, examples,
        tpe, enum_, const, format,
        allOf, anyOf, oneOf, not, if_, then_, else_, dependentSchemas,
        multipleOf, minimum, exclusiveMinimum, maximum, exclusiveMaximum,
        maxLength, minLength, pattern,
        maxItems, minItems, uniqueItems, maxContains, minContains,
        prefixItems, items, contains, unevaluatedItems,
        maxProperties, minProperties, required, dependentRequired, discriminator,
        properties, patternProperties, additionalProperties, propertyNames, unevaluatedProperties,
        externalDocs, extensions
      )
    }

    adjustSyntax(withExtensions(derived))
  }

  // OAS 3.0/3.1 normalization helpers, operating on JsonObject before deriving.
  private def translateDefinitionsTo$def(obj: JsonObject): JsonObject = {
    val definitions = obj("definitions").orElse(obj("$defs"))
    definitions.map(j => obj.remove("definitions").remove("$defs").add("$defs", j)).getOrElse(obj)
  }
  private def adjustMaximum(obj: JsonObject): JsonObject =
    (obj("maximum"), obj("exclusiveMaximum")) match {
      case (Some(max), Some(Json.Bool(true))) => obj.remove("maximum").add("exclusiveMaximum", max)
      case _                                  => obj
    }
  private def adjustMinimum(obj: JsonObject): JsonObject =
    (obj("minimum"), obj("exclusiveMinimum")) match {
      case (Some(min), Some(Json.Bool(true))) => obj.remove("minimum").add("exclusiveMinimum", min)
      case _                                  => obj
    }
  private def adjustExample(obj: JsonObject): JsonObject =
    obj("example") match {
      case Some(example) => obj.remove("example").add("examples", Json.arr(example))
      case _             => obj
    }
  private def adjustType(obj: JsonObject): JsonObject =
    obj("type") match {
      case Some(tpe) if tpe.isString => obj.add("type", Json.arr(tpe))
      case _                         => obj
    }

  private def mapObject(json: Json, f: JsonObject => JsonObject): Json = json match {
    case Json.Obj(obj) => Json.Obj(f(obj))
    case other         => other
  }

  private def adjustSyntax(decoder: Decoder[Schema]): Decoder[Schema] = Decoder.fromCursor { c =>
    val nullable = c.get[Boolean]("nullable").toOption.contains(true)
    val modded = c.withFocus { json =>
      var j = mapObject(json, adjustType)
      j = mapObject(j, adjustMaximum)
      j = mapObject(j, adjustMinimum)
      j = mapObject(j, adjustExample)
      j = mapObject(j, translateDefinitionsTo$def)
      j
    }
    decoder.apply(modded.focus).map(s => if (nullable) s.nullable else s)
  }

  implicit val anySchemaDecoder: Decoder[AnySchema] = Decoder.instance { json =>
    def fromBool(b: Boolean): AnySchema = if (b) AnySchema.Anything else AnySchema.Nothing
    def fromObject(obj: JsonObject): Option[AnySchema] = {
      val target = Json.obj("not" -> Json.obj()).asObject.get
      if (obj.isEmpty) Some(AnySchema.Anything)
      else if (obj.fields == target.fields) Some(AnySchema.Nothing)
      else None
    }
    val result: Option[AnySchema] = json.fold(
      None,
      b => Some(fromBool(b)),
      _ => None,
      _ => None,
      _ => None,
      fromObject
    )
    result.toRight(DecodingFailure("Unable to decode AnyObject"))
  }

  implicit val schemaLikeDecoder: Decoder[SchemaLike] =
    anySchemaDecoder.widen[SchemaLike].or(schemaDecoder.widen[SchemaLike])

  /** Strips `x-`-prefixed keys into a synthetic `extensions` object before delegating, mirroring circe. */
  def withExtensions[A](decoder: Decoder[A]): Decoder[A] = Decoder.fromCursor { c =>
    val modded = c.withFocus { json =>
      mapObject(
        json,
        obj => {
          val withoutExt = JsonAstOps.filterKeys(obj, !_.startsWith("x-"))
          val exts = JsonAstOps.filterKeys(obj, _.startsWith("x-"))
          withoutExt.add("extensions", Json.Obj(exts))
        }
      )
    }
    decoder.apply(modded.focus)
  }
}
