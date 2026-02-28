package hearth.kindlings.tapirschemaderivation.internal.runtime

import sttp.tapir.{FieldName, Schema, SchemaType, Validator}
import sttp.tapir.Schema.SName

object TapirSchemaUtils {

  /** Apply Tapir annotations to a schema at runtime. Ignores non-Tapir annotations silently. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def enrichSchema[T](schema: Schema[T], annotations: List[Any]): Schema[T] =
    annotations.foldLeft(schema) {
      case (s, ann: Schema.annotations.description)                => s.description(ann.text)
      case (s, ann: Schema.annotations.encodedExample)             => s.encodedExample(ann.example)
      case (s, ann: Schema.annotations.default[T @unchecked])      => s.default(ann.default, ann.encoded)
      case (s, ann: Schema.annotations.validate[T @unchecked])     => s.validate(ann.v)
      case (s, ann: Schema.annotations.validateEach[T @unchecked]) =>
        s.modifyUnsafe(Schema.ModifyCollectionElements)((_: Schema[T]).validate(ann.v))
      case (s, ann: Schema.annotations.format)    => s.format(ann.format)
      case (s, ann: Schema.annotations.title)     => s.title(ann.name)
      case (s, _: Schema.annotations.deprecated)  => s.deprecated(true)
      case (s, _: Schema.annotations.hidden)      => s.hidden(true)
      case (s, ann: Schema.annotations.customise) => ann.f(s).asInstanceOf[Schema[T]]
      case (s, _)                                 => s // ignore non-Tapir annotations
    }

  /** Build an SProductField with annotation support. @encodedName from annotations overrides the JSON-config encoded
    * name.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def productFieldWithAnnotations[T](
      scalaName: String,
      jsonConfigEncodedName: String,
      fieldSchema: Schema[Any],
      index: Int,
      fieldAnnotations: List[Any]
  ): SchemaType.SProductField[T] = {
    val encodedName = fieldAnnotations
      .collectFirst { case ann: Schema.annotations.encodedName =>
        ann.name
      }
      .getOrElse(jsonConfigEncodedName)
    val enrichedSchema = enrichSchema(fieldSchema, fieldAnnotations)
    SchemaType.SProductField[T, Any](
      FieldName(scalaName, encodedName),
      enrichedSchema,
      t => Some(t.asInstanceOf[Product].productElement(index))
    )
  }

  /** Empty field list, typed for use in cross-quotes list construction. */
  def emptyFieldList[T]: List[SchemaType.SProductField[T]] = Nil

  /** Create an SProductField for a case class field using productElement access. */
  def productField[T](
      scalaName: String,
      encodedName: String,
      fieldSchema: Schema[Any],
      index: Int
  ): SchemaType.SProductField[T] =
    SchemaType.SProductField[T, Any](
      FieldName(scalaName, encodedName),
      fieldSchema,
      t => Some(t.asInstanceOf[Product].productElement(index))
    )

  /** Create a Schema with SProduct schema type. */
  def productSchema[T](
      name: SName,
      fields: List[SchemaType.SProductField[T]]
  ): Schema[T] =
    Schema[T](SchemaType.SProduct[T](fields), Some(name))

  /** Create a Schema with SCoproduct schema type. Uses name-based runtime matching.
    *
    * @param resolvedNames
    *   constructor names after applying the JSON library's name transform (e.g., `config.transformConstructorNames`).
    *   Used as discriminator mapping keys. Must be the same size and order as `subtypes`.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def coproductSchema[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String]
  ): Schema[T] = {
    val disc: Option[SchemaType.SDiscriminator] = discriminator.map { discFieldName =>
      val mapping: Map[String, SchemaType.SRef[Any]] = subtypes
        .zip(resolvedNames)
        .flatMap { case (s, resolvedName) =>
          s.name.map { sname =>
            resolvedName -> SchemaType.SRef[Any](sname)
          }
        }
        .toMap
      SchemaType.SDiscriminator(FieldName(discFieldName, discFieldName), mapping)
    }

    // When a discriminator is present, add the discriminator field to each child SProduct schema
    // and set the encodedDiscriminatorValue attribute, matching upstream Tapir's addDiscriminatorField.
    val enrichedSubtypes: List[Schema[Any]] = discriminator match {
      case Some(discFieldName) =>
        subtypes.zip(resolvedNames).map { case (childSchema, resolvedName) =>
          val withDiscField = childSchema.schemaType match {
            case p: SchemaType.SProduct[Any @unchecked] =>
              val alreadyHasField = p.fields.exists(_.name.encodedName == discFieldName)
              if (alreadyHasField) childSchema
              else {
                val discFieldSchema = Schema.string.copy(
                  validator = Validator.enumeration(List(resolvedName), (v: String) => Some(v))
                )
                val discField = SchemaType.SProductField[Any, String](
                  FieldName(discFieldName, discFieldName),
                  discFieldSchema,
                  _ => Some(resolvedName)
                )
                childSchema.copy(schemaType = p.copy(fields = discField :: p.fields))
              }
            case _ => childSchema
          }
          withDiscField.attribute(
            Schema.EncodedDiscriminatorValue.Attribute,
            Schema.EncodedDiscriminatorValue(resolvedName)
          )
        }
      case None => subtypes
    }

    Schema[T](
      SchemaType.SCoproduct[T](enrichedSubtypes, disc) { (value: T) =>
        val cn = value.getClass.getName
        enrichedSubtypes.collectFirst {
          case s if s.name.exists(sn => cn == sn.fullName || cn == (sn.fullName + "$")) =>
            SchemaType.SchemaWithValue(s.asInstanceOf[Schema[Any]], value)
        }
      },
      Some(name)
    )
  }

  /** Create a Schema with string-based enum schema type (for case object-only sealed traits). */
  def stringEnumSchema[T](
      name: SName,
      values: List[T],
      encodedNames: List[String]
  ): Schema[T] = {
    val valMap = values.zip(encodedNames).toMap
    Schema
      .string[T]
      .name(name)
      .copy(validator = sttp.tapir.Validator.enumeration(values, v => valMap.get(v)))
  }

  /** Create a Schema with SRef schema type for recursive references. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def refSchema[T](name: SName): Schema[T] =
    Schema[T](SchemaType.SRef[T](name))

  /** Wrap an element schema as Option. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def optionSchema[E](elementSchema: Schema[E]): Schema[Any] =
    elementSchema.asOption.asInstanceOf[Schema[Any]]

  /** Wrap an element schema as a collection (List, Vector, Set, etc.). */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def collectionSchema[E](elementSchema: Schema[E]): Schema[Any] =
    Schema[Any](
      SchemaType.SArray[Any, E](elementSchema)(_.asInstanceOf[Iterable[E]]),
      isOptional = true
    )

  /** Wrap a value schema as a Map[String, V]. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def mapSchema[V](valueSchema: Schema[V]): Schema[Any] =
    Schema[Any](
      SchemaType.SOpenProduct[Any, V](Nil, valueSchema)(_.asInstanceOf[Map[String, V]]),
      isOptional = true
    )

  /** Wrap a value schema as a Map encoded as array of key-value pair objects (for jsoniter mapAsArray). */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def mapAsArraySchema[V](valueSchema: Schema[V]): Schema[Any] = {
    val pairSchema: Schema[Any] = Schema[Any](
      SchemaType.SProduct[Any](
        List(
          SchemaType.SProductField[Any, String](FieldName("key"), Schema.string[String], _ => None),
          SchemaType.SProductField[Any, V](FieldName("value"), valueSchema, _ => None)
        )
      )
    )
    Schema[Any](
      SchemaType.SArray[Any, Any](pairSchema)(_.asInstanceOf[Iterable[Any]]),
      isOptional = true
    )
  }

  /** Conditionally mark a schema as optional at runtime. */
  def markFieldOptional(schema: Schema[Any], optional: Boolean): Schema[Any] =
    if (optional) schema.copy(isOptional = true) else schema

  /** Conditionally add "string" format to a schema at runtime. */
  def markFieldStringFormat(schema: Schema[Any], stringified: Boolean): Schema[Any] =
    if (stringified) schema.format("string") else schema

  /** Parse a fully-qualified type name (with type parameters) into an SName.
    *
    * For `"com.example.Foo[com.example.Bar, scala.Int]"`:
    *   - `fullName = "com.example.Foo"`
    *   - `typeParameterShortNames = List("Bar", "Int")`
    */
  def parseSName(fullTypeName: String): SName = {
    val bracketIdx = fullTypeName.indexOf('[')
    if (bracketIdx < 0) SName(fullTypeName)
    else {
      val baseName = fullTypeName.substring(0, bracketIdx)
      val typeParamsStr = fullTypeName.substring(bracketIdx + 1, fullTypeName.length - 1)
      val typeParams = splitTopLevelTypeParams(typeParamsStr).map(shortenTypeName)
      SName(baseName, typeParams)
    }
  }

  private def splitTopLevelTypeParams(s: String): List[String] = {
    val result = List.newBuilder[String]
    var depth = 0
    var start = 0
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '['               => depth += 1
        case ']'               => depth -= 1
        case ',' if depth == 0 =>
          result += s.substring(start, i).trim
          start = i + 1
        case _ =>
      }
      i += 1
    }
    if (start < s.length) result += s.substring(start).trim
    result.result()
  }

  private def shortenTypeName(fullName: String): String = {
    val bracketIdx = fullName.indexOf('[')
    if (bracketIdx < 0) {
      fullName.split('.').last
    } else {
      val base = fullName.substring(0, bracketIdx).split('.').last
      val params = fullName.substring(bracketIdx + 1, fullName.length - 1)
      val shortParams = splitTopLevelTypeParams(params).map(shortenTypeName)
      base + shortParams.mkString("[", ",", "]")
    }
  }
}
