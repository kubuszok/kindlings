package hearth.kindlings.tapirschemaderivation.internal.runtime

import hearth.kindlings.tapirschemaderivation.KindlingsSchema
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
  def coproductSchema[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String]
  ): Schema[T] = coproductSchema[T](name, subtypes, discriminator, resolvedNames, wrapNonObjects = false)

  /** With `wrapNonObjects` (circe), a subtype whose schema is not a product is shown, when there is a discriminator, in
    * the wrapped form circe writes for it (`{"Name": value}`): a product with a single field named after the subtype,
    * without the discriminator field and outside the discriminator mapping.
    */
  def coproductSchema[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String],
      wrapNonObjects: Boolean
  ): Schema[T] = coproductSchema[T](name, subtypes, discriminator, resolvedNames, wrapNonObjects, wrapped = false)

  /** With `wrapped` (circe), without a discriminator every subtype is shown in the wrapped form the codec writes:
    * `{"Circle":{"radius":1.0}}`, a product with a single field named after the subtype.
    */
  def coproductSchema[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String],
      wrapNonObjects: Boolean,
      wrapped: Boolean
  ): Schema[T] =
    coproductSchemaWithEncoding[T](
      name,
      subtypes,
      discriminator,
      resolvedNames,
      singletons = subtypes.map(_ => false),
      singletonsAsStrings = false,
      wrapped = wrapped,
      wrapNonObjects = wrapNonObjects
    )

  /** As `coproductSchema`, also describing how the subtypes are encoded without a discriminator.
    *
    * @param singletons
    *   which subtypes are singletons (case objects). Same size and order as `subtypes`.
    * @param singletonsAsStrings
    *   whether, without a discriminator, singleton subtypes are JSON strings (their resolved names)
    * @param wrapped
    *   whether, without a discriminator, the (other) subtypes are wrapped in a single-field object keyed by their
    *   resolved names (`{"Circle":{"radius":1.0}}`)
    * @param wrapNonObjects
    *   whether, with a discriminator, subtypes that are not JSON objects are shown in the wrapped form instead of
    *   carrying the discriminator (circe)
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def coproductSchemaWithEncoding[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String],
      singletons: List[Boolean],
      singletonsAsStrings: Boolean,
      wrapped: Boolean,
      wrapNonObjects: Boolean = false
  ): Schema[T] = {
    // Objects (products, maps, sums of objects, references to named types) carry the discriminator.
    def isWrapped(s: Schema[Any]): Boolean = wrapNonObjects && discriminator.isDefined && (s.schemaType match {
      case _: SchemaType.SProduct[?] | _: SchemaType.SOpenProduct[?, ?] | _: SchemaType.SCoproduct[?] |
          _: SchemaType.SRef[?] =>
        false
      case _ => true
    })
    val disc: Option[SchemaType.SDiscriminator] = discriminator.map { discFieldName =>
      val mapping: Map[String, SchemaType.SRef[Any]] = subtypes
        .zip(resolvedNames)
        .filterNot { case (s, _) => isWrapped(s) }
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
          if (isWrapped(childSchema)) {
            val wrapperField = SchemaType.SProductField[Any, Any](
              FieldName(resolvedName, resolvedName),
              childSchema,
              (v: Any) => Some(v)
            )
            childSchema.copy(
              schemaType = SchemaType.SProduct[Any](List(wrapperField)),
              format = None,
              validator = Validator.pass
            )
          } else {
            // As tapir's `SCoproduct.addDiscriminatorField`: the discriminator is appended to the subtype's fields
            // (unless already there), and the field's schema carries the value (rendered as `const`).
            childSchema.schemaType match {
              case p: SchemaType.SProduct[Any @unchecked] =>
                val withField =
                  if (p.fields.exists(_.name.encodedName == discFieldName)) p.fields
                  else
                    p.fields :+ SchemaType.SProductField[Any, String](
                      FieldName(discFieldName, discFieldName),
                      Schema.string,
                      _ => Some(resolvedName)
                    )
                val fields = withField.map { field =>
                  if (field.name.encodedName != discFieldName) field
                  else
                    SchemaType.SProductField[Any, field.FieldType](
                      field.name,
                      field.schema.encodedDiscriminatorValue(resolvedName),
                      field.get
                    )
                }
                childSchema.copy(schemaType = p.copy(fields = fields))
              case _ => childSchema
            }
          }
        }
      case None =>
        subtypes.zip(resolvedNames).zip(singletons).map { case ((childSchema, resolvedName), isSingleton) =>
          if (isSingleton && singletonsAsStrings) singletonStringSchema(childSchema.name, resolvedName)
          else if (wrapped) Schema.wrapWithSingleFieldProduct(childSchema, FieldName(resolvedName, resolvedName))
          else childSchema
        }
    }

    // Subtypes are matched by the names of the (unwrapped) child schemas.
    val matchable = subtypes.map(_.name).zip(enrichedSubtypes)
    Schema[T](
      SchemaType.SCoproduct[T](enrichedSubtypes, disc) { (value: T) =>
        val cn = value.getClass.getName
        // nested classes and objects: `pkg.Outer$Inner` / `pkg.Outer$Inner$` for `pkg.Outer.Inner`
        val dotted = cn.replace('$', '.')
        matchable.collectFirst {
          case (childName, s)
              if childName.exists(sn =>
                cn == sn.fullName || cn == (sn.fullName + "$") || dotted == sn.fullName || dotted == (sn.fullName + ".")
              ) =>
            SchemaType.SchemaWithValue(s.asInstanceOf[Schema[Any]], value)
        }
      },
      Some(name)
    )
  }

  /** As [[coproductSchemaWithEncoding]], choosing the subtype of a value (`SCoproduct.subtypeSchema`, used e.g. by
    * `Schema.applyValidation`) by its index in `subtypes`, computed by `subtypeIndex` (generated with a pattern match
    * on the subtypes; `-1` for none) instead of by the subtypes' names, which neither identify Scala 3 enum cases nor
    * subtypes renamed with `@encodedName`.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def coproductSchemaWithSubtypeIndex[T](
      name: SName,
      subtypes: List[Schema[Any]],
      discriminator: Option[String],
      resolvedNames: List[String],
      singletons: List[Boolean],
      singletonsAsStrings: Boolean,
      wrapped: Boolean,
      wrapNonObjects: Boolean,
      subtypeIndex: T => Int
  ): Schema[T] = {
    val base = coproductSchemaWithEncoding[T](
      name,
      subtypes,
      discriminator,
      resolvedNames,
      singletons,
      singletonsAsStrings,
      wrapped,
      wrapNonObjects
    )
    base.schemaType match {
      case c: SchemaType.SCoproduct[T @unchecked] =>
        val chosen = c.subtypes.toVector
        base.copy(schemaType = SchemaType.SCoproduct[T](c.subtypes, c.discriminator) { (value: T) =>
          val index = subtypeIndex(value)
          if (index >= 0 && index < chosen.size)
            Some(SchemaType.SchemaWithValue(chosen(index).asInstanceOf[Schema[Any]], value))
          else None
        })
      case _ => base
    }
  }

  /** A singleton subtype encoded as a JSON string: its resolved name. */
  private def singletonStringSchema(name: Option[SName], resolvedName: String): Schema[Any] =
    Schema
      .string[Any]
      .copy(name = name, validator = Validator.enumeration[Any](List(resolvedName), (v: Any) => Some(v)))

  /** A `scala.Enumeration` whose values are encoded as their (integer) `id`s. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def enumerationIdSchema[T](name: SName, values: List[T]): Schema[T] = enumerationIdSchemaNamed[T](name, name, values)

  /** As [[enumerationIdSchema]], with the name of the enumeration in the validator (as tapir's
    * `Schema.derivedEnumerationValueCustomise`), which may differ from the schema's (`@encodedName`).
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def enumerationIdSchemaNamed[T](name: SName, validatorName: SName, values: List[T]): Schema[T] =
    Schema[T](SchemaType.SInteger[T](), Some(name), format = Some("int32"))
      .copy(validator =
        Validator.enumeration(
          inEnumerationOrder(values),
          (v: T) => Some(v.asInstanceOf[scala.Enumeration#Value].id),
          Some(validatorName)
        )
      )

  /** A tuple encoded as a JSON array of its elements (positional `_1`, `_2`, ... fields, marked as a tuple, which
    * OpenAPI renders as an array with `prefixItems`).
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def tupleSchema[T](elements: List[Schema[Any]]): Schema[T] =
    Schema[T](
      SchemaType.SProduct[T](elements.zipWithIndex.map { case (elementSchema, index) =>
        SchemaType.SProductField[T, Any](
          FieldName(s"_${index + 1}"),
          elementSchema,
          t => Some(t.asInstanceOf[Product].productElement(index))
        )
      })
    ).attribute(Schema.Tuple.Attribute, Schema.Tuple(true))

  /** An `Option` nested in an `Option`, encoded as an ADT: `Some` (an object with the value under `valueFieldName`) and
    * `None` (a singleton), using the given ADT encoding.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def nestedOptionAdtSchema[V](
      valueSchema: Schema[V],
      valueTypeName: String,
      someName: String,
      noneName: String,
      valueFieldName: String,
      discriminator: Option[String],
      singletonsAsStrings: Boolean,
      wrapped: Boolean
  ): Schema[Any] = {
    val someSchema = Schema[Any](
      SchemaType.SProduct[Any](
        List(
          SchemaType.SProductField[Any, V](
            FieldName("value", valueFieldName),
            valueSchema,
            o => o.asInstanceOf[Option[V]]
          )
        )
      ),
      Some(SName("scala.Some", List(valueTypeName)))
    )
    val noneSchema = Schema[Any](SchemaType.SProduct[Any](Nil), Some(SName("scala.None")))
    coproductSchemaWithEncoding[Any](
      SName("scala.Option", List(valueTypeName)),
      List(someSchema, noneSchema),
      discriminator,
      List(someName, noneName),
      singletons = List(false, true),
      singletonsAsStrings = singletonsAsStrings,
      wrapped = wrapped
    )
  }

  /** Create a Schema with string-based enum schema type (for case object-only sealed traits). */
  def stringEnumSchema[T](
      name: SName,
      values: List[T],
      encodedNames: List[String]
  ): Schema[T] = stringEnumSchemaNamed[T](name, name, values, encodedNames)

  /** As [[stringEnumSchema]], with the name of the enumerated type in the validator (as tapir's
    * `Validator.derivedEnumeration` / `Schema.derivedEnumerationValue`), which may differ from the schema's
    * (`@encodedName`).
    */
  def stringEnumSchemaNamed[T](
      name: SName,
      validatorName: SName,
      values: List[T],
      encodedNames: List[String]
  ): Schema[T] = {
    val valMap = values.zip(encodedNames).toMap
    Schema
      .string[T]
      .name(name)
      .copy(validator =
        sttp.tapir.Validator.enumeration(inEnumerationOrder(values), (v: T) => valMap.get(v), Some(validatorName))
      )
  }

  /** `scala.Enumeration` values in the order of their ids (as `Enumeration#values`, which tapir lists), whatever the
    * order the compiler reports them in; other values unchanged.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.IsInstanceOf"))
  private def inEnumerationOrder[T](values: List[T]): List[T] =
    if (values.nonEmpty && values.forall(_.isInstanceOf[scala.Enumeration#Value]))
      values.sortBy(_.asInstanceOf[scala.Enumeration#Value].id)
    else values

  /** The name of a schema: `SName(encodedName)` when the type is annotated with `@encodedName` (as in tapir's
    * derivation, the type parameters are then not part of the name), otherwise `name`.
    */
  def sNameWithEncodedName(name: SName, typeAnnotations: List[Any]): SName =
    typeAnnotations.collectFirst { case ann: Schema.annotations.encodedName => SName(ann.name) }.getOrElse(name)

  /** Create a Schema with SRef schema type for recursive references. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def refSchema[T](name: SName): Schema[T] =
    Schema[T](SchemaType.SRef[T](name))

  /** Wrap an element schema as Option. */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def optionSchema[E](elementSchema: Schema[E]): Schema[Any] =
    elementSchema.asOption.asInstanceOf[Schema[Any]]

  /** The schema of a Scala 3 nullable field (`E | Null`): optional, and `null` or a value described by `elementSchema`.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Null"))
  def nullableSchema[E](elementSchema: Schema[E]): Schema[Any] =
    Schema[Any](
      SchemaType.SOption[Any, E](elementSchema)(v => Option(v.asInstanceOf[E])),
      isOptional = true,
      format = elementSchema.format,
      deprecated = elementSchema.deprecated
    )

  /** Wrap an element schema as a collection (List, Vector, Set, etc.). */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def collectionSchema[E](elementSchema: Schema[E]): Schema[Any] =
    Schema[Any](
      SchemaType.SArray[Any, E](elementSchema)(_.asInstanceOf[Iterable[E]]),
      isOptional = true
    )

  /** [[collectionSchema]] of a collection that is not an `Iterable` (e.g. cats' `NonEmptyList`), whose elements are
    * returned by `toIterable` (so that e.g. `Schema.applyValidation` can reach them).
    */
  def collectionSchemaVia[E](elementSchema: Schema[E], toIterable: Any => Iterable[E]): Schema[Any] =
    Schema[Any](SchemaType.SArray[Any, E](elementSchema)(toIterable), isOptional = true)

  /** The schema of a collection (or map) that cannot be empty (e.g. cats' `NonEmptyList`), as in tapir-cats: required,
    * with `Validator.minSize(1)` (`minItems: 1`) checked on the elements returned by `toIterable`.
    */
  def nonEmptyCollectionSchema(schema: Schema[Any], toIterable: Any => Iterable[Any]): Schema[Any] =
    schema.copy(isOptional = false).validate(sttp.tapir.Validator.minSize[Any, Iterable](1).contramap(toIterable))

  /** Wrap an element schema as a Set (collection with unique items, same as tapir's `Schema.schemaForSet`). */
  def setSchema[E](elementSchema: Schema[E]): Schema[Any] =
    collectionSchema[E](elementSchema).attribute(Schema.UniqueItems.Attribute, Schema.UniqueItems(true))

  /** Wrap a value schema as a Map[String, V], named like tapir's `Schema.schemaForMap` (`Map_V`); not optional, as
    * tapir's (fields set their optionality from the JSON config).
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def mapSchema[V](valueSchema: Schema[V], name: SName): Schema[Any] =
    Schema[Any](
      SchemaType.SOpenProduct[Any, V](Nil, valueSchema)(_.asInstanceOf[Map[String, V]]),
      name = Some(name)
    )

  /** Wrap a value schema as a Map[K, V], whose keys are converted to Strings with `keyToString` (e.g. unwrapping a
    * value type/opaque type key), named like tapir's `Schema.schemaForMap[K, V](keyToString)` (`Map_K_V`).
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def mapSchemaWithKeys[K, V](valueSchema: Schema[V], keyToString: K => String, name: SName): Schema[Any] =
    Schema[Any](
      SchemaType.SOpenProduct[Any, V](Nil, valueSchema)(
        _.asInstanceOf[Map[K, V]].map { case (k, v) => keyToString(k) -> v }
      ),
      name = Some(name)
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

  /** Mark a schema as optional or required at runtime (a field is optional exactly when the decoder accepts it absent).
    */
  def setFieldOptional(schema: Schema[Any], optional: Boolean): Schema[Any] =
    if (schema.isOptional == optional) schema else schema.copy(isOptional = optional)

  /** Conditionally add "string" format to a schema at runtime. */
  def markFieldStringFormat(schema: Schema[Any], stringified: Boolean): Schema[Any] =
    if (stringified) schema.format("string") else schema

  /** Full type name used to name a type parameter, when only a KindlingsSchema of it is available. */
  def typeNameOf[A](kindlingsSchema: KindlingsSchema[A], fallback: String): String =
    kindlingsSchema.schema.name.map(_.show).getOrElse {
      kindlingsSchema match {
        case named: TapirSchemaDerivationFactories.NamedKindlingsSchema[?] => named.typeName
        case _                                                             => fallback
      }
    }

  /** Parse a fully-qualified type name (with type parameters) into an SName.
    *
    * For `"com.example.Foo[com.example.Bar, scala.Int]"`:
    *   - `fullName = "com.example.Foo"`
    *   - `typeParameterShortNames = List("com.example.Bar", "scala.Int")`
    *
    * Despite its name, tapir fills `typeParameterShortNames` with full names (the name is legacy, kept for backward
    * compatibility), so that type parameters sharing a short name produce distinct SNames (softwaremill/tapir#3500).
    */
  def parseSName(fullTypeName: String): SName = {
    val bracketIdx = fullTypeName.indexOf('[')
    // A singleton's type prints as `pkg.Obj.type`; tapir names it `pkg.Obj` (otherwise its short name would be `type`).
    if (bracketIdx < 0) SName(fullTypeName.stripSuffix(".type"))
    else {
      val baseName = fullTypeName.substring(0, bracketIdx)
      val typeParamsStr = fullTypeName.substring(bracketIdx + 1, fullTypeName.length - 1)
      val typeParams = splitTopLevelTypeParams(typeParamsStr).flatMap(flattenTypeName)
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

  /** Recursively flattens a type name into one fully-qualified name per type-parameter token e.g.
    * `scala.List[com.example.Foo]` becomes `List("scala.List", "com.example.Foo")`.
    */
  private def flattenTypeName(fullName: String): List[String] = {
    val bracketIdx = fullName.indexOf('[')
    if (bracketIdx < 0) {
      List(fullName)
    } else {
      val base = fullName.substring(0, bracketIdx)
      val params = fullName.substring(bracketIdx + 1, fullName.length - 1)
      val flatParams = splitTopLevelTypeParams(params).flatMap(flattenTypeName)
      base :: flatParams
    }
  }
}
