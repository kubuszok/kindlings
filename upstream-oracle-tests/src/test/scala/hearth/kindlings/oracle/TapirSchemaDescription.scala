package hearth.kindlings.oracle

import sttp.tapir.{Schema, SchemaType, Validator}

/** Everything a tapir `Schema` contains, as plain data that [[Oracle.diff]] can compare: the parts the rendered JSON
  * Schema hides (the `SName`s with their type parameters, `FieldName(name, encodedName)`, attributes such as
  * `encodedDiscriminatorValue` or the ones set by `@customise`, `hidden`, raw defaults/examples, validators with the
  * encoded values of enumerations).
  *
  * Functions inside a schema (field getters, `SCoproduct.subtypeSchema`, validators' logic) are compared by applying
  * them to sample values, see [[TapirSchemaDescription.fieldValues]], [[TapirSchemaDescription.subtypeOf]] and
  * `Schema.applyValidation`.
  */
final case class SchemaDescription(
    schemaType: String,
    name: Option[String],
    isOptional: Boolean,
    description: Option[String],
    default: Option[String],
    format: Option[String],
    encodedExample: Option[String],
    deprecated: Boolean,
    hidden: Boolean,
    validator: String,
    attributes: Map[String, String],
    fields: List[FieldDescription],
    element: Option[SchemaDescription],
    subtypes: List[SchemaDescription],
    discriminator: Option[String]
)

final case class FieldDescription(name: String, encodedName: String, schema: SchemaDescription)

object TapirSchemaDescription {

  private def sname(name: Schema.SName): String =
    if (name.typeParameterShortNames.isEmpty) name.fullName
    else name.fullName + name.typeParameterShortNames.mkString("[", ", ", "]")

  /** With `sortSubtypes`, the subtypes of coproducts are sorted by name (to compare their schemas separately from their
    * order).
    */
  def describe(schema: Schema[?], sortSubtypes: Boolean = false): SchemaDescription = {
    val s = schema.asInstanceOf[Schema[Any]]
    val (tpe, fields, element, subtypes, discriminator) = s.schemaType match {
      case SchemaType.SProduct(fs) =>
        (
          "SProduct",
          fs.map(f => FieldDescription(f.name.name, f.name.encodedName, describe(f.schema, sortSubtypes))),
          None,
          Nil,
          None
        )
      case p: SchemaType.SOpenProduct[?, ?] =>
        (
          "SOpenProduct",
          p.fields.map(f => FieldDescription(f.name.name, f.name.encodedName, describe(f.schema, sortSubtypes))),
          Some(describe(p.valueSchema, sortSubtypes)),
          Nil,
          None
        )
      case a: SchemaType.SArray[?, ?]  => ("SArray", Nil, Some(describe(a.element, sortSubtypes)), Nil, None)
      case o: SchemaType.SOption[?, ?] => ("SOption", Nil, Some(describe(o.element, sortSubtypes)), Nil, None)
      case c: SchemaType.SCoproduct[?] =>
        val disc = c.discriminator.map { d =>
          s"${d.name.name}/${d.name.encodedName} -> " +
            d.mapping.toList.sortBy(_._1).map { case (k, v) => s"$k=${sname(v.name)}" }.mkString(", ")
        }
        val subtypes = c.subtypes.map(describe(_, sortSubtypes))
        ("SCoproduct", Nil, None, if (sortSubtypes) subtypes.sortBy(_.name.getOrElse("")) else subtypes, disc)
      case r: SchemaType.SRef[?] => (s"SRef(${sname(r.name)})", Nil, None, Nil, None)
      case other                 => (other.getClass.getSimpleName, Nil, None, Nil, None)
    }
    SchemaDescription(
      schemaType = tpe,
      name = s.name.map(sname),
      isOptional = s.isOptional,
      description = s.description,
      default = s.default.map { case (value, encoded) => s"$value (encoded: $encoded)" },
      format = s.format,
      encodedExample = s.encodedExample.map(_.toString),
      deprecated = s.deprecated,
      hidden = s.hidden,
      validator = describeValidator(s.validator),
      attributes = attributesOf(s),
      fields = fields,
      element = element,
      subtypes = subtypes,
      discriminator = discriminator
    )
  }

  private def attributesOf(schema: Schema[Any]): Map[String, String] =
    schema.attributes.productElement(0).asInstanceOf[Map[String, Any]].map { case (k, v) => k -> v.toString }

  def describeValidator(validator: Validator[?]): String = validator match {
    case Validator.Enumeration(values, encode, name) =>
      val encoded = encode match {
        case Some(f) => values.map(v => f.asInstanceOf[Any => Option[Any]](v).getOrElse(v))
        case None    => values
      }
      s"Enumeration(${encoded.mkString(", ")}; name=${name.map(sname)})"
    case Validator.Mapped(wrapped, _) => s"Mapped(${describeValidator(wrapped)})"
    case Validator.All(vs)            => vs.map(describeValidator).mkString("All(", ", ", ")")
    case Validator.Any(vs)            => vs.map(describeValidator).mkString("Any(", ", ", ")")
    case Validator.Custom(_, msg)     => s"Custom($msg)"
    case other                        => other.toString
  }

  /** `description` with every schema name (also in discriminator mappings) changed by `f`. */
  def mapNames(description: SchemaDescription)(f: String => String): SchemaDescription =
    description.copy(
      name = description.name.map(f),
      fields = description.fields.map(field => field.copy(schema = mapNames(field.schema)(f))),
      element = description.element.map(mapNames(_)(f)),
      subtypes = description.subtypes.map(mapNames(_)(f)),
      discriminator = description.discriminator.map(
        _.split(", ")
          .map(_.split("=", 2) match {
            case Array(value, name) => s"$value=${f(name)}"
            case other              => other.mkString("=")
          })
          .mkString(", ")
      )
    )

  /** `description` with the subtypes of every coproduct sorted by name. */
  def sorted(description: SchemaDescription): SchemaDescription =
    description.copy(
      fields = description.fields.map(field => field.copy(schema = sorted(field.schema))),
      element = description.element.map(sorted),
      subtypes = description.subtypes.map(sorted).sortBy(_.name.getOrElse(""))
    )

  /** The names of the subtypes of a coproduct schema, in order. */
  def subtypeNames(schema: Schema[?]): List[Option[String]] = schema.schemaType match {
    case c: SchemaType.SCoproduct[?] => c.subtypes.map(_.name.map(sname))
    case _                           => Nil
  }

  /** The values of the fields of `value` as read by the schema's field getters, by encoded field name. */
  def fieldValues[T](schema: Schema[T], value: T): List[(String, Option[Any])] = schema.schemaType match {
    case SchemaType.SProduct(fields) => fields.map(f => f.name.encodedName -> f.get(value))
    case _                           => Nil
  }

  /** The name of the subtype schema the coproduct schema chooses for `value`. */
  def subtypeOf[T](schema: Schema[T], value: T): Option[Option[String]] = schema.schemaType match {
    case c: SchemaType.SCoproduct[T @unchecked] => c.subtypeSchema(value).map(_.schema.name.map(sname))
    case _                                      => None
  }

  /** The validation errors of `value`: the validator and the path. */
  def validationErrors[T](schema: Schema[T], value: T): List[String] =
    schema.applyValidation(value).map { e =>
      s"${e.path.map(_.encodedName).mkString(".")}: ${describeValidator(e.validator)} on ${e.invalidValue}"
    }
}
