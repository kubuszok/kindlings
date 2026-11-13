package hearth.kindlings.tapirschemaderivation

import io.circe.Json
import sttp.tapir.{Schema, SchemaType, Validator}

/** Test helper: checks that a JSON value (e.g. written by a JSON library's codec) conforms to a tapir Schema, so that
  * tests can verify that the schemas derived with a JSON library's config describe what that library's codecs produce.
  *
  * Strict on purpose: objects may not contain keys which the schema does not describe, required fields must be
  * present, string enums must contain the value, discriminators must select a subtype.
  */
object SchemaConformance {

  def errors(schema: Schema[?], json: Json): List[String] =
    check(schema, json, "$", collectNamed(schema, Map.empty))

  private def collectNamed(schema: Schema[?], acc: Map[Schema.SName, Schema[?]]): Map[Schema.SName, Schema[?]] =
    schema.schemaType match {
      case _: SchemaType.SRef[?]                                => acc
      case _ if schema.name.exists(acc.contains)                => acc
      case st =>
        val withSelf = schema.name.fold(acc)(n => acc + (n -> schema))
        val children: List[Schema[?]] = st match {
          case p: SchemaType.SProduct[?]        => p.fields.map(_.schema)
          case o: SchemaType.SOpenProduct[?, ?] => List(o.valueSchema)
          case a: SchemaType.SArray[?, ?]       => List(a.element)
          case o: SchemaType.SOption[?, ?]      => List(o.element)
          case c: SchemaType.SCoproduct[?]      => c.subtypes
          case _                                => Nil
        }
        children.foldLeft(withSelf)((m, s) => collectNamed(s, m))
    }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def enumValues(validator: Validator[?]): Option[List[String]] = validator match {
    case e: Validator.Enumeration[?] =>
      val en = e.asInstanceOf[Validator.Enumeration[Any]]
      Some(en.possibleValues.flatMap(v => en.encode.flatMap(f => f(v)).map(_.toString)))
    case all: Validator.All[?] => all.validators.flatMap(v => enumValues(v)).headOption
    case _                     => None
  }

  private def checkEnum(schema: Schema[?], value: String, path: String): List[String] =
    enumValues(schema.validator) match {
      case Some(values) if !values.contains(value) => List(s"$path: '$value' is not one of ${values.mkString(", ")}")
      case _                                       => Nil
    }

  private def isTuple(schema: Schema[?]): Boolean =
    schema.attribute(Schema.Tuple.Attribute).exists(_.isTuple)

  private def check(schema: Schema[?], json: Json, path: String, named: Map[Schema.SName, Schema[?]]): List[String] = {
    def fail(expected: String) = List(s"$path: expected $expected, got ${json.noSpaces}")
    def stringified = schema.format.contains("string")
    schema.schemaType match {
      case SchemaType.SRef(name) =>
        named.get(name) match {
          case Some(s) => check(s, json, path, named)
          case None    => List(s"$path: unresolved reference to $name")
        }
      case o: SchemaType.SOption[?, ?] => if (json.isNull) Nil else check(o.element, json, path, named)
      case SchemaType.SString()        =>
        json.asString match {
          case Some(s) => checkEnum(schema, s, path)
          case None    => fail("a string")
        }
      case SchemaType.SInteger() =>
        if (json.isNumber && json.asNumber.exists(_.toBigInt.isDefined)) checkEnum(schema, json.noSpaces, path)
        else if (stringified && json.isString) Nil
        else fail("an integer")
      case SchemaType.SNumber() =>
        if (json.isNumber || (stringified && json.isString)) Nil else fail("a number")
      case SchemaType.SBoolean() => if (json.isBoolean) Nil else fail("a boolean")
      case a: SchemaType.SArray[?, ?] =>
        json.asArray match {
          case Some(items) => items.toList.zipWithIndex.flatMap { case (j, i) => check(a.element, j, s"$path[$i]", named) }
          case None        => fail("an array")
        }
      case p: SchemaType.SProduct[?] if isTuple(schema) =>
        json.asArray match {
          case Some(items) if items.size == p.fields.size =>
            p.fields.zip(items.toList).zipWithIndex.flatMap { case ((f, j), i) => check(f.schema, j, s"$path[$i]", named) }
          case _ => fail(s"an array of ${p.fields.size} elements")
        }
      case p: SchemaType.SProduct[?] =>
        json.asObject match {
          case Some(obj) =>
            val known = p.fields.map(_.name.encodedName).toSet
            val unknown = obj.keys.filterNot(known).toList.map(k => s"$path: unexpected key '$k'")
            val fieldErrors = p.fields.flatMap { f =>
              obj(f.name.encodedName) match {
                case Some(j) => check(f.schema, j, s"$path.${f.name.encodedName}", named)
                case None    =>
                  if (f.schema.isOptional) Nil else List(s"$path: missing required field '${f.name.encodedName}'")
              }
            }
            unknown ++ fieldErrors
          case None => fail("an object")
        }
      case o: SchemaType.SOpenProduct[?, ?] =>
        json.asObject match {
          case Some(obj) => obj.toList.flatMap { case (k, j) => check(o.valueSchema, j, s"$path.$k", named) }
          case None      => fail("an object")
        }
      case c: SchemaType.SCoproduct[?] =>
        val candidates: Either[List[String], List[Schema[?]]] = c.discriminator match {
          case Some(d) =>
            json.asObject.flatMap(_(d.name.encodedName)).flatMap(_.asString) match {
              case Some(v) =>
                Right(c.subtypes.filter(_.attribute(Schema.EncodedDiscriminatorValue.Attribute).exists(_.v == v)))
              case None => Left(fail(s"an object with a '${d.name.encodedName}' discriminator"))
            }
          case None => Right(c.subtypes)
        }
        candidates match {
          case Left(errors)    => errors
          case Right(Nil)      => fail(s"one of the subtypes of ${schema.name.fold("?")(_.fullName)}")
          case Right(subtypes) =>
            val results = subtypes.map(s => check(s, json, path, named))
            if (results.exists(_.isEmpty)) Nil
            else fail(s"one of the subtypes of ${schema.name.fold("?")(_.fullName)} (${results.flatten.mkString("; ")})")
        }
      case _ => Nil // binary, dates, etc.
    }
  }
}
