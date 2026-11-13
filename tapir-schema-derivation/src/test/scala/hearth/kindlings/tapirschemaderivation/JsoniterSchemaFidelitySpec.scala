package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import hearth.MacroSuite
import sttp.tapir.{Schema, SchemaType, Validator}

/** The tapir Schemas derived with a JsoniterConfig describe what the jsoniter codecs (derived with the same config)
  * write: every sample value written by the codec conforms to the schema (see [[SchemaConformance]]).
  */
final class JsoniterSchemaFidelitySpec extends MacroSuite {

  private val all: List[FidDerived] = List(
    FidDefault.derived,
    FidDiscriminator.derived,
    FidCirceLike.derived,
    FidEnumAsStrings.derived,
    FidFullNames.derived,
    FidSkipNestedOptions.derived,
    FidRuntime.derived
  )

  private def fields(schema: Schema[?]): Map[String, Schema[?]] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.encodedName -> (f.schema: Schema[?])).toMap
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  private def subtypes(schema: Schema[?]): List[Schema[?]] = schema.schemaType match {
    case c: SchemaType.SCoproduct[?] => c.subtypes
    case other                       => fail(s"Expected SCoproduct, got: $other")
  }

  private def enumValues(schema: Schema[?]): List[String] = schema.validator match {
    case e: Validator.Enumeration[?] =>
      val en = e.asInstanceOf[Validator.Enumeration[Any]]
      en.possibleValues.flatMap(v => en.encode.flatMap(f => f(v)).map(_.toString))
    case other => fail(s"Expected an enumeration validator, got: $other")
  }

  group("jsoniter codec output conforms to the derived schema") {
    all.foreach { derived =>
      test(s"config: ${derived.name}") {
        FidHolder.samples.foreach { sample =>
          val json = writeToString(sample)(derived.codec)
          val parsed = io.circe.parser.parse(json).fold(e => fail(s"Invalid JSON $json: $e"), identity)
          val errors = SchemaConformance.errors(derived.schema, parsed)
          assert(errors.isEmpty, s"$json does not conform to the schema:\n${errors.mkString("\n")}")
        }
      }
    }
  }

  group("schema shapes") {

    test("case objects of a mixed ADT are strings in wrapper mode, case classes are wrapped") {
      val mixed = subtypes(fields(FidDefault.derived.schema)("mixed"))
      val strings = mixed.filter(_.schemaType.isInstanceOf[SchemaType.SString[?]]).flatMap(enumValues)
      assertEquals(strings.sorted, List("Origin", "nobody"))
      val wrapperKeys = mixed.flatMap { s =>
        s.schemaType match {
          case p: SchemaType.SProduct[?] => p.fields.map(_.name.encodedName)
          case _                         => Nil
        }
      }
      assertEquals(wrapperKeys.sorted, List("Point", "renamed-leaf"))
    }

    test("case objects of a mixed ADT are objects with circeLikeObjectEncoding") {
      val mixed = subtypes(fields(FidCirceLike.derived.schema)("mixed"))
      assert(mixed.forall(_.schemaType.isInstanceOf[SchemaType.SProduct[?]]), mixed.map(_.schemaType).toString)
    }

    test("Enumeration values are strings (or ids with useScalaEnumValueId), whatever the discriminator") {
      assertEquals(enumValues(fields(FidDiscriminator.derived.schema)("size")), List("Small", "BIG"))
      val ids = fields(FidFullNames.derived.schema)("size")
      assert(ids.schemaType.isInstanceOf[SchemaType.SInteger[?]], ids.schemaType.toString)
      assertEquals(enumValues(ids), List("0", "1"))
    }

    test("tuples are arrays") {
      val tuple = fields(FidDefault.derived.schema)("tuple")
      assertEquals(tuple.attribute(Schema.Tuple.Attribute), Some(Schema.Tuple(true)))
    }

    test("@named on a field renames it") {
      assert(fields(FidDefault.derived.schema).contains("renamed_field"))
    }

    test("@named leaves and fully-qualified names passed to adtLeafClassNameMapper are discriminator values") {
      fields(FidFullNames.derived.schema)("mixed").schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          assertEquals(
            c.discriminator.map(_.mapping.keySet).getOrElse(Set.empty),
            Set(
              "hearth.kindlings.tapirschemaderivation.FidMixed.Origin",
              "hearth.kindlings.tapirschemaderivation.FidMixed.Point",
              "renamed-leaf",
              "nobody"
            )
          )
        case other => fail(s"Expected SCoproduct, got: $other")
      }
    }

    test("nested sealed hierarchies are flattened to their leaves") {
      fields(FidDiscriminator.derived.schema)("vehicle").schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          assertEquals(c.discriminator.map(_.mapping.keySet).getOrElse(Set.empty), Set("FidTruck", "FidBike"))
        case other => fail(s"Expected SCoproduct, got: $other")
      }
    }

    test("an Option nested in an Option is an ADT (unless skipNestedOptionValues)") {
      def inner(schema: Schema[?]): Schema[?] = fields(schema)("nested").schemaType match {
        case o: SchemaType.SOption[?, ?] => o.element
        case other                       => fail(s"Expected SOption, got: $other")
      }
      assert(inner(FidDiscriminator.derived.schema).schemaType.isInstanceOf[SchemaType.SCoproduct[?]])
      assert(inner(FidSkipNestedOptions.derived.schema).schemaType.isInstanceOf[SchemaType.SOption[?, ?]])
    }
  }
}
