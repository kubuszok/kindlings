package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.jsoniterderivation.JsoniterConfig
import sttp.tapir.{Schema, SchemaType}

final class KindlingsSchemaSpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  group("KindlingsSchema.derive") {

    group("case classes") {

      test("simple case class") {
        val schema = KindlingsSchema.derive[SimplePerson]
        schema.schemaType match {
          case p: SchemaType.SProduct[SimplePerson] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("name", "age"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("nested case class") {
        val schema = KindlingsSchema.derive[Nested]
        schema.schemaType match {
          case p: SchemaType.SProduct[Nested] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("person", "note"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("schema has correct SName") {
        val schema = KindlingsSchema.derive[SimplePerson]
        assert(schema.name.isDefined, "Schema should have a name")
        assert(
          schema.name.get.fullName.endsWith("SimplePerson"),
          s"Expected name ending with SimplePerson, got: ${schema.name.get.fullName}"
        )
      }
    }

    group("sealed traits") {

      test("sealed trait derives coproduct") {
        val schema = KindlingsSchema.derive[Shape]
        schema.schemaType match {
          case _: SchemaType.SCoproduct[Shape] => () // success
          case other                           =>
            fail(s"Expected SCoproduct, got: $other")
        }
      }

      test("sealed trait has subtypes") {
        val schema = KindlingsSchema.derive[Shape]
        schema.schemaType match {
          case c: SchemaType.SCoproduct[Shape] =>
            assertEquals(c.subtypes.size, 2)
          case other =>
            fail(s"Expected SCoproduct, got: $other")
        }
      }
    }

    group("annotations") {

      test("@description on type") {
        val schema = KindlingsSchema.derive[AnnotatedPerson]
        assertEquals(schema.description, Some("A person with metadata"))
      }

      test("@title on type") {
        val schema = KindlingsSchema.derive[AnnotatedPerson]
        val titleOpt = schema.attributes.get(Schema.Title.Attribute)
        assert(titleOpt.isDefined, "Schema should have a Title attribute")
        assertEquals(titleOpt.get.value, "PersonMeta")
      }

      test("@description on field") {
        val schema = KindlingsSchema.derive[AnnotatedPerson]
        schema.schemaType match {
          case p: SchemaType.SProduct[AnnotatedPerson] =>
            val nameField = p.fields.find(_.name.name == "name")
            assert(nameField.isDefined, "Should have a 'name' field")
            assertEquals(nameField.get.schema.description, Some("The name"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@format on field") {
        val schema = KindlingsSchema.derive[AnnotatedPerson]
        schema.schemaType match {
          case p: SchemaType.SProduct[AnnotatedPerson] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            assertEquals(ageField.get.schema.format, Some("int32"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@encodedName overrides JSON config name") {
        val schema = KindlingsSchema.derive[WithEncodedName]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithEncodedName] =>
            val userNameField = p.fields.find(_.name.name == "userName")
            assert(userNameField.isDefined, "Should have a 'userName' field")
            assertEquals(userNameField.get.name.encodedName, "user_name")
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@deprecated on type") {
        val schema = KindlingsSchema.derive[DeprecatedType]
        assert(schema.deprecated, "Schema should be deprecated")
      }

      test("@hidden on field") {
        val schema = KindlingsSchema.derive[WithHiddenField]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithHiddenField] =>
            val secretField = p.fields.find(_.name.name == "secret")
            assert(secretField.isDefined, "Should have a 'secret' field")
            assert(secretField.get.schema.hidden, "secret field schema should be hidden")
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@validate on field") {
        val schema = KindlingsSchema.derive[WithValidation]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithValidation] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            assert(ageField.get.schema.validator != sttp.tapir.Validator.pass, "age field should have a validator")
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }
    }

    group("structural types") {

      test("optional field") {
        val schema = KindlingsSchema.derive[WithOptional]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithOptional] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("required", "optional"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("collection fields") {
        val schema = KindlingsSchema.derive[WithCollections]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithCollections] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("tags", "counts"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("map field") {
        val schema = KindlingsSchema.derive[WithMap]
        schema.schemaType match {
          case p: SchemaType.SProduct[WithMap] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("metadata"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("recursive type uses SRef") {
        val schema = KindlingsSchema.derive[RecursiveTree]
        schema.schemaType match {
          case p: SchemaType.SProduct[RecursiveTree] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("value", "children"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }
    }
  }

  group("KindlingsSchema.derived") {

    test("provides implicit Schema") {
      implicit val ks: KindlingsSchema[SimplePerson] = KindlingsSchema.derived[SimplePerson]
      val schema: Schema[SimplePerson] = ks.schema
      schema.schemaType match {
        case p: SchemaType.SProduct[SimplePerson] =>
          val fieldNames = p.fields.map(_.name.name)
          assertEquals(fieldNames, List("name", "age"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }

  group("PreferSchemaConfig") {

    test("selects circe config with PreferSchemaConfig[Configuration]") {
      // The class-level implicit preferCirce selects circe.
      // With default circe Configuration, field names are unchanged.
      val schema = KindlingsSchema.derive[SimplePerson]
      schema.schemaType match {
        case p: SchemaType.SProduct[SimplePerson] =>
          val fieldNames = p.fields.map(_.name.name)
          assertEquals(fieldNames, List("name", "age"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("selects jsoniter config with PreferSchemaConfig[JsoniterConfig]") {
      assertEquals(
        JsoniterPreferredDerivation.snakeCaseFieldNames,
        List("first_name", "last_name")
      )
    }
  }
}

/** Helper object that derives a schema preferring jsoniter-scala config with snake_case field names. Separate object
  * needed because macro expansion uses the implicits at the call site.
  */
object JsoniterPreferredDerivation {
  implicit val jsoniterConfig: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]

  private val schema: Schema[CamelCasePerson] = KindlingsSchema.derive[CamelCasePerson]

  val snakeCaseFieldNames: List[String] = schema.schemaType match {
    case p: SchemaType.SProduct[CamelCasePerson] => p.fields.map(_.name.encodedName)
    case _                                       => Nil
  }
}
