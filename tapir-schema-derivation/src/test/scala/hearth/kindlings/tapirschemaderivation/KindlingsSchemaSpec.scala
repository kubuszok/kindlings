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

      test("parameterized type SName has fullName without type params") {
        val schema = KindlingsSchema.derive[Box[SimplePerson]]
        assert(schema.name.isDefined, "Schema should have a name")
        val name = schema.name.get
        assert(
          name.fullName.endsWith("Box"),
          s"Expected fullName ending with Box (no type params), got: ${name.fullName}"
        )
        assert(
          !name.fullName.contains("["),
          s"fullName should not contain type params, got: ${name.fullName}"
        )
      }

      test("parameterized type SName has typeParameterShortNames") {
        val schema = KindlingsSchema.derive[Box[SimplePerson]]
        val name = schema.name.get
        assertEquals(
          name.typeParameterShortNames,
          List("SimplePerson"),
          s"Expected typeParameterShortNames = List(SimplePerson), got: ${name.typeParameterShortNames}"
        )
      }

      test("multi-param type SName has all type parameter short names") {
        val schema = KindlingsSchema.derive[Pair[SimplePerson, Nested]]
        val name = schema.name.get
        assert(
          name.fullName.endsWith("Pair"),
          s"Expected fullName ending with Pair, got: ${name.fullName}"
        )
        assertEquals(
          name.typeParameterShortNames,
          List("SimplePerson", "Nested"),
          s"Expected two type parameter short names, got: ${name.typeParameterShortNames}"
        )
      }

      test("non-parameterized type SName has empty typeParameterShortNames") {
        val schema = KindlingsSchema.derive[SimplePerson]
        val name = schema.name.get
        assertEquals(
          name.typeParameterShortNames,
          Nil,
          s"Expected empty typeParameterShortNames, got: ${name.typeParameterShortNames}"
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

  group("runtime type parameter resolution") {

    test("generic derivation resolves abstract type parameter in SName at runtime") {
      val schema = GenericDerivation.boxOfPerson
      val name = schema.name.get
      assert(
        name.fullName.endsWith("Box"),
        s"Expected fullName ending with Box, got: ${name.fullName}"
      )
      assert(
        name.typeParameterShortNames.nonEmpty,
        s"Expected type parameter short names to be populated, got empty"
      )
      assert(
        name.typeParameterShortNames.head.contains("SimplePerson"),
        s"Expected type param to contain SimplePerson, got: ${name.typeParameterShortNames}"
      )
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

  group("config features (dual-library)") {

    group("field renaming (snake_case)") {

      test("circe config transforms field names") {
        assertEquals(
          CirceSnakeCaseDerivation.fieldEncodedNames,
          List("first_name", "last_name")
        )
      }

      test("jsoniter config transforms field names") {
        assertEquals(
          JsoniterPreferredDerivation.snakeCaseFieldNames,
          List("first_name", "last_name")
        )
      }
    }

    group("constructor name transforms + discriminator") {

      test("circe config uses resolved constructor names in discriminator") {
        val schema = CirceDiscriminatorDerivation.schema
        schema.schemaType match {
          case c: SchemaType.SCoproduct[Shape] =>
            c.discriminator match {
              case Some(disc) =>
                val mappingKeys = disc.mapping.keys.toSet
                assertEquals(mappingKeys, Set("circle", "rectangle"))
              case None => fail("Expected discriminator")
            }
          case other => fail(s"Expected SCoproduct, got: $other")
        }
      }

      test("jsoniter config uses resolved constructor names in discriminator") {
        val schema = JsoniterDiscriminatorDerivation.schema
        schema.schemaType match {
          case c: SchemaType.SCoproduct[Shape] =>
            c.discriminator match {
              case Some(disc) =>
                val mappingKeys = disc.mapping.keys.toSet
                assertEquals(mappingKeys, Set("circle", "rectangle"))
              case None => fail("Expected discriminator")
            }
          case other => fail(s"Expected SCoproduct, got: $other")
        }
      }
    }

    group("@transientField") {

      test("circe @transientField excludes field from schema") {
        val schema = CirceTransientDerivation.schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithCirceTransientField] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("visible"))
          case other => fail(s"Expected SProduct, got: $other")
        }
      }

      test("jsoniter @transientField excludes field from schema") {
        val schema = JsoniterTransientDerivation.schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithJsoniterTransientField] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("visible"))
          case other => fail(s"Expected SProduct, got: $other")
        }
      }
    }

    group("enumAsStrings") {

      test("circe config produces string enum schema") {
        val schema = CirceEnumAsStringsDerivation.schema
        schema.schemaType match {
          case _: SchemaType.SString[?] => () // string schema for enum
          case other                    => fail(s"Expected SString, got: $other")
        }
      }

      test("jsoniter config produces string enum schema") {
        val schema = JsoniterEnumAsStringsDerivation.schema
        schema.schemaType match {
          case _: SchemaType.SString[?] => () // string schema for enum
          case other                    => fail(s"Expected SString, got: $other")
        }
      }
    }

    group("fieldsWithDefaultsAreOptional") {

      test("circe useDefaults marks fields with defaults as optional") {
        val schema = CirceDefaultsOptionalDerivation.schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithDefaults] =>
            val nameField = p.fields.find(_.name.name == "name").get
            val ageField = p.fields.find(_.name.name == "age").get
            val activeField = p.fields.find(_.name.name == "active").get
            // name has no default — not optional
            assertEquals(nameField.schema.isOptional, false)
            // age has default — optional
            assertEquals(ageField.schema.isOptional, true)
            // active has default — optional
            assertEquals(activeField.schema.isOptional, true)
          case other => fail(s"Expected SProduct, got: $other")
        }
      }

      test("jsoniter transientDefault marks fields with defaults as optional") {
        val schema = JsoniterDefaultsOptionalDerivation.schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithDefaults] =>
            val nameField = p.fields.find(_.name.name == "name").get
            val ageField = p.fields.find(_.name.name == "age").get
            val activeField = p.fields.find(_.name.name == "active").get
            assertEquals(nameField.schema.isOptional, false)
            assertEquals(ageField.schema.isOptional, true)
            assertEquals(activeField.schema.isOptional, true)
          case other => fail(s"Expected SProduct, got: $other")
        }
      }
    }
  }

  group("config features (jsoniter-only)") {

    test("mapAsArray produces array schema for maps") {
      val schema = JsoniterMapAsArrayDerivation.schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithMap] =>
          val metadataField = p.fields.find(_.name.name == "metadata").get
          metadataField.schema.schemaType match {
            case _: SchemaType.SArray[?, ?] => () // array schema for map
            case other                      => fail(s"Expected SArray for map, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("isStringified adds string format to numeric fields") {
      val schema = JsoniterStringifiedDerivation.schema
      schema.schemaType match {
        case p: SchemaType.SProduct[NumericFields] =>
          val xField = p.fields.find(_.name.name == "x").get
          val yField = p.fields.find(_.name.name == "y").get
          val nameField = p.fields.find(_.name.name == "name").get
          // Numeric fields get "string" format
          assertEquals(xField.schema.format, Some("string"))
          assertEquals(yField.schema.format, Some("string"))
          // Non-numeric field is unaffected
          assertEquals(nameField.schema.format, None)
        case other => fail(s"Expected SProduct, got: $other")
      }
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

// --- Dual-library helper objects ---

object CirceSnakeCaseDerivation {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  private val schema: Schema[CamelCasePerson] = KindlingsSchema.derive[CamelCasePerson]
  val fieldEncodedNames: List[String] = schema.schemaType match {
    case p: SchemaType.SProduct[CamelCasePerson] => p.fields.map(_.name.encodedName)
    case _                                       => Nil
  }
}

object CirceDiscriminatorDerivation {
  implicit val config: Configuration =
    Configuration(discriminator = Some("type"), transformConstructorNames = _.toLowerCase)
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[Shape] = KindlingsSchema.derive[Shape]
}

object JsoniterDiscriminatorDerivation {
  implicit val config: JsoniterConfig =
    JsoniterConfig(discriminatorFieldName = Some("type"), adtLeafClassNameMapper = _.toLowerCase)
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[Shape] = KindlingsSchema.derive[Shape]
}

object CirceTransientDerivation {
  implicit val config: Configuration = Configuration.default
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[WithCirceTransientField] = KindlingsSchema.derive[WithCirceTransientField]
}

object JsoniterTransientDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithJsoniterTransientField] = KindlingsSchema.derive[WithJsoniterTransientField]
}

object CirceEnumAsStringsDerivation {
  implicit val config: Configuration = Configuration.default.withEnumAsStrings
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[Color] = KindlingsSchema.derive[Color]
}

object JsoniterEnumAsStringsDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[Color] = KindlingsSchema.derive[Color]
}

object CirceDefaultsOptionalDerivation {
  implicit val config: Configuration = Configuration.default.withDefaults
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[WithDefaults] = KindlingsSchema.derive[WithDefaults]
}

object JsoniterDefaultsOptionalDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithDefaults] = KindlingsSchema.derive[WithDefaults]
}

// --- Jsoniter-only helper objects ---

object JsoniterMapAsArrayDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithMap] = KindlingsSchema.derive[WithMap]
}

object JsoniterStringifiedDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withStringified
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[NumericFields] = KindlingsSchema.derive[NumericFields]
}
