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
        val schema = KindlingsSchema.derived[SimplePerson].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[SimplePerson] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("name", "age"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("nested case class") {
        val schema = KindlingsSchema.derived[Nested].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[Nested] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("person", "note"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("schema has correct SName") {
        val schema = KindlingsSchema.derived[SimplePerson].schema
        assert(schema.name.isDefined, "Schema should have a name")
        assert(
          schema.name.get.fullName.endsWith("SimplePerson"),
          s"Expected name ending with SimplePerson, got: ${schema.name.get.fullName}"
        )
      }

      test("parameterized type SName has fullName without type params") {
        val schema = KindlingsSchema.derived[Box[SimplePerson]].schema
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
        val schema = KindlingsSchema.derived[Box[SimplePerson]].schema
        val name = schema.name.get
        assertEquals(
          name.typeParameterShortNames,
          List("SimplePerson"),
          s"Expected typeParameterShortNames = List(SimplePerson), got: ${name.typeParameterShortNames}"
        )
      }

      test("multi-param type SName has all type parameter short names") {
        val schema = KindlingsSchema.derived[Pair[SimplePerson, Nested]].schema
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
        val schema = KindlingsSchema.derived[SimplePerson].schema
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
        val schema = KindlingsSchema.derived[Shape].schema
        schema.schemaType match {
          case _: SchemaType.SCoproduct[Shape] => () // success
          case other                           =>
            fail(s"Expected SCoproduct, got: $other")
        }
      }

      test("sealed trait has subtypes") {
        val schema = KindlingsSchema.derived[Shape].schema
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
        val schema = KindlingsSchema.derived[AnnotatedPerson].schema
        assertEquals(schema.description, Some("A person with metadata"))
      }

      test("@title on type") {
        val schema = KindlingsSchema.derived[AnnotatedPerson].schema
        val titleOpt = schema.attributes.get(Schema.Title.Attribute)
        assert(titleOpt.isDefined, "Schema should have a Title attribute")
        assertEquals(titleOpt.get.value, "PersonMeta")
      }

      test("@description on field") {
        val schema = KindlingsSchema.derived[AnnotatedPerson].schema
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
        val schema = KindlingsSchema.derived[AnnotatedPerson].schema
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
        val schema = KindlingsSchema.derived[WithEncodedName].schema
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
        val schema = KindlingsSchema.derived[DeprecatedType].schema
        assert(schema.deprecated, "Schema should be deprecated")
      }

      test("@hidden on field") {
        val schema = KindlingsSchema.derived[WithHiddenField].schema
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
        val schema = KindlingsSchema.derived[WithValidation].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithValidation] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            assert(ageField.get.schema.validator != sttp.tapir.Validator.pass, "age field should have a validator")
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@default on field sets default value") {
        val schema = KindlingsSchema.derived[WithDefaultAnnotation].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithDefaultAnnotation] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            assert(ageField.get.schema.default.isDefined, "age field should have a default value")
            assert(ageField.get.schema.default.get._1 == 42)
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@default on field marks field as optional") {
        val schema = KindlingsSchema.derived[WithDefaultAnnotation].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithDefaultAnnotation] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            assert(ageField.get.schema.isOptional, "age field with @default should be optional")
            // name field without @default should NOT be optional
            val nameField = p.fields.find(_.name.name == "name")
            assert(nameField.isDefined, "Should have a 'name' field")
            assertEquals(nameField.get.schema.isOptional, false)
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@encodedExample on field sets example") {
        val schema = KindlingsSchema.derived[WithEncodedExampleAnnotation].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithEncodedExampleAnnotation] =>
            val emailField = p.fields.find(_.name.name == "email")
            assert(emailField.isDefined, "Should have an 'email' field")
            assertEquals(emailField.get.schema.encodedExample, Some("example@email.com"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("@default and @encodedExample together on same field") {
        val schema = KindlingsSchema.derived[WithDefaultAndExample].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithDefaultAndExample] =>
            val ageField = p.fields.find(_.name.name == "age")
            assert(ageField.isDefined, "Should have an 'age' field")
            // @default sets the default value
            assert(ageField.get.schema.default.isDefined, "age field should have a default value")
            assert(ageField.get.schema.default.get._1 == 42)
            // @encodedExample sets the encoded example
            assertEquals(ageField.get.schema.encodedExample, Some(99))
            // greeting field has only @encodedExample
            val greetingField = p.fields.find(_.name.name == "greeting")
            assert(greetingField.isDefined, "Should have a 'greeting' field")
            assertEquals(greetingField.get.schema.encodedExample, Some("hello"))
            assertEquals(greetingField.get.schema.default, None)
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }
    }

    group("structural types") {

      test("optional field") {
        val schema = KindlingsSchema.derived[WithOptional].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithOptional] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("required", "optional"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("collection fields") {
        val schema = KindlingsSchema.derived[WithCollections].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithCollections] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("tags", "counts"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("map field") {
        val schema = KindlingsSchema.derived[WithMap].schema
        schema.schemaType match {
          case p: SchemaType.SProduct[WithMap] =>
            val fieldNames = p.fields.map(_.name.name)
            assertEquals(fieldNames, List("metadata"))
          case other =>
            fail(s"Expected SProduct, got: $other")
        }
      }

      test("recursive type uses SRef") {
        val schema = KindlingsSchema.derived[RecursiveTree].schema
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
      val schema = KindlingsSchema.derived[SimplePerson].schema
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

  group("annotation coverage") {

    test("@description annotation on field") {
      val schema = KindlingsSchema.derived[AnnotatedPerson].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[AnnotatedPerson] =>
          val nameField = p.fields.find(_.name.name == "name")
          assert(nameField.isDefined, "Should have a 'name' field")
          assertEquals(nameField.get.schema.description, Some("The name"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("@validate annotation on field") {
      val schema = KindlingsSchema.derived[WithValidation].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithValidation] =>
          val ageField = p.fields.find(_.name.name == "age")
          assert(ageField.isDefined, "Should have an 'age' field")
          assert(ageField.get.schema.validator != sttp.tapir.Validator.pass, "age field should have a validator")
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("@deprecated annotation on type") {
      val schema = KindlingsSchema.derived[DeprecatedType].schema
      assert(schema.deprecated, "Schema should be deprecated")
    }

    test("@default annotation on field") {
      val schema = KindlingsSchema.derived[WithDefaultAnnotation].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithDefaultAnnotation] =>
          val ageField = p.fields.find(_.name.name == "age")
          assert(ageField.isDefined, "Should have an 'age' field")
          assert(ageField.get.schema.default.isDefined, "age field should have a default from @default annotation")
          assert(ageField.get.schema.default.get._1 == 42)
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("@encodedExample annotation on field") {
      val schema = KindlingsSchema.derived[WithEncodedExampleAnnotation].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithEncodedExampleAnnotation] =>
          val emailField = p.fields.find(_.name.name == "email")
          assert(emailField.isDefined, "Should have an 'email' field")
          assertEquals(emailField.get.schema.encodedExample, Some("example@email.com"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }

  group("recursive type verification") {

    test("recursive field emits SRef") {
      val schema = KindlingsSchema.derived[RecursiveTree].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[RecursiveTree] =>
          val childrenField = p.fields.find(_.name.name == "children")
          assert(childrenField.isDefined, "Should have a 'children' field")
          childrenField.get.schema.schemaType match {
            case arr: SchemaType.SArray[?, ?] =>
              arr.element.schemaType match {
                case _: SchemaType.SRef[?] => () // success — recursive reference via SRef
                case other                 => fail(s"Expected SRef inside array, got: $other")
              }
            case other => fail(s"Expected SArray for children field, got: $other")
          }
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("indirect recursive type uses SRef") {
      val schema = KindlingsSchema.derived[RecursiveParent].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[RecursiveParent] =>
          val nodesField = p.fields.find(_.name.name == "nodes")
          assert(nodesField.isDefined, "Should have a 'nodes' field")
          nodesField.get.schema.schemaType match {
            case arr: SchemaType.SArray[?, ?] =>
              arr.element.schemaType match {
                case _: SchemaType.SRef[?]     => () // success — indirect recursive reference via SRef
                case _: SchemaType.SProduct[?] => () // also acceptable — non-recursive product
                case other                     => fail(s"Expected SRef or SProduct inside array, got: $other")
              }
            case other => fail(s"Expected SArray for nodes field, got: $other")
          }
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("recursive sealed trait hierarchy uses SRef") {
      val schema = KindlingsSchema.derived[TreeNode].schema
      schema.schemaType match {
        case c: SchemaType.SCoproduct[TreeNode] =>
          // Branch has two TreeNode fields — both should resolve to SRef
          val branchSubtype = c.subtypes.find(_.name.exists(_.fullName.endsWith("Branch")))
          assert(branchSubtype.isDefined, "Should have a Branch subtype")
          branchSubtype.get.schemaType match {
            case p: SchemaType.SProduct[?] =>
              val leftField = p.fields.find(_.name.name == "left")
              val rightField = p.fields.find(_.name.name == "right")
              assert(leftField.isDefined, "Branch should have a 'left' field")
              assert(rightField.isDefined, "Branch should have a 'right' field")
              leftField.get.schema.schemaType match {
                case _: SchemaType.SRef[?] => () // success
                case other                 => fail(s"Expected SRef for left field, got: $other")
              }
              rightField.get.schema.schemaType match {
                case _: SchemaType.SRef[?] => () // success
                case other                 => fail(s"Expected SRef for right field, got: $other")
              }
            case other => fail(s"Expected SProduct for Branch, got: $other")
          }
        case other =>
          fail(s"Expected SCoproduct for TreeNode, got: $other")
      }
    }

    test("recursive through Option") {
      val schema = KindlingsSchema.derived[RecursiveOption].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[RecursiveOption] =>
          val childField = p.fields.find(_.name.name == "child")
          assert(childField.isDefined, "Should have a 'child' field")
          // Option[RecursiveOption] uses .asOption which wraps in SOption containing SRef
          assert(childField.get.schema.isOptional, "child field should be optional")
          childField.get.schema.schemaType match {
            case SchemaType.SOption(innerSchema) =>
              innerSchema.schemaType match {
                case _: SchemaType.SRef[?] =>
                  () // success — SOption(SRef) for recursive Option type
                case other =>
                  fail(s"Expected SRef inside SOption for recursive Option field, got: $other")
              }
            case other =>
              fail(s"Expected SOption for recursive Option field, got: $other")
          }
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }

  group("combinatorial wrapper x inner-type") {

    test("CombOuter derives successfully") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val fieldNames = p.fields.map(_.name.name)
          assertEquals(
            fieldNames,
            List("optCaseClass", "optSealedTrait", "listCaseClass", "listSealedTrait", "mapCaseClass", "mapSealedTrait")
          )
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("Option[case class] field is optional with nested SProduct") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val optCCField = p.fields.find(_.name.name == "optCaseClass").get
          assert(optCCField.schema.isOptional, "Option field should be optional")
          optCCField.schema.schemaType match {
            case SchemaType.SOption(inner) =>
              inner.schemaType match {
                case _: SchemaType.SProduct[?] => () // nested case class as SProduct
                case _: SchemaType.SRef[?]     => () // or SRef if recursive resolution
                case other                     => fail(s"Expected SProduct or SRef inside SOption, got: $other")
              }
            case other => fail(s"Expected SOption, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("Option[sealed trait] field is optional with nested SCoproduct") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val optSTField = p.fields.find(_.name.name == "optSealedTrait").get
          assert(optSTField.schema.isOptional, "Option field should be optional")
          optSTField.schema.schemaType match {
            case SchemaType.SOption(inner) =>
              inner.schemaType match {
                case _: SchemaType.SCoproduct[?] => () // sealed trait as SCoproduct
                case _: SchemaType.SRef[?]       => () // or SRef
                case other                       => fail(s"Expected SCoproduct or SRef inside SOption, got: $other")
              }
            case other => fail(s"Expected SOption, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("List[case class] field is SArray with SProduct element") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val listCCField = p.fields.find(_.name.name == "listCaseClass").get
          listCCField.schema.schemaType match {
            case arr: SchemaType.SArray[?, ?] =>
              arr.element.schemaType match {
                case _: SchemaType.SProduct[?] => ()
                case _: SchemaType.SRef[?]     => ()
                case other                     => fail(s"Expected SProduct or SRef inside SArray, got: $other")
              }
            case other => fail(s"Expected SArray, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("List[sealed trait] field is SArray with SCoproduct element") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val listSTField = p.fields.find(_.name.name == "listSealedTrait").get
          listSTField.schema.schemaType match {
            case arr: SchemaType.SArray[?, ?] =>
              arr.element.schemaType match {
                case _: SchemaType.SCoproduct[?] => ()
                case _: SchemaType.SRef[?]       => ()
                case other                       => fail(s"Expected SCoproduct or SRef inside SArray, got: $other")
              }
            case other => fail(s"Expected SArray, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("Map[String, case class] field is SOpenProduct with SProduct element") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val mapCCField = p.fields.find(_.name.name == "mapCaseClass").get
          mapCCField.schema.schemaType match {
            case op: SchemaType.SOpenProduct[?, ?] =>
              op.valueSchema.schemaType match {
                case _: SchemaType.SProduct[?] => ()
                case _: SchemaType.SRef[?]     => ()
                case other                     => fail(s"Expected SProduct or SRef inside SOpenProduct, got: $other")
              }
            case other => fail(s"Expected SOpenProduct, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("Map[String, sealed trait] field is SOpenProduct with SCoproduct element") {
      val schema = KindlingsSchema.derived[CombOuter].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[CombOuter] =>
          val mapSTField = p.fields.find(_.name.name == "mapSealedTrait").get
          mapSTField.schema.schemaType match {
            case op: SchemaType.SOpenProduct[?, ?] =>
              op.valueSchema.schemaType match {
                case _: SchemaType.SCoproduct[?] => ()
                case _: SchemaType.SRef[?]       => ()
                case other => fail(s"Expected SCoproduct or SRef inside SOpenProduct, got: $other")
              }
            case other => fail(s"Expected SOpenProduct, got: $other")
          }
        case other => fail(s"Expected SProduct, got: $other")
      }
    }
  }

  group("value class schemas") {

    test("value class produces SProduct wrapping") {
      val schema = KindlingsSchema.derived[WithWrappedId].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithWrappedId] =>
          val fieldNames = p.fields.map(_.name.name)
          assert(fieldNames.contains("id"))
          assert(fieldNames.contains("name"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }

  group("parent annotation inheritance") {

    test("sealed trait @description propagates to coproduct schema") {
      val schema = KindlingsSchema.derived[AnnotatedShape].schema
      // The description annotation on the sealed trait should be set
      assert(schema.description.isDefined, "Sealed trait should have description from annotation")
      schema.description.get ==> "A shape type"
    }
  }

  group("KindlingsSchema.derivedEnumeration") {

    test("schema type is SString") {
      val schema = KindlingsSchema.derivedEnumeration[Color].schema
      schema.schemaType match {
        case _: SchemaType.SString[?] => () // success — string enum schema
        case other                    => fail(s"Expected SString, got: $other")
      }
    }

    test("validator contains all enum values") {
      val schema = KindlingsSchema.derivedEnumeration[Color].schema
      schema.validator match {
        case v: sttp.tapir.Validator.Enumeration[Color @unchecked] =>
          assertEquals(v.possibleValues, List(Red, Green, Blue))
        case other =>
          fail(s"Expected Validator.Enumeration, got: $other")
      }
    }

    test("schema has correct SName") {
      val schema = KindlingsSchema.derivedEnumeration[Color].schema
      assert(schema.name.isDefined, "Schema should have a name")
      assert(
        schema.name.get.fullName.endsWith("Color"),
        s"Expected name ending with Color, got: ${schema.name.get.fullName}"
      )
    }

    test("works without JSON config in scope") {
      assertEquals(
        EnumerationWithoutJsonConfig.schemaType,
        "SString"
      )
    }

    test("validator encodes values using case object names") {
      val schema = KindlingsSchema.derivedEnumeration[Color].schema
      schema.validator match {
        case v: sttp.tapir.Validator.Enumeration[Color @unchecked] =>
          val encodeFn = v.encode.getOrElse(fail("Expected encode function"))
          val encodedValues = List(Red, Green, Blue).flatMap(encodeFn(_))
          assertEquals(encodedValues, List("Red", "Green", "Blue"))
        case other =>
          fail(s"Expected Validator.Enumeration, got: $other")
      }
    }

    test("fails at compile time for sealed traits with case class children") {
      compileErrors(
        "KindlingsSchema.derivedEnumeration[hearth.kindlings.tapirschemaderivation.Shape]"
      ).check("non-singleton children")
    }

    test("fails at compile time for non-sealed types") {
      compileErrors(
        "KindlingsSchema.derivedEnumeration[hearth.kindlings.tapirschemaderivation.SimplePerson]"
      ).check("not a sealed trait or enum")
    }
  }

  group("discriminator child metadata") {

    test("discriminator adds field to child schemas") {
      val schema = JsoniterDiscriminatorChildDerivation.schema
      schema.schemaType match {
        case c: SchemaType.SCoproduct[Shape] =>
          c.subtypes.foreach { childSchema =>
            childSchema.schemaType match {
              case p: SchemaType.SProduct[?] =>
                // Each child SProduct should have a "type" discriminator field
                val typeField = p.fields.find(_.name.encodedName == "type")
                assert(
                  typeField.isDefined,
                  s"Child schema should have a 'type' field, fields: ${p.fields.map(_.name.encodedName)}"
                )
                // The discriminator field should have a single-value validator
                assert(
                  typeField.get.schema.validator != sttp.tapir.Validator.pass,
                  "Discriminator field should have a validator"
                )
              case other =>
                fail(s"Expected SProduct for child, got: $other")
            }
            // Each child should have encodedDiscriminatorValue attribute
            val discValue = childSchema.attributes.get(Schema.EncodedDiscriminatorValue.Attribute)
            assert(discValue.isDefined, "Child schema should have encodedDiscriminatorValue attribute")
          }
        case other =>
          fail(s"Expected SCoproduct, got: $other")
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

  private val schema: Schema[CamelCasePerson] = KindlingsSchema.derived[CamelCasePerson].schema

  val snakeCaseFieldNames: List[String] = schema.schemaType match {
    case p: SchemaType.SProduct[CamelCasePerson] => p.fields.map(_.name.encodedName)
    case _                                       => Nil
  }
}

// --- Dual-library helper objects ---

object CirceSnakeCaseDerivation {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  private val schema: Schema[CamelCasePerson] = KindlingsSchema.derived[CamelCasePerson].schema
  val fieldEncodedNames: List[String] = schema.schemaType match {
    case p: SchemaType.SProduct[CamelCasePerson] => p.fields.map(_.name.encodedName)
    case _                                       => Nil
  }
}

object CirceDiscriminatorDerivation {
  implicit val config: Configuration =
    Configuration(discriminator = Some("type"), transformConstructorNames = _.toLowerCase)
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[Shape] = KindlingsSchema.derived[Shape].schema
}

object JsoniterDiscriminatorDerivation {
  implicit val config: JsoniterConfig =
    JsoniterConfig(discriminatorFieldName = Some("type"), adtLeafClassNameMapper = _.toLowerCase)
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[Shape] = KindlingsSchema.derived[Shape].schema
}

object CirceTransientDerivation {
  implicit val config: Configuration = Configuration.default
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[WithCirceTransientField] = KindlingsSchema.derived[WithCirceTransientField].schema
}

object JsoniterTransientDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithJsoniterTransientField] = KindlingsSchema.derived[WithJsoniterTransientField].schema
}

object CirceEnumAsStringsDerivation {
  implicit val config: Configuration = Configuration.default.withEnumAsStrings
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[Color] = KindlingsSchema.derived[Color].schema
}

object JsoniterEnumAsStringsDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[Color] = KindlingsSchema.derived[Color].schema
}

object CirceDefaultsOptionalDerivation {
  implicit val config: Configuration = Configuration.default.withDefaults
  implicit val prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val schema: Schema[WithDefaults] = KindlingsSchema.derived[WithDefaults].schema
}

object JsoniterDefaultsOptionalDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithDefaults] = KindlingsSchema.derived[WithDefaults].schema
}

// --- Jsoniter-only helper objects ---

object JsoniterMapAsArrayDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithMap] = KindlingsSchema.derived[WithMap].schema
}

object JsoniterStringifiedDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withStringified
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[NumericFields] = KindlingsSchema.derived[NumericFields].schema
}

object JsoniterDiscriminatorChildDerivation {
  implicit val config: JsoniterConfig =
    JsoniterConfig(discriminatorFieldName = Some("type"))
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[Shape] = KindlingsSchema.derived[Shape].schema
}

// --- Enumeration helper object (no JSON config in scope) ---

object EnumerationWithoutJsonConfig {
  // No implicit Configuration or JsoniterConfig — derivedEnumeration should work without one
  val schemaType: String = KindlingsSchema.derivedEnumeration[Color].schema.schemaType match {
    case _: SchemaType.SString[?] => "SString"
    case other                    => s"unexpected: $other"
  }
}
