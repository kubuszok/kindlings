package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final class XmlConfigSpec extends MacroSuite {

  group("XmlConfig defaults") {

    test("default config has Element field mode") {
      val config = XmlConfig()
      assert(config.defaultFieldMode == XmlFieldMode.Element)
    }

    test("default config has identity fieldNameMapper") {
      val config = XmlConfig()
      assert(config.fieldNameMapper("firstName") == "firstName")
    }

    test("default config has identity constructorNameMapper") {
      val config = XmlConfig()
      assert(config.constructorNameMapper("MyClass") == "MyClass")
    }

    test("default config has Some(type) discriminatorAttribute") {
      val config = XmlConfig()
      assert(config.discriminatorAttribute == Some("type"))
    }

    test("default config has enumAsStrings = false") {
      val config = XmlConfig()
      assert(config.enumAsStrings == false)
    }

    test("default config has useDefaults = false") {
      val config = XmlConfig()
      assert(config.useDefaults == false)
    }

    test("default config has transientNone = false") {
      val config = XmlConfig()
      assert(config.transientNone == false)
    }

    test("default config has transientEmpty = false") {
      val config = XmlConfig()
      assert(config.transientEmpty == false)
    }

    test("implicit default is same as XmlConfig()") {
      assert(XmlConfig.default == XmlConfig())
    }
  }

  group("XmlConfig builder methods") {

    test("withAttributesByDefault sets Attribute mode") {
      val config = XmlConfig().withAttributesByDefault
      assert(config.defaultFieldMode == XmlFieldMode.Attribute)
    }

    test("withElementsByDefault sets Element mode") {
      val config = XmlConfig().withAttributesByDefault.withElementsByDefault
      assert(config.defaultFieldMode == XmlFieldMode.Element)
    }

    test("withDefaultFieldMode(Attribute) sets Attribute mode") {
      val config = XmlConfig().withDefaultFieldMode(XmlFieldMode.Attribute)
      assert(config.defaultFieldMode == XmlFieldMode.Attribute)
    }

    test("withDefaultFieldMode(Element) sets Element mode") {
      val config = XmlConfig().withDefaultFieldMode(XmlFieldMode.Element)
      assert(config.defaultFieldMode == XmlFieldMode.Element)
    }

    test("withFieldNameMapper sets custom mapper") {
      val config = XmlConfig().withFieldNameMapper(_.toUpperCase)
      assert(config.fieldNameMapper("hello") == "HELLO")
    }

    test("withConstructorNameMapper sets custom mapper") {
      val config = XmlConfig().withConstructorNameMapper(_.toLowerCase)
      assert(config.constructorNameMapper("MyClass") == "myclass")
    }

    test("withSnakeCaseFieldNames uses snake_case") {
      val config = XmlConfig().withSnakeCaseFieldNames
      assert(config.fieldNameMapper("firstName") == "first_name")
    }

    test("withKebabCaseFieldNames uses kebab-case") {
      val config = XmlConfig().withKebabCaseFieldNames
      assert(config.fieldNameMapper("firstName") == "first-name")
    }

    test("withPascalCaseFieldNames uses PascalCase") {
      val config = XmlConfig().withPascalCaseFieldNames
      assert(config.fieldNameMapper("firstName") == "FirstName")
    }

    test("withScreamingSnakeCaseFieldNames uses SCREAMING_SNAKE_CASE") {
      val config = XmlConfig().withScreamingSnakeCaseFieldNames
      assert(config.fieldNameMapper("firstName") == "FIRST_NAME")
    }

    test("withSnakeCaseConstructorNames uses snake_case") {
      val config = XmlConfig().withSnakeCaseConstructorNames
      assert(config.constructorNameMapper("MyClass") == "my_class")
    }

    test("withKebabCaseConstructorNames uses kebab-case") {
      val config = XmlConfig().withKebabCaseConstructorNames
      assert(config.constructorNameMapper("MyClass") == "my-class")
    }

    test("withDiscriminator sets custom discriminator") {
      val config = XmlConfig().withDiscriminator("kind")
      assert(config.discriminatorAttribute == Some("kind"))
    }

    test("withNoDiscriminator removes discriminator") {
      val config = XmlConfig().withNoDiscriminator
      assert(config.discriminatorAttribute == None)
    }

    test("withEnumAsStrings enables enum as strings") {
      val config = XmlConfig().withEnumAsStrings
      assert(config.enumAsStrings == true)
    }

    test("withUseDefaults enables default values") {
      val config = XmlConfig().withUseDefaults
      assert(config.useDefaults == true)
    }

    test("withTransientNone enables transient None") {
      val config = XmlConfig().withTransientNone
      assert(config.transientNone == true)
    }

    test("withTransientEmpty enables transient empty collections") {
      val config = XmlConfig().withTransientEmpty
      assert(config.transientEmpty == true)
    }

    test("builder methods can be chained") {
      val config = XmlConfig()
        .withSnakeCaseFieldNames
        .withKebabCaseConstructorNames
        .withNoDiscriminator
        .withEnumAsStrings
        .withUseDefaults
        .withTransientNone
        .withTransientEmpty
        .withAttributesByDefault

      assert(config.defaultFieldMode == XmlFieldMode.Attribute)
      assert(config.fieldNameMapper("firstName") == "first_name")
      assert(config.constructorNameMapper("MyClass") == "my-class")
      assert(config.discriminatorAttribute == None)
      assert(config.enumAsStrings == true)
      assert(config.useDefaults == true)
      assert(config.transientNone == true)
      assert(config.transientEmpty == true)
    }
  }

  group("name transformation functions") {

    group("snakeCase") {

      test("simple camelCase") {
        assert(XmlConfig.snakeCase("firstName") == "first_name")
      }

      test("multiple uppercase letters") {
        assert(XmlConfig.snakeCase("firstNameLastName") == "first_name_last_name")
      }

      test("single word lowercase") {
        assert(XmlConfig.snakeCase("name") == "name")
      }

      test("single character") {
        assert(XmlConfig.snakeCase("a") == "a")
      }

      test("empty string") {
        assert(XmlConfig.snakeCase("") == "")
      }

      test("leading uppercase") {
        assert(XmlConfig.snakeCase("Name") == "name")
      }

      test("all lowercase") {
        assert(XmlConfig.snakeCase("already") == "already")
      }

      test("consecutive uppercase treated individually") {
        assert(XmlConfig.snakeCase("HTMLParser") == "h_t_m_l_parser")
      }
    }

    group("kebabCase") {

      test("simple camelCase") {
        assert(XmlConfig.kebabCase("firstName") == "first-name")
      }

      test("multiple uppercase letters") {
        assert(XmlConfig.kebabCase("firstNameLastName") == "first-name-last-name")
      }

      test("single word lowercase") {
        assert(XmlConfig.kebabCase("name") == "name")
      }

      test("single character") {
        assert(XmlConfig.kebabCase("a") == "a")
      }

      test("empty string") {
        assert(XmlConfig.kebabCase("") == "")
      }

      test("leading uppercase") {
        assert(XmlConfig.kebabCase("Name") == "name")
      }
    }

    group("pascalCase") {

      test("simple camelCase") {
        assert(XmlConfig.pascalCase("firstName") == "FirstName")
      }

      test("single word lowercase") {
        assert(XmlConfig.pascalCase("name") == "Name")
      }

      test("already PascalCase") {
        assert(XmlConfig.pascalCase("Name") == "Name")
      }

      test("single character") {
        assert(XmlConfig.pascalCase("a") == "A")
      }

      test("empty string") {
        assert(XmlConfig.pascalCase("") == "")
      }
    }

    group("screamingSnakeCase") {

      test("simple camelCase") {
        assert(XmlConfig.screamingSnakeCase("firstName") == "FIRST_NAME")
      }

      test("multiple uppercase letters") {
        assert(XmlConfig.screamingSnakeCase("firstNameLastName") == "FIRST_NAME_LAST_NAME")
      }

      test("single word lowercase") {
        assert(XmlConfig.screamingSnakeCase("name") == "NAME")
      }

      test("single character") {
        assert(XmlConfig.screamingSnakeCase("a") == "A")
      }

      test("empty string") {
        assert(XmlConfig.screamingSnakeCase("") == "")
      }

      test("leading uppercase") {
        assert(XmlConfig.screamingSnakeCase("Name") == "NAME")
      }
    }
  }

  group("XmlConfig with derivation") {

    group("withSnakeCaseFieldNames") {

      test("encoder derives with snake_case config") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
        val encoder = KindlingsXmlEncoder.derive[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert(result.label == "person")
      }

      test("decoder derives with snake_case config") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
        val decoder = KindlingsXmlDecoder.derive[CamelCasePerson]
        // The decoder exists and can be called
        assert(decoder != null)
      }
    }

    group("withKebabCaseFieldNames") {

      test("encoder derives with kebab-case config") {
        implicit val config: XmlConfig = XmlConfig().withKebabCaseFieldNames
        val encoder = KindlingsXmlEncoder.derive[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert(result.label == "person")
      }
    }

    group("withPascalCaseFieldNames") {

      test("encoder derives with PascalCase config") {
        implicit val config: XmlConfig = XmlConfig().withPascalCaseFieldNames
        val encoder = KindlingsXmlEncoder.derive[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert(result.label == "person")
      }
    }

    group("withScreamingSnakeCaseFieldNames") {

      test("encoder derives with SCREAMING_SNAKE_CASE config") {
        implicit val config: XmlConfig = XmlConfig().withScreamingSnakeCaseFieldNames
        val encoder = KindlingsXmlEncoder.derive[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert(result.label == "person")
      }
    }

    group("withSnakeCaseConstructorNames") {

      test("encoder derives sealed trait with snake_case constructor names") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseConstructorNames
        val encoder = KindlingsXmlEncoder.derive[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.label == "shape")
      }
    }

    group("withKebabCaseConstructorNames") {

      test("encoder derives sealed trait with kebab-case constructor names") {
        implicit val config: XmlConfig = XmlConfig().withKebabCaseConstructorNames
        val encoder = KindlingsXmlEncoder.derive[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.label == "shape")
      }
    }

    group("withNoDiscriminator") {

      test("encoder derives sealed trait without discriminator") {
        implicit val config: XmlConfig = XmlConfig().withNoDiscriminator
        val encoder = KindlingsXmlEncoder.derive[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.label == "shape")
      }
    }

    group("withEnumAsStrings") {

      test("encoder derives sealed trait with enumAsStrings config") {
        implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
        val encoder = KindlingsXmlEncoder.derive[Animal]
        val result = encoder.encode(Dog("Rex", "Labrador"), "animal")
        assert(result.label == "animal")
      }
    }

    group("withUseDefaults") {

      test("encoder derives with useDefaults config") {
        implicit val config: XmlConfig = XmlConfig().withUseDefaults
        val encoder = KindlingsXmlEncoder.derive[WithDefaults]
        val result = encoder.encode(WithDefaults("Alice"), "user")
        assert(result.label == "user")
      }

      test("decoder derives with useDefaults config") {
        implicit val config: XmlConfig = XmlConfig().withUseDefaults
        val decoder = KindlingsXmlDecoder.derive[WithDefaults]
        assert(decoder != null)
      }
    }

    group("withTransientNone") {

      test("encoder derives with transientNone config") {
        implicit val config: XmlConfig = XmlConfig().withTransientNone
        val encoder = KindlingsXmlEncoder.derive[Box[Option[Int]]]
        val resultSome = encoder.encode(Box(Some(42)), "box")
        assert(resultSome.label == "box")
        val resultNone = encoder.encode(Box(None), "box")
        assert(resultNone.label == "box")
      }
    }

    group("withTransientEmpty") {

      test("encoder derives with transientEmpty config") {
        implicit val config: XmlConfig = XmlConfig().withTransientEmpty
        val encoder = KindlingsXmlEncoder.derive[TeamWithMembers]
        val result = encoder.encode(TeamWithMembers("Team A", List.empty), "team")
        assert(result.label == "team")
      }
    }

    group("withAttributesByDefault") {

      test("encoder derives with Attribute default field mode") {
        implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
        val encoder = KindlingsXmlEncoder.derive[SimplePerson]
        val result = encoder.encode(SimplePerson("Alice", 30), "person")
        assert(result.label == "person")
      }
    }

    group("combined config options") {

      test("encoder derives with multiple config options") {
        implicit val config: XmlConfig = XmlConfig()
          .withSnakeCaseFieldNames
          .withUseDefaults
          .withTransientNone
        val encoder = KindlingsXmlEncoder.derive[WithDefaults]
        val result = encoder.encode(WithDefaults("Alice"), "user")
        assert(result.label == "user")
      }
    }
  }
}
