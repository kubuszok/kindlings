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
      val config =
        XmlConfig().withSnakeCaseFieldNames.withKebabCaseConstructorNames.withNoDiscriminator.withEnumAsStrings.withUseDefaults.withTransientNone.withTransientEmpty.withAttributesByDefault

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

    group("fieldNameMapper") {

      test("encoder uses snake_case field names") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
        val encoder = KindlingsXmlEncoder.derived[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert(result.label == "person")
        assert((result \ "first_name").text == "Alice")
        assert((result \ "last_name").text == "Smith")
        // Original camelCase names should not be present
        assert((result \ "firstName").isEmpty)
        assert((result \ "lastName").isEmpty)
      }

      test("decoder uses snake_case field names") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
        val decoder = KindlingsXmlDecoder.derived[CamelCasePerson]
        val xml = <person><first_name>Alice</first_name><last_name>Smith</last_name></person>
        val result = decoder.decode(xml)
        assert(result == Right(CamelCasePerson("Alice", "Smith")))
      }

      test("encoder uses kebab-case field names") {
        implicit val config: XmlConfig = XmlConfig().withKebabCaseFieldNames
        val encoder = KindlingsXmlEncoder.derived[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert((result \ "first-name").text == "Alice")
        assert((result \ "last-name").text == "Smith")
      }

      test("encoder uses PascalCase field names") {
        implicit val config: XmlConfig = XmlConfig().withPascalCaseFieldNames
        val encoder = KindlingsXmlEncoder.derived[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert((result \ "FirstName").text == "Alice")
        assert((result \ "LastName").text == "Smith")
      }

      test("encoder uses SCREAMING_SNAKE_CASE field names") {
        implicit val config: XmlConfig = XmlConfig().withScreamingSnakeCaseFieldNames
        val encoder = KindlingsXmlEncoder.derived[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert((result \ "FIRST_NAME").text == "Alice")
        assert((result \ "LAST_NAME").text == "Smith")
      }

      test("@xmlName annotation overrides fieldNameMapper") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
        val encoder = KindlingsXmlEncoder.derived[XmlWithFieldName]
        val result = encoder.encode(XmlWithFieldName("John", 30), "user")
        // @xmlName("user_name") takes precedence over snake_case mapper
        assert((result \ "user_name").text == "John")
        assert((result \ "age").text == "30")
      }
    }

    group("constructorNameMapper") {

      test("encoder uses snake_case constructor names in discriminator") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseConstructorNames
        val encoder = KindlingsXmlEncoder.derived[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.attribute("type").map(_.text) == Some("circle"))
      }

      test("encoder uses kebab-case constructor names in discriminator") {
        implicit val config: XmlConfig = XmlConfig().withKebabCaseConstructorNames
        val encoder = KindlingsXmlEncoder.derived[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.attribute("type").map(_.text) == Some("circle"))
      }

      test("decoder uses snake_case constructor names") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseConstructorNames
        val decoder = KindlingsXmlDecoder.derived[Shape]
        val xml = <shape type="circle"><radius>5.0</radius></shape>
        val result = decoder.decode(xml)
        assert(result == Right(Circle(5.0)))
      }
    }

    group("discriminatorAttribute") {

      test("encoder uses custom discriminator attribute name") {
        implicit val config: XmlConfig = XmlConfig().withDiscriminator("kind")
        val encoder = KindlingsXmlEncoder.derived[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        assert(result.attribute("kind").map(_.text) == Some("Circle"))
        assert(result.attribute("type").isEmpty)
      }

      test("decoder uses custom discriminator attribute name") {
        implicit val config: XmlConfig = XmlConfig().withDiscriminator("kind")
        val decoder = KindlingsXmlDecoder.derived[Shape]
        val xml = <shape kind="Circle"><radius>5.0</radius></shape>
        val result = decoder.decode(xml)
        assert(result == Right(Circle(5.0)))
      }

      test("encoder wraps with type name when no discriminator") {
        implicit val config: XmlConfig = XmlConfig().withNoDiscriminator
        val encoder = KindlingsXmlEncoder.derived[Shape]
        val result = encoder.encode(Circle(5.0), "shape")
        // Without discriminator, the result is wrapped: <Circle><radius>5.0</radius></Circle>
        assert(result.label == "Circle")
        assert((result \\ "radius").text == "5.0")
      }

      test("decoder reads wrapped element when no discriminator") {
        implicit val config: XmlConfig = XmlConfig().withNoDiscriminator
        val decoder = KindlingsXmlDecoder.derived[Shape]
        val xml = <shape><Circle><radius>5.0</radius></Circle></shape>
        val result = decoder.decode(xml)
        assert(result == Right(Circle(5.0)))
      }
    }

    group("useDefaults") {

      test("encoder with useDefaults still encodes all fields") {
        implicit val config: XmlConfig = XmlConfig().withUseDefaults
        val encoder = KindlingsXmlEncoder.derived[WithDefaults]
        val result = encoder.encode(WithDefaults("Alice"), "user")
        assert((result \ "name").text == "Alice")
        assert((result \ "age").text == "25")
        assert((result \ "active").text == "true")
      }

      test("decoder uses default values for missing fields") {
        implicit val config: XmlConfig = XmlConfig().withUseDefaults
        val decoder = KindlingsXmlDecoder.derived[WithDefaults]
        // Only provide name, omit age and active
        val xml = <user><name>Alice</name></user>
        val result = decoder.decode(xml)
        assert(result == Right(WithDefaults("Alice", 25, true)))
      }

      test("decoder prefers provided values over defaults") {
        implicit val config: XmlConfig = XmlConfig().withUseDefaults
        val decoder = KindlingsXmlDecoder.derived[WithDefaults]
        val xml = <user><name>Bob</name><age>30</age><active>false</active></user>
        val result = decoder.decode(xml)
        assert(result == Right(WithDefaults("Bob", 30, false)))
      }
    }

    group("transientNone") {

      test("encoder omits None fields with transientNone") {
        implicit val config: XmlConfig = XmlConfig().withTransientNone
        val encoder = KindlingsXmlEncoder.derived[Box[Option[Int]]]
        val resultNone = encoder.encode(Box(None), "box")
        // With transientNone, the <value> element should be absent for None
        assert(resultNone.child.collect { case e: scala.xml.Elem => e }.isEmpty)
      }

      test("encoder keeps Some fields with transientNone") {
        implicit val config: XmlConfig = XmlConfig().withTransientNone
        val encoder = KindlingsXmlEncoder.derived[Box[Option[Int]]]
        val resultSome = encoder.encode(Box(Some(42)), "box")
        assert((resultSome \\ "value").nonEmpty)
      }

      test("encoder includes None fields without transientNone") {
        // Default config (transientNone = false)
        val encoder = KindlingsXmlEncoder.derived[Box[Option[Int]]]
        val resultNone = encoder.encode(Box(None), "box")
        // Without transientNone, the <value> element should still be present
        assert(resultNone.child.collect { case e: scala.xml.Elem => e }.nonEmpty)
      }
    }

    group("transientEmpty") {

      test("encoder omits empty collections with transientEmpty") {
        implicit val config: XmlConfig = XmlConfig().withTransientEmpty
        val encoder = KindlingsXmlEncoder.derived[TeamWithMembers]
        val result = encoder.encode(TeamWithMembers("Team A", List.empty), "team")
        assert((result \ "name").text == "Team A")
        // The <members> element should be absent for empty list
        val memberElems = result.child.collect { case e: scala.xml.Elem if e.label == "members" => e }
        assert(memberElems.isEmpty)
      }

      test("encoder keeps non-empty collections with transientEmpty") {
        implicit val config: XmlConfig = XmlConfig().withTransientEmpty
        val encoder = KindlingsXmlEncoder.derived[TeamWithMembers]
        val result = encoder.encode(TeamWithMembers("Team A", List(SimplePerson("Alice", 30))), "team")
        assert((result \ "name").text == "Team A")
        val memberElems = result.child.collect { case e: scala.xml.Elem if e.label == "members" => e }
        assert(memberElems.nonEmpty)
      }
    }

    group("defaultFieldMode") {

      test("encoder uses attributes by default with withAttributesByDefault") {
        implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
        val encoder = KindlingsXmlEncoder.derived[SimplePerson]
        val result = encoder.encode(SimplePerson("Alice", 30), "person")
        assert(result.label == "person")
        // Fields should be rendered as attributes, not child elements
        assert(result.attribute("name").map(_.text) == Some("Alice"))
        assert(result.attribute("age").map(_.text) == Some("30"))
        assert((result \ "name").isEmpty)
        assert((result \ "age").isEmpty)
      }
    }

    group("combined config options") {

      test("encoder uses snake_case fields + transientNone together") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames.withTransientNone
        val encoder = KindlingsXmlEncoder.derived[CamelCasePerson]
        val result = encoder.encode(CamelCasePerson("Alice", "Smith"), "person")
        assert((result \ "first_name").text == "Alice")
        assert((result \ "last_name").text == "Smith")
      }

      test("encoder uses snake_case fields + useDefaults together") {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames.withUseDefaults
        val encoder = KindlingsXmlEncoder.derived[WithDefaults]
        val result = encoder.encode(WithDefaults("Alice"), "user")
        assert((result \ "name").text == "Alice")
        assert((result \ "age").text == "25")
        assert((result \ "active").text == "true")
      }
    }
  }
}
