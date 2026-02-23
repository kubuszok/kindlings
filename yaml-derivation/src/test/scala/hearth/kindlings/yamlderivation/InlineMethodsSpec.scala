package hearth.kindlings.yamlderivation

import hearth.MacroSuite

class NotAYamlType

final class InlineMethodsSpec extends MacroSuite {

  group("KindlingsYamlEncoder.toYamlString") {

    test("simple case class") {
      val yaml = KindlingsYamlEncoder.toYamlString(SimplePerson("Alice", 30))
      assert(yaml.contains("Alice"), s"Expected 'Alice' in: $yaml")
      assert(yaml.contains("30"), s"Expected '30' in: $yaml")
    }

    test("with custom config") {
      implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
      val yaml = KindlingsYamlEncoder.toYamlString(CamelCasePerson("Alice", "Smith"))
      assert(yaml.contains("first_name"), s"Expected 'first_name' in: $yaml")
      assert(yaml.contains("last_name"), s"Expected 'last_name' in: $yaml")
    }
  }

  group("KindlingsYamlDecoder.fromYamlString") {

    test("simple case class success") {
      val yaml = "name: Alice\nage: 30\n"
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson](yaml)
      assertEquals(result, Right(SimplePerson("Alice", 30)))
    }

    test("invalid YAML returns Left") {
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson]("not: [valid: yaml: {")
      assert(result.isLeft)
    }
  }

  group("syntax.toYamlString") {

    test("simple case class") {
      import syntax.*
      val yaml = SimplePerson("Alice", 30).toYamlString
      assert(yaml.contains("Alice"), s"Expected 'Alice' in: $yaml")
      assert(yaml.contains("30"), s"Expected '30' in: $yaml")
    }

    test("with custom config") {
      import syntax.*
      implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
      val yaml = CamelCasePerson("Alice", "Smith").toYamlString
      assert(yaml.contains("first_name"), s"Expected 'first_name' in: $yaml")
      assert(yaml.contains("last_name"), s"Expected 'last_name' in: $yaml")
    }
  }

  group("syntax.fromYamlString") {

    test("simple case class success") {
      import syntax.*
      val result = "name: Alice\nage: 30\n".fromYamlString[SimplePerson]
      assertEquals(result, Right(SimplePerson("Alice", 30)))
    }

    test("invalid YAML returns Left") {
      import syntax.*
      val result = "not: [valid: yaml: {".fromYamlString[SimplePerson]
      assert(result.isLeft)
    }
  }

  group("round-trip") {

    test("toYamlString then fromYamlString") {
      val value = SimplePerson("Bob", 25)
      val yaml = KindlingsYamlEncoder.toYamlString(value)
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson](yaml)
      assertEquals(result, Right(value))
    }

    test("syntax toYamlString then fromYamlString") {
      import syntax.*
      val value = SimplePerson("Bob", 25)
      val yaml = value.toYamlString
      val result = yaml.fromYamlString[SimplePerson]
      assertEquals(result, Right(value))
    }
  }

  group("compile-time errors") {

    test("encode with unhandled type produces error message") {
      compileErrors(
        """
        import hearth.kindlings.yamlderivation.{KindlingsYamlEncoder, NotAYamlType}
        KindlingsYamlEncoder.encode(new NotAYamlType)
        """
      ).check(
        "Macro derivation failed with the following errors:",
        "  - The type hearth.kindlings.yamlderivation.NotAYamlType was not handled by any encoder derivation rule:",
        "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlEncoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
      )
    }

    test("fromYamlString with unhandled type produces error message") {
      compileErrors(
        """
        import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, NotAYamlType}
        KindlingsYamlDecoder.fromYamlString[NotAYamlType]("foo: bar")
        """
      ).check(
        "Macro derivation failed with the following errors:",
        "  - The type hearth.kindlings.yamlderivation.NotAYamlType was not handled by any decoder derivation rule:",
        "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlDecoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
      )
    }
  }
}
