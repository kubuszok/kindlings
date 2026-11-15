package hearth.kindlings.yamlderivation

import hearth.MacroSuite

class NotAYamlType

final class InlineMethodsSpec extends MacroSuite {

  group("KindlingsYamlEncoder.toYamlString") {

    test("simple case class") {
      val yaml = KindlingsYamlEncoder.toYamlString(SimplePerson("Alice", 30))
      yaml.contains("Alice") ==> true
      yaml.contains("30") ==> true
    }

    test("with custom config") {
      implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
      val yaml = KindlingsYamlEncoder.toYamlString(CamelCasePerson("Alice", "Smith"))
      yaml.contains("first_name") ==> true
      yaml.contains("last_name") ==> true
    }

    // the derived body used to be spliced as a local val's right-hand side: Scala 3 -Xcheck-macros rejected the
    // Option lambda it creates ("Block contains definition with different owners")
    test("recursive type reached through an Option") {
      val value = RecursiveOption("a", Some(RecursiveOption("b", None)))
      val yaml = KindlingsYamlEncoder.toYamlString(value)
      KindlingsYamlDecoder.fromYamlString[RecursiveOption](yaml) ==> Right(value)
    }
  }

  group("KindlingsYamlDecoder.fromYamlString") {

    test("simple case class success") {
      val yaml = "name: Alice\nage: 30\n"
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson](yaml)
      result ==> Right(SimplePerson("Alice", 30))
    }

    test("invalid YAML returns Left") {
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson]("not: [valid: yaml: {")
      result.isLeft ==> true
    }
  }

  group("toYamlString of empty collections") {

    val empties = WithEmpties(Nil, Map.empty, List(Nil, List(1)), Map("k" -> Nil), "x")
    val lines = (yaml: String) => yaml.linesIterator.map(_.trim).toSet

    test("default: printed as an empty value, as scala-yaml (does not decode back)") {
      val yaml = KindlingsYamlEncoder.toYamlString(empties)
      assert(lines(yaml).contains("list:"), yaml)
      assert(lines(yaml).contains("map:"), yaml)
      assert(!yaml.contains("[]") && !yaml.contains("{}"), yaml)
      assert(KindlingsYamlDecoder.fromYamlString[WithEmpties](yaml).isLeft)
    }

    test("withEmptyCollectionsAsFlow: printed as [] and {} (also nested) and decodes back") {
      implicit val config: YamlConfig = YamlConfig.default.withEmptyCollectionsAsFlow
      val yaml = KindlingsYamlEncoder.toYamlString(empties)
      val printed = lines(yaml)
      assert(printed.contains("list: []"), yaml)
      assert(printed.contains("map: {}"), yaml)
      assert(printed.contains("- []"), yaml) // the empty list nested in `nested`
      assert(printed.contains("k: []"), yaml) // the empty list nested in `mapOfLists`
      KindlingsYamlDecoder.fromYamlString[WithEmpties](yaml) ==> Right(empties)
    }

    test("withEmptyCollectionsAsFlow: a top-level empty collection and non-empty ones") {
      implicit val config: YamlConfig = YamlConfig.default.withEmptyCollectionsAsFlow
      KindlingsYamlEncoder.toYamlString(List.empty[Int]).trim ==> "[]"
      KindlingsYamlEncoder.toYamlString(Map.empty[String, Int]).trim ==> "{}"
      val full = WithEmpties(List(1), Map("a" -> 1), List(List(2)), Map("k" -> List(3)), "x")
      KindlingsYamlDecoder.fromYamlString[WithEmpties](KindlingsYamlEncoder.toYamlString(full)) ==> Right(full)
    }

    test("withoutEmptyCollectionsAsFlow restores the default") {
      implicit val config: YamlConfig = YamlConfig.default.withEmptyCollectionsAsFlow.withoutEmptyCollectionsAsFlow
      config.emptyCollectionsAsFlow ==> false
      assert(!KindlingsYamlEncoder.toYamlString(empties).contains("[]"))
    }

    test("syntax.toYamlString honours withEmptyCollectionsAsFlow") {
      import syntax.*
      implicit val config: YamlConfig = YamlConfig.default.withEmptyCollectionsAsFlow
      KindlingsYamlDecoder.fromYamlString[WithEmpties](empties.toYamlString) ==> Right(empties)
    }
  }

  group("syntax.toYamlString") {

    test("simple case class") {
      import syntax.*
      val yaml = SimplePerson("Alice", 30).toYamlString
      yaml.contains("Alice") ==> true
      yaml.contains("30") ==> true
    }

    test("recursive type reached through an Option") {
      import syntax.*
      val value = RecursiveOption("a", Some(RecursiveOption("b", None)))
      KindlingsYamlDecoder.fromYamlString[RecursiveOption](value.toYamlString) ==> Right(value)
    }

    test("with custom config") {
      import syntax.*
      implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
      val yaml = CamelCasePerson("Alice", "Smith").toYamlString
      yaml.contains("first_name") ==> true
      yaml.contains("last_name") ==> true
    }
  }

  group("syntax.fromYamlString") {

    test("simple case class success") {
      import syntax.*
      val result = "name: Alice\nage: 30\n".fromYamlString[SimplePerson]
      result ==> Right(SimplePerson("Alice", 30))
    }

    test("invalid YAML returns Left") {
      import syntax.*
      val result = "not: [valid: yaml: {".fromYamlString[SimplePerson]
      result.isLeft ==> true
    }
  }

  group("round-trip") {

    test("toYamlString then fromYamlString") {
      val value = SimplePerson("Bob", 25)
      val yaml = KindlingsYamlEncoder.toYamlString(value)
      val result = KindlingsYamlDecoder.fromYamlString[SimplePerson](yaml)
      result ==> Right(value)
    }

    test("syntax toYamlString then fromYamlString") {
      import syntax.*
      val value = SimplePerson("Bob", 25)
      val yaml = value.toYamlString
      val result = yaml.fromYamlString[SimplePerson]
      result ==> Right(value)
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

    test("fromYamlString with Nothing type parameter produces clear error") {
      compileErrors(
        """
        import hearth.kindlings.yamlderivation.KindlingsYamlDecoder
        val result = KindlingsYamlDecoder.fromYamlString("foo: bar")
        """
      ).check(
        "type parameter was inferred as"
      )
    }
  }
}
