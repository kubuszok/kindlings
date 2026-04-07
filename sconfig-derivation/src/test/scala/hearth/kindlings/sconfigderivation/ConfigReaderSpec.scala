package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.{ConfigFactory, ConfigValue}

final class ConfigReaderSpec extends MacroSuite {

  /** Build a `ConfigValue` from a HOCON string. */
  private def value(hocon: String): ConfigValue =
    ConfigFactory.parseString(hocon).root

  group("ConfigReader") {

    group("primitives via case-class fields") {

      test("Int field") {
        val r = ConfigReader.derive[SingleField]
        r.from(value("{ value = 42 }")) ==> Right(SingleField(42))
      }

      test("String + Int + Boolean") {
        val r = ConfigReader.derive[WithDefaults]
        r.from(value("{ name = Alice, age = 30, active = true }")) ==> Right(WithDefaults("Alice", 30, true))
      }
    }

    group("case classes") {

      test("simple") {
        val r = ConfigReader.derive[SimplePerson]
        r.from(value("{ name = Alice, age = 30 }")) ==> Right(SimplePerson("Alice", 30))
      }

      test("empty") {
        val r = ConfigReader.derive[EmptyClass]
        r.from(value("{}")) ==> Right(EmptyClass())
      }

      test("nested") {
        val r = ConfigReader.derive[PersonWithAddress]
        r.from(
          value("{ name = Bob, age = 25, address = { street = \"123 Main\", city = Springfield } }")
        ) ==> Right(PersonWithAddress("Bob", 25, Address("123 Main", "Springfield")))
      }

      test("missing required field returns failure") {
        val r = ConfigReader.derive[SimplePerson]
        val result = r.from(value("{ name = Alice }"))
        assert(result.isLeft)
      }
    }

    group("collections") {

      test("List field of primitives") {
        val r = ConfigReader.derive[WithList]
        r.from(value("{ items = [1, 2, 3] }")) ==> Right(WithList(List(1, 2, 3)))
      }

      test("List of case classes") {
        val r = ConfigReader.derive[TeamWithMembers]
        r.from(
          value("{ name = Eagles, members = [ { name = A, age = 1 }, { name = B, age = 2 } ] }")
        ) ==> Right(TeamWithMembers("Eagles", List(SimplePerson("A", 1), SimplePerson("B", 2))))
      }

      test("Map[String, Int] field") {
        val r = ConfigReader.derive[WithMap]
        r.from(value("{ scores = { a = 1, b = 2 } }")) ==> Right(WithMap(Map("a" -> 1, "b" -> 2)))
      }
    }

    group("options") {

      test("None from missing key") {
        val r = ConfigReader.derive[WithOption]
        r.from(value("{ name = Alice }")) ==> Right(WithOption("Alice", None))
      }

      test("Some from present key") {
        val r = ConfigReader.derive[WithOption]
        r.from(value("{ name = Alice, nickname = Allie }")) ==> Right(WithOption("Alice", Some("Allie")))
      }
    }

    group("annotations") {

      test("@configKey overrides field name") {
        val r = ConfigReader.derive[WithConfigKey]
        r.from(value("{ user_name = jdoe, age = 30 }")) ==> Right(WithConfigKey("jdoe", 30))
      }

      test("@transientField uses default") {
        val r = ConfigReader.derive[WithTransient]
        r.from(value("{ name = Alice }")) ==> Right(WithTransient("Alice", None))
      }
    }

    group("defaults via useDefaults") {

      test("missing field uses default") {
        implicit val cfg: SConfig = SConfig().withUseDefaults
        val r = ConfigReader.derive[WithDefaults]
        r.from(value("{ name = Bob }")) ==> Right(WithDefaults("Bob"))
      }
    }

    group("strict mode (allowUnknownKeys = false)") {

      test("default allowUnknownKeys = true accepts extra keys") {
        val r = ConfigReader.derive[SimplePerson]
        // Extra `extra` field is silently ignored under default config (matching upstream).
        r.from(value("{ name = Alice, age = 30, extra = ignored }")) ==> Right(SimplePerson("Alice", 30))
      }

      test("global SConfig.withStrictDecoding rejects extra keys") {
        implicit val cfg: SConfig = SConfig().withStrictDecoding
        val r = ConfigReader.derive[SimplePerson]
        val result = r.from(value("{ name = Alice, age = 30, extra = ignored }"))
        assert(result.isLeft)
      }

      test("per-type ProductHint allowUnknownKeys = false rejects extras") {
        // Even with global config permissive, the per-type hint enforces strict mode.
        implicit val strictHint: ProductHint[SimplePerson] =
          ProductHint[SimplePerson](allowUnknownKeys = false)
        val r = ConfigReader.derive[SimplePerson]
        val result = r.from(value("{ name = Alice, age = 30, extra = ignored }"))
        assert(result.isLeft)
      }
    }

    group("per-type ProductHint override") {

      test("ProductHint switches a single type to snake_case") {
        // Default global config is camelCase → kebab-case. With a per-type hint, this
        // specific case class uses camelCase → snake_case while everything else keeps
        // the global default.
        implicit val withSnake: ProductHint[WithConfigKey] =
          ProductHint[WithConfigKey](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
        val r = ConfigReader.derive[WithConfigKey]
        // @configKey("user_name") wins over the hint, so userName still maps to "user_name".
        // age has no annotation so the hint applies (single-token, snake_case == "age").
        r.from(value("{ user_name = jdoe, age = 30 }")) ==> Right(WithConfigKey("jdoe", 30))
      }

      test("ProductHint useDefaults overrides global setting") {
        implicit val noDefaults: ProductHint[WithDefaults] =
          ProductHint[WithDefaults](useDefaults = false)
        val r = ConfigReader.derive[WithDefaults]
        // Even though the global SConfig has useDefaults = true, the per-type hint
        // overrides to false, so a missing field becomes a failure.
        val result = r.from(value("{ name = Bob }"))
        assert(result.isLeft)
      }
    }

    group("per-type CoproductHint override") {

      test("Field hint overrides discriminator field name") {
        // Per-type hint changes the discriminator key from "type" (global default) to "kind".
        implicit val hint: CoproductHint.Field[Shape] =
          CoproductHint.Field[Shape](fieldName = "kind")
        val r = ConfigReader.derive[Shape]
        r.from(value("{ kind = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
      }

      test("Field hint with custom transformConstructorNames") {
        // Per-type hint switches the subtype-name transformation to PascalCase (no transform).
        implicit val hint: CoproductHint.Field[Shape] =
          CoproductHint.Field[Shape](
            fieldName = "type",
            transformConstructorNames = ConfigFieldMapping(PascalCase, PascalCase)
          )
        val r = ConfigReader.derive[Shape]
        // With PascalCase → PascalCase, the discriminator value matches the original
        // class name (no kebab-case conversion).
        r.from(value("{ type = Circle, radius = 1.5 }")) ==> Right(Circle(1.5))
      }

      test("Wrapped hint switches to single-key wrapping") {
        implicit val hint: CoproductHint.Wrapped[Shape] =
          CoproductHint.Wrapped[Shape]()
        val r = ConfigReader.derive[Shape]
        // Single-key wrapping: `{"circle": {radius = 1.5}}`
        r.from(value("{ circle = { radius = 1.5 } }")) ==> Right(Circle(1.5))
      }
    }

    group("sealed traits / enums") {

      test("discriminator-based encoding (default = PascalCase → kebab-case)") {
        // Default constructor name transform mirrors PureConfig:
        // `Circle` → discriminator value `"circle"`.
        val r = ConfigReader.derive[Shape]
        r.from(value("{ type = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
        r.from(value("{ type = rectangle, width = 2.0, height = 3.0 }")) ==> Right(Rectangle(2.0, 3.0))
      }

      test("case-object enum (as string field of a wrapping object)") {
        val r = ConfigReader.derive[CardinalDirection]
        val rootObj = value("{ direction = north }").asInstanceOf[org.ekrich.config.ConfigObject]
        val innerVal = rootObj.get("direction")
        r.from(innerVal) ==> Right(North)
      }
    }
  }
}
