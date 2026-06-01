package hearth.kindlings.pureconfigderivation

import com.typesafe.config.ConfigFactory
import hearth.MacroSuite
import pureconfig.ConfigCursor

final class KindlingsConfigReaderSpec extends MacroSuite {

  /** Build a `ConfigCursor` from a HOCON string. */
  private def cursor(hocon: String): ConfigCursor =
    ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  group("KindlingsConfigReader") {

    group("primitives via case-class fields") {

      test("Int field") {
        val r = KindlingsConfigReader.derived[SingleField]
        r.from(cursor("{ value = 42 }")) ==> Right(SingleField(42))
      }

      test("String + Int + Boolean") {
        val r = KindlingsConfigReader.derived[WithDefaults]
        r.from(cursor("{ name = Alice, age = 30, active = true }")) ==> Right(WithDefaults("Alice", 30, true))
      }
    }

    group("case classes") {

      test("simple") {
        val r = KindlingsConfigReader.derived[SimplePerson]
        r.from(cursor("{ name = Alice, age = 30 }")) ==> Right(SimplePerson("Alice", 30))
      }

      test("empty") {
        val r = KindlingsConfigReader.derived[EmptyClass]
        r.from(cursor("{}")) ==> Right(EmptyClass())
      }

      test("nested") {
        val r = KindlingsConfigReader.derived[PersonWithAddress]
        r.from(
          cursor("{ name = Bob, age = 25, address = { street = \"123 Main\", city = Springfield } }")
        ) ==> Right(PersonWithAddress("Bob", 25, Address("123 Main", "Springfield")))
      }

      test("missing required field returns failure") {
        val r = KindlingsConfigReader.derived[SimplePerson]
        val result = r.from(cursor("{ name = Alice }"))
        assert(result.isLeft)
      }
    }

    group("collections") {

      test("List field of primitives") {
        val r = KindlingsConfigReader.derived[WithList]
        r.from(cursor("{ items = [1, 2, 3] }")) ==> Right(WithList(List(1, 2, 3)))
      }

      test("List of case classes") {
        val r = KindlingsConfigReader.derived[TeamWithMembers]
        r.from(
          cursor("{ name = Eagles, members = [ { name = A, age = 1 }, { name = B, age = 2 } ] }")
        ) ==> Right(TeamWithMembers("Eagles", List(SimplePerson("A", 1), SimplePerson("B", 2))))
      }
    }

    group("maps") {

      test("Map[String, Int] field") {
        val r = KindlingsConfigReader.derived[WithMap]
        r.from(cursor("{ scores = { a = 1, b = 2 } }")) ==> Right(WithMap(Map("a" -> 1, "b" -> 2)))
      }

      test("empty Map field") {
        val r = KindlingsConfigReader.derived[WithMap]
        r.from(cursor("{ scores = {} }")) ==> Right(WithMap(Map.empty))
      }

      test("nested Map[String, Map[String, Int]]") {
        val r = KindlingsConfigReader.derived[WithNestedMap]
        r.from(cursor("{ data = { group1 = { a = 1, b = 2 }, group2 = { c = 3 } } }")) ==>
          Right(WithNestedMap(Map("group1" -> Map("a" -> 1, "b" -> 2), "group2" -> Map("c" -> 3))))
      }

      test("Map[String, CaseClass] with derived inner type") {
        val r = KindlingsConfigReader.derived[WithMapOfCaseClass]
        r.from(cursor("{ data = { alice = { name = Alice, age = 30 }, bob = { name = Bob, age = 25 } } }")) ==>
          Right(WithMapOfCaseClass(Map("alice" -> SimplePerson("Alice", 30), "bob" -> SimplePerson("Bob", 25))))
      }
    }

    group("options") {

      test("None from missing key") {
        val r = KindlingsConfigReader.derived[WithOption]
        r.from(cursor("{ name = Alice }")) ==> Right(WithOption("Alice", None))
      }

      test("Some from present key") {
        val r = KindlingsConfigReader.derived[WithOption]
        r.from(cursor("{ name = Alice, nickname = Allie }")) ==> Right(WithOption("Alice", Some("Allie")))
      }
    }

    group("annotations") {

      test("@configKey overrides field name") {
        val r = KindlingsConfigReader.derived[WithConfigKey]
        r.from(cursor("{ user_name = jdoe, age = 30 }")) ==> Right(WithConfigKey("jdoe", 30))
      }

      test("@transientField uses default") {
        val r = KindlingsConfigReader.derived[WithTransient]
        r.from(cursor("{ name = Alice }")) ==> Right(WithTransient("Alice", None))
      }
    }

    group("defaults via useDefaults") {

      test("missing field uses default") {
        implicit val cfg: PureConfig = PureConfig().withUseDefaults
        val r = KindlingsConfigReader.derived[WithDefaults]
        r.from(cursor("{ name = Bob }")) ==> Right(WithDefaults("Bob"))
      }
    }

    group("strict mode (allowUnknownKeys = false)") {

      test("default allowUnknownKeys = true accepts extra keys") {
        val r = KindlingsConfigReader.derived[SimplePerson]
        // Extra `extra` field is silently ignored under default config (matching upstream).
        r.from(cursor("{ name = Alice, age = 30, extra = ignored }")) ==> Right(SimplePerson("Alice", 30))
      }

      test("global PureConfig.withStrictDecoding rejects extra keys") {
        implicit val cfg: PureConfig = PureConfig().withStrictDecoding
        val r = KindlingsConfigReader.derived[SimplePerson]
        val result = r.from(cursor("{ name = Alice, age = 30, extra = ignored }"))
        assert(result.isLeft)
      }

      test("per-type ProductHint allowUnknownKeys = false rejects extras") {
        // Even with global config permissive, the per-type hint enforces strict mode.
        implicit val strictHint: KindlingsProductHint[SimplePerson] =
          KindlingsProductHint[SimplePerson](allowUnknownKeys = false)
        val r = KindlingsConfigReader.derived[SimplePerson]
        val result = r.from(cursor("{ name = Alice, age = 30, extra = ignored }"))
        assert(result.isLeft)
      }
    }

    group("per-type ProductHint override") {

      test("ProductHint switches a single type to snake_case") {
        // Default global config is camelCase → kebab-case. With a per-type hint, this
        // specific case class uses camelCase → snake_case while everything else keeps
        // the global default.
        import pureconfig.{CamelCase, ConfigFieldMapping, SnakeCase}
        implicit val withSnake: KindlingsProductHint[WithConfigKey] =
          KindlingsProductHint[WithConfigKey](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
        val r = KindlingsConfigReader.derived[WithConfigKey]
        // @configKey("user_name") wins over the hint, so userName still maps to "user_name".
        // age has no annotation so the hint applies (single-token, snake_case == "age").
        r.from(cursor("{ user_name = jdoe, age = 30 }")) ==> Right(WithConfigKey("jdoe", 30))
      }

      test("ProductHint useDefaults overrides global setting") {
        implicit val noDefaults: KindlingsProductHint[WithDefaults] =
          KindlingsProductHint[WithDefaults](useDefaults = false)
        val r = KindlingsConfigReader.derived[WithDefaults]
        // Even though the global PureConfig has useDefaults = true, the per-type hint
        // overrides to false, so a missing field becomes a failure.
        val result = r.from(cursor("{ name = Bob }"))
        assert(result.isLeft)
      }
    }

    group("per-type CoproductHint override") {

      test("Field hint overrides discriminator field name") {
        // Per-type hint changes the discriminator key from "type" (global default) to "kind".
        implicit val hint: KindlingsCoproductHint.Field[Shape] =
          KindlingsCoproductHint.Field[Shape](fieldName = "kind")
        val r = KindlingsConfigReader.derived[Shape]
        r.from(cursor("{ kind = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
      }

      test("Field hint with custom transformConstructorNames") {
        // Per-type hint switches the subtype-name transformation to PascalCase (no transform).
        import pureconfig.{ConfigFieldMapping, PascalCase}
        implicit val hint: KindlingsCoproductHint.Field[Shape] =
          KindlingsCoproductHint.Field[Shape](
            fieldName = "type",
            transformConstructorNames = ConfigFieldMapping(PascalCase, PascalCase)
          )
        val r = KindlingsConfigReader.derived[Shape]
        // With PascalCase → PascalCase, the discriminator value matches the original
        // class name (no kebab-case conversion).
        r.from(cursor("{ type = Circle, radius = 1.5 }")) ==> Right(Circle(1.5))
      }

      test("Wrapped hint switches to single-key wrapping") {
        implicit val hint: KindlingsCoproductHint.Wrapped[Shape] =
          KindlingsCoproductHint.Wrapped[Shape]()
        val r = KindlingsConfigReader.derived[Shape]
        // Single-key wrapping: `{"circle": {radius = 1.5}}`
        r.from(cursor("{ circle = { radius = 1.5 } }")) ==> Right(Circle(1.5))
      }
    }

    group("sealed traits / enums") {

      test("discriminator-based encoding (default = PascalCase → kebab-case)") {
        // Default constructor name transform is PascalCase → kebab-case (matching upstream
        // PureConfig's FieldCoproductHint.defaultMapping). So `Circle` encodes as `"circle"`.
        val r = KindlingsConfigReader.derived[Shape]
        r.from(cursor("{ type = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
        r.from(cursor("{ type = rectangle, width = 2.0, height = 3.0 }")) ==> Right(Rectangle(2.0, 3.0))
      }

      test("case-object enum (as string field of a wrapping object)") {
        // HOCON top-level must be an object — wrap the bare scalar in a single-key object
        // and read the inner cursor.
        val r = KindlingsConfigReader.derived[CardinalDirection]
        val rootCur = cursor("{ direction = north }")
        val innerCur = rootCur.asObjectCursor.flatMap(_.atKey("direction"))
        innerCur.flatMap(r.from) ==> Right(North)
      }
    }

    group("value classes") {

      test("value class field read") {
        val r = KindlingsConfigReader.derived[WithWrappedInt]
        r.from(cursor("{ value = 42 }")) ==> Right(WithWrappedInt(WrappedInt(42)))
      }
    }

    group("recursive product types") {

      test("recursive via Option") {
        val r = KindlingsConfigReader.derived[LinkedNode]
        r.from(cursor("{ value = a, next = { value = b } }")) ==>
          Right(LinkedNode("a", Some(LinkedNode("b", None))))
      }

      test("recursive tree") {
        val r = KindlingsConfigReader.derived[RecursiveTree]
        r.from(cursor("{ value = 1, children = [{ value = 2, children = [] }] }")) ==>
          Right(RecursiveTree(1, List(RecursiveTree(2, Nil))))
      }
    }

    group("recursive sealed trait") {

      test("read a Leaf") {
        val r = KindlingsConfigReader.derived[TreeNode]
        r.from(cursor("{ type = leaf, value = 42 }")) ==> Right(Leaf(42))
      }

      test("read a Branch with Leaf children") {
        val r = KindlingsConfigReader.derived[TreeNode]
        r.from(cursor("""{
          type = branch,
          value = 1,
          left = { type = leaf, value = 2 },
          right = { type = leaf, value = 3 }
        }""")) ==> Right(Branch(1, Leaf(2), Leaf(3)))
      }

      test("read a nested recursive structure") {
        val r = KindlingsConfigReader.derived[TreeNode]
        r.from(cursor("""{
          type = branch,
          value = 1,
          left = {
            type = branch,
            value = 2,
            left = { type = leaf, value = 3 },
            right = { type = leaf, value = 4 }
          },
          right = { type = leaf, value = 5 }
        }""")) ==> Right(Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5)))
      }
    }

    group("complex defaults") {

      test("missing fields with List/Map defaults") {
        val r = KindlingsConfigReader.derived[WithComplexDefaults]
        r.from(cursor("{ name = Alice }")) ==> Right(WithComplexDefaults("Alice"))
      }

      test("provided fields override defaults") {
        val r = KindlingsConfigReader.derived[WithComplexDefaults]
        r.from(cursor("{ name = Alice, tags = [a, b] }")) ==>
          Right(WithComplexDefaults("Alice", List("a", "b")))
      }
    }

    group("Option field handling") {

      test("missing Option field reads as None") {
        val r = KindlingsConfigReader.derived[WithOption]
        r.from(cursor("{ name = Alice }")) ==> Right(WithOption("Alice", None))
      }

      test("present Option field reads as Some") {
        val r = KindlingsConfigReader.derived[WithOption]
        r.from(cursor("{ name = Alice, nickname = Bob }")) ==> Right(WithOption("Alice", Some("Bob")))
      }

      test("Option with default: missing uses default None") {
        val r = KindlingsConfigReader.derived[WithOptionDefault]
        r.from(cursor("{ name = Alice }")) ==> Right(WithOptionDefault("Alice", None))
      }
    }

    group("failure accumulation") {

      test("multiple errors reported for wrong types") {
        val r = KindlingsConfigReader.derived[SimplePerson]
        val result = r.from(cursor("{ name = 42, age = hello }"))
        assert(result.isLeft)
      }

      test("missing required field reports error") {
        val r = KindlingsConfigReader.derived[SimplePerson]
        val result = r.from(cursor("{ name = Alice }"))
        assert(result.isLeft)
      }
    }

    group("enum error messages") {

      test("unknown enum value produces error") {
        val r = KindlingsConfigReader.derived[CardinalDirection]
        val rootCur = cursor("{ direction = northwest }")
        val innerCur = rootCur.asObjectCursor.flatMap(_.atKey("direction"))
        val result = innerCur.flatMap(r.from)
        assert(result.isLeft)
      }
    }
  }
}
