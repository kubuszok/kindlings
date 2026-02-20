package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

case class Person(name: String, age: Int)
case class Empty()
case class Single(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class Team(name: String, members: List[Person])
case class Tree(value: Int, children: List[Tree])
final case class ExampleValueClass(a: Int) extends AnyVal
class NotAHandledType

final class FastShowPrettySpec extends MacroSuite {

  group("FastShowPretty") {

    group("render") {

      group("primitive types") {

        test("Boolean true") {
          val result = FastShowPretty.render(true, RenderConfig.Default)
          assertEquals(result, "true")
        }

        test("Boolean false") {
          val result = FastShowPretty.render(false, RenderConfig.Default)
          assertEquals(result, "false")
        }

        test("Byte") {
          val result = FastShowPretty.render(42.toByte, RenderConfig.Default)
          assertEquals(result, "42.toByte")
        }

        test("Short") {
          val result = FastShowPretty.render(42.toShort, RenderConfig.Default)
          assertEquals(result, "42.toShort")
        }

        test("Int") {
          val result = FastShowPretty.render(42, RenderConfig.Default)
          assertEquals(result, "42")
        }

        test("Long") {
          val result = FastShowPretty.render(42L, RenderConfig.Default)
          assertEquals(result, "42L")
        }

        test("Float") {
          val result = FastShowPretty.render(42.5f, RenderConfig.Default)
          assertEquals(result, "42.5f")
        }

        test("Double") {
          val result = FastShowPretty.render(42.5, RenderConfig.Default)
          assertEquals(result, "42.5d")
        }

        test("Char") {
          val result = FastShowPretty.render('a', RenderConfig.Default)
          assertEquals(result, "'a'")
        }

        test("String") {
          val result = FastShowPretty.render("hello", RenderConfig.Default)
          assertEquals(result, "\"hello\"")
        }

        test("String with quotes") {
          val result = FastShowPretty.render("say \"hello\"", RenderConfig.Default)
          assertEquals(result, "\"say \\\"hello\\\"\"")
        }

        test("String with newlines") {
          val result = FastShowPretty.render("line1\nline2", RenderConfig.Default)
          assertEquals(result, "\"line1\\nline2\"")
        }
      }

      group("value types") {

        test("ExampleValueClass") {
          val result = FastShowPretty.render(ExampleValueClass(42), RenderConfig.Default)
          assertEquals(result, "42")
        }

        test("ExampleValueClass with derived") {
          import FastShowPretty.derived
          val instance = implicitly[FastShowPretty[ExampleValueClass]]
          val result = instance.render(new StringBuilder, RenderConfig.Default, 0)(ExampleValueClass(99)).toString
          assertEquals(result, "99")
        }
      }

      group("case classes") {

        test("compact (no indent)") {
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Compact)
          assertEquals(
            result,
            """Person(
              |name = "Alice",
              |age = 30
              |)""".stripMargin
          )
        }

        test("tabs") {
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Tabs)
          assertEquals(
            result,
            s"""Person(
               |\tname = "Alice",
               |\tage = 30
               |)""".stripMargin
          )
        }

        test("four spaces") {
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.FourSpaces)
          assertEquals(
            result,
            """Person(
              |    name = "Alice",
              |    age = 30
              |)""".stripMargin
          )
        }

        test("nested with tabs") {
          val address = Address("123 Main St", "New York")
          val person = PersonWithAddress("Bob", 25, address)
          val result = FastShowPretty.render(person, RenderConfig.Tabs)
          assertEquals(
            result,
            s"""PersonWithAddress(
               |\tname = "Bob",
               |\tage = 25,
               |\taddress = Address(
               |\t\tstreet = "123 Main St",
               |\t\tcity = "New York"
               |\t)
               |)""".stripMargin
          )
        }

        test("case class with collection field") {
          val result = FastShowPretty.render(
            Team("Engineering", List(Person("Alice", 30), Person("Bob", 25))),
            RenderConfig.Default
          )
          assertEquals(
            result,
            """Team(
              |  name = "Engineering",
              |  members = List(
              |    Person(
              |      name = "Alice",
              |      age = 30
              |    ),
              |    Person(
              |      name = "Bob",
              |      age = 25
              |    )
              |  )
              |)""".stripMargin
          )
        }
      }

      group("collections") {

        test("List of Ints") {
          val result = FastShowPretty.render(List(1, 2, 3), RenderConfig.Default)
          assertEquals(
            result,
            """List(
              |  1,
              |  2,
              |  3
              |)""".stripMargin
          )
        }

        test("empty List") {
          val result = FastShowPretty.render(List.empty[Int], RenderConfig.Default)
          assertEquals(result, "List()")
        }

        test("Vector of Strings") {
          val result = FastShowPretty.render(Vector("a", "b", "c"), RenderConfig.Default)
          assertEquals(
            result,
            """Vector(
              |  "a",
              |  "b",
              |  "c"
              |)""".stripMargin
          )
        }

        test("Set of Ints") {
          val result = FastShowPretty.render(Set(1), RenderConfig.Default)
          assertEquals(
            result,
            """Set(
              |  1
              |)""".stripMargin
          )
        }

        test("List of case classes") {
          val result = FastShowPretty.render(
            List(PersonWithAddress("Bob", 25, Address("123 Main St", "New York"))),
            RenderConfig.Default
          )
          assertEquals(
            result,
            """List(
              |  PersonWithAddress(
              |    name = "Bob",
              |    age = 25,
              |    address = Address(
              |      street = "123 Main St",
              |      city = "New York"
              |    )
              |  )
              |)""".stripMargin
          )
        }
      }

      group("maps") {

        test("Map of String to Int") {
          val result = FastShowPretty.render(Map("a" -> 1), RenderConfig.Default)
          assertEquals(
            result,
            """Map(
              |  ("a", 1)
              |)""".stripMargin
          )
        }

        test("empty Map") {
          val result = FastShowPretty.render(Map.empty[String, Int], RenderConfig.Default)
          assertEquals(result, "Map()")
        }

        test("Map with multiple entries") {
          val result =
            FastShowPretty.render(scala.collection.immutable.ListMap("x" -> 10, "y" -> 20), RenderConfig.Default)
          assertEquals(
            result,
            """ListMap(
              |  ("x", 10),
              |  ("y", 20)
              |)""".stripMargin
          )
        }
      }

      group("custom implicit instances") {

        test("uses custom implicit instance") {
          implicit val customIntInstance: FastShowPretty[Int] = new FastShowPretty[Int] {
            def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: Int): StringBuilder =
              sb.append("custom(").append(value).append(")")
          }

          val result = FastShowPretty.render(42, RenderConfig.Default)
          assertEquals(result, "custom(42)")
        }
      }

      group("edge cases") {

        test("zero values") {
          assertEquals(FastShowPretty.render(0, RenderConfig.Default), "0")
          assertEquals(FastShowPretty.render(0L, RenderConfig.Default), "0L")
          assertEquals(FastShowPretty.render(0.0f, RenderConfig.Default), "0.0f")
          assertEquals(FastShowPretty.render(0.0, RenderConfig.Default), "0.0d")
        }

        test("negative numbers") {
          assertEquals(FastShowPretty.render(-42, RenderConfig.Default), "-42")
          assertEquals(FastShowPretty.render(-42L, RenderConfig.Default), "-42L")
        }

        test("empty string") {
          assertEquals(FastShowPretty.render("", RenderConfig.Default), "\"\"")
        }

        test("unicode characters") {
          val result = FastShowPretty.render("Hello 世界", RenderConfig.Default)
          assertEquals(result, "\"Hello 世界\"")
        }

        test("special characters in string") {
          val result = FastShowPretty.render("tab\tquote\"newline\n", RenderConfig.Default)
          assertEquals(result, "\"tab\\tquote\\\"newline\\n\"")
        }

        test("backslash in string") {
          val result = FastShowPretty.render("path\\to\\file", RenderConfig.Default)
          assertEquals(result, "\"path\\\\to\\\\file\"")
        }
      }
    }

    group("derived") {

      test("Int instance") {
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
        assertEquals(result, "42")
      }

      test("case class instance") {
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 0)(Person("Alice", 30)).toString
        assertEquals(
          result,
          """Person(
            |  name = "Alice",
            |  age = 30
            |)""".stripMargin
        )
      }

      test("instance reuse StringBuilder") {
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder("prefix: ")
        val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
        assertEquals(result, "prefix: 42")
      }

      test("instance with custom config") {
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Tabs, 0)(Person("Alice", 30)).toString
        assertEquals(
          result,
          s"""Person(
             |\tname = "Alice",
             |\tage = 30
             |)""".stripMargin
        )
      }

      test("instance with start level") {
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 1)(Person("Alice", 30)).toString
        assertEquals(
          result,
          """Person(
            |    name = "Alice",
            |    age = 30
            |  )""".stripMargin
        )
      }
    }

    group("compile-time") {

      test("render compiles for supported types") {
        val _: String = FastShowPretty.render(42, RenderConfig.Default)
        val _: String = FastShowPretty.render("test", RenderConfig.Default)
        val _: String = FastShowPretty.render(Person("Alice", 30), RenderConfig.Default)
        assert(true)
      }

      test("derived compiles for supported types") {
        val _: FastShowPretty[Int] = implicitly[FastShowPretty[Int]]
        val _: FastShowPretty[Person] = implicitly[FastShowPretty[Person]]
        assert(true)
      }
    }

    group("StringBuilder reuse") {

      test("multiple appends") {
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder("start: ")
        val _ = instance.render(sb, RenderConfig.Default, 0)(1)
        sb.append(", ")
        val _ = instance.render(sb, RenderConfig.Default, 0)(2)
        sb.append(", ")
        val _ = instance.render(sb, RenderConfig.Default, 0)(3)
        assertEquals(sb.toString, "start: 1, 2, 3")
      }
    }

    group("compile-time errors") {

      test("unhandled type produces error message") {
        compileErrors(
          """
          import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}
          FastShowPretty.render(new hearth.kindlings.fastshowpretty.NotAHandledType, RenderConfig.Default)
          """
        ).check(
          "Macro derivation failed with the following errors:",
          "  - The type hearth.kindlings.fastshowpretty.NotAHandledType was not handled by any derivation rule:",
          "    - The rule use implicit when available was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType does not have an implicit FastShowPretty instance: No implicit value of type hearth.kindlings.fastshowpretty.FastShowPretty[hearth.kindlings.fastshowpretty.NotAHandledType] found",
          "    - The rule use built-in support when handling primitive types was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a built-in type",
          "    - The rule handle as value type when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a value type",
          "    - The rule handle as map when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a map",
          "    - The rule handle as collection when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a collection",
          "    - The rule handle as named tuple when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a named tuple",
          "    - The rule handle as case class when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be a case class",
          "    - The rule handle as enum when possible was not applicable, for the following reasons: The type hearth.kindlings.fastshowpretty.NotAHandledType is not considered to be an enum"
        )
      }
    }

    group("recursive data structures") {

      test("leaf node") {
        val result = FastShowPretty.render(Tree(1, List.empty[Tree]), RenderConfig.Default)
        assertEquals(
          result,
          """Tree(
            |  value = 1,
            |  children = List()
            |)""".stripMargin
        )
      }

      test("nested tree") {
        val tree = Tree(1, List(Tree(2, List.empty[Tree]), Tree(3, List(Tree(4, List.empty[Tree])))))
        val result = FastShowPretty.render(tree, RenderConfig.Default)
        assertEquals(
          result,
          """Tree(
            |  value = 1,
            |  children = List(
            |    Tree(
            |      value = 2,
            |      children = List()
            |    ),
            |    Tree(
            |      value = 3,
            |      children = List(
            |        Tree(
            |          value = 4,
            |          children = List()
            |        )
            |      )
            |    )
            |  )
            |)""".stripMargin
        )
      }
    }
  }
}
