package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

final class FastShowPrettyScala3Spec extends MacroSuite {

  group("FastShowPretty") {

    group("named tuples (Scala 3.7+)") {

      test("simple named tuple") {
        val nt: (name: String, age: Int) = ("Alice", 42)
        val result = FastShowPretty.render(nt, RenderConfig.Default)
        result ==>
          """(
            |  name = "Alice",
            |  age = 42
            |)""".stripMargin
      }

      test("compact (no indent)") {
        val nt: (name: String, age: Int) = ("Alice", 42)
        val result = FastShowPretty.render(nt, RenderConfig.Compact)
        result ==>
          """(
            |name = "Alice",
            |age = 42
            |)""".stripMargin
      }

      test("nested named tuple with case class") {
        val nt: (person: Person, score: Int) = (Person("Bob", 25), 100)
        val result = FastShowPretty.render(nt, RenderConfig.Default)
        result ==>
          """(
            |  person = Person(
            |    name = "Bob",
            |    age = 25
            |  ),
            |  score = 100
            |)""".stripMargin
      }
    }

    group("Scala 3 enums") {

      test("parameterized enum") {
        val result = FastShowPretty.render[Fruit](Fruit.Apple(1.5), RenderConfig.Default)
        result ==>
          """(Apple(
            |    weight = 1.5d
            |  )): Fruit""".stripMargin
      }
    }
  }
}
