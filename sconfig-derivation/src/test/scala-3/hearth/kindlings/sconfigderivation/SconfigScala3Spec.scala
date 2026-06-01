package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.{ConfigFactory, ConfigRenderOptions, ConfigValue}

final class SconfigScala3Spec extends MacroSuite {

  /** Build a `ConfigValue` from a HOCON string. */
  private def value(hocon: String): ConfigValue =
    ConfigFactory.parseString(hocon).root

  private def renderConcise(value: ConfigValue): String =
    value.render(ConfigRenderOptions.concise)

  group("named tuples (Scala 3.7+)") {

    group("ConfigReader") {

      test("simple named tuple") {
        val r = ConfigReader.derived[(name: String, age: Int)]
        r.from(value("{ name = Alice, age = 42 }")) ==> Right(("Alice", 42))
      }

      test("named tuple with nested case class") {
        val r = ConfigReader.derived[(person: SimplePerson, score: Int)]
        r.from(value("{ person = { name = Bob, age = 25 }, score = 100 }")) ==>
          Right((SimplePerson("Bob", 25), 100))
      }

      test("single-element named tuple") {
        val r = ConfigReader.derived[(field: Int)]
        r.from(value("{ field = 7 }")) ==> Right(Tuple1(7))
      }

      test("missing required field returns failure") {
        val r = ConfigReader.derived[(name: String, age: Int)]
        val result = r.from(value("{ name = Alice }"))
        assert(result.isLeft)
      }
    }

    group("ConfigWriter") {

      test("simple named tuple") {
        val w = ConfigWriter.derived[(name: String, age: Int)]
        val rendered = renderConcise(w.to(("Alice", 42)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"age\":42"))
      }

      test("named tuple with nested case class") {
        val w = ConfigWriter.derived[(person: SimplePerson, score: Int)]
        val rendered = renderConcise(w.to((SimplePerson("Bob", 25), 100)))
        assert(rendered.contains("\"score\":100"))
        assert(rendered.contains("\"name\":\"Bob\""))
        assert(rendered.contains("\"age\":25"))
      }

      test("single-element named tuple") {
        val w = ConfigWriter.derived[(field: Int)]
        val rendered = renderConcise(w.to(Tuple1(7)))
        rendered ==> """{"field":7}"""
      }
    }

    group("round-trip") {

      test("simple named tuple round-trip") {
        val r = ConfigReader.derived[(name: String, age: Int)]
        val w = ConfigWriter.derived[(name: String, age: Int)]
        val original: (name: String, age: Int) = ("Alice", 42)
        r.from(w.to(original)) ==> Right(original)
      }

      test("named tuple with nested case class round-trip") {
        val r = ConfigReader.derived[(person: SimplePerson, score: Int)]
        val w = ConfigWriter.derived[(person: SimplePerson, score: Int)]
        val original: (person: SimplePerson, score: Int) = (SimplePerson("Bob", 25), 100)
        r.from(w.to(original)) ==> Right(original)
      }

      test("single-element named tuple round-trip") {
        val r = ConfigReader.derived[(field: Int)]
        val w = ConfigWriter.derived[(field: Int)]
        val original: (field: Int) = Tuple1(7)
        r.from(w.to(original)) ==> Right(original)
      }
    }
  }
}
