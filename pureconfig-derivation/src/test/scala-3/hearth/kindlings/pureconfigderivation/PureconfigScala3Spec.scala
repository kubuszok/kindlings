package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import hearth.MacroSuite
import pureconfig.ConfigCursor

final class PureconfigScala3Spec extends MacroSuite {

  /** Build a `ConfigCursor` from a HOCON string. */
  private def cursor(hocon: String): ConfigCursor =
    ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  private def renderConcise(value: com.typesafe.config.ConfigValue): String =
    value.render(ConfigRenderOptions.concise())

  group("named tuples (Scala 3.7+)") {

    group("reading") {

      test("simple named tuple") {
        val r = KindlingsConfigReader.derive[(name: String, age: Int)]
        r.from(cursor("{ name = Alice, age = 42 }")) ==> Right(("Alice", 42))
      }

      test("named tuple with nested case class") {
        val r = KindlingsConfigReader.derive[(person: SimplePerson, score: Int)]
        r.from(cursor("{ person = { name = Bob, age = 25 }, score = 100 }")) ==>
          Right((SimplePerson("Bob", 25), 100))
      }

      test("single-element named tuple") {
        val r = KindlingsConfigReader.derive[(field: Int)]
        r.from(cursor("{ field = 3 }")) ==> Right(Tuple1(3))
      }

      test("missing required field returns failure") {
        val r = KindlingsConfigReader.derive[(name: String, age: Int)]
        val result = r.from(cursor("{ name = Alice }"))
        assert(result.isLeft)
      }

      test("named tuple with snake_case member names") {
        implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
        val r = KindlingsConfigReader.derive[(firstName: String, lastName: String)]
        r.from(cursor("{ first_name = Alice, last_name = Smith }")) ==> Right(("Alice", "Smith"))
      }

      test("named tuple with default kebab-case member names") {
        // Default transform is CamelCase -> KebabCase
        val r = KindlingsConfigReader.derive[(firstName: String, lastName: String)]
        r.from(cursor("{ first-name = Alice, last-name = Smith }")) ==> Right(("Alice", "Smith"))
      }
    }

    group("writing") {

      test("simple named tuple") {
        val w = KindlingsConfigWriter.derive[(name: String, age: Int)]
        val rendered = renderConcise(w.to(("Alice", 42)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"age\":42"))
      }

      test("named tuple with nested case class") {
        val w = KindlingsConfigWriter.derive[(person: SimplePerson, score: Int)]
        val rendered = renderConcise(w.to((SimplePerson("Bob", 25), 100)))
        assert(rendered.contains("\"score\":100"))
        assert(rendered.contains("\"person\""))
        assert(rendered.contains("\"name\":\"Bob\""))
      }

      test("single-element named tuple") {
        val w = KindlingsConfigWriter.derive[(field: Int)]
        val rendered = renderConcise(w.to(Tuple1(3)))
        assert(rendered.contains("\"field\":3"))
      }

      test("named tuple with snake_case member names") {
        implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
        val w = KindlingsConfigWriter.derive[(firstName: String, lastName: String)]
        val rendered = renderConcise(w.to(("Alice", "Smith")))
        assert(rendered.contains("\"first_name\":\"Alice\""))
        assert(rendered.contains("\"last_name\":\"Smith\""))
      }

      test("named tuple with default kebab-case member names") {
        val w = KindlingsConfigWriter.derive[(firstName: String, lastName: String)]
        val rendered = renderConcise(w.to(("Alice", "Smith")))
        assert(rendered.contains("\"first-name\":\"Alice\""))
        assert(rendered.contains("\"last-name\":\"Smith\""))
      }
    }

    group("round-trip") {

      test("simple named tuple round-trip") {
        val r = KindlingsConfigReader.derive[(name: String, age: Int)]
        val w = KindlingsConfigWriter.derive[(name: String, age: Int)]
        val original: (name: String, age: Int) = ("Alice", 42)
        val written = w.to(original)
        r.from(ConfigCursor(written, Nil)) ==> Right(original)
      }

      test("single-element named tuple round-trip") {
        val r = KindlingsConfigReader.derive[(field: Int)]
        val w = KindlingsConfigWriter.derive[(field: Int)]
        val original: (field: Int) = Tuple1(3)
        val written = w.to(original)
        r.from(ConfigCursor(written, Nil)) ==> Right(original)
      }

      test("named tuple with nested case class round-trip") {
        val r = KindlingsConfigReader.derive[(person: SimplePerson, score: Int)]
        val w = KindlingsConfigWriter.derive[(person: SimplePerson, score: Int)]
        val original: (person: SimplePerson, score: Int) = (SimplePerson("Bob", 25), 100)
        val written = w.to(original)
        r.from(ConfigCursor(written, Nil)) ==> Right(original)
      }

      test("named tuple with snake_case round-trip") {
        implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
        val r = KindlingsConfigReader.derive[(firstName: String, lastName: String)]
        val w = KindlingsConfigWriter.derive[(firstName: String, lastName: String)]
        val original: (firstName: String, lastName: String) = ("Alice", "Smith")
        val written = w.to(original)
        r.from(ConfigCursor(written, Nil)) ==> Right(original)
      }
    }
  }
}
