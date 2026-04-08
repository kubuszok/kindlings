package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigRenderOptions

final class ConfigWriterSpec extends MacroSuite {

  private def renderConcise(value: org.ekrich.config.ConfigValue): String =
    value.render(ConfigRenderOptions.concise)

  group("ConfigWriter") {

    group("case classes") {

      test("simple") {
        val w = ConfigWriter.derive[SimplePerson]
        val rendered = renderConcise(w.to(SimplePerson("Alice", 30)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"age\":30"))
      }

      test("nested") {
        val w = ConfigWriter.derive[PersonWithAddress]
        val rendered = renderConcise(w.to(PersonWithAddress("Bob", 25, Address("123 Main", "Springfield"))))
        assert(rendered.contains("\"name\":\"Bob\""))
        assert(rendered.contains("\"city\":\"Springfield\""))
      }
    }

    group("collections") {

      test("List field") {
        val w = ConfigWriter.derive[WithList]
        renderConcise(w.to(WithList(List(1, 2, 3)))) ==> """{"items":[1,2,3]}"""
      }

      test("Map field") {
        val w = ConfigWriter.derive[WithMap]
        val rendered = renderConcise(w.to(WithMap(Map("a" -> 1, "b" -> 2))))
        assert(rendered.contains("\"a\":1") && rendered.contains("\"b\":2"))
      }
    }

    group("options") {

      test("None encodes as null field") {
        val w = ConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", None)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"nickname\":null"))
      }

      test("Some encodes as field value") {
        val w = ConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", Some("Allie"))))
        assert(rendered.contains("\"nickname\":\"Allie\""))
      }
    }

    group("annotations") {

      test("@configKey overrides field name") {
        val w = ConfigWriter.derive[WithConfigKey]
        val rendered = renderConcise(w.to(WithConfigKey("jdoe", 30)))
        assert(rendered.contains("\"user_name\":\"jdoe\""))
        assert(!rendered.contains("\"userName\""))
      }

      test("@transientField is omitted") {
        val w = ConfigWriter.derive[WithTransient]
        val rendered = renderConcise(w.to(WithTransient("Alice", Some("ignored"))))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(!rendered.contains("cache"))
      }
    }

    group("sealed traits / enums") {

      test("discriminator-based encoding (default = PascalCase → kebab-case)") {
        val w = ConfigWriter.derive[Shape]
        val circleOut = renderConcise(w.to(Circle(1.5)))
        assert(circleOut.contains("\"type\":\"circle\""))
        assert(circleOut.contains("\"radius\":1.5"))

        val rectOut = renderConcise(w.to(Rectangle(2.0, 3.0)))
        assert(rectOut.contains("\"type\":\"rectangle\""))
      }

      test("case-object enum encodes as string when no discriminator") {
        implicit val cfg: SConfig = SConfig().withWrappedSubtypes
        val w = ConfigWriter.derive[CardinalDirection]
        // PascalCase → kebab-case, so `North` → `"north"`
        renderConcise(w.to(North)) ==> "\"north\""
      }
    }
  }
}
