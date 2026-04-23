package hearth.kindlings.pureconfigderivation

import com.typesafe.config.ConfigRenderOptions
import hearth.MacroSuite

final class KindlingsConfigWriterSpec extends MacroSuite {

  private def renderConcise(value: com.typesafe.config.ConfigValue): String =
    value.render(ConfigRenderOptions.concise())

  group("KindlingsConfigWriter") {

    group("case classes") {

      test("simple") {
        val w = KindlingsConfigWriter.derive[SimplePerson]
        val out = w.to(SimplePerson("Alice", 30))
        renderConcise(out) ==> """{"age":30,"name":"Alice"}"""
      }

      test("nested") {
        val w = KindlingsConfigWriter.derive[PersonWithAddress]
        val out = w.to(PersonWithAddress("Bob", 25, Address("123 Main", "Springfield")))
        // PureConfig writers emit object keys in alphabetical order via fromMap
        renderConcise(out) ==> """{"address":{"city":"Springfield","street":"123 Main"},"age":25,"name":"Bob"}"""
      }
    }

    group("collections") {

      test("List field") {
        val w = KindlingsConfigWriter.derive[WithList]
        renderConcise(w.to(WithList(List(1, 2, 3)))) ==> """{"items":[1,2,3]}"""
      }

      test("Map field") {
        val w = KindlingsConfigWriter.derive[WithMap]
        // LinkedHashMap preserves insertion order; Map.iterator order is implementation-dependent
        val rendered = renderConcise(w.to(WithMap(Map("a" -> 1, "b" -> 2))))
        assert(rendered.contains("\"a\":1") && rendered.contains("\"b\":2"))
      }
    }

    group("options") {

      test("None encodes as null field") {
        val w = KindlingsConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", None)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"nickname\":null"))
      }

      test("Some encodes as field value") {
        val w = KindlingsConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", Some("Allie"))))
        assert(rendered.contains("\"nickname\":\"Allie\""))
      }
    }

    group("annotations") {

      test("@configKey overrides field name") {
        val w = KindlingsConfigWriter.derive[WithConfigKey]
        val rendered = renderConcise(w.to(WithConfigKey("jdoe", 30)))
        assert(rendered.contains("\"user_name\":\"jdoe\""))
        assert(!rendered.contains("\"userName\""))
      }

      test("@transientField is omitted") {
        val w = KindlingsConfigWriter.derive[WithTransient]
        val rendered = renderConcise(w.to(WithTransient("Alice", Some("ignored"))))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(!rendered.contains("cache"))
      }
    }

    group("sealed traits / enums") {

      test("discriminator-based encoding (default = PascalCase → kebab-case)") {
        val w = KindlingsConfigWriter.derive[Shape]
        val circleOut = renderConcise(w.to(Circle(1.5)))
        assert(circleOut.contains("\"type\":\"circle\""))
        assert(circleOut.contains("\"radius\":1.5"))

        val rectOut = renderConcise(w.to(Rectangle(2.0, 3.0)))
        assert(rectOut.contains("\"type\":\"rectangle\""))
        assert(rectOut.contains("\"width\":2"))
        assert(rectOut.contains("\"height\":3"))
      }

      test("case-object enum encodes as string when no discriminator") {
        implicit val cfg: PureConfig = PureConfig().withWrappedSubtypes
        val w = KindlingsConfigWriter.derive[CardinalDirection]
        // PascalCase → kebab-case, so `North` → `"north"`
        renderConcise(w.to(North)) ==> "\"north\""
      }
    }

    group("value classes") {

      test("value class field write") {
        val w = KindlingsConfigWriter.derive[WithWrappedInt]
        val rendered = renderConcise(w.to(WithWrappedInt(WrappedInt(42))))
        assert(rendered.contains("42"))
      }
    }

    group("Option field omission") {

      test("None Option field is written as null") {
        val w = KindlingsConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", None)))
        assert(rendered.contains("\"name\":\"Alice\""))
      }

      test("Some Option field is written") {
        val w = KindlingsConfigWriter.derive[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", Some("Bob"))))
        assert(rendered.contains("\"nickname\":\"Bob\""))
      }
    }
  }
}
