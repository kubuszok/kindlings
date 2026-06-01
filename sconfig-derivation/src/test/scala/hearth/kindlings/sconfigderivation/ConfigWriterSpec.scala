package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigRenderOptions

final class ConfigWriterSpec extends MacroSuite {

  private def renderConcise(value: org.ekrich.config.ConfigValue): String =
    value.render(ConfigRenderOptions.concise)

  group("ConfigWriter") {

    group("case classes") {

      test("simple") {
        val w = ConfigWriter.derived[SimplePerson]
        val rendered = renderConcise(w.to(SimplePerson("Alice", 30)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"age\":30"))
      }

      test("nested") {
        val w = ConfigWriter.derived[PersonWithAddress]
        val rendered = renderConcise(w.to(PersonWithAddress("Bob", 25, Address("123 Main", "Springfield"))))
        assert(rendered.contains("\"name\":\"Bob\""))
        assert(rendered.contains("\"city\":\"Springfield\""))
      }
    }

    group("collections") {

      test("List field") {
        val w = ConfigWriter.derived[WithList]
        renderConcise(w.to(WithList(List(1, 2, 3)))) ==> """{"items":[1,2,3]}"""
      }

      test("Map field") {
        val w = ConfigWriter.derived[WithMap]
        val rendered = renderConcise(w.to(WithMap(Map("a" -> 1, "b" -> 2))))
        assert(rendered.contains("\"a\":1") && rendered.contains("\"b\":2"))
      }
    }

    group("options") {

      test("None encodes as null field") {
        val w = ConfigWriter.derived[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", None)))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"nickname\":null"))
      }

      test("Some encodes as field value") {
        val w = ConfigWriter.derived[WithOption]
        val rendered = renderConcise(w.to(WithOption("Alice", Some("Allie"))))
        assert(rendered.contains("\"nickname\":\"Allie\""))
      }
    }

    group("annotations") {

      test("@configKey overrides field name") {
        val w = ConfigWriter.derived[WithConfigKey]
        val rendered = renderConcise(w.to(WithConfigKey("jdoe", 30)))
        assert(rendered.contains("\"user_name\":\"jdoe\""))
        assert(!rendered.contains("\"userName\""))
      }

      test("@transientField is omitted") {
        val w = ConfigWriter.derived[WithTransient]
        val rendered = renderConcise(w.to(WithTransient("Alice", Some("ignored"))))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(!rendered.contains("cache"))
      }
    }

    group("recursive sealed trait") {

      test("write a Leaf") {
        val w = ConfigWriter.derived[TreeNode]
        val rendered = renderConcise(w.to(Leaf(42)))
        assert(rendered.contains("\"type\":\"leaf\""))
        assert(rendered.contains("\"value\":42"))
      }

      test("write a Branch with Leaf children") {
        val w = ConfigWriter.derived[TreeNode]
        val rendered = renderConcise(w.to(Branch(1, Leaf(2), Leaf(3))))
        assert(rendered.contains("\"type\":\"branch\""))
        assert(rendered.contains("\"value\":1"))
      }

      test("round-trip nested recursive structure") {
        val r = ConfigReader.derived[TreeNode]
        val w = ConfigWriter.derived[TreeNode]
        val tree: TreeNode = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
        val written = w.to(tree)
        r.from(written) ==> Right(tree)
      }
    }

    group("sealed traits / enums") {

      test("discriminator-based encoding (default = PascalCase → kebab-case)") {
        val w = ConfigWriter.derived[Shape]
        val circleOut = renderConcise(w.to(Circle(1.5)))
        assert(circleOut.contains("\"type\":\"circle\""))
        assert(circleOut.contains("\"radius\":1.5"))

        val rectOut = renderConcise(w.to(Rectangle(2.0, 3.0)))
        assert(rectOut.contains("\"type\":\"rectangle\""))
      }

      test("case-object enum encodes as string when no discriminator") {
        implicit val cfg: SConfig = SConfig().withWrappedSubtypes
        val w = ConfigWriter.derived[CardinalDirection]
        // PascalCase → kebab-case, so `North` → `"north"`
        renderConcise(w.to(North)) ==> "\"north\""
      }
    }

    group("combinatorial: wrapper x inner type") {

      test("write CombOuter with all fields populated") {
        val w = ConfigWriter.derived[CombOuter]
        val rendered = renderConcise(
          w.to(
            CombOuter(
              optPrimitive = Some(42),
              optCaseClass = Some(SimplePerson("Alice", 30)),
              optSealedTrait = Some(Circle(1.5)),
              listCaseClass = List(SimplePerson("Bob", 25)),
              mapCaseClass = Map("key1" -> SimplePerson("Carol", 40))
            )
          )
        )
        assert(rendered.contains("\"opt-primitive\":42"))
        assert(rendered.contains("\"name\":\"Alice\""))
        assert(rendered.contains("\"type\":\"circle\""))
        assert(rendered.contains("\"radius\":1.5"))
        assert(rendered.contains("\"name\":\"Bob\""))
        assert(rendered.contains("\"key1\""))
        assert(rendered.contains("\"name\":\"Carol\""))
      }

      test("write CombOuter with None and empty collections") {
        val w = ConfigWriter.derived[CombOuter]
        val rendered = renderConcise(
          w.to(
            CombOuter(
              optPrimitive = None,
              optCaseClass = None,
              optSealedTrait = None,
              listCaseClass = Nil,
              mapCaseClass = Map.empty
            )
          )
        )
        assert(rendered.contains("\"opt-primitive\":null"))
        assert(rendered.contains("\"opt-case-class\":null"))
        assert(rendered.contains("\"opt-sealed-trait\":null"))
        assert(rendered.contains("\"list-case-class\":[]"))
        assert(rendered.contains("\"map-case-class\":{}"))
      }
    }

    group("annotation x type shape") {

      test("@configKey on a sealed trait subtype field (writer)") {
        val w = ConfigWriter.derived[AnnotatedShape]
        val rendered = renderConcise(w.to(AnnotatedCircle(2.5)))
        assert(rendered.contains("\"r\":2.5"))
        assert(!rendered.contains("\"radius\""))
      }

      test("@configKey on multiple fields of a sealed trait subtype (writer)") {
        val w = ConfigWriter.derived[AnnotatedShape]
        val rendered = renderConcise(w.to(AnnotatedRect(4.0, 5.0)))
        assert(rendered.contains("\"w\":4"))
        assert(rendered.contains("\"h\":5"))
        assert(!rendered.contains("\"width\""))
        assert(!rendered.contains("\"height\""))
      }

      test("@transientField on a sealed trait subtype (writer)") {
        val w = ConfigWriter.derived[TransientShape]
        val rendered = renderConcise(w.to(TransientCircle(3.0, "cached")))
        assert(rendered.contains("\"radius\":3"))
        assert(!rendered.contains("memo"))
        assert(!rendered.contains("cached"))
      }

      test("@transientField on a sealed trait subtype with extra fields (writer)") {
        val w = ConfigWriter.derived[TransientShape]
        val rendered = renderConcise(w.to(TransientRect(6.0, 7.0, "cached")))
        assert(rendered.contains("\"width\":6"))
        assert(rendered.contains("\"height\":7"))
        assert(!rendered.contains("memo"))
        assert(!rendered.contains("cached"))
      }
    }
  }
}
