package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigFactory

final class ConfigCodecSpec extends MacroSuite {

  group("ConfigCodec") {

    test("derive returns a ConfigCodec extending Reader and Writer") {
      val c: ConfigCodec[SimplePerson] = ConfigCodec.derive[SimplePerson]
      // Both reader and writer interfaces are accessible from a single value.
      val person = SimplePerson("Alice", 30)
      val written = c.to(person)
      val readBack = c.from(written)
      readBack ==> Right(person)
    }

    test("round-trip case class with nested fields") {
      val c = ConfigCodec.derive[PersonWithAddress]
      val original = PersonWithAddress("Bob", 25, Address("123 Main", "Springfield"))
      val written = c.to(original)
      c.from(written) ==> Right(original)
    }

    test("round-trip sealed trait with discriminator") {
      val c = ConfigCodec.derive[Shape]
      val circle: Shape = Circle(1.5)
      val rect: Shape = Rectangle(2.0, 3.0)
      c.from(c.to(circle)) ==> Right(circle)
      c.from(c.to(rect)) ==> Right(rect)
    }

    test("round-trip with @configKey annotation") {
      val c = ConfigCodec.derive[WithConfigKey]
      val original = WithConfigKey("jdoe", 30)
      c.from(c.to(original)) ==> Right(original)
    }

    test("round-trip via standalone reader and writer instances") {
      val r = ConfigReader.derive[TeamWithMembers]
      val w = ConfigWriter.derive[TeamWithMembers]
      val original = TeamWithMembers("Eagles", List(SimplePerson("A", 1), SimplePerson("B", 2)))
      r.from(w.to(original)) ==> Right(original)
    }

    test("HOCON round-trip via parseString") {
      val c = ConfigCodec.derive[SimplePerson]
      val original = SimplePerson("Charlie", 40)
      val written = c.to(original)
      val rendered = written.render
      val reparsed = ConfigFactory.parseString(rendered).root
      c.from(reparsed) ==> Right(original)
    }
  }
}
