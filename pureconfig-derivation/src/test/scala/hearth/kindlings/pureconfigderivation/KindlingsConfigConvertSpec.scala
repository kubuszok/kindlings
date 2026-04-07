package hearth.kindlings.pureconfigderivation

import hearth.MacroSuite
import pureconfig.ConfigCursor

final class KindlingsConfigConvertSpec extends MacroSuite {

  group("KindlingsConfigConvert") {

    test("derive returns a KindlingsConfigConvert subtype") {
      val c: KindlingsConfigConvert[SimplePerson] = KindlingsConfigConvert.derived[SimplePerson]
      // The derived value implements both ConfigReader and ConfigWriter; this also asserts
      // that ConfigConvert (and hence ConfigReader + ConfigWriter) interfaces are satisfied.
      val person = SimplePerson("Alice", 30)
      val written = c.to(person)
      val readBack = c.from(ConfigCursor(written, Nil))
      readBack ==> Right(person)
    }

    test("round-trip case class with nested fields") {
      val c = KindlingsConfigConvert.derived[PersonWithAddress]
      val original = PersonWithAddress("Bob", 25, Address("123 Main", "Springfield"))
      val written = c.to(original)
      c.from(ConfigCursor(written, Nil)) ==> Right(original)
    }

    test("round-trip sealed trait with discriminator") {
      val c = KindlingsConfigConvert.derived[Shape]
      val circle: Shape = Circle(1.5)
      val rect: Shape = Rectangle(2.0, 3.0)
      c.from(ConfigCursor(c.to(circle), Nil)) ==> Right(circle)
      c.from(ConfigCursor(c.to(rect), Nil)) ==> Right(rect)
    }

    test("round-trip with @configKey annotation") {
      val c = KindlingsConfigConvert.derived[WithConfigKey]
      val original = WithConfigKey("jdoe", 30)
      c.from(ConfigCursor(c.to(original), Nil)) ==> Right(original)
    }

    test("round-trip via standalone reader and writer instances") {
      val r = KindlingsConfigReader.derive[TeamWithMembers]
      val w = KindlingsConfigWriter.derive[TeamWithMembers]
      val original = TeamWithMembers("Eagles", List(SimplePerson("A", 1), SimplePerson("B", 2)))
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }

    test("interop: kindlings reader works with pureconfig.ConfigSource") {
      // The whole point of subtyping pureconfig.ConfigReader is that the derived instance
      // can be passed to PureConfig's normal load API.
      implicit val reader: KindlingsConfigReader[SimplePerson] = KindlingsConfigReader.derived[SimplePerson]
      val loaded = pureconfig.ConfigSource.string("name = Alice, age = 30").load[SimplePerson]
      loaded ==> Right(SimplePerson("Alice", 30))
    }
  }
}
