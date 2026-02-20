package hearth.kindlings.circederivation

import hearth.MacroSuite

@scala.annotation.nowarn("msg=is never used")
final class RoundTripSpec extends MacroSuite {

  group("RoundTrip") {

    group("case classes") {

      test("simple case class") {
        val value = SimplePerson("Alice", 30)
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[SimplePerson](json.hcursor)
        assertEquals(decoded, Right(value))
      }

      test("empty case class") {
        val value = EmptyClass()
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[EmptyClass](json.hcursor)
        assertEquals(decoded, Right(value))
      }

      test("single field case class") {
        val value = SingleField(42)
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[SingleField](json.hcursor)
        assertEquals(decoded, Right(value))
      }
    }

    group("value classes") {

      test("value class roundtrips") {
        val value = WrappedInt(99)
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[WrappedInt](json.hcursor)
        assertEquals(decoded, Right(value))
      }
    }

    group("sealed traits") {

      test("Circle roundtrip") {
        val value: Shape = Circle(5.0)
        val json = KindlingsEncoder.encode[Shape](value)
        val decoded = KindlingsDecoder.decode[Shape](json.hcursor)
        assertEquals(decoded, Right(value))
      }

      test("Rectangle roundtrip") {
        val value: Shape = Rectangle(3.0, 4.0)
        val json = KindlingsEncoder.encode[Shape](value)
        val decoded = KindlingsDecoder.decode[Shape](json.hcursor)
        assertEquals(decoded, Right(value))
      }

      test("Dog roundtrip with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val value: Animal = Dog("Rex", "Labrador")
        val json = KindlingsEncoder.encode[Animal](value)
        val decoded = KindlingsDecoder.decode[Animal](json.hcursor)
        assertEquals(decoded, Right(value))
      }

      test("Cat roundtrip with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val value: Animal = Cat("Whiskers", true)
        val json = KindlingsEncoder.encode[Animal](value)
        val decoded = KindlingsDecoder.decode[Animal](json.hcursor)
        assertEquals(decoded, Right(value))
      }
    }

    group("with configuration") {

      test("custom constructor name transform roundtrip") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val value: Shape = Circle(2.5)
        val json = KindlingsEncoder.encode[Shape](value)
        val decoded = KindlingsDecoder.decode[Shape](json.hcursor)
        assertEquals(decoded, Right(value))
      }
    }
  }
}
