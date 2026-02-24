package hearth.kindlings.circederivation

import hearth.MacroSuite

final class RoundTripSpec extends MacroSuite {

  group("RoundTrip") {

    group("case classes") {

      test("simple case class") {
        val value = SimplePerson("Alice", 30)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[SimplePerson](json) ==> Right(value)
      }

      test("empty case class") {
        val value = EmptyClass()
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[EmptyClass](json) ==> Right(value)
      }

      test("single field case class") {
        val value = SingleField(42)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[SingleField](json) ==> Right(value)
      }
    }

    group("value classes") {

      test("value class roundtrips") {
        val value = WrappedInt(99)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WrappedInt](json) ==> Right(value)
      }
    }

    group("sealed traits") {

      test("Circle roundtrip") {
        val value: Shape = Circle(5.0)
        val json = KindlingsEncoder.encode[Shape](value)
        KindlingsDecoder.decode[Shape](json) ==> Right(value)
      }

      test("Rectangle roundtrip") {
        val value: Shape = Rectangle(3.0, 4.0)
        val json = KindlingsEncoder.encode[Shape](value)
        KindlingsDecoder.decode[Shape](json) ==> Right(value)
      }

      test("Dog roundtrip with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val value: Animal = Dog("Rex", "Labrador")
        val json = KindlingsEncoder.encode[Animal](value)
        KindlingsDecoder.decode[Animal](json) ==> Right(value)
      }

      test("Cat roundtrip with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val value: Animal = Cat("Whiskers", true)
        val json = KindlingsEncoder.encode[Animal](value)
        KindlingsDecoder.decode[Animal](json) ==> Right(value)
      }
    }

    group("sets") {

      test("Set roundtrip") {
        val value = Set(1, 2, 3)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Set[Int]](json) ==> Right(value)
      }
    }

    group("with configuration") {

      test("custom constructor name transform roundtrip") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val value: Shape = Circle(2.5)
        val json = KindlingsEncoder.encode[Shape](value)
        KindlingsDecoder.decode[Shape](json) ==> Right(value)
      }

      test("snake_case member name roundtrip") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val value = CamelCaseFields("Alice", "Smith")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(value)
      }
    }
  }
}
