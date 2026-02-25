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

    group("Scala Enumeration roundtrip") {

      test("Scala Enumeration roundtrip with enumAsStrings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        val value: ScalaColor.Value = ScalaColor.Green
        val json = KindlingsEncoder.encode[ScalaColor.Value](value)
        KindlingsDecoder.decode[ScalaColor.Value](json) ==> Right(value)
      }
    }

    // Java enum roundtrip tests are in RoundTripJvmSpec (src/test/scalajvm)

    group("sets") {

      test("Set roundtrip") {
        val value = Set(1, 2, 3)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Set[Int]](json) ==> Right(value)
      }
    }

    group("tuples") {

      test("(Int, String) roundtrip") {
        val value = (42, "hello")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[(Int, String)](json) ==> Right(value)
      }

      test("(Int, String, Boolean) roundtrip") {
        val value = (42, "hello", true)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[(Int, String, Boolean)](json) ==> Right(value)
      }
    }

    group("generic case classes") {

      test("Box[Int] roundtrip") {
        val value = Box(42)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Box[Int]](json) ==> Right(value)
      }

      test("Pair[String, Int] roundtrip") {
        val value = Pair("hello", 42)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Pair[String, Int]](json) ==> Right(value)
      }
    }

    group("deeply nested") {

      test("PersonFull roundtrip") {
        val value = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[PersonFull](json) ==> Right(value)
      }
    }

    group("type aliases") {

      test("WithAlias roundtrip") {
        val value = WithAlias("Alice", 30)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithAlias](json) ==> Right(value)
      }
    }

    group("specialized collections") {

      test("mutable.ArrayBuffer round-trip") {
        val value = scala.collection.mutable.ArrayBuffer(1, 2, 3)
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[scala.collection.mutable.ArrayBuffer[Int]](json)
        decoded ==> Right(value)
      }

      test("case class with mutable.ArrayBuffer round-trip") {
        val value = WithMutableBuffer(scala.collection.mutable.ArrayBuffer(10, 20, 30))
        val json = KindlingsEncoder.encode(value)
        val decoded = KindlingsDecoder.decode[WithMutableBuffer](json)
        decoded.map(_.items.toList) ==> Right(value.items.toList)
      }

      test("Vector round-trip") {
        val value = WithVector(Vector("a", "b", "c"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithVector](json) ==> Right(value)
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
