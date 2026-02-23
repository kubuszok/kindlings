package hearth.kindlings.yamlderivation

import hearth.MacroSuite

final class RoundTripSpec extends MacroSuite {

  group("RoundTrip") {

    group("case classes") {

      test("simple case class") {
        val value = SimplePerson("Alice", 30)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[SimplePerson](node)
        assertEquals(decoded, Right(value))
      }

      test("empty case class") {
        val value = EmptyClass()
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[EmptyClass](node)
        assertEquals(decoded, Right(value))
      }

      test("single field case class") {
        val value = SingleField(42)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[SingleField](node)
        assertEquals(decoded, Right(value))
      }
    }

    group("value classes") {

      test("value class roundtrips") {
        val value = WrappedInt(99)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[WrappedInt](node)
        assertEquals(decoded, Right(value))
      }
    }

    group("sealed traits") {

      test("Circle roundtrip") {
        val value: Shape = Circle(5.0)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        assertEquals(decoded, Right(value))
      }

      test("Rectangle roundtrip") {
        val value: Shape = Rectangle(3.0, 4.0)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        assertEquals(decoded, Right(value))
      }

      test("Dog roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val value: Animal = Dog("Rex", "Labrador")
        val node = KindlingsYamlEncoder.encode[Animal](value)
        val decoded = KindlingsYamlDecoder.decode[Animal](node)
        assertEquals(decoded, Right(value))
      }

      test("Cat roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val value: Animal = Cat("Whiskers", true)
        val node = KindlingsYamlEncoder.encode[Animal](value)
        val decoded = KindlingsYamlDecoder.decode[Animal](node)
        assertEquals(decoded, Right(value))
      }
    }

    group("with configuration") {

      test("custom constructor name transform roundtrip") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val value: Shape = Circle(2.5)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        assertEquals(decoded, Right(value))
      }
    }
  }
}
