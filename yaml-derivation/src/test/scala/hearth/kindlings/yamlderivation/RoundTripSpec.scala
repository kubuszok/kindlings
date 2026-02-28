package hearth.kindlings.yamlderivation

import hearth.MacroSuite

final class RoundTripSpec extends MacroSuite {

  group("RoundTrip") {

    group("case classes") {

      test("simple case class") {
        val value = SimplePerson("Alice", 30)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[SimplePerson](node)
        decoded ==> Right(value)
      }

      test("empty case class") {
        val value = EmptyClass()
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[EmptyClass](node)
        decoded ==> Right(value)
      }

      test("single field case class") {
        val value = SingleField(42)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[SingleField](node)
        decoded ==> Right(value)
      }
    }

    group("value classes") {

      test("value class roundtrips") {
        val value = WrappedInt(99)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[WrappedInt](node)
        decoded ==> Right(value)
      }
    }

    group("sealed traits") {

      test("Circle roundtrip") {
        val value: Shape = Circle(5.0)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        decoded ==> Right(value)
      }

      test("Rectangle roundtrip") {
        val value: Shape = Rectangle(3.0, 4.0)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        decoded ==> Right(value)
      }

      test("Dog roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val value: Animal = Dog("Rex", "Labrador")
        val node = KindlingsYamlEncoder.encode[Animal](value)
        val decoded = KindlingsYamlDecoder.decode[Animal](node)
        decoded ==> Right(value)
      }

      test("Cat roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
        val value: Animal = Cat("Whiskers", true)
        val node = KindlingsYamlEncoder.encode[Animal](value)
        val decoded = KindlingsYamlDecoder.decode[Animal](node)
        decoded ==> Right(value)
      }
    }

    group("Scala Enumeration roundtrip") {

      test("Scala Enumeration roundtrip with enumAsStrings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        val value: ScalaColor.Value = ScalaColor.Green
        val node = KindlingsYamlEncoder.encode[ScalaColor.Value](value)
        KindlingsYamlDecoder.decode[ScalaColor.Value](node) ==> Right(value)
      }
    }

    // Java enum roundtrip tests are in RoundTripJvmSpec (src/test/scalajvm)

    group("sets") {

      test("Set roundtrip") {
        val value = Set(1, 2, 3)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[Set[Int]](node)
        decoded ==> Right(value)
      }
    }

    group("tuples") {

      test("(Int, String) roundtrip") {
        val value = (42, "hello")
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[(Int, String)](node) ==> Right(value)
      }

      test("(Int, String, Boolean) roundtrip") {
        val value = (42, "hello", true)
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[(Int, String, Boolean)](node) ==> Right(value)
      }
    }

    group("generic case classes") {

      test("Box[Int] roundtrip") {
        val value = Box(42)
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[Box[Int]](node) ==> Right(value)
      }

      test("Pair[String, Int] roundtrip") {
        val value = Pair("hello", 42)
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[Pair[String, Int]](node) ==> Right(value)
      }
    }

    group("deeply nested") {

      test("PersonFull roundtrip") {
        val value = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[PersonFull](node) ==> Right(value)
      }
    }

    group("type aliases") {

      test("WithAlias roundtrip") {
        val value = WithAlias("Alice", 30)
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[WithAlias](node) ==> Right(value)
      }
    }

    group("with configuration") {

      test("custom constructor name transform roundtrip") {
        implicit val config: YamlConfig =
          YamlConfig(transformConstructorNames = _.toLowerCase)
        val value: Shape = Circle(2.5)
        val node = KindlingsYamlEncoder.encode[Shape](value)
        val decoded = KindlingsYamlDecoder.decode[Shape](node)
        decoded ==> Right(value)
      }

      test("snake_case member name roundtrip") {
        implicit val config: YamlConfig = YamlConfig.default.withSnakeCaseMemberNames
        val value = CamelCasePerson("Alice", "Smith")
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[CamelCasePerson](node)
        decoded ==> Right(value)
      }
    }

    group("KindlingsYamlCodec") {

      test("simple case class roundtrip") {
        val codec = KindlingsYamlCodec.derive[SimplePerson]
        val value = SimplePerson("Bob", 25)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }

      test("sealed trait roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig.default.withDiscriminator("kind")
        val codec = KindlingsYamlCodec.derive[Shape]
        val value: Shape = Circle(3.14)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }

      test("derived codec via implicit") {
        implicit val codec: KindlingsYamlCodec[SimplePerson] = KindlingsYamlCodec.derived[SimplePerson]
        val value = SimplePerson("Charlie", 40)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }

      test("codec with annotations round-trip") {
        val codec = KindlingsYamlCodec.derive[YamlWithFieldName]
        val value = YamlWithFieldName("Alice", 30)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }
    }
  }
}
