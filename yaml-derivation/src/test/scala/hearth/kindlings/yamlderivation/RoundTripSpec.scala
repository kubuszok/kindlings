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

    group("maps") {

      test("Map[String, Int] roundtrip") {
        val value = Map("a" -> 1, "b" -> 2)
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[Map[String, Int]](node) ==> Right(value)
      }

      test("empty map roundtrip") {
        val value = Map.empty[String, Int]
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[Map[String, Int]](node) ==> Right(value)
      }

      test("case class with Map field roundtrip") {
        val value = WithMapField(Map("x" -> 10, "y" -> 20))
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[WithMapField](node) ==> Right(value)
      }

      test("nested Map[String, Map[String, Int]] roundtrip") {
        val value = WithNestedMap(Map("group1" -> Map("a" -> 1, "b" -> 2), "group2" -> Map("c" -> 3)))
        val node = KindlingsYamlEncoder.encode(value)
        KindlingsYamlDecoder.decode[WithNestedMap](node) ==> Right(value)
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
        val codec = KindlingsYamlCodec.derived[SimplePerson]
        val value = SimplePerson("Bob", 25)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }

      test("sealed trait roundtrip with discriminator") {
        implicit val config: YamlConfig = YamlConfig.default.withDiscriminator("kind")
        val codec = KindlingsYamlCodec.derived[Shape]
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
        val codec = KindlingsYamlCodec.derived[YamlWithFieldName]
        val value = YamlWithFieldName("Alice", 30)
        val node = codec.asNode(value)
        val decoded = codec.construct(node)
        decoded ==> Right(value)
      }
    }

    group("mixed sealed traits") {

      test("mixed sealed trait case class round-trip") {
        val value: MixedPet = Budgie("Polly", true)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[MixedPet](node)
        decoded ==> Right(value)
      }

      test("mixed sealed trait case object round-trip") {
        val value: MixedPet = Goldfish
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[MixedPet](node)
        decoded ==> Right(value)
      }
    }

    group("multi-level sealed hierarchy") {

      test("leaf through intermediate round-trip") {
        val value: YamlVehicle = YamlCar("Honda")
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[YamlVehicle](node)
        decoded ==> Right(value)
      }

      test("direct child of base round-trip") {
        val value: YamlVehicle = YamlBicycle(18)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[YamlVehicle](node)
        decoded ==> Right(value)
      }
    }

    group("recursive types") {

      test("indirect recursive type round-trip") {
        val value = RecursiveParent(
          "root",
          List(
            RecursiveNode("a", List(RecursiveNode("b", Nil))),
            RecursiveNode("c", Nil)
          )
        )
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[RecursiveParent](node)
        decoded ==> Right(value)
      }
    }

    group("recursive types - def caching") {

      test("direct recursive sealed trait round-trip") {
        val value: TreeNode = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
        val node = KindlingsYamlEncoder.encode[TreeNode](value)
        val decoded = KindlingsYamlDecoder.decode[TreeNode](node)
        decoded ==> Right(value)
      }

      test("leaf-only recursive sealed trait round-trip") {
        val value: TreeNode = Leaf(42)
        val node = KindlingsYamlEncoder.encode[TreeNode](value)
        val decoded = KindlingsYamlDecoder.decode[TreeNode](node)
        decoded ==> Right(value)
      }

      test("deeply nested recursive sealed trait round-trip") {
        val value: TreeNode = Branch(1, Branch(2, Branch(3, Leaf(4), Leaf(5)), Leaf(6)), Branch(7, Leaf(8), Leaf(9)))
        val node = KindlingsYamlEncoder.encode[TreeNode](value)
        val decoded = KindlingsYamlDecoder.decode[TreeNode](node)
        decoded ==> Right(value)
      }

      test("mutual recursion round-trip") {
        val value = MutRecA(1, Some(MutRecB("x", Some(MutRecA(2, None)))))
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[MutRecA](node)
        decoded ==> Right(value)
      }

      test("mutual recursion round-trip from B side") {
        val value = MutRecB("root", Some(MutRecA(1, Some(MutRecB("leaf", None)))))
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[MutRecB](node)
        decoded ==> Right(value)
      }

      test("mutual recursion with no nesting") {
        val value = MutRecA(42, None)
        val node = KindlingsYamlEncoder.encode(value)
        val decoded = KindlingsYamlDecoder.decode[MutRecA](node)
        decoded ==> Right(value)
      }
    }

    // Note: List[Shape] (collection of sealed trait) fails on Scala 3 due to splice isolation issue
    // in yaml encoder macro — see KindlingsYamlEncoderSpec.

    group("combinatorial: wrapper x inner type") {

      // CombOuter round-trip decode tests are Scala 3 only — Scala 2 macro
      // hits "not found: value x$1" when inline-decoding sealed traits in composite types.
      // See YamlScala3Spec for the Scala 3 versions of these tests.

      test("CombOuter encode all fields populated") {
        val value = CombOuter(
          optPrimitive = Some(42),
          optCaseClass = Some(CombInnerCC("hello", 7)),
          optSealedTrait = Some(CombVariantA(99)),
          listCaseClass = List(CombInnerCC("a", 1), CombInnerCC("b", 2)),
          mapCaseClass = Map("k1" -> CombInnerCC("m", 10))
        )
        val node = KindlingsYamlEncoder.encode(value)
        assert(node.toString.contains("42"))
      }

      test("@fieldName on sealed trait subtype field round-trips") {
        val value: CombAnnotatedST = CombAnnotA("test")
        val node = KindlingsYamlEncoder.encode[CombAnnotatedST](value)
        val decoded = KindlingsYamlDecoder.decode[CombAnnotatedST](node)
        decoded ==> Right(value)
      }

      test("@fieldName on sealed trait subtype field round-trips (other variant)") {
        val value: CombAnnotatedST = CombAnnotB(true)
        val node = KindlingsYamlEncoder.encode[CombAnnotatedST](value)
        val decoded = KindlingsYamlDecoder.decode[CombAnnotatedST](node)
        decoded ==> Right(value)
      }

      test("@transientField on sealed trait subtype round-trips (field absent, default used)") {
        val value: CombTransientST = CombTransientA("Alice", "should-be-dropped")
        val node = KindlingsYamlEncoder.encode[CombTransientST](value)
        val decoded = KindlingsYamlDecoder.decode[CombTransientST](node)
        decoded ==> Right(CombTransientA("Alice", ""))
      }

      test("@transientField on sealed trait subtype round-trips (Option field, default None)") {
        val value: CombTransientST = CombTransientB(42, Some("should-be-dropped"))
        val node = KindlingsYamlEncoder.encode[CombTransientST](value)
        val decoded = KindlingsYamlDecoder.decode[CombTransientST](node)
        decoded ==> Right(CombTransientB(42, None))
      }
    }
  }
}
