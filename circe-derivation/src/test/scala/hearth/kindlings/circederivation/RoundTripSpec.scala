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

    group("non-string key maps") {

      test("Map[Int, String] roundtrip") {
        val value = Map(1 -> "a", 2 -> "b")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Int, String]](json) ==> Right(value)
      }

      test("Map[Long, String] roundtrip") {
        val value = Map(100L -> "x", 200L -> "y")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Long, String]](json) ==> Right(value)
      }

      test("case class with Map[Int, String] field roundtrip") {
        val value = WithIntKeyMap(Map(1 -> "a", 2 -> "b"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithIntKeyMap](json) ==> Right(value)
      }

      test("value type key Map[UserId, String] roundtrip") {
        val value = Map(UserId(42) -> "alice", UserId(99) -> "bob")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[UserId, String]](json) ==> Right(value)
      }

      test("enum key Map[CardinalDirection, String] roundtrip") {
        val value = Map[CardinalDirection, String](North -> "up", South -> "down")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[CardinalDirection, String]](json) ==> Right(value)
      }

      test("empty Map[Int, String] roundtrip") {
        val value = Map.empty[Int, String]
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Int, String]](json) ==> Right(value)
      }

      test("Map[Int, List[String]] nested roundtrip") {
        val value = Map(1 -> List("a", "b"), 2 -> List("c"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Int, List[String]]](json) ==> Right(value)
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

    group("KindlingsCodecAsObject") {

      test("derive returns Codec.AsObject") {
        val codec: io.circe.Codec.AsObject[SimplePerson] = KindlingsCodecAsObject.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("round-trip via derived codec") {
        val codec = KindlingsCodecAsObject.derived[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main", "NYC"))
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("produces same output as separate derivation") {
        val codec = KindlingsCodecAsObject.derived[SimplePerson]
        val encoder = KindlingsEncoder.deriveAsObject[SimplePerson]
        val value = SimplePerson("Alice", 30)
        codec.encodeObject(value) ==> encoder.encodeObject(value)
      }

      test("with snake_case configuration") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val codec = KindlingsCodecAsObject.derived[CamelCaseFields]
        val value = CamelCaseFields("Alice", "Smith")
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("sealed trait round-trip with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val codec = KindlingsCodecAsObject.derived[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("sealed trait round-trip without discriminator") {
        val codec = KindlingsCodecAsObject.derived[Shape]
        val value: Shape = Rectangle(3.0, 4.0)
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("empty case class") {
        val codec = KindlingsCodecAsObject.derived[EmptyClass]
        val value = EmptyClass()
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("derived instance via KindlingsCodecAsObject.derived") {
        val codec = KindlingsCodecAsObject.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = codec(value)
        codec.decodeJson(json) ==> Right(value)
      }

      test("Codec.AsObject with strictDecoding rejects extra fields") {
        implicit val config: Configuration = Configuration(strictDecoding = true)
        val codec = KindlingsCodecAsObject.derived[SimplePerson]
        val json = io.circe.parser.parse("""{"name":"Alice","age":30,"extra":true}""").getOrElse(io.circe.Json.Null)
        assert(codec.decodeJson(json).isLeft)
      }

      test("Codec.AsObject with @transientField") {
        val codec = KindlingsCodecAsObject.derived[CirceWithTransient]
        val value = CirceWithTransient("Alice", Some("cached"))
        val json = codec(value)
        // Transient field should not appear in encoded JSON
        assert(!json.noSpaces.contains("cache"))
        // Decoded value should use default
        codec.decodeJson(json) ==> Right(CirceWithTransient("Alice", None))
      }
    }

    group("recursive types") {

      test("recursive type auto-derivation round-trip") {
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[RecursiveTree](json) ==> Right(value)
      }

      test("indirect recursive type round-trip") {
        val value = RecursiveParent(
          "root",
          List(
            RecursiveNode("a", List(RecursiveNode("b", Nil))),
            RecursiveNode("c", Nil)
          )
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[RecursiveParent](json) ==> Right(value)
      }
    }

    group("recursive types - def caching") {

      test("direct recursive sealed trait round-trip") {
        val value: TreeNode = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
        val json = KindlingsEncoder.encode[TreeNode](value)
        KindlingsDecoder.decode[TreeNode](json) ==> Right(value)
      }

      test("leaf-only recursive sealed trait round-trip") {
        val value: TreeNode = Leaf(42)
        val json = KindlingsEncoder.encode[TreeNode](value)
        KindlingsDecoder.decode[TreeNode](json) ==> Right(value)
      }

      test("deeply nested recursive sealed trait round-trip") {
        val value: TreeNode = Branch(1, Branch(2, Branch(3, Leaf(4), Leaf(5)), Leaf(6)), Branch(7, Leaf(8), Leaf(9)))
        val json = KindlingsEncoder.encode[TreeNode](value)
        KindlingsDecoder.decode[TreeNode](json) ==> Right(value)
      }

      test("mutual recursion round-trip") {
        val value = MutRecA(1, Some(MutRecB("x", Some(MutRecA(2, None)))))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[MutRecA](json) ==> Right(value)
      }

      test("mutual recursion round-trip from B side") {
        val value = MutRecB("root", Some(MutRecA(1, Some(MutRecB("leaf", None)))))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[MutRecB](json) ==> Right(value)
      }

      test("mutual recursion with no nesting") {
        val value = MutRecA(42, None)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[MutRecA](json) ==> Right(value)
      }
    }

    group("Option of derived type") {

      test("Some(derived case class) roundtrip") {
        val value = WithOptionalPerson("test", Some(SimplePerson("Alice", 30)))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithOptionalPerson](json) ==> Right(value)
      }

      test("None for derived case class roundtrip") {
        val value = WithOptionalPerson("test", None)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithOptionalPerson](json) ==> Right(value)
      }
    }

    group("value class in various positions") {

      test("case class with value class fields roundtrip") {
        val value = WithValueClassFields(UserId(1), WrappedString("hello"), Some(UserId(2)), List(UserId(3), UserId(4)))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithValueClassFields](json) ==> Right(value)
      }

      test("case class with None value class field roundtrip") {
        val value = WithValueClassFields(UserId(1), WrappedString("hello"), None, Nil)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithValueClassFields](json) ==> Right(value)
      }
    }

    group("non-string key maps extended") {

      test("Map[Short, String] roundtrip") {
        val value = Map(1.toShort -> "a", 2.toShort -> "b")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Short, String]](json) ==> Right(value)
      }

      test("Map[Byte, String] roundtrip") {
        val value = Map(1.toByte -> "a", 2.toByte -> "b")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Byte, String]](json) ==> Right(value)
      }

      test("Map[Double, String] roundtrip") {
        val value = Map(1.5 -> "a", 2.5 -> "b")
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[Map[Double, String]](json) ==> Right(value)
      }

      test("case class with Map[Short, String] field roundtrip") {
        val value = WithShortKeyMap(Map(10.toShort -> "x"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithShortKeyMap](json) ==> Right(value)
      }

      test("case class with Map[Byte, String] field roundtrip") {
        val value = WithByteKeyMap(Map(5.toByte -> "y"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithByteKeyMap](json) ==> Right(value)
      }

      test("case class with Map[Double, String] field roundtrip") {
        val value = WithDoubleKeyMap(Map(3.14 -> "pi"))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithDoubleKeyMap](json) ==> Right(value)
      }

      test("Map[Long, List[String]] nested roundtrip") {
        val value = WithMapOfLists(Map(1L -> List("a", "b"), 2L -> List("c")))
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[WithMapOfLists](json) ==> Right(value)
      }
    }

    group("strict decoding with discriminator") {

      test("strict decoding without discriminator passes with exact fields") {
        implicit val config: Configuration = Configuration(strictDecoding = true)
        val value: Shape = Circle(5.0)
        val json = KindlingsEncoder.encode[Shape](value)
        KindlingsDecoder.decode[Shape](json) ==> Right(value)
      }

      test("strict decoding without discriminator rejects extra fields on inner") {
        implicit val config: Configuration = Configuration(strictDecoding = true)
        val json = io.circe.parser
          .parse("""{"Circle":{"radius":5.0,"extra":"oops"}}""")
          .getOrElse(io.circe.Json.Null)
        assert(KindlingsDecoder.decode[Shape](json).isLeft)
      }
    }

    group("accumulating + defaults") {

      test("accumulating decoder with defaults for missing fields") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = io.circe.Json.obj()
        val decoder = KindlingsDecoder.derived[MultiOptionDefaults]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isValid)
        result.fold(
          _ => fail("expected valid"),
          value => {
            value.a ==> None
            value.b ==> "default-b"
            value.c ==> Some("default-c")
            value.d ==> 42
          }
        )
      }

      test("accumulating decoder with defaults uses provided values over defaults") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = io.circe.Json.obj(
          "a" -> io.circe.Json.fromInt(10),
          "b" -> io.circe.Json.fromString("custom"),
          "c" -> io.circe.Json.fromString("custom-c"),
          "d" -> io.circe.Json.fromInt(99)
        )
        val decoder = KindlingsDecoder.derived[MultiOptionDefaults]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isValid)
        result.fold(
          _ => fail("expected valid"),
          value => value ==> MultiOptionDefaults(Some(10), "custom", Some("custom-c"), 99)
        )
      }
    }

    group("combinatorial: wrapper x inner type") {

      test("CombOuter round-trip with all fields populated") {
        val value = CombOuter(
          optPrimitive = Some(42),
          optCaseClass = Some(SimplePerson("Alice", 30)),
          optSealedTrait = Some(Circle(5.0)),
          optValueClass = Some(WrappedInt(99)),
          listCaseClass = List(SimplePerson("Bob", 25), SimplePerson("Carol", 35)),
          listSealedTrait = List(Circle(1.0), Rectangle(2.0, 3.0)),
          mapCaseClass = Map("a" -> SimplePerson("Dave", 40)),
          mapSealedTrait = Map("x" -> Circle(7.0))
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("CombOuter round-trip with None and empty collections") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("Option[Shape] with Some (sealed trait in Option - bug #78 pattern)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("Option[Shape] with None (sealed trait in Option - bug #78 pattern)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("List[Shape] (sealed trait in List)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("Map[String, Shape] (sealed trait in Map)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("Option[SimplePerson] (case class in Option - bug #120 pattern)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = Some(SimplePerson("Alice", 30)),
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map.empty,
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }

      test("Map[String, SimplePerson] (case class in Map)") {
        val value = CombOuter(
          optPrimitive = None,
          optCaseClass = None,
          optSealedTrait = None,
          optValueClass = None,
          listCaseClass = Nil,
          listSealedTrait = Nil,
          mapCaseClass = Map("alice" -> SimplePerson("Alice", 30), "bob" -> SimplePerson("Bob", 25)),
          mapSealedTrait = Map.empty
        )
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CombOuter](json) ==> Right(value)
      }
    }

    group("bug pattern regressions") {

      group("Option[DerivedType] with no pre-existing implicit (bug #120 pattern)") {

        test("encode then decode Some(InnerForOpt) round-trips") {
          val value = OuterWithOptInner(Some(InnerForOpt(1, "a")))
          val json = KindlingsEncoder.encode(value)
          KindlingsDecoder.decode[OuterWithOptInner](json) ==> Right(value)
        }

        test("decode null data field to None") {
          val json = io.circe.parser.parse("""{"data": null}""").getOrElse(io.circe.Json.Null)
          KindlingsDecoder.decode[OuterWithOptInner](json) ==> Right(OuterWithOptInner(None))
        }

        test("decode absent data field to None") {
          val json = io.circe.parser.parse("""{}""").getOrElse(io.circe.Json.Null)
          KindlingsDecoder.decode[OuterWithOptInner](json) ==> Right(OuterWithOptInner(None))
        }
      }

      group("@fieldName on sealed trait subtypes (bug #108 pattern)") {

        test("decoding with original field name fails") {
          // AnnotatedLeafA has @fieldName("full_name") on fullName — decoding with "fullName" should fail
          val json = io.circe.Json.obj(
            "AnnotatedLeafA" -> io.circe.Json.obj(
              "fullName" -> io.circe.Json.fromString("Alice"),
              "value" -> io.circe.Json.fromInt(10)
            )
          )
          assert(KindlingsDecoder.decode[AnnotatedADT](json).isLeft)
        }
      }

      group("useDefaults with derived inner types (bug #120 + defaults pattern)") {

        test("missing Option[DerivedType] field uses default value") {
          implicit val config: Configuration = Configuration.default.withDefaults
          val json = io.circe.Json.obj("label" -> io.circe.Json.fromString("test"))
          KindlingsDecoder.decode[OuterWithOptInnerDefault](json) ==>
            Right(OuterWithOptInnerDefault("test", Some(InnerForOpt(0, "default"))))
        }

        test("null Option[DerivedType] field overrides default to None") {
          implicit val config: Configuration = Configuration.default.withDefaults
          val json = io.circe.Json.obj(
            "label" -> io.circe.Json.fromString("test"),
            "data" -> io.circe.Json.Null
          )
          KindlingsDecoder.decode[OuterWithOptInnerDefault](json) ==>
            Right(OuterWithOptInnerDefault("test", None))
        }

        test("provided Option[DerivedType] field overrides default") {
          implicit val config: Configuration = Configuration.default.withDefaults
          val json = io.circe.Json.obj(
            "label" -> io.circe.Json.fromString("test"),
            "data" -> io.circe.Json.obj("x" -> io.circe.Json.fromInt(99), "y" -> io.circe.Json.fromString("custom"))
          )
          KindlingsDecoder.decode[OuterWithOptInnerDefault](json) ==>
            Right(OuterWithOptInnerDefault("test", Some(InnerForOpt(99, "custom"))))
        }

        test("full round-trip with default values") {
          implicit val config: Configuration = Configuration.default.withDefaults
          val value = OuterWithOptInnerDefault("test", Some(InnerForOpt(5, "five")))
          val json = KindlingsEncoder.encode(value)
          KindlingsDecoder.decode[OuterWithOptInnerDefault](json) ==> Right(value)
        }
      }
    }

    group("annotation x type shape") {

      test("@fieldName on case class field round-trip") {
        val value = CirceWithFieldName("Alice", 30)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CirceWithFieldName](json) ==> Right(value)
      }

      test("@transientField on case class field round-trip") {
        val value = CirceWithTransient("Alice", Some("cached"))
        val json = KindlingsEncoder.encode(value)
        // transient field uses default on decode
        KindlingsDecoder.decode[CirceWithTransient](json) ==> Right(CirceWithTransient("Alice", None))
      }

      test("@fieldName + @transientField combined on case class round-trip") {
        val value = CirceWithBothAnnotations("Alice", 42, true)
        val json = KindlingsEncoder.encode(value)
        KindlingsDecoder.decode[CirceWithBothAnnotations](json) ==> Right(CirceWithBothAnnotations("Alice", 0, true))
      }

      test("@fieldName on sealed trait subtype field round-trip (bug #108 pattern)") {
        val value: AnnotatedADT = AnnotatedLeafA("Alice Smith", 42)
        val json = KindlingsEncoder.encode[AnnotatedADT](value)
        KindlingsDecoder.decode[AnnotatedADT](json) ==> Right(value)
      }

      test("@fieldName on sealed trait subtype encodes with renamed field") {
        val json = KindlingsEncoder.encode[AnnotatedADT](AnnotatedLeafA("Alice", 10): AnnotatedADT)
        val inner = json.asObject.flatMap(_("AnnotatedLeafA")).flatMap(_.asObject)
        assert(inner.isDefined)
        assert(inner.get.contains("full_name"))
        assert(!inner.get.contains("fullName"))
      }

      test("@transientField on sealed trait subtype field round-trip (bug #108 pattern)") {
        val value: AnnotatedADT = AnnotatedLeafB("test", 999)
        val json = KindlingsEncoder.encode[AnnotatedADT](value)
        // transient field should not be encoded, and decoding uses default
        KindlingsDecoder.decode[AnnotatedADT](json) ==> Right(AnnotatedLeafB("test", 0): AnnotatedADT)
      }

      test("@transientField on sealed trait subtype excludes field from encoding") {
        val json = KindlingsEncoder.encode[AnnotatedADT](AnnotatedLeafB("test", 999): AnnotatedADT)
        val inner = json.asObject.flatMap(_("AnnotatedLeafB")).flatMap(_.asObject)
        assert(inner.isDefined)
        assert(!inner.get.contains("hidden"))
        inner.get("label") ==> Some(io.circe.Json.fromString("test"))
      }

      test("both annotations on sealed trait subtype field round-trip (bug #108 pattern)") {
        val value: AnnotatedADT = AnnotatedLeafBoth("Hello", Some("scratch"), true)
        val json = KindlingsEncoder.encode[AnnotatedADT](value)
        KindlingsDecoder.decode[AnnotatedADT](json) ==> Right(AnnotatedLeafBoth("Hello", None, true): AnnotatedADT)
      }

      test("both annotations on sealed trait subtype encode correctly") {
        val json =
          KindlingsEncoder.encode[AnnotatedADT](AnnotatedLeafBoth("Hello", Some("scratch"), true): AnnotatedADT)
        val inner = json.asObject.flatMap(_("AnnotatedLeafBoth")).flatMap(_.asObject)
        assert(inner.isDefined)
        assert(inner.get.contains("display_label"))
        assert(!inner.get.contains("displayLabel"))
        assert(!inner.get.contains("scratch"))
        inner.get("active") ==> Some(io.circe.Json.True)
      }

      test("sealed trait with annotated subtypes with discriminator round-trip") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val value: AnnotatedADT = AnnotatedLeafA("Bob", 7)
        val json = KindlingsEncoder.encode[AnnotatedADT](value)
        KindlingsDecoder.decode[AnnotatedADT](json) ==> Right(value)
      }

      test("sealed trait with annotated subtypes with discriminator encodes renamed fields") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = KindlingsEncoder.encode[AnnotatedADT](AnnotatedLeafA("Bob", 7): AnnotatedADT)
        val obj = json.asObject.get
        obj("type") ==> Some(io.circe.Json.fromString("AnnotatedLeafA"))
        obj("full_name") ==> Some(io.circe.Json.fromString("Bob"))
        assert(!obj.contains("fullName"))
      }
    }
  }
}
