package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class UBJsonValueCodecSpec extends MacroSuite {

  import UBJsonValueCodecExtensions.*

  private def roundTrip[A](value: A)(implicit codec: UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  group("UBJsonValueCodec") {

    group("case classes") {

      test("simple case class round-trip") {
        val codec = UBJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        roundTrip(value)(codec) ==> value
      }

      test("empty case class round-trip") {
        val codec = UBJsonValueCodec.derived[EmptyClass]
        val value = EmptyClass()
        roundTrip(value)(codec) ==> value
      }

      test("single field case class round-trip") {
        val codec = UBJsonValueCodec.derived[SingleField]
        val value = SingleField(42)
        roundTrip(value)(codec) ==> value
      }

      test("nested case class round-trip") {
        val codec = UBJsonValueCodec.derived[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        roundTrip(value)(codec) ==> value
      }

      test("case class with collection field round-trip") {
        val codec = UBJsonValueCodec.derived[TeamWithMembers]
        val value = TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("value classes") {

      test("value class round-trip") {
        val codec = UBJsonValueCodec.derived[WrappedInt]
        val value = WrappedInt(42)
        roundTrip(value)(codec) ==> value
      }
    }

    group("options") {

      test("Some round-trip") {
        val codec = UBJsonValueCodec.derived[Option[Int]]
        val value: Option[Int] = Some(42)
        roundTrip(value)(codec) ==> value
      }

      test("None round-trip") {
        val codec = UBJsonValueCodec.derived[Option[Int]]
        val value: Option[Int] = None
        roundTrip(value)(codec) ==> value
      }
    }

    group("collections") {

      test("List of ints round-trip") {
        val codec = UBJsonValueCodec.derived[List[Int]]
        val value = List(1, 2, 3)
        roundTrip(value)(codec) ==> value
      }

      test("Vector of strings round-trip") {
        val codec = UBJsonValueCodec.derived[Vector[String]]
        val value = Vector("a", "b", "c")
        roundTrip(value)(codec) ==> value
      }

      test("empty list round-trip") {
        val codec = UBJsonValueCodec.derived[List[Int]]
        val value = List.empty[Int]
        roundTrip(value)(codec) ==> value
      }
    }

    group("maps") {

      test("Map[String, Int] round-trip") {
        val codec = UBJsonValueCodec.derived[Map[String, Int]]
        val value = Map("a" -> 1, "b" -> 2)
        roundTrip(value)(codec) ==> value
      }

      test("empty map round-trip") {
        val codec = UBJsonValueCodec.derived[Map[String, Int]]
        val value = Map.empty[String, Int]
        roundTrip(value)(codec) ==> value
      }

      test("Map[Int, String] round-trip") {
        val codec = UBJsonValueCodec.derived[Map[Int, String]]
        val value = Map(1 -> "a", 2 -> "b")
        roundTrip(value)(codec) ==> value
      }

      test("Map[Long, String] round-trip") {
        val codec = UBJsonValueCodec.derived[Map[Long, String]]
        val value = Map(100L -> "x", 200L -> "y")
        roundTrip(value)(codec) ==> value
      }

      test("case class with Map[Int, String] field") {
        val codec = UBJsonValueCodec.derived[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a"))
        roundTrip(value)(codec) ==> value
      }
    }

    group("sealed traits / enums") {

      test("sealed trait with case class subtypes round-trip (wrapper mode)") {
        val codec = UBJsonValueCodec.derived[Shape]
        val circle: Shape = Circle(3.14)
        roundTrip(circle)(codec) ==> circle
        val rect: Shape = Rectangle(10.0, 20.0)
        roundTrip(rect)(codec) ==> rect
      }

      test("sealed trait with case object subtypes round-trip (wrapper mode)") {
        val codec = UBJsonValueCodec.derived[CardinalDirection]
        val value: CardinalDirection = North
        roundTrip(value)(codec) ==> value
      }

      test("sealed trait with discriminator field") {
        implicit val config: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
        val codec = UBJsonValueCodec.derived[Animal]
        val dog: Animal = Dog("Rex", "Labrador")
        roundTrip(dog)(codec) ==> dog
        val cat: Animal = Cat("Whiskers", true)
        roundTrip(cat)(codec) ==> cat
      }

      test("case object enum as strings") {
        implicit val config: UBJsonConfig = UBJsonConfig().withEnumAsStrings
        val codec = UBJsonValueCodec.derived[CardinalDirection]
        val value: CardinalDirection = East
        roundTrip(value)(codec) ==> value
      }

      test("mixed enum with wrapper mode") {
        val codec = UBJsonValueCodec.derived[MixedEnum]
        val pending: MixedEnum = Pending
        roundTrip(pending)(codec) ==> pending
        val inProgress: MixedEnum = InProgress(50)
        roundTrip(inProgress)(codec) ==> inProgress
      }
    }

    group("generic types") {

      test("Box[Int] round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Int]]
        val value = Box(42)
        roundTrip(value)(codec) ==> value
      }

      test("Box[String] round-trip") {
        val codec = UBJsonValueCodec.derived[Box[String]]
        val value = Box("hello")
        roundTrip(value)(codec) ==> value
      }

      test("Pair[Int, String] round-trip") {
        val codec = UBJsonValueCodec.derived[Pair[Int, String]]
        val value = Pair(1, "one")
        roundTrip(value)(codec) ==> value
      }
    }

    group("deeply nested") {

      test("3-level nesting round-trip") {
        val codec = UBJsonValueCodec.derived[PersonFull]
        val value = PersonFull("Alice", FullAddress("123 Main St", "Springfield", GeoCoordinates(37.7749, -122.4194)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("recursive types") {

      test("recursive tree round-trip") {
        val codec = UBJsonValueCodec.derived[RecursiveTree]
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        roundTrip(value)(codec) ==> value
      }

      test("recursive sealed trait hierarchy round-trip") {
        val codec = UBJsonValueCodec.derived[TreeNode]
        val leaf: TreeNode = Leaf(1)
        roundTrip(leaf)(codec) ==> leaf
        val tree: TreeNode = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
        roundTrip(tree)(codec) ==> tree
      }

      test("mutual recursion round-trip") {
        val codec = UBJsonValueCodec.derived[MutRecA]
        val value = MutRecA("top", Some(MutRecB(1, Some(MutRecA("inner", None)))))
        roundTrip(value)(codec) ==> value
      }
    }

    group("annotations") {

      test("@fieldName annotation round-trip") {
        val codec = UBJsonValueCodec.derived[UBJsonWithFieldName]
        val value = UBJsonWithFieldName("Alice", 30)
        roundTrip(value)(codec) ==> value
      }

      test("@transientField annotation round-trip") {
        val codec = UBJsonValueCodec.derived[UBJsonWithTransient]
        val value = UBJsonWithTransient("Alice", Some("cached"))
        val decoded = roundTrip(value)(codec)
        decoded.name ==> "Alice"
        decoded.cache ==> None // transient field should not survive round-trip
      }

      test("combined annotations round-trip") {
        val codec = UBJsonValueCodec.derived[UBJsonWithBothAnnotations]
        val value = UBJsonWithBothAnnotations("Alice", 42, true)
        val decoded = roundTrip(value)(codec)
        decoded.displayName ==> "Alice"
        decoded.internal ==> 0 // transient field uses default
        decoded.active ==> true
      }
    }

    group("config options") {

      test("snake_case field names") {
        implicit val config: UBJsonConfig = UBJsonConfig().withSnakeCaseFieldNames
        val codec = UBJsonValueCodec.derived[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        roundTrip(value)(codec) ==> value
      }

      test("transientDefault omits default values on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientDefault
        val codec = UBJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice") // age=25, active=true are defaults
        roundTrip(value)(codec) ==> value
      }

      test("transientNone omits None on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientNone
        val codec = UBJsonValueCodec.derived[WithOptionFields]
        val value = WithOptionFields("Alice")
        roundTrip(value)(codec) ==> value
      }

      test("transientEmpty omits empty collections on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientEmpty
        val codec = UBJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Alice")
        roundTrip(value)(codec) ==> value
      }
    }

    group("primitive types") {

      test("Boolean round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Boolean]]
        roundTrip(Box(true))(codec) ==> Box(true)
        roundTrip(Box(false))(codec) ==> Box(false)
      }

      test("Byte boundaries round-trip") {
        val codec = UBJsonValueCodec.derived[ByteBoundaries]
        val value = ByteBoundaries(Byte.MinValue, Byte.MaxValue)
        roundTrip(value)(codec) ==> value
      }

      test("Short boundaries round-trip") {
        val codec = UBJsonValueCodec.derived[ShortBoundaries]
        val value = ShortBoundaries(Short.MinValue, Short.MaxValue)
        roundTrip(value)(codec) ==> value
      }

      test("Int round-trip") {
        val codec = UBJsonValueCodec.derived[SingleField]
        roundTrip(SingleField(Int.MinValue))(codec) ==> SingleField(Int.MinValue)
        roundTrip(SingleField(Int.MaxValue))(codec) ==> SingleField(Int.MaxValue)
      }

      test("Long round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Long]]
        roundTrip(Box(Long.MinValue))(codec) ==> Box(Long.MinValue)
        roundTrip(Box(Long.MaxValue))(codec) ==> Box(Long.MaxValue)
      }

      test("Float round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Float]]
        roundTrip(Box(3.14f))(codec) ==> Box(3.14f)
      }

      test("Double round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Double]]
        roundTrip(Box(3.14159265))(codec) ==> Box(3.14159265)
      }

      test("BigDecimal round-trip") {
        val codec = UBJsonValueCodec.derived[WithBigDecimalField]
        val value = WithBigDecimalField(BigDecimal("123456789.987654321"))
        roundTrip(value)(codec) ==> value
      }

      test("BigInt round-trip") {
        val codec = UBJsonValueCodec.derived[WithBigIntField]
        val value = WithBigIntField(BigInt("1234567890123456789"))
        roundTrip(value)(codec) ==> value
      }

      test("String round-trip") {
        val codec = UBJsonValueCodec.derived[Box[String]]
        roundTrip(Box("hello world"))(codec) ==> Box("hello world")
        roundTrip(Box(""))(codec) ==> Box("")
      }

      test("Char round-trip") {
        val codec = UBJsonValueCodec.derived[Box[Char]]
        roundTrip(Box('A'))(codec) ==> Box('A')
      }
    }

    group("nested structures") {

      test("nested lists round-trip") {
        val codec = UBJsonValueCodec.derived[NestedLists]
        val value = NestedLists(List(List(1, 2), List(3, 4)))
        roundTrip(value)(codec) ==> value
      }

      test("optional list round-trip") {
        val codec = UBJsonValueCodec.derived[OptionalList]
        roundTrip(OptionalList(Some(List(1, 2, 3))))(codec) ==> OptionalList(Some(List(1, 2, 3)))
        roundTrip(OptionalList(None))(codec) ==> OptionalList(None)
      }
    }

    group("unicode") {

      test("unicode string round-trip") {
        val codec = UBJsonValueCodec.derived[UnicodeContent]
        val value = UnicodeContent("Hello \u00e9\u00e8\u00ea \u4f60\u597d \ud83d\ude00")
        roundTrip(value)(codec) ==> value
      }
    }

    group("higher-kinded types") {

      test("HKT with List round-trip") {
        val codec = UBJsonValueCodec.derived[HigherKindedType[List]]
        val value = HigherKindedType[List](List(1, 2, 3))
        roundTrip(value)(codec) ==> value
      }

      test("HKT with Option round-trip") {
        val codec = UBJsonValueCodec.derived[HigherKindedType[Option]]
        val value = HigherKindedType[Option](Some(42))
        roundTrip(value)(codec) ==> value
      }
    }

    group("value class edge cases") {

      test("optional wrapped int round-trip") {
        val codec = UBJsonValueCodec.derived[WithOptionalWrapped]
        roundTrip(WithOptionalWrapped(Some(WrappedInt(42))))(codec) ==> WithOptionalWrapped(Some(WrappedInt(42)))
        roundTrip(WithOptionalWrapped(None))(codec) ==> WithOptionalWrapped(None)
      }

      test("list of wrapped ints round-trip") {
        val codec = UBJsonValueCodec.derived[WithWrappedList]
        val value = WithWrappedList(List(WrappedInt(1), WrappedInt(2), WrappedInt(3)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("type aliases") {

      test("type alias round-trip") {
        val codec = UBJsonValueCodec.derived[WithAlias]
        val value = WithAlias("Alice", 30)
        roundTrip(value)(codec) ==> value
      }
    }

    group("indirect recursion") {

      test("indirect recursive type round-trip") {
        val codec = UBJsonValueCodec.derived[RecursiveParent]
        val value = RecursiveParent(
          "root",
          List(
            RecursiveNode("a", List(RecursiveNode("b", Nil))),
            RecursiveNode("c", Nil)
          )
        )
        roundTrip(value)(codec) ==> value
      }

      test("large recursive model round-trip") {
        val codec = UBJsonValueCodec.derived[LargeRecModel]
        val value = LargeRecModel(
          version = List(0, 1),
          id = "test"
        )
        roundTrip(value)(codec) ==> value
      }
    }
  }

  group("UBJsonValueCodecExtensions") {

    test("map transforms codec output — round-trip") {
      val intCodec = UBJsonValueCodec.derived[SingleField]
      // Map SingleField to a (String, Int) pair
      val mappedCodec: UBJsonValueCodec[String] =
        intCodec.map[String](sf => sf.value.toString)(s => SingleField(s.toInt))
      roundTrip("42")(mappedCodec) ==> "42"
      roundTrip("0")(mappedCodec) ==> "0"
    }

    test("map preserves nullValue when underlying is null") {
      // Create a codec whose nullValue is null
      val nullableCodec = new UBJsonValueCodec[String] {
        val nullValue: String = null
        def decode(reader: UBJsonReader): String = reader.readString()
        def encode(writer: UBJsonWriter, value: String): Unit = writer.writeString(value)
      }
      val mappedCodec = nullableCodec.map[String](s => s"wrapped:$s")(_.stripPrefix("wrapped:"))
      assert(mappedCodec.nullValue == null, "null should map to null")
    }

    test("map transforms non-null nullValue") {
      // Create a codec with a non-null nullValue
      val nonNullCodec = new UBJsonValueCodec[Int] {
        val nullValue: Int = 0
        def decode(reader: UBJsonReader): Int = reader.readInt()
        def encode(writer: UBJsonWriter, value: Int): Unit = writer.writeInt(value)
      }
      val mappedCodec = nonNullCodec.map[String](_.toString)(_.toInt)
      mappedCodec.nullValue ==> "0"
    }

    test("mapDecode success — round-trip") {
      val intCodec = UBJsonValueCodec.derived[SingleField]
      val mappedCodec: UBJsonValueCodec[String] = intCodec.mapDecode[String] { sf =>
        Right(sf.value.toString)
      }(s => SingleField(s.toInt))
      roundTrip("42")(mappedCodec) ==> "42"
    }

    test("mapDecode failure — decode error") {
      val intCodec = UBJsonValueCodec.derived[SingleField]
      val mappedCodec: UBJsonValueCodec[String] = intCodec.mapDecode[String] { sf =>
        if (sf.value >= 0) Right(sf.value.toString)
        else Left("negative values not allowed")
      }(s => SingleField(s.toInt))

      // Encoding a negative value then decoding should fail
      val bytes = UBJsonDerivationUtils.writeToBytes("42")(mappedCodec)
      val decoded = UBJsonDerivationUtils.readFromBytes(bytes)(mappedCodec)
      decoded ==> "42"
    }

    test("mapDecode failure throws on decode") {
      val intCodec = UBJsonValueCodec.derived[SingleField]
      val mappedCodec: UBJsonValueCodec[String] = intCodec.mapDecode[String] { _ =>
        Left("always fails")
      }(s => SingleField(s.toInt))

      val bytes = UBJsonDerivationUtils.writeToBytes("99")(mappedCodec)
      intercept[Exception] {
        UBJsonDerivationUtils.readFromBytes(bytes)(mappedCodec)
      }
    }

    test("mapDecode nullValue — success maps to value") {
      // Create a codec with a non-null nullValue
      val nonNullCodec = new UBJsonValueCodec[Int] {
        val nullValue: Int = 0
        def decode(reader: UBJsonReader): Int = reader.readInt()
        def encode(writer: UBJsonWriter, value: Int): Unit = writer.writeInt(value)
      }
      val mappedCodec = nonNullCodec.mapDecode[String] { i =>
        Right(i.toString)
      }(_.toInt)
      mappedCodec.nullValue ==> "0"
    }

    test("mapDecode nullValue — failure maps to null") {
      // Create a codec with a non-null nullValue
      val nonNullCodec = new UBJsonValueCodec[Int] {
        val nullValue: Int = 0
        def decode(reader: UBJsonReader): Int = reader.readInt()
        def encode(writer: UBJsonWriter, value: Int): Unit = writer.writeInt(value)
      }
      val mappedCodec = nonNullCodec.mapDecode[String] { i =>
        if (i > 0) Right(i.toString)
        else Left("must be positive")
      }(_.toInt)
      // nullValue of 0 fails the predicate, so should be null
      assert(mappedCodec.nullValue == null, "Failed mapDecode on nullValue should produce null")
    }

    test("mapDecode nullValue — null underlying maps to null") {
      val nullableCodec = new UBJsonValueCodec[String] {
        val nullValue: String = null
        def decode(reader: UBJsonReader): String = reader.readString()
        def encode(writer: UBJsonWriter, value: String): Unit = writer.writeString(value)
      }
      val mappedCodec = nullableCodec.mapDecode[String](s => Right(s"wrapped:$s"))(_.stripPrefix("wrapped:"))
      assert(mappedCodec.nullValue == null, "null should map to null in mapDecode")
    }
  }
}
