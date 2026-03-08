package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class KindlingsUBJsonValueCodecSpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit codec: UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  group("KindlingsUBJsonValueCodec") {

    group("case classes") {

      test("simple case class round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        roundTrip(value)(codec) ==> value
      }

      test("empty case class round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[EmptyClass]
        val value = EmptyClass()
        roundTrip(value)(codec) ==> value
      }

      test("single field case class round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[SingleField]
        val value = SingleField(42)
        roundTrip(value)(codec) ==> value
      }

      test("nested case class round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        roundTrip(value)(codec) ==> value
      }

      test("case class with collection field round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[TeamWithMembers]
        val value = TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("value classes") {

      test("value class round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WrappedInt]
        val value = WrappedInt(42)
        roundTrip(value)(codec) ==> value
      }
    }

    group("options") {

      test("Some round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = Some(42)
        roundTrip(value)(codec) ==> value
      }

      test("None round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = None
        roundTrip(value)(codec) ==> value
      }
    }

    group("collections") {

      test("List of ints round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[List[Int]]
        val value = List(1, 2, 3)
        roundTrip(value)(codec) ==> value
      }

      test("Vector of strings round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Vector[String]]
        val value = Vector("a", "b", "c")
        roundTrip(value)(codec) ==> value
      }

      test("empty list round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[List[Int]]
        val value = List.empty[Int]
        roundTrip(value)(codec) ==> value
      }
    }

    group("maps") {

      test("Map[String, Int] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Map[String, Int]]
        val value = Map("a" -> 1, "b" -> 2)
        roundTrip(value)(codec) ==> value
      }

      test("empty map round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Map[String, Int]]
        val value = Map.empty[String, Int]
        roundTrip(value)(codec) ==> value
      }

      test("Map[Int, String] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Map[Int, String]]
        val value = Map(1 -> "a", 2 -> "b")
        roundTrip(value)(codec) ==> value
      }

      test("Map[Long, String] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Map[Long, String]]
        val value = Map(100L -> "x", 200L -> "y")
        roundTrip(value)(codec) ==> value
      }

      test("case class with Map[Int, String] field") {
        val codec = KindlingsUBJsonValueCodec.derive[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a"))
        roundTrip(value)(codec) ==> value
      }
    }

    group("sealed traits / enums") {

      test("sealed trait with case class subtypes round-trip (wrapper mode)") {
        val codec = KindlingsUBJsonValueCodec.derive[Shape]
        val circle: Shape = Circle(3.14)
        roundTrip(circle)(codec) ==> circle
        val rect: Shape = Rectangle(10.0, 20.0)
        roundTrip(rect)(codec) ==> rect
      }

      test("sealed trait with case object subtypes round-trip (wrapper mode)") {
        val codec = KindlingsUBJsonValueCodec.derive[CardinalDirection]
        val value: CardinalDirection = North
        roundTrip(value)(codec) ==> value
      }

      test("sealed trait with discriminator field") {
        implicit val config: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
        val codec = KindlingsUBJsonValueCodec.derive[Animal]
        val dog: Animal = Dog("Rex", "Labrador")
        roundTrip(dog)(codec) ==> dog
        val cat: Animal = Cat("Whiskers", true)
        roundTrip(cat)(codec) ==> cat
      }

      test("case object enum as strings") {
        implicit val config: UBJsonConfig = UBJsonConfig().withEnumAsStrings
        val codec = KindlingsUBJsonValueCodec.derive[CardinalDirection]
        val value: CardinalDirection = East
        roundTrip(value)(codec) ==> value
      }

      test("mixed enum with wrapper mode") {
        val codec = KindlingsUBJsonValueCodec.derive[MixedEnum]
        val pending: MixedEnum = Pending
        roundTrip(pending)(codec) ==> pending
        val inProgress: MixedEnum = InProgress(50)
        roundTrip(inProgress)(codec) ==> inProgress
      }
    }

    group("generic types") {

      test("Box[Int] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Int]]
        val value = Box(42)
        roundTrip(value)(codec) ==> value
      }

      test("Box[String] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[String]]
        val value = Box("hello")
        roundTrip(value)(codec) ==> value
      }

      test("Pair[Int, String] round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Pair[Int, String]]
        val value = Pair(1, "one")
        roundTrip(value)(codec) ==> value
      }
    }

    group("deeply nested") {

      test("3-level nesting round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[PersonFull]
        val value = PersonFull("Alice", FullAddress("123 Main St", "Springfield", GeoCoordinates(37.7749, -122.4194)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("recursive types") {

      test("recursive tree round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[RecursiveTree]
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        roundTrip(value)(codec) ==> value
      }
    }

    group("annotations") {

      test("@fieldName annotation round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[UBJsonWithFieldName]
        val value = UBJsonWithFieldName("Alice", 30)
        roundTrip(value)(codec) ==> value
      }

      test("@transientField annotation round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[UBJsonWithTransient]
        val value = UBJsonWithTransient("Alice", Some("cached"))
        val decoded = roundTrip(value)(codec)
        decoded.name ==> "Alice"
        decoded.cache ==> None // transient field should not survive round-trip
      }

      test("combined annotations round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[UBJsonWithBothAnnotations]
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
        val codec = KindlingsUBJsonValueCodec.derive[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        roundTrip(value)(codec) ==> value
      }

      test("transientDefault omits default values on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientDefault
        val codec = KindlingsUBJsonValueCodec.derive[WithDefaultFields]
        val value = WithDefaultFields("Alice") // age=25, active=true are defaults
        roundTrip(value)(codec) ==> value
      }

      test("transientNone omits None on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientNone
        val codec = KindlingsUBJsonValueCodec.derive[WithOptionFields]
        val value = WithOptionFields("Alice")
        roundTrip(value)(codec) ==> value
      }

      test("transientEmpty omits empty collections on encode") {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientEmpty
        val codec = KindlingsUBJsonValueCodec.derive[WithCollectionFields]
        val value = WithCollectionFields("Alice")
        roundTrip(value)(codec) ==> value
      }
    }

    group("primitive types") {

      test("Boolean round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Boolean]]
        roundTrip(Box(true))(codec) ==> Box(true)
        roundTrip(Box(false))(codec) ==> Box(false)
      }

      test("Byte boundaries round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[ByteBoundaries]
        val value = ByteBoundaries(Byte.MinValue, Byte.MaxValue)
        roundTrip(value)(codec) ==> value
      }

      test("Short boundaries round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[ShortBoundaries]
        val value = ShortBoundaries(Short.MinValue, Short.MaxValue)
        roundTrip(value)(codec) ==> value
      }

      test("Int round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[SingleField]
        roundTrip(SingleField(Int.MinValue))(codec) ==> SingleField(Int.MinValue)
        roundTrip(SingleField(Int.MaxValue))(codec) ==> SingleField(Int.MaxValue)
      }

      test("Long round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Long]]
        roundTrip(Box(Long.MinValue))(codec) ==> Box(Long.MinValue)
        roundTrip(Box(Long.MaxValue))(codec) ==> Box(Long.MaxValue)
      }

      test("Float round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Float]]
        roundTrip(Box(3.14f))(codec) ==> Box(3.14f)
      }

      test("Double round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Double]]
        roundTrip(Box(3.14159265))(codec) ==> Box(3.14159265)
      }

      test("BigDecimal round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WithBigDecimalField]
        val value = WithBigDecimalField(BigDecimal("123456789.987654321"))
        roundTrip(value)(codec) ==> value
      }

      test("BigInt round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WithBigIntField]
        val value = WithBigIntField(BigInt("1234567890123456789"))
        roundTrip(value)(codec) ==> value
      }

      test("String round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[String]]
        roundTrip(Box("hello world"))(codec) ==> Box("hello world")
        roundTrip(Box(""))(codec) ==> Box("")
      }

      test("Char round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[Box[Char]]
        roundTrip(Box('A'))(codec) ==> Box('A')
      }
    }

    group("nested structures") {

      test("nested lists round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[NestedLists]
        val value = NestedLists(List(List(1, 2), List(3, 4)))
        roundTrip(value)(codec) ==> value
      }

      test("optional list round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[OptionalList]
        roundTrip(OptionalList(Some(List(1, 2, 3))))(codec) ==> OptionalList(Some(List(1, 2, 3)))
        roundTrip(OptionalList(None))(codec) ==> OptionalList(None)
      }
    }

    group("unicode") {

      test("unicode string round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[UnicodeContent]
        val value = UnicodeContent("Hello \u00e9\u00e8\u00ea \u4f60\u597d \ud83d\ude00")
        roundTrip(value)(codec) ==> value
      }
    }

    group("higher-kinded types") {

      test("HKT with List round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[HigherKindedType[List]]
        val value = HigherKindedType[List](List(1, 2, 3))
        roundTrip(value)(codec) ==> value
      }

      test("HKT with Option round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[HigherKindedType[Option]]
        val value = HigherKindedType[Option](Some(42))
        roundTrip(value)(codec) ==> value
      }
    }

    group("value class edge cases") {

      test("optional wrapped int round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WithOptionalWrapped]
        roundTrip(WithOptionalWrapped(Some(WrappedInt(42))))(codec) ==> WithOptionalWrapped(Some(WrappedInt(42)))
        roundTrip(WithOptionalWrapped(None))(codec) ==> WithOptionalWrapped(None)
      }

      test("list of wrapped ints round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WithWrappedList]
        val value = WithWrappedList(List(WrappedInt(1), WrappedInt(2), WrappedInt(3)))
        roundTrip(value)(codec) ==> value
      }
    }

    group("type aliases") {

      test("type alias round-trip") {
        val codec = KindlingsUBJsonValueCodec.derive[WithAlias]
        val value = WithAlias("Alice", 30)
        roundTrip(value)(codec) ==> value
      }
    }
  }
}
