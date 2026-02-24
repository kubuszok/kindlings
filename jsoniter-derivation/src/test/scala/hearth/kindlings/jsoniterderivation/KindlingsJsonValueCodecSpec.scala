package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

case class CamelCasePerson(firstName: String, lastName: String)
case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])
case class RecursiveTree(value: Int, children: List[RecursiveTree])
final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object JsoniterAliases {
  type Name = String
}
case class WithAlias(name: JsoniterAliases.Name, age: Int)

final class KindlingsJsonValueCodecSpec extends MacroSuite {

  group("KindlingsJsonValueCodec") {

    group("case classes") {

      test("simple case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("empty case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[EmptyClass]
        val value = EmptyClass()
        val json = writeToString(value)(codec)
        val decoded = readFromString[EmptyClass](json)(codec)
        decoded ==> value
      }

      test("single field case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[SingleField]
        val value = SingleField(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SingleField](json)(codec)
        decoded ==> value
      }

      test("nested case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[PersonWithAddress](json)(codec)
        decoded ==> value
      }

      test("case class with collection field round-trip") {
        val codec = KindlingsJsonValueCodec.derive[TeamWithMembers]
        val value = TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[TeamWithMembers](json)(codec)
        decoded ==> value
      }
    }

    group("value classes") {

      test("value class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WrappedInt]
        val value = WrappedInt(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WrappedInt](json)(codec)
        decoded ==> value
      }
    }

    group("options") {

      test("Some round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = Some(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        decoded ==> value
      }

      test("None round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = None
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("collections") {

      test("List of ints round-trip") {
        val codec = KindlingsJsonValueCodec.derive[List[Int]]
        val value = List(1, 2, 3)
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        decoded ==> value
      }

      test("Vector of strings round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Vector[String]]
        val value = Vector("a", "b", "c")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Vector[String]](json)(codec)
        decoded ==> value
      }

      test("empty list round-trip") {
        val codec = KindlingsJsonValueCodec.derive[List[Int]]
        val value = List.empty[Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("maps") {

      test("Map[String, Int] round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val value = Map("a" -> 1, "b" -> 2)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> value
      }

      test("empty map round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val value = Map.empty[String, Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> value
      }
    }

    group("sealed traits") {

      test("wrapper-style round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("wrapper-style second case round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Rectangle(3.0, 4.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("discriminator-style round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(discriminatorFieldName = Some("type"))
        val codec = KindlingsJsonValueCodec.derive[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Animal](json)(codec)
        decoded ==> value
      }
    }

    group("string enum encoding (enumAsStrings)") {

      test("encode case-object-only sealed trait as string") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[CardinalDirection]
        writeToString[CardinalDirection](North)(codec) ==> "\"North\""
      }

      test("round-trip all cases as strings") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[CardinalDirection]
        List[CardinalDirection](North, South, East, West).foreach { dir =>
          val json = writeToString[CardinalDirection](dir)(codec)
          val decoded = readFromString[CardinalDirection](json)(codec)
          decoded ==> dir
        }
      }

      test("enum as string with constructor name transform") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(enumAsStrings = true, adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derive[CardinalDirection]
        writeToString[CardinalDirection](North)(codec) ==> "\"north\""
        readFromString[CardinalDirection]("\"north\"")(codec) ==> (North: CardinalDirection)
      }

      test("enumAsStrings=false still uses wrapper-style") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = false)
        val codec = KindlingsJsonValueCodec.derive[CardinalDirection]
        val json = writeToString[CardinalDirection](North)(codec)
        assert(json.contains("\"North\""))
        assert(json.contains("{"))
      }
    }

    group("recursive types") {

      test("recursive tree round-trip") {
        val codec = KindlingsJsonValueCodec.derive[RecursiveTree]
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val json = writeToString(value)(codec)
        val decoded = readFromString[RecursiveTree](json)(codec)
        decoded ==> value
      }
    }

    group("auto-derivation") {

      test("derived is available as implicit") {
        val codec = implicitly[KindlingsJsonValueCodec[SimplePerson]]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("nested types derived automatically") {
        val codec = implicitly[KindlingsJsonValueCodec[PersonWithAddress]]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[PersonWithAddress](json)(codec)
        decoded ==> value
      }

      test("auto-derivation uses custom implicit config") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
        val codec = implicitly[KindlingsJsonValueCodec[CamelCasePerson]]
        val value = CamelCasePerson("Alice", "Smith")
        val json = writeToString(value)(codec)
        json.contains("\"first-name\"") ==> true
        json.contains("\"last-name\"") ==> true
        val decoded = readFromString[CamelCasePerson](json)(codec)
        decoded ==> value
      }
    }

    group("configuration") {

      test("snake_case field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main", "SF"))
        val json = writeToString(value)(codec)
        (json.contains("\"person_with_address\"") || json.contains("\"name\"")) ==> true
        val decoded = readFromString[PersonWithAddress](json)(codec)
        decoded ==> value
      }

      test("kebab-case field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        val json = writeToString(value)(codec)
        json.contains("\"first-name\"") ==> true
        json.contains("\"last-name\"") ==> true
        val decoded = readFromString[CamelCasePerson](json)(codec)
        decoded ==> value
      }

      test("PascalCase field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withPascalCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        json.contains("\"Name\"") ==> true
        json.contains("\"Age\"") ==> true
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("SCREAMING_SNAKE_CASE field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withScreamingSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        val json = writeToString(value)(codec)
        json.contains("\"FIRST_NAME\"") ==> true
        json.contains("\"LAST_NAME\"") ==> true
        val decoded = readFromString[CamelCasePerson](json)(codec)
        decoded ==> value
      }

      test("snake_case ADT leaf class names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("kebab-case ADT leaf class names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("discriminator with ADT name mapper") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withDiscriminator("type").withSnakeCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derive[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        json.contains("\"type\":\"dog\"") ==> true
        val decoded = readFromString[Animal](json)(codec)
        decoded ==> value
      }

      test("custom constructor name transform") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("skipUnexpectedFields=true (default) ignores extra fields") {
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val json = """{"name":"Alice","extraField":"ignored","age":30}"""
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> SimplePerson("Alice", 30)
      }

      test("skipUnexpectedFields=false rejects extra fields") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSkipUnexpectedFields(false)
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val json = """{"name":"Alice","extraField":"boom","age":30}"""
        intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
      }
    }

    group("sets") {

      test("Set of ints round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Set[Int]]
        val value = Set(1, 2, 3)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Set[Int]](json)(codec)
        decoded ==> value
      }

      test("empty set round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Set[Int]]
        val value = Set.empty[Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[Set[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("error handling") {

      test("missing field uses null/default value") {
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val json = """{"name":"Alice"}"""
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> SimplePerson("Alice", 0)
      }
    }

    group("derive and derived") {

      test("explicit derive returns JsonValueCodec") {
        val codec: JsonValueCodec[SimplePerson] = KindlingsJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("derived provides KindlingsJsonValueCodec") {
        val codec: KindlingsJsonValueCodec[SimplePerson] = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }
    }

    group("user-provided implicit priority") {

      test("user-provided codec for nested field is used over derivation") {
        // User-provided implicits for NESTED types take priority (the derived type itself is always derived)
        @scala.annotation.nowarn("msg=is never used")
        implicit val customIntCodec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
          def nullValue: Int = 0
          def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Int): Int =
            in.readInt() * 10
          def encodeValue(x: Int, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
            out.writeVal(x * 10)
        }
        val codec = KindlingsJsonValueCodec.derive[SingleField]
        val json = writeToString(SingleField(5))(codec)
        json ==> """{"value":50}"""
        val decoded = readFromString[SingleField](json)(codec)
        decoded ==> SingleField(500)
      }
    }
  }

  group("tuples") {

    test("(Int, String) round-trip") {
      val codec = KindlingsJsonValueCodec.derive[(Int, String)]
      val value = (42, "hello")
      val json = writeToString(value)(codec)
      val decoded = readFromString[(Int, String)](json)(codec)
      decoded ==> value
    }

    test("(Int, String, Boolean) round-trip") {
      val codec = KindlingsJsonValueCodec.derive[(Int, String, Boolean)]
      val value = (42, "hello", true)
      val json = writeToString(value)(codec)
      val decoded = readFromString[(Int, String, Boolean)](json)(codec)
      decoded ==> value
    }
  }

  group("generic case classes") {

    test("Box[Int] round-trip") {
      val codec = KindlingsJsonValueCodec.derive[Box[Int]]
      val value = Box(42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[Box[Int]](json)(codec)
      decoded ==> value
    }

    test("Pair[String, Int] round-trip") {
      val codec = KindlingsJsonValueCodec.derive[Pair[String, Int]]
      val value = Pair("hello", 42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[Pair[String, Int]](json)(codec)
      decoded ==> value
    }
  }

  group("deeply nested") {

    test("PersonFull with 3-level nesting round-trip") {
      val codec = KindlingsJsonValueCodec.derive[PersonFull]
      val value = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
      val json = writeToString(value)(codec)
      val decoded = readFromString[PersonFull](json)(codec)
      decoded ==> value
    }
  }

  group("type aliases") {

    test("WithAlias round-trip") {
      val codec = KindlingsJsonValueCodec.derive[WithAlias]
      val value = WithAlias("Alice", 30)
      val json = writeToString(value)(codec)
      val decoded = readFromString[WithAlias](json)(codec)
      decoded ==> value
    }
  }

  group("combined configuration") {

    test("snake_case + discriminator + constructor transform") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        .withDiscriminator("type")
        .withSnakeCaseAdtLeafClassNames
      val codec = KindlingsJsonValueCodec.derive[Animal]
      val value: Animal = Dog("Rex", "Labrador")
      val json = writeToString(value)(codec)
      json.contains("\"type\":\"dog\"") ==> true
      val decoded = readFromString[Animal](json)(codec)
      decoded ==> value
    }
  }

  group("JsonValueCodecExtensions") {

    test("map transforms codec") {
      import JsonValueCodecExtensions.*
      val intCodec = KindlingsJsonValueCodec.derive[SingleField]
      val stringCodec = intCodec.map[String](sf => sf.value.toString)(s => SingleField(s.toInt))
      val json = writeToString("42")(stringCodec)
      val decoded = readFromString[String](json)(stringCodec)
      decoded ==> "42"
    }

    test("mapDecode with Right") {
      import JsonValueCodecExtensions.*
      val intCodec = KindlingsJsonValueCodec.derive[SingleField]
      val positiveCodec =
        intCodec.mapDecode[Int](sf => if (sf.value > 0) Right(sf.value) else Left("must be positive"))(v =>
          SingleField(v)
        )
      val json = writeToString(42)(positiveCodec)
      val decoded = readFromString[Int](json)(positiveCodec)
      decoded ==> 42
    }
  }
}
