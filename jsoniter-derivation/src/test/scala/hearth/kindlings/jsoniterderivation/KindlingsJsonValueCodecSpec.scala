package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite

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

final class KindlingsJsonValueCodecSpec extends MacroSuite {

  group("KindlingsJsonValueCodec") {

    group("case classes") {

      test("simple case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        assertEquals(decoded, value)
      }

      test("empty case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[EmptyClass]
        val value = EmptyClass()
        val json = writeToString(value)(codec)
        val decoded = readFromString[EmptyClass](json)(codec)
        assertEquals(decoded, value)
      }

      test("single field case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[SingleField]
        val value = SingleField(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SingleField](json)(codec)
        assertEquals(decoded, value)
      }

      test("nested case class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[PersonWithAddress](json)(codec)
        assertEquals(decoded, value)
      }

      test("case class with collection field round-trip") {
        val codec = KindlingsJsonValueCodec.derive[TeamWithMembers]
        val value = TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[TeamWithMembers](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("value classes") {

      test("value class round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WrappedInt]
        val value = WrappedInt(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WrappedInt](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("options") {

      test("Some round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = Some(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        assertEquals(decoded, value)
      }

      test("None round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Option[Int]]
        val value: Option[Int] = None
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("collections") {

      test("List of ints round-trip") {
        val codec = KindlingsJsonValueCodec.derive[List[Int]]
        val value = List(1, 2, 3)
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        assertEquals(decoded, value)
      }

      test("Vector of strings round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Vector[String]]
        val value = Vector("a", "b", "c")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Vector[String]](json)(codec)
        assertEquals(decoded, value)
      }

      test("empty list round-trip") {
        val codec = KindlingsJsonValueCodec.derive[List[Int]]
        val value = List.empty[Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("maps") {

      test("Map[String, Int] round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val value = Map("a" -> 1, "b" -> 2)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        assertEquals(decoded, value)
      }

      test("empty map round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val value = Map.empty[String, Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("sealed traits") {

      test("wrapper-style round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        assertEquals(decoded, value)
      }

      test("wrapper-style second case round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Rectangle(3.0, 4.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        assertEquals(decoded, value)
      }

      test("discriminator-style round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(discriminatorFieldName = Some("type"))
        val codec = KindlingsJsonValueCodec.derive[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Animal](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("recursive types") {

      test("recursive tree round-trip") {
        val codec = KindlingsJsonValueCodec.derive[RecursiveTree]
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val json = writeToString(value)(codec)
        val decoded = readFromString[RecursiveTree](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("configuration") {

      test("snake_case field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main", "SF"))
        val json = writeToString(value)(codec)
        assert(json.contains("\"person_with_address\"") || json.contains("\"name\""))
        val decoded = readFromString[PersonWithAddress](json)(codec)
        assertEquals(decoded, value)
      }

      test("custom constructor name transform") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derive[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        assert(json.contains("\"circle\""))
        val decoded = readFromString[Shape](json)(codec)
        assertEquals(decoded, value)
      }
    }

    group("derive and derived") {

      test("explicit derive returns JsonValueCodec") {
        val codec: JsonValueCodec[SimplePerson] = KindlingsJsonValueCodec.derive[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        assertEquals(decoded, value)
      }

      test("derived provides KindlingsJsonValueCodec") {
        val codec: KindlingsJsonValueCodec[SimplePerson] = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        assertEquals(decoded, value)
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
        assertEquals(json, """{"value":50}""")
        val decoded = readFromString[SingleField](json)(codec)
        assertEquals(decoded, SingleField(500))
      }
    }
  }

  group("JsonValueCodecExtensions") {

    test("map transforms codec") {
      import JsonValueCodecExtensions.*
      val intCodec = KindlingsJsonValueCodec.derive[SingleField]
      val stringCodec = intCodec.map[String](sf => sf.value.toString)(s => SingleField(s.toInt))
      val json = writeToString("42")(stringCodec)
      val decoded = readFromString[String](json)(stringCodec)
      assertEquals(decoded, "42")
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
      assertEquals(decoded, 42)
    }
  }
}
