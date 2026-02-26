package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonKeyCodec,
  JsonReader,
  JsonReaderException,
  JsonValueCodec,
  JsonWriter
}
import hearth.MacroSuite

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

      test("Map[Int, String] encodes with int keys as strings") {
        val codec = KindlingsJsonValueCodec.derive[Map[Int, String]]
        val value = Map(1 -> "a", 2 -> "b")
        val json = writeToString(value)(codec)
        json.contains("\"1\":\"a\"") ==> true
        json.contains("\"2\":\"b\"") ==> true
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("Map[Long, String] encodes with long keys as strings") {
        val codec = KindlingsJsonValueCodec.derive[Map[Long, String]]
        val value = Map(100L -> "x", 200L -> "y")
        val json = writeToString(value)(codec)
        json.contains("\"100\":\"x\"") ==> true
        val decoded = readFromString[Map[Long, String]](json)(codec)
        decoded ==> value
      }

      test("empty Map[Int, String] round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Int, String]]
        val value = Map.empty[Int, String]
        val json = writeToString(value)(codec)
        json ==> "{}"
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("case class with Map[Int, String] field") {
        val codec = KindlingsJsonValueCodec.derive[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a"))
        val json = writeToString(value)(codec)
        json.contains("\"data\":{\"1\":\"a\"}") ==> true
        val decoded = readFromString[WithIntKeyMap](json)(codec)
        decoded ==> value
      }

      test("Map[Int, List[String]] nested") {
        val codec = KindlingsJsonValueCodec.derive[Map[Int, List[String]]]
        val value = Map(1 -> List("a", "b"))
        val json = writeToString(value)(codec)
        json.contains("\"1\":[\"a\",\"b\"]") ==> true
        val decoded = readFromString[Map[Int, List[String]]](json)(codec)
        decoded ==> value
      }

      test("value type key Map[UserId, String] encodes with unwrapped int key") {
        val codec = KindlingsJsonValueCodec.derive[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("enum key Map[CardinalDirection, String] encodes with case name as key") {
        val codec = KindlingsJsonValueCodec.derive[Map[CardinalDirection, String]]
        val value = Map[CardinalDirection, String](North -> "up", South -> "down")
        val json = writeToString(value)(codec)
        json.contains("\"North\":\"up\"") ==> true
        json.contains("\"South\":\"down\"") ==> true
        val decoded = readFromString[Map[CardinalDirection, String]](json)(codec)
        decoded ==> value
      }
    }

    group("key codec derivation") {

      test("Int key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Int, String]]
        val value = Map(42 -> "a")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"a\"") ==> true
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("Long key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Long, String]]
        val value = Map(100L -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Long, String]](json)(codec)
        decoded ==> value
      }

      test("Double key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Double, String]]
        val value = Map(3.14 -> "pi")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Double, String]](json)(codec)
        decoded ==> value
      }

      test("Float key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Float, String]]
        val value = Map(1.5f -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Float, String]](json)(codec)
        decoded ==> value
      }

      test("Short key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Short, String]]
        val value = Map(42.toShort -> "a")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"a\"") ==> true
        val decoded = readFromString[Map[Short, String]](json)(codec)
        decoded ==> value
      }

      test("Boolean key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[Boolean, String]]
        val value = Map(true -> "yes", false -> "no")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Boolean, String]](json)(codec)
        decoded ==> value
      }

      test("BigDecimal key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[BigDecimal, String]]
        val value = Map(BigDecimal("3.14") -> "pi")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[BigDecimal, String]](json)(codec)
        decoded ==> value
      }

      test("BigInt key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[BigInt, String]]
        val value = Map(BigInt(123) -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[BigInt, String]](json)(codec)
        decoded ==> value
      }

      test("user-provided JsonKeyCodec[UserId] is used") {
        @scala.annotation.nowarn("msg=is never used")
        implicit val userIdKeyCodec: JsonKeyCodec[UserId] = new JsonKeyCodec[UserId] {
          def decodeKey(in: JsonReader): UserId = {
            val s = in.readKeyAsString()
            if (s.startsWith("user-")) UserId(s.stripPrefix("user-").toInt)
            else in.decodeError("expected user- prefix")
          }
          def encodeKey(x: UserId, out: JsonWriter): Unit = out.writeKey(s"user-${x.value}")
        }
        val codec = KindlingsJsonValueCodec.derive[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"user-42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("value type key uses unwrap") {
        val codec = KindlingsJsonValueCodec.derive[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("enum key round-trip") {
        val codec = KindlingsJsonValueCodec.derive[Map[CardinalDirection, String]]
        val value = Map[CardinalDirection, String](North -> "up")
        val json = writeToString(value)(codec)
        json.contains("\"North\":\"up\"") ==> true
        val decoded = readFromString[Map[CardinalDirection, String]](json)(codec)
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

    group("Scala Enumeration (enumAsStrings)") {

      test("Scala Enumeration round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[ScalaColor.Value]
        val json = writeToString[ScalaColor.Value](ScalaColor.Red)(codec)
        json ==> "\"Red\""
        readFromString[ScalaColor.Value](json)(codec) ==> ScalaColor.Red
      }

      test("all Scala Enumeration values round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[ScalaColor.Value]
        Seq(ScalaColor.Red, ScalaColor.Green, ScalaColor.Blue).foreach { v =>
          readFromString[ScalaColor.Value](writeToString[ScalaColor.Value](v)(codec))(codec) ==> v
        }
      }
    }

    // Java enum tests are in KindlingsJsonValueCodecJvmSpec (src/test/scalajvm)

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
        val error = intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
        assert(error.getMessage.contains("extraField"))
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

  group("higher-kinded types") {

    test("HigherKindedType[List] round-trip") {
      val codec = KindlingsJsonValueCodec.derive[HigherKindedType[List]]
      val value = HigherKindedType[List](List(1, 2, 3))
      val json = writeToString(value)(codec)
      val decoded = readFromString[HigherKindedType[List]](json)(codec)
      decoded ==> value
    }

    test("HigherKindedType[Option] round-trip") {
      val codec = KindlingsJsonValueCodec.derive[HigherKindedType[Option]]
      val value = HigherKindedType[Option](Some(42))
      val json = writeToString(value)(codec)
      val decoded = readFromString[HigherKindedType[Option]](json)(codec)
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

  group("KindlingsJsonValueCodec") {
    group("per-field annotations") {

      test("@fieldName encodes with custom name") {
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithFieldName]
        val json = writeToString(JsoniterWithFieldName("Alice", 30))(codec)
        json.contains("\"user_name\"") ==> true
        json.contains("\"userName\"") ==> false
      }

      test("@fieldName decodes with custom name") {
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithFieldName]
        val decoded = readFromString[JsoniterWithFieldName]("""{"user_name":"Alice","age":30}""")(codec)
        decoded ==> JsoniterWithFieldName("Alice", 30)
      }

      test("@fieldName overrides config fieldNameMapper") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithFieldName]
        val json = writeToString(JsoniterWithFieldName("Alice", 30))(codec)
        // @fieldName("user_name") takes precedence, age uses snake_case from config (already snake_case)
        json.contains("\"user_name\"") ==> true
      }

      test("@transientField excludes field from encoding") {
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithTransient]
        val json = writeToString(JsoniterWithTransient("Alice", Some("cached")))(codec)
        json.contains("\"cache\"") ==> false
        json.contains("\"name\"") ==> true
      }

      test("@transientField decodes without the field") {
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithTransient]
        val decoded = readFromString[JsoniterWithTransient]("""{"name":"Alice"}""")(codec)
        decoded ==> JsoniterWithTransient("Alice", None)
      }

      test("both annotations combined") {
        val codec = KindlingsJsonValueCodec.derive[JsoniterWithBothAnnotations]
        val value = JsoniterWithBothAnnotations("Alice", 42, active = true)
        val json = writeToString(value)(codec)
        json.contains("\"display_name\"") ==> true
        json.contains("\"internal\"") ==> false
        json.contains("\"active\"") ==> true
        val decoded = readFromString[JsoniterWithBothAnnotations]("""{"display_name":"Alice","active":true}""")(codec)
        decoded ==> JsoniterWithBothAnnotations("Alice", 0, active = true)
      }

      test("@transientField without default is compile error") {
        compileErrors(
          """
          import hearth.kindlings.jsoniterderivation.annotations.transientField
          case class BadTransient(name: String, @transientField noDefault: Int)
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derive[BadTransient]
          """
        ).check(
          "@transientField on field 'noDefault'"
        )
      }
    }

    group("@stringified") {

      test("@stringified Int encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derive[WithStringifiedInt]
        val json = writeToString(WithStringifiedInt(42, "Alice"))(codec)
        json.contains("\"42\"") ==> true
        val decoded = readFromString[WithStringifiedInt](json)(codec)
        decoded ==> WithStringifiedInt(42, "Alice")
      }

      test("@stringified Long encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derive[WithStringifiedLong]
        val json = writeToString(WithStringifiedLong(123456789L, "test"))(codec)
        json.contains("\"123456789\"") ==> true
        val decoded = readFromString[WithStringifiedLong](json)(codec)
        decoded ==> WithStringifiedLong(123456789L, "test")
      }

      test("@stringified BigDecimal encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derive[WithStringifiedBigDecimal]
        val json = writeToString(WithStringifiedBigDecimal(BigDecimal("3.14")))(codec)
        json.contains("\"3.14\"") ==> true
        val decoded = readFromString[WithStringifiedBigDecimal](json)(codec)
        decoded ==> WithStringifiedBigDecimal(BigDecimal("3.14"))
      }

      test("mixed @stringified and normal fields") {
        val codec = KindlingsJsonValueCodec.derive[WithMixedStringified]
        val value = WithMixedStringified(42, "Alice", 3.14)
        val json = writeToString(value)(codec)
        // count should be stringified, name should be normal, score should be stringified
        json.contains("\"42\"") ==> true
        json.contains("\"3.14\"") ==> true
        val decoded = readFromString[WithMixedStringified](json)(codec)
        decoded ==> value
      }

      test("@stringified on non-numeric field is compile error") {
        compileErrors(
          """
          import hearth.kindlings.jsoniterderivation.annotations.stringified
          case class BadStringified(name: String, @stringified label: String)
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derive[BadStringified]
          """
        ).check(
          "@stringified on field 'label'"
        )
      }
    }

    group("mapAsArray") {

      test("Map[String, Int] with mapAsArray encodes as array of pairs") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val json = writeToString(Map("a" -> 1, "b" -> 2))(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> Map("a" -> 1, "b" -> 2)
      }

      test("Map[Int, String] with mapAsArray encodes as array of pairs") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derive[Map[Int, String]]
        val json = writeToString(Map(1 -> "a", 2 -> "b"))(codec)
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> Map(1 -> "a", 2 -> "b")
      }

      test("empty map with mapAsArray encodes as empty array") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val json = writeToString(Map.empty[String, Int])(codec)
        json ==> "[]"
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> Map.empty[String, Int]
      }

      test("default config uses object-style encoding") {
        val codec = KindlingsJsonValueCodec.derive[Map[String, Int]]
        val json = writeToString(Map("a" -> 1))(codec)
        json.contains("{") ==> true
        json.contains("[") ==> false
      }

      test("case class with map field and mapAsArray") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derive[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a", 2 -> "b"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithIntKeyMap](json)(codec)
        decoded ==> value
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
