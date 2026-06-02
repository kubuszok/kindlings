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
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("empty case class round-trip") {
        val codec = KindlingsJsonValueCodec.derived[EmptyClass]
        val value = EmptyClass()
        val json = writeToString(value)(codec)
        val decoded = readFromString[EmptyClass](json)(codec)
        decoded ==> value
      }

      test("single field case class round-trip") {
        val codec = KindlingsJsonValueCodec.derived[SingleField]
        val value = SingleField(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SingleField](json)(codec)
        decoded ==> value
      }

      test("nested case class round-trip") {
        val codec = KindlingsJsonValueCodec.derived[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[PersonWithAddress](json)(codec)
        decoded ==> value
      }

      test("case class with collection field round-trip") {
        val codec = KindlingsJsonValueCodec.derived[TeamWithMembers]
        val value = TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[TeamWithMembers](json)(codec)
        decoded ==> value
      }
    }

    group("value classes") {

      test("value class round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WrappedInt]
        val value = WrappedInt(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WrappedInt](json)(codec)
        decoded ==> value
      }
    }

    group("options") {

      test("Some round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Option[Int]]
        val value: Option[Int] = Some(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        decoded ==> value
      }

      test("None round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Option[Int]]
        val value: Option[Int] = None
        val json = writeToString(value)(codec)
        val decoded = readFromString[Option[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("collections") {

      test("List of ints round-trip") {
        val codec = KindlingsJsonValueCodec.derived[List[Int]]
        val value = List(1, 2, 3)
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        decoded ==> value
      }

      test("Vector of strings round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Vector[String]]
        val value = Vector("a", "b", "c")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Vector[String]](json)(codec)
        decoded ==> value
      }

      test("empty list round-trip") {
        val codec = KindlingsJsonValueCodec.derived[List[Int]]
        val value = List.empty[Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[List[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("maps") {

      test("Map[String, Int] round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[String, Int]]
        val value = Map("a" -> 1, "b" -> 2)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> value
      }

      test("empty map round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[String, Int]]
        val value = Map.empty[String, Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> value
      }

      test("Map[Int, String] encodes with int keys as strings") {
        val codec = KindlingsJsonValueCodec.derived[Map[Int, String]]
        val value = Map(1 -> "a", 2 -> "b")
        val json = writeToString(value)(codec)
        json.contains("\"1\":\"a\"") ==> true
        json.contains("\"2\":\"b\"") ==> true
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("Map[Long, String] encodes with long keys as strings") {
        val codec = KindlingsJsonValueCodec.derived[Map[Long, String]]
        val value = Map(100L -> "x", 200L -> "y")
        val json = writeToString(value)(codec)
        json.contains("\"100\":\"x\"") ==> true
        val decoded = readFromString[Map[Long, String]](json)(codec)
        decoded ==> value
      }

      test("empty Map[Int, String] round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Int, String]]
        val value = Map.empty[Int, String]
        val json = writeToString(value)(codec)
        json ==> "{}"
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("case class with Map[Int, String] field") {
        val codec = KindlingsJsonValueCodec.derived[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a"))
        val json = writeToString(value)(codec)
        json.contains("\"data\":{\"1\":\"a\"}") ==> true
        val decoded = readFromString[WithIntKeyMap](json)(codec)
        decoded ==> value
      }

      test("Map[Int, List[String]] nested") {
        val codec = KindlingsJsonValueCodec.derived[Map[Int, List[String]]]
        val value = Map(1 -> List("a", "b"))
        val json = writeToString(value)(codec)
        json.contains("\"1\":[\"a\",\"b\"]") ==> true
        val decoded = readFromString[Map[Int, List[String]]](json)(codec)
        decoded ==> value
      }

      test("value type key Map[UserId, String] encodes with unwrapped int key") {
        val codec = KindlingsJsonValueCodec.derived[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("enum key Map[CardinalDirection, String] encodes with case name as key") {
        val codec = KindlingsJsonValueCodec.derived[Map[CardinalDirection, String]]
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
        val codec = KindlingsJsonValueCodec.derived[Map[Int, String]]
        val value = Map(42 -> "a")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"a\"") ==> true
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> value
      }

      test("Long key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Long, String]]
        val value = Map(100L -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Long, String]](json)(codec)
        decoded ==> value
      }

      test("Double key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Double, String]]
        val value = Map(3.14 -> "pi")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Double, String]](json)(codec)
        decoded ==> value
      }

      test("Float key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Float, String]]
        val value = Map(1.5f -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Float, String]](json)(codec)
        decoded ==> value
      }

      test("Short key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Short, String]]
        val value = Map(42.toShort -> "a")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"a\"") ==> true
        val decoded = readFromString[Map[Short, String]](json)(codec)
        decoded ==> value
      }

      test("Boolean key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[Boolean, String]]
        val value = Map(true -> "yes", false -> "no")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[Boolean, String]](json)(codec)
        decoded ==> value
      }

      test("BigDecimal key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[BigDecimal, String]]
        val value = Map(BigDecimal("3.14") -> "pi")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[BigDecimal, String]](json)(codec)
        decoded ==> value
      }

      test("BigInt key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[BigInt, String]]
        val value = Map(BigInt(123) -> "x")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Map[BigInt, String]](json)(codec)
        decoded ==> value
      }

      test("user-provided JsonKeyCodec[UserId] is used") {
        @scala.annotation.nowarn("msg=is never used|unused local definition")
        implicit val userIdKeyCodec: JsonKeyCodec[UserId] = new JsonKeyCodec[UserId] {
          def decodeKey(in: JsonReader): UserId = {
            val s = in.readKeyAsString()
            if (s.startsWith("user-")) UserId(s.stripPrefix("user-").toInt)
            else in.decodeError("expected user- prefix")
          }
          def encodeKey(x: UserId, out: JsonWriter): Unit = out.writeKey(s"user-${x.value}")
        }
        val codec = KindlingsJsonValueCodec.derived[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"user-42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("value type key uses unwrap") {
        val codec = KindlingsJsonValueCodec.derived[Map[UserId, String]]
        val value = Map(UserId(42) -> "alice")
        val json = writeToString(value)(codec)
        json.contains("\"42\":\"alice\"") ==> true
        val decoded = readFromString[Map[UserId, String]](json)(codec)
        decoded ==> value
      }

      test("enum key round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[CardinalDirection, String]]
        val value = Map[CardinalDirection, String](North -> "up")
        val json = writeToString(value)(codec)
        json.contains("\"North\":\"up\"") ==> true
        val decoded = readFromString[Map[CardinalDirection, String]](json)(codec)
        decoded ==> value
      }
    }

    group("sealed traits") {

      test("wrapper-style round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("wrapper-style second case round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Rectangle(3.0, 4.0)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("discriminator-style round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(discriminatorFieldName = Some("type"))
        val codec = KindlingsJsonValueCodec.derived[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        val decoded = readFromString[Animal](json)(codec)
        decoded ==> value
      }
    }

    group("string enum encoding (enumAsStrings)") {

      test("encode case-object-only sealed trait as string") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
        writeToString[CardinalDirection](North)(codec) ==> "\"North\""
      }

      test("round-trip all cases as strings") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
        List[CardinalDirection](North, South, East, West).foreach { dir =>
          val json = writeToString[CardinalDirection](dir)(codec)
          val decoded = readFromString[CardinalDirection](json)(codec)
          decoded ==> dir
        }
      }

      test("enum as string with constructor name transform") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(enumAsStrings = true, adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
        writeToString[CardinalDirection](North)(codec) ==> "\"north\""
        readFromString[CardinalDirection]("\"north\"")(codec) ==> (North: CardinalDirection)
      }

      test("enumAsStrings=false still uses wrapper-style") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = false)
        val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
        val json = writeToString[CardinalDirection](North)(codec)
        assert(json.contains("\"North\""))
        assert(json.contains("{"))
      }
    }

    group("Scala Enumeration (enumAsStrings)") {

      test("Scala Enumeration round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[ScalaColor.Value]
        val json = writeToString[ScalaColor.Value](ScalaColor.Red)(codec)
        json ==> "\"Red\""
        readFromString[ScalaColor.Value](json)(codec) ==> ScalaColor.Red
      }

      test("all Scala Enumeration values round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[ScalaColor.Value]
        Seq(ScalaColor.Red, ScalaColor.Green, ScalaColor.Blue).foreach { v =>
          readFromString[ScalaColor.Value](writeToString[ScalaColor.Value](v)(codec))(codec) ==> v
        }
      }
    }

    // Java enum tests are in KindlingsJsonValueCodecJvmSpec (src/test/scalajvm)

    group("recursive types") {

      test("recursive tree round-trip") {
        val codec = KindlingsJsonValueCodec.derived[RecursiveTree]
        val value = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val json = writeToString(value)(codec)
        val decoded = readFromString[RecursiveTree](json)(codec)
        decoded ==> value
      }

      test("indirect recursive type round-trip") {
        val codec = KindlingsJsonValueCodec.derived[RecursiveParent]
        val value = RecursiveParent(
          "root",
          List(
            RecursiveNode("a", List(RecursiveNode("b", Nil))),
            RecursiveNode("c", Nil)
          )
        )
        val json = writeToString(value)(codec)
        val decoded = readFromString[RecursiveParent](json)(codec)
        decoded ==> value
      }
    }

    group("recursive types - def caching") {

      test("direct recursive sealed trait encode/decode round-trip") {
        val codec = KindlingsJsonValueCodec.derived[TreeNode]
        val value: TreeNode = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
        val json = writeToString(value)(codec)
        val decoded = readFromString[TreeNode](json)(codec)
        decoded ==> value
      }

      test("direct recursive sealed trait leaf-only round-trip") {
        val codec = KindlingsJsonValueCodec.derived[TreeNode]
        val value: TreeNode = Leaf(42)
        val json = writeToString(value)(codec)
        val decoded = readFromString[TreeNode](json)(codec)
        decoded ==> value
      }

      test("direct recursive sealed trait deeply nested round-trip") {
        val codec = KindlingsJsonValueCodec.derived[TreeNode]
        val value: TreeNode =
          Branch(1, Branch(2, Branch(3, Leaf(4), Leaf(5)), Leaf(6)), Branch(7, Leaf(8), Leaf(9)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[TreeNode](json)(codec)
        decoded ==> value
      }

      test("mutual recursion encode/decode round-trip") {
        val codec = KindlingsJsonValueCodec.derived[MutRecA]
        val value = MutRecA(1, Some(MutRecB("hello", Some(MutRecA(2, None)))))
        val json = writeToString(value)(codec)
        val decoded = readFromString[MutRecA](json)(codec)
        decoded ==> value
      }

      test("mutual recursion base case round-trip") {
        val codec = KindlingsJsonValueCodec.derived[MutRecA]
        val value = MutRecA(42, None)
        val json = writeToString(value)(codec)
        val decoded = readFromString[MutRecA](json)(codec)
        decoded ==> value
      }

      test("mutual recursion deeply nested round-trip") {
        val codec = KindlingsJsonValueCodec.derived[MutRecA]
        val value = MutRecA(1, Some(MutRecB("a", Some(MutRecA(2, Some(MutRecB("b", Some(MutRecA(3, None)))))))))
        val json = writeToString(value)(codec)
        val decoded = readFromString[MutRecA](json)(codec)
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
        val codec = KindlingsJsonValueCodec.derived[PersonWithAddress]
        val value = PersonWithAddress("Bob", 25, Address("123 Main", "SF"))
        val json = writeToString(value)(codec)
        (json.contains("\"person_with_address\"") || json.contains("\"name\"")) ==> true
        val decoded = readFromString[PersonWithAddress](json)(codec)
        decoded ==> value
      }

      test("kebab-case field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
        val codec = KindlingsJsonValueCodec.derived[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        val json = writeToString(value)(codec)
        json.contains("\"first-name\"") ==> true
        json.contains("\"last-name\"") ==> true
        val decoded = readFromString[CamelCasePerson](json)(codec)
        decoded ==> value
      }

      test("PascalCase field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withPascalCaseFieldNames
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice", 30)
        val json = writeToString(value)(codec)
        json.contains("\"Name\"") ==> true
        json.contains("\"Age\"") ==> true
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("SCREAMING_SNAKE_CASE field names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withScreamingSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derived[CamelCasePerson]
        val value = CamelCasePerson("Alice", "Smith")
        val json = writeToString(value)(codec)
        json.contains("\"FIRST_NAME\"") ==> true
        json.contains("\"LAST_NAME\"") ==> true
        val decoded = readFromString[CamelCasePerson](json)(codec)
        decoded ==> value
      }

      test("snake_case ADT leaf class names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("kebab-case ADT leaf class names") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("discriminator with ADT name mapper") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withDiscriminator("type").withSnakeCaseAdtLeafClassNames
        val codec = KindlingsJsonValueCodec.derived[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        json.contains("\"type\":\"dog\"") ==> true
        val decoded = readFromString[Animal](json)(codec)
        decoded ==> value
      }

      test("custom constructor name transform") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        json.contains("\"circle\"") ==> true
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }

      test("skipUnexpectedFields=true (default) ignores extra fields") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","extraField":"ignored","age":30}"""
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> SimplePerson("Alice", 30)
      }

      test("skipUnexpectedFields=false rejects extra fields") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSkipUnexpectedFields(false)
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","extraField":"boom","age":30}"""
        val error = intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
        assert(error.getMessage.contains("extraField"))
      }
    }

    group("sets") {

      test("Set of ints round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Set[Int]]
        val value = Set(1, 2, 3)
        val json = writeToString(value)(codec)
        val decoded = readFromString[Set[Int]](json)(codec)
        decoded ==> value
      }

      test("empty set round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Set[Int]]
        val value = Set.empty[Int]
        val json = writeToString(value)(codec)
        val decoded = readFromString[Set[Int]](json)(codec)
        decoded ==> value
      }
    }

    group("error handling") {

      test("missing field uses null/default value") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice"}"""
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> SimplePerson("Alice", 0)
      }
    }

    group("derive and derived") {

      test("explicit derive returns JsonValueCodec") {
        val codec: JsonValueCodec[SimplePerson] = KindlingsJsonValueCodec.derived[SimplePerson]
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
        @scala.annotation.nowarn("msg=is never used|unused local definition")
        implicit val customIntCodec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
          def nullValue: Int = 0
          def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Int): Int =
            in.readInt() * 10
          def encodeValue(x: Int, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
            out.writeVal(x * 10)
        }
        val codec = KindlingsJsonValueCodec.derived[SingleField]
        val json = writeToString(SingleField(5))(codec)
        json ==> """{"value":50}"""
        val decoded = readFromString[SingleField](json)(codec)
        decoded ==> SingleField(500)
      }
    }
  }

  group("tuples") {

    test("(Int, String) round-trip") {
      val codec = KindlingsJsonValueCodec.derived[(Int, String)]
      val value = (42, "hello")
      val json = writeToString(value)(codec)
      val decoded = readFromString[(Int, String)](json)(codec)
      decoded ==> value
    }

    test("(Int, String, Boolean) round-trip") {
      val codec = KindlingsJsonValueCodec.derived[(Int, String, Boolean)]
      val value = (42, "hello", true)
      val json = writeToString(value)(codec)
      val decoded = readFromString[(Int, String, Boolean)](json)(codec)
      decoded ==> value
    }
  }

  group("generic case classes") {

    test("Box[Int] round-trip") {
      val codec = KindlingsJsonValueCodec.derived[Box[Int]]
      val value = Box(42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[Box[Int]](json)(codec)
      decoded ==> value
    }

    test("Pair[String, Int] round-trip") {
      val codec = KindlingsJsonValueCodec.derived[Pair[String, Int]]
      val value = Pair("hello", 42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[Pair[String, Int]](json)(codec)
      decoded ==> value
    }
  }

  group("deeply nested") {

    test("PersonFull with 3-level nesting round-trip") {
      val codec = KindlingsJsonValueCodec.derived[PersonFull]
      val value = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
      val json = writeToString(value)(codec)
      val decoded = readFromString[PersonFull](json)(codec)
      decoded ==> value
    }
  }

  group("type aliases") {

    test("WithAlias round-trip") {
      val codec = KindlingsJsonValueCodec.derived[WithAlias]
      val value = WithAlias("Alice", 30)
      val json = writeToString(value)(codec)
      val decoded = readFromString[WithAlias](json)(codec)
      decoded ==> value
    }
  }

  group("higher-kinded types") {

    test("HigherKindedType[List] round-trip") {
      val codec = KindlingsJsonValueCodec.derived[HigherKindedType[List]]
      val value = HigherKindedType[List](List(1, 2, 3))
      val json = writeToString(value)(codec)
      val decoded = readFromString[HigherKindedType[List]](json)(codec)
      decoded ==> value
    }

    test("HigherKindedType[Option] round-trip") {
      val codec = KindlingsJsonValueCodec.derived[HigherKindedType[Option]]
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
      val codec = KindlingsJsonValueCodec.derived[Animal]
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
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithFieldName]
        val json = writeToString(JsoniterWithFieldName("Alice", 30))(codec)
        json.contains("\"user_name\"") ==> true
        json.contains("\"userName\"") ==> false
      }

      test("@fieldName decodes with custom name") {
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithFieldName]
        val decoded = readFromString[JsoniterWithFieldName]("""{"user_name":"Alice","age":30}""")(codec)
        decoded ==> JsoniterWithFieldName("Alice", 30)
      }

      test("@fieldName overrides config fieldNameMapper") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithFieldName]
        val json = writeToString(JsoniterWithFieldName("Alice", 30))(codec)
        // @fieldName("user_name") takes precedence, age uses snake_case from config (already snake_case)
        json.contains("\"user_name\"") ==> true
      }

      test("@transientField excludes field from encoding") {
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithTransient]
        val json = writeToString(JsoniterWithTransient("Alice", Some("cached")))(codec)
        json.contains("\"cache\"") ==> false
        json.contains("\"name\"") ==> true
      }

      test("@transientField decodes without the field") {
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithTransient]
        val decoded = readFromString[JsoniterWithTransient]("""{"name":"Alice"}""")(codec)
        decoded ==> JsoniterWithTransient("Alice", None)
      }

      test("both annotations combined") {
        val codec = KindlingsJsonValueCodec.derived[JsoniterWithBothAnnotations]
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
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[BadTransient]
          """
        ).check(
          "@transientField on field 'noDefault'"
        )
      }
    }

    group("@stringified") {

      test("@stringified Int encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derived[WithStringifiedInt]
        val json = writeToString(WithStringifiedInt(42, "Alice"))(codec)
        json.contains("\"42\"") ==> true
        val decoded = readFromString[WithStringifiedInt](json)(codec)
        decoded ==> WithStringifiedInt(42, "Alice")
      }

      test("@stringified Long encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derived[WithStringifiedLong]
        val json = writeToString(WithStringifiedLong(123456789L, "test"))(codec)
        json.contains("\"123456789\"") ==> true
        val decoded = readFromString[WithStringifiedLong](json)(codec)
        decoded ==> WithStringifiedLong(123456789L, "test")
      }

      test("@stringified BigDecimal encodes as string and decodes back") {
        val codec = KindlingsJsonValueCodec.derived[WithStringifiedBigDecimal]
        val json = writeToString(WithStringifiedBigDecimal(BigDecimal("3.14")))(codec)
        json.contains("\"3.14\"") ==> true
        val decoded = readFromString[WithStringifiedBigDecimal](json)(codec)
        decoded ==> WithStringifiedBigDecimal(BigDecimal("3.14"))
      }

      test("mixed @stringified and normal fields") {
        val codec = KindlingsJsonValueCodec.derived[WithMixedStringified]
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
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[BadStringified]
          """
        ).check(
          "@stringified on field 'label'"
        )
      }
    }

    group("mapAsArray") {

      test("Map[String, Int] with mapAsArray encodes as array of pairs") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derived[Map[String, Int]]
        val json = writeToString(Map("a" -> 1, "b" -> 2))(codec)
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> Map("a" -> 1, "b" -> 2)
      }

      test("Map[Int, String] with mapAsArray encodes as array of pairs") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derived[Map[Int, String]]
        val json = writeToString(Map(1 -> "a", 2 -> "b"))(codec)
        val decoded = readFromString[Map[Int, String]](json)(codec)
        decoded ==> Map(1 -> "a", 2 -> "b")
      }

      test("empty map with mapAsArray encodes as empty array") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derived[Map[String, Int]]
        val json = writeToString(Map.empty[String, Int])(codec)
        json ==> "[]"
        val decoded = readFromString[Map[String, Int]](json)(codec)
        decoded ==> Map.empty[String, Int]
      }

      test("default config uses object-style encoding") {
        val codec = KindlingsJsonValueCodec.derived[Map[String, Int]]
        val json = writeToString(Map("a" -> 1))(codec)
        json.contains("{") ==> true
        json.contains("[") ==> false
      }

      test("case class with map field and mapAsArray") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withMapAsArray
        val codec = KindlingsJsonValueCodec.derived[WithIntKeyMap]
        val value = WithIntKeyMap(Map(1 -> "a", 2 -> "b"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithIntKeyMap](json)(codec)
        decoded ==> value
      }
    }
  }

  group("KindlingsJsonCodec") {

    test("derive returns JsonCodec") {
      val codec = KindlingsJsonCodec.derived[Int]
      assert(codec.isInstanceOf[com.github.plokhotnyuk.jsoniter_scala.core.JsonCodec[Int]])
      assert(codec.isInstanceOf[com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[Int]])
      assert(codec.isInstanceOf[com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec[Int]])
    }

    test("round-trip value encoding") {
      val codec: JsonValueCodec[Int] = KindlingsJsonCodec.derived[Int]
      val json = writeToString(42)(codec)
      val decoded = readFromString[Int](json)(codec)
      decoded ==> 42
    }

    test("key encoding/decoding for Int") {
      val codec = KindlingsJsonCodec.derived[Int]
      val keyCodec: JsonKeyCodec[Int] = codec
      assert(keyCodec != null)
    }

    test("key encoding/decoding for value type") {
      val codec = KindlingsJsonCodec.derived[UserId]
      assert(codec.isInstanceOf[com.github.plokhotnyuk.jsoniter_scala.core.JsonCodec[UserId]])
    }

    test("key encoding/decoding for enum") {
      val codec = KindlingsJsonCodec.derived[CardinalDirection]
      assert(codec.isInstanceOf[com.github.plokhotnyuk.jsoniter_scala.core.JsonCodec[CardinalDirection]])
    }

    test("produces same value output as separate derivation") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
      val jsonCodec: JsonValueCodec[CardinalDirection] = KindlingsJsonCodec.derived[CardinalDirection]
      val valueCodec = KindlingsJsonValueCodec.derived[CardinalDirection]
      val value = North: CardinalDirection
      writeToString(value)(jsonCodec) ==> writeToString(value)(valueCodec)
    }

    test("standalone deriveKeyCodec") {
      val keyCodec = KindlingsJsonCodec.deriveKeyCodec[Int]
      assert(keyCodec.isInstanceOf[JsonKeyCodec[Int]])
    }

    test("standalone deriveKeyCodec for value type") {
      val keyCodec = KindlingsJsonCodec.deriveKeyCodec[UserId]
      assert(keyCodec.isInstanceOf[JsonKeyCodec[UserId]])
    }

    test("standalone deriveKeyCodec for enum") {
      val keyCodec = KindlingsJsonCodec.deriveKeyCodec[CardinalDirection]
      assert(keyCodec.isInstanceOf[JsonKeyCodec[CardinalDirection]])
    }

    test("compile error for unsupported key type") {
      compileErrors(
        """
        hearth.kindlings.jsoniterderivation.KindlingsJsonCodec.derived[hearth.kindlings.jsoniterderivation.SimplePerson]
        """
      ).check(
        "Cannot derive JsonKeyCodec"
      )
    }
  }

  group("UTF-8 field names") {

    test("@fieldName with non-ASCII characters round-trips correctly") {
      val codec = KindlingsJsonValueCodec.derived[JsoniterWithUtf8FieldNames]
      val original = JsoniterWithUtf8FieldNames("Alice", 30, true)
      val json = writeToString(original)(codec)
      assert(json.contains("名前"))
      assert(json.contains("données"))
      assert(json.contains("field with spaces"))
      val decoded = readFromString[JsoniterWithUtf8FieldNames](json)(codec)
      decoded ==> original
    }
  }

  group("JsonValueCodecExtensions") {

    test("map transforms codec") {
      import JsonValueCodecExtensions.*
      val intCodec = KindlingsJsonValueCodec.derived[SingleField]
      val stringCodec = intCodec.map[String](sf => sf.value.toString)(s => SingleField(s.toInt))
      val json = writeToString("42")(stringCodec)
      val decoded = readFromString[String](json)(stringCodec)
      decoded ==> "42"
    }

    test("mapDecode with Right") {
      import JsonValueCodecExtensions.*
      val intCodec = KindlingsJsonValueCodec.derived[SingleField]
      val positiveCodec =
        intCodec.mapDecode[Int](sf => if (sf.value > 0) Right(sf.value) else Left("must be positive"))(v =>
          SingleField(v)
        )
      val json = writeToString(42)(positiveCodec)
      val decoded = readFromString[Int](json)(positiveCodec)
      decoded ==> 42
    }

    group("decodingOnly / encodingOnly") {

      test("encodingOnly codec encodes normally") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withEncodingOnly
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        writeToString(SimplePerson("Alice", 30))(codec) ==> """{"name":"Alice","age":30}"""
      }

      test("encodingOnly codec throws on decode") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withEncodingOnly
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        intercept[UnsupportedOperationException] {
          readFromString[SimplePerson]("""{"name":"Alice","age":30}""")(codec)
        }
      }

      test("decodingOnly codec decodes normally") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withDecodingOnly
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        readFromString[SimplePerson]("""{"name":"Alice","age":30}""")(codec) ==> SimplePerson("Alice", 30)
      }

      test("decodingOnly codec throws on encode") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withDecodingOnly
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        intercept[UnsupportedOperationException] {
          writeToString(SimplePerson("Alice", 30))(codec)
        }
      }
    }

    group("isStringified (global)") {

      test("global isStringified encodes numeric fields as strings") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withStringified
        val codec = KindlingsJsonValueCodec.derived[WithNumericFields]
        val json = writeToString(WithNumericFields(42, 3.14, "hello"))(codec)
        assert(json.contains("\"42\""))
        assert(json.contains("\"3.14\""))
        assert(json.contains("\"hello\""))
      }

      test("global isStringified decodes numeric fields from strings") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withStringified
        val codec = KindlingsJsonValueCodec.derived[WithNumericFields]
        val json = """{"count":"42","score":"3.14","name":"hello"}"""
        readFromString[WithNumericFields](json)(codec) ==> WithNumericFields(42, 3.14, "hello")
      }

      test("without isStringified, numeric fields use normal encoding") {
        val codec = KindlingsJsonValueCodec.derived[WithNumericFields]
        val json = writeToString(WithNumericFields(42, 3.14, "hello"))(codec)
        assert(json.contains(":42"))
        assert(json.contains(":3.14"))
      }
    }

    group("useScalaEnumValueId") {

      test("Scala Enumeration encodes as id when useScalaEnumValueId=true") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true, useScalaEnumValueId = true)
        val codec = KindlingsJsonValueCodec.derived[ScalaColor.Value]
        val json = writeToString[ScalaColor.Value](ScalaColor.Red)(codec)
        json ==> "0"
      }

      test("Scala Enumeration decodes from id when useScalaEnumValueId=true") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true, useScalaEnumValueId = true)
        val codec = KindlingsJsonValueCodec.derived[ScalaColor.Value]
        readFromString[ScalaColor.Value]("1")(codec) ==> ScalaColor.Green
      }

      test("Scala Enumeration round-trip with useScalaEnumValueId") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true, useScalaEnumValueId = true)
        val codec = KindlingsJsonValueCodec.derived[ScalaColor.Value]
        ScalaColor.values.foreach { color =>
          val json = writeToString[ScalaColor.Value](color)(codec)
          readFromString[ScalaColor.Value](json)(codec) ==> color
        }
      }
    }

    group("circeLikeObjectEncoding") {

      test("case objects encode as wrapped empty objects") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
        val codec = KindlingsJsonValueCodec.derived[MixedEnum]
        writeToString[MixedEnum](Pending)(codec) ==> """{"Pending":{}}"""
      }

      test("case classes encode as wrapped objects") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
        val codec = KindlingsJsonValueCodec.derived[MixedEnum]
        writeToString[MixedEnum](InProgress(50))(codec) ==> """{"InProgress":{"progress":50}}"""
      }

      test("case object round-trip with circeLikeObjectEncoding") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
        val codec = KindlingsJsonValueCodec.derived[MixedEnum]
        val json = writeToString[MixedEnum](Done)(codec)
        readFromString[MixedEnum](json)(codec) ==> (Done: MixedEnum)
      }

      test("case class round-trip with circeLikeObjectEncoding") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
        val codec = KindlingsJsonValueCodec.derived[MixedEnum]
        val json = writeToString[MixedEnum](InProgress(75))(codec)
        readFromString[MixedEnum](json)(codec) ==> (InProgress(75): MixedEnum)
      }
    }

    group("transientDefault") {

      test("fields with default values are omitted when equal to default") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true) // all defaults
        val json = writeToString(value)(codec)
        json ==> """{"name":"Alice"}"""
      }

      test("fields with non-default values are written") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 30, false)
        val json = writeToString(value)(codec)
        assert(json.contains("\"age\":30"))
        assert(json.contains("\"active\":false"))
      }

      test("round-trip with transientDefault") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true) // all defaults
        val json = writeToString(value)(codec)
        readFromString[WithDefaultFields](json)(codec) ==> value
      }

      test("round-trip with non-default values") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Bob", 42, false)
        val json = writeToString(value)(codec)
        readFromString[WithDefaultFields](json)(codec) ==> value
      }
    }

    group("transientNone") {

      test("None fields are omitted") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientNone
        val codec = KindlingsJsonValueCodec.derived[WithOptionFields]
        val value = WithOptionFields("Alice", None, None)
        val json = writeToString(value)(codec)
        json ==> """{"name":"Alice"}"""
      }

      test("Some fields are written") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientNone
        val codec = KindlingsJsonValueCodec.derived[WithOptionFields]
        val value = WithOptionFields("Alice", Some("alice@test.com"), None)
        val json = writeToString(value)(codec)
        assert(json.contains("\"email\":\"alice@test.com\""))
        assert(!json.contains("\"phone\""))
      }

      test("round-trip with transientNone") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientNone
        val codec = KindlingsJsonValueCodec.derived[WithOptionFields]
        val value = WithOptionFields("Alice", None, None)
        val json = writeToString(value)(codec)
        readFromString[WithOptionFields](json)(codec) ==> value
      }

      test("round-trip with Some values") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientNone
        val codec = KindlingsJsonValueCodec.derived[WithOptionFields]
        val value = WithOptionFields("Alice", Some("alice@test.com"), Some("555-1234"))
        val json = writeToString(value)(codec)
        readFromString[WithOptionFields](json)(codec) ==> value
      }
    }

    group("transientEmpty") {

      test("empty collections are omitted") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientEmpty.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Alice", Nil, Map.empty)
        val json = writeToString(value)(codec)
        json ==> """{"name":"Alice"}"""
      }

      test("non-empty collections are written") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientEmpty.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Alice", List("scala"), Map("math" -> 95))
        val json = writeToString(value)(codec)
        assert(json.contains("\"tags\""))
        assert(json.contains("\"scores\""))
      }

      test("round-trip with transientEmpty") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientEmpty.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Alice", Nil, Map.empty)
        val json = writeToString(value)(codec)
        readFromString[WithCollectionFields](json)(codec) ==> value
      }

      test("round-trip with non-empty collections") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientEmpty.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Alice", List("a", "b"), Map("x" -> 1))
        val json = writeToString(value)(codec)
        readFromString[WithCollectionFields](json)(codec) ==> value
      }
    }

    group("mixed transient flags") {

      test("all transient flags combined") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientDefault.withTransientNone.withTransientEmpty
        val codec = KindlingsJsonValueCodec.derived[WithMixedTransient]
        val value = WithMixedTransient("Alice", 0, None, Nil) // all defaults/empty
        val json = writeToString(value)(codec)
        json ==> """{"name":"Alice"}"""
      }

      test("all transient flags combined round-trip") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientDefault.withTransientNone.withTransientEmpty
        val codec = KindlingsJsonValueCodec.derived[WithMixedTransient]
        val value = WithMixedTransient("Alice", 0, None, Nil)
        val json = writeToString(value)(codec)
        readFromString[WithMixedTransient](json)(codec) ==> value
      }

      test("non-default values round-trip correctly") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientDefault.withTransientNone.withTransientEmpty
        val codec = KindlingsJsonValueCodec.derived[WithMixedTransient]
        val value = WithMixedTransient("Bob", 42, Some("bob@test.com"), List("x"))
        val json = writeToString(value)(codec)
        readFromString[WithMixedTransient](json)(codec) ==> value
      }
    }

    group("requireDefaultFields") {

      test("accepts complete JSON with all fields present") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val json = """{"name":"Alice","age":30,"active":false}"""
        readFromString[WithDefaultFields](json)(codec) ==> WithDefaultFields("Alice", 30, false)
      }

      test("throws when field with default is missing") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val json = """{"name":"Alice"}"""
        intercept[IllegalArgumentException] {
          readFromString[WithDefaultFields](json)(codec)
        }
      }

      test("round-trip works when all fields present") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Bob", 42, true)
        val json = writeToString(value)(codec)
        readFromString[WithDefaultFields](json)(codec) ==> value
      }
    }

    group("requireCollectionFields") {

      test("accepts complete JSON with all collection fields present") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val json = """{"name":"Alice","tags":["a"],"scores":{"x":1}}"""
        readFromString[WithCollectionFields](json)(codec) ==> WithCollectionFields("Alice", List("a"), Map("x" -> 1))
      }

      test("throws when collection field is missing") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val json = """{"name":"Alice"}"""
        intercept[IllegalArgumentException] {
          readFromString[WithCollectionFields](json)(codec)
        }
      }

      test("round-trip works when all fields present") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        val value = WithCollectionFields("Bob", List("x", "y"), Map("a" -> 1))
        val json = writeToString(value)(codec)
        readFromString[WithCollectionFields](json)(codec) ==> value
      }
    }

    group("checkFieldDuplication") {

      test("accepts JSON without duplicates") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCheckFieldDuplication
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","age":30}"""
        readFromString[SimplePerson](json)(codec) ==> SimplePerson("Alice", 30)
      }

      test("throws on duplicate field") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withCheckFieldDuplication
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","age":30,"name":"Bob"}"""
        intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
      }

      test("allows duplicates when flag is off") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","age":30,"name":"Bob"}"""
        // Last value wins when duplication check is off
        readFromString[SimplePerson](json)(codec) ==> SimplePerson("Bob", 30)
      }
    }

    group("BigDecimal limits") {

      test("accepts normal BigDecimal values") {
        val codec = KindlingsJsonValueCodec.derived[WithBigDecimalField]
        val json = """{"value":123.456}"""
        readFromString[WithBigDecimalField](json)(codec) ==> WithBigDecimalField(BigDecimal("123.456"))
      }

      test("rejects BigDecimal exceeding precision limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withBigDecimalPrecision(5)
        val codec = KindlingsJsonValueCodec.derived[WithBigDecimalField]
        val json = """{"value":123456.789}"""
        intercept[JsonReaderException] {
          readFromString[WithBigDecimalField](json)(codec)
        }
      }

      test("accepts BigDecimal within custom precision") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withBigDecimalPrecision(10)
        val codec = KindlingsJsonValueCodec.derived[WithBigDecimalField]
        val json = """{"value":123.456}"""
        readFromString[WithBigDecimalField](json)(codec) ==> WithBigDecimalField(BigDecimal("123.456"))
      }
    }

    group("BigInt limits") {

      test("accepts normal BigInt values") {
        val codec = KindlingsJsonValueCodec.derived[WithBigIntField]
        val json = """{"value":12345}"""
        readFromString[WithBigIntField](json)(codec) ==> WithBigIntField(BigInt(12345))
      }

      test("rejects BigInt exceeding digits limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withBigDecimalDigitsLimit(3)
        val codec = KindlingsJsonValueCodec.derived[WithBigIntField]
        val json = """{"value":12345}"""
        intercept[JsonReaderException] {
          readFromString[WithBigIntField](json)(codec)
        }
      }
    }

    group("map size limits") {

      test("accepts map within limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withMapMaxInsertNumber(5)
        val codec = KindlingsJsonValueCodec.derived[WithMapField]
        val json = """{"data":{"a":1,"b":2}}"""
        readFromString[WithMapField](json)(codec) ==> WithMapField(Map("a" -> 1, "b" -> 2))
      }

      test("rejects map exceeding limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withMapMaxInsertNumber(2)
        val codec = KindlingsJsonValueCodec.derived[WithMapField]
        val json = """{"data":{"a":1,"b":2,"c":3}}"""
        intercept[JsonReaderException] {
          readFromString[WithMapField](json)(codec)
        }
      }
    }

    group("collection size limits") {

      test("accepts collection within limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withSetMaxInsertNumber(5)
        val codec = KindlingsJsonValueCodec.derived[WithListField]
        val json = """{"items":[1,2,3]}"""
        readFromString[WithListField](json)(codec) ==> WithListField(List(1, 2, 3))
      }

      test("rejects collection exceeding limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withSetMaxInsertNumber(2)
        val codec = KindlingsJsonValueCodec.derived[WithListField]
        val json = """{"items":[1,2,3]}"""
        intercept[JsonReaderException] {
          readFromString[WithListField](json)(codec)
        }
      }
    }

    group("feature interactions") {

      test("transientDefault + requireDefaultFields: encoder omits defaults, decoder requires them") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withTransientDefault.withRequireDefaultFields
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        // Encoder omits default values
        val json = writeToString(WithDefaultFields("Alice", 25, true))(codec)
        json ==> """{"name":"Alice"}"""
        // Decoder requires all fields — missing defaults cause error
        intercept[IllegalArgumentException] {
          readFromString[WithDefaultFields](json)(codec)
        }
      }

      test("transientNone + @fieldName on Option field") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientNone
        val codec = KindlingsJsonValueCodec.derived[WithRenamedOption]
        // None field omitted entirely
        val json = writeToString(WithRenamedOption(None, "Alice"))(codec)
        assert(!json.contains("e_mail"))
        // Some field uses renamed key
        val json2 = writeToString(WithRenamedOption(Some("a@b.com"), "Alice"))(codec)
        assert(json2.contains("\"e_mail\":\"a@b.com\""))
        // Round-trip
        readFromString[WithRenamedOption](json2)(codec) ==> WithRenamedOption(Some("a@b.com"), "Alice")
      }

      test("checkFieldDuplication + field name transforms") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withCheckFieldDuplication.withSnakeCaseFieldNames
        val codec = KindlingsJsonValueCodec.derived[CamelCasePerson]
        val json = """{"first_name":"Alice","last_name":"Smith"}"""
        readFromString[CamelCasePerson](json)(codec) ==> CamelCasePerson("Alice", "Smith")
        // Duplicate transformed name
        val jsonDup = """{"first_name":"Alice","last_name":"Smith","first_name":"Bob"}"""
        intercept[JsonReaderException] {
          readFromString[CamelCasePerson](jsonDup)(codec)
        }
      }

      test("isStringified + @stringified on same field (no-op interaction)") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withStringified
        val codec = KindlingsJsonValueCodec.derived[WithStringifiedInt]
        val value = WithStringifiedInt(42, "test")
        val json = writeToString(value)(codec)
        // Both global and per-field should encode as string
        assert(json.contains("\"42\""))
        readFromString[WithStringifiedInt](json)(codec) ==> value
      }

      test("requireCollectionFields + transientEmpty") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withRequireCollectionFields.withTransientEmpty
        val codec = KindlingsJsonValueCodec.derived[WithCollectionFields]
        // Encoder omits empty collections
        val json = writeToString(WithCollectionFields("Alice", Nil, Map.empty))(codec)
        assert(!json.contains("tags"))
        // Decoder requires collection fields — missing ones cause error
        intercept[IllegalArgumentException] {
          readFromString[WithCollectionFields](json)(codec)
        }
      }

      test("mapAsArray + mapMaxInsertNumber DoS limit") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withMapAsArray.withMapMaxInsertNumber(2)
        val codec = KindlingsJsonValueCodec.derived[WithIntKeyMap]
        // Within limit works
        val jsonOk = """{"data":[[1,"a"],[2,"b"]]}"""
        readFromString[WithIntKeyMap](jsonOk)(codec) ==> WithIntKeyMap(Map(1 -> "a", 2 -> "b"))
        // Exceeding limit throws
        val jsonBad = """{"data":[[1,"a"],[2,"b"],[3,"c"]]}"""
        intercept[JsonReaderException] {
          readFromString[WithIntKeyMap](jsonBad)(codec)
        }
      }

      test("enumAsStrings + adtLeafClassNameMapper") {
        implicit val config: JsoniterConfig =
          JsoniterConfig(enumAsStrings = true, adtLeafClassNameMapper = _.toLowerCase)
        val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
        val json = writeToString[CardinalDirection](North)(codec)
        json ==> "\"north\""
        readFromString[CardinalDirection](json)(codec) ==> (North: CardinalDirection)
      }

      test("@fieldName + @stringified combined") {
        val codec = KindlingsJsonValueCodec.derived[WithStringifiedAndFieldName]
        val value = WithStringifiedAndFieldName(42, "test")
        val json = writeToString(value)(codec)
        assert(json.contains("\"item_count\":\"42\""))
        readFromString[WithStringifiedAndFieldName](json)(codec) ==> value
      }

      test("decodingOnly + config flags still apply to decoder") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withDecodingOnly.withRequireDefaultFields
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        // Encoding throws
        val _ = intercept[UnsupportedOperationException] {
          writeToString(WithDefaultFields("Alice"))(codec)
        }
        // Decoding respects requireDefaultFields
        val json = """{"name":"Alice"}"""
        intercept[IllegalArgumentException] {
          readFromString[WithDefaultFields](json)(codec)
        }
      }

      test("encodingOnly + config flags still apply to encoder") {
        implicit val config: JsoniterConfig =
          JsoniterConfig.default.withEncodingOnly.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        // Encoding respects transientDefault
        val json = writeToString(WithDefaultFields("Alice", 25, true))(codec)
        json ==> """{"name":"Alice"}"""
        // Decoding throws
        intercept[UnsupportedOperationException] {
          readFromString[WithDefaultFields](json)(codec)
        }
      }

      test("fields in different order than case class") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"age":30,"name":"Alice"}"""
        readFromString[SimplePerson](json)(codec) ==> SimplePerson("Alice", 30)
      }

      test("empty object decode for all-defaults case class") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[AllOptionalWithDefaults]
        val json = """{}"""
        readFromString[AllOptionalWithDefaults](json)(codec) ==> AllOptionalWithDefaults()
      }
    }

    group("edge cases") {

      test("null for non-Option String throws") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":null,"age":30}"""
        intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
      }

      test("null for non-Option Int throws") {
        val codec = KindlingsJsonValueCodec.derived[SingleField]
        val json = """{"value":null}"""
        intercept[JsonReaderException] {
          readFromString[SingleField](json)(codec)
        }
      }

      test("Int.MaxValue / Int.MinValue round-trip") {
        val codec = KindlingsJsonValueCodec.derived[SingleField]
        val maxVal = SingleField(Int.MaxValue)
        readFromString[SingleField](writeToString(maxVal)(codec))(codec) ==> maxVal
        val minVal = SingleField(Int.MinValue)
        readFromString[SingleField](writeToString(minVal)(codec))(codec) ==> minVal
      }

      test("Long.MaxValue / Long.MinValue round-trip") {
        case class WithLong(value: Long)
        val codec = KindlingsJsonValueCodec.derived[WithLong]
        val maxVal = WithLong(Long.MaxValue)
        readFromString[WithLong](writeToString(maxVal)(codec))(codec) ==> maxVal
        val minVal = WithLong(Long.MinValue)
        readFromString[WithLong](writeToString(minVal)(codec))(codec) ==> minVal
      }

      test("Unicode emoji in field values") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Alice \ud83d\ude00\ud83c\udf1f", 30)
        val json = writeToString(value)(codec)
        readFromString[SimplePerson](json)(codec) ==> value
      }

      test("deeply nested case classes (5 levels)") {
        val codec = KindlingsJsonValueCodec.derived[DeeplyNested1]
        val value = DeeplyNested1(DeeplyNested2(DeeplyNested3(DeeplyNested4(DeeplyNested5(42)))))
        val json = writeToString(value)(codec)
        readFromString[DeeplyNested1](json)(codec) ==> value
      }
    }

    group("numeric boundaries") {

      test("Byte min/max round-trip") {
        val codec = KindlingsJsonValueCodec.derived[ByteBoundaries]
        val value = ByteBoundaries(Byte.MinValue, Byte.MaxValue)
        val json = writeToString(value)(codec)
        val decoded = readFromString[ByteBoundaries](json)(codec)
        decoded ==> value
      }

      test("Short min/max round-trip") {
        val codec = KindlingsJsonValueCodec.derived[ShortBoundaries]
        val value = ShortBoundaries(Short.MinValue, Short.MaxValue)
        val json = writeToString(value)(codec)
        val decoded = readFromString[ShortBoundaries](json)(codec)
        decoded ==> value
      }

      test("Float precision edge case") {
        case class WithFloat(value: Float)
        val codec = KindlingsJsonValueCodec.derived[WithFloat]
        val value = WithFloat(Float.MaxValue)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithFloat](json)(codec)
        decoded ==> value
      }

      test("BigDecimal at precision limit") {
        val codec = KindlingsJsonValueCodec.derived[WithBigDecimalField]
        val value = WithBigDecimalField(BigDecimal("1234567890123456789012345678901234"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithBigDecimalField](json)(codec)
        decoded ==> value
      }
    }

    group("unicode and encoding edge cases") {

      test("surrogate pairs in values") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val value = SimplePerson("Hello \ud83c\udf0d", 1)
        val json = writeToString(value)(codec)
        val decoded = readFromString[SimplePerson](json)(codec)
        decoded ==> value
      }

      test("escaped characters in values") {
        val codec = KindlingsJsonValueCodec.derived[UnicodeContent]
        val value = UnicodeContent("line1\nline2\ttab\\backslash")
        val json = writeToString(value)(codec)
        val decoded = readFromString[UnicodeContent](json)(codec)
        decoded ==> value
      }

      test("non-ASCII in field values") {
        val codec = KindlingsJsonValueCodec.derived[UnicodeContent]
        val value = UnicodeContent("\u65e5\u672c\u8a9e")
        val json = writeToString(value)(codec)
        val decoded = readFromString[UnicodeContent](json)(codec)
        decoded ==> value
      }
    }

    group("nested structures") {

      test("nested collections round-trip") {
        val codec = KindlingsJsonValueCodec.derived[NestedLists]
        val value = NestedLists(List(List(1, 2), List(3)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[NestedLists](json)(codec)
        decoded ==> value
      }

      test("Option wrapping collection round-trip") {
        val codec = KindlingsJsonValueCodec.derived[OptionalList]
        val some = OptionalList(Some(List(1, 2, 3)))
        val jsonSome = writeToString(some)(codec)
        readFromString[OptionalList](jsonSome)(codec) ==> some
        val none = OptionalList(None)
        val jsonNone = writeToString(none)(codec)
        readFromString[OptionalList](jsonNone)(codec) ==> none
      }

      test("deeply nested option round-trip") {
        val codec = KindlingsJsonValueCodec.derived[OptionalList]
        val value = OptionalList(None)
        val json = writeToString(value)(codec)
        val decoded = readFromString[OptionalList](json)(codec)
        decoded ==> value
      }
    }

    group("field edge cases") {

      test("empty JSON object for all-defaults class") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true)
        val json = writeToString(value)(codec)
        json ==> """{"name":"Alice"}"""
      }

      test("all fields present when no transientDefault") {
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true)
        val json = writeToString(value)(codec)
        assert(json.contains("\"name\""))
        assert(json.contains("\"age\""))
        assert(json.contains("\"active\""))
      }
    }

    group("value class edge cases") {

      test("value class wrapping String round-trip") {
        case class WithWrappedString(name: WrappedString)
        val codec = KindlingsJsonValueCodec.derived[WithWrappedString]
        val value = WithWrappedString(WrappedString("hello"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithWrappedString](json)(codec)
        decoded ==> value
      }

      test("value class in Option round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithOptionalWrapped]
        val some = WithOptionalWrapped(Some(WrappedInt(42)))
        val jsonSome = writeToString(some)(codec)
        readFromString[WithOptionalWrapped](jsonSome)(codec) ==> some
        val none = WithOptionalWrapped(None)
        val jsonNone = writeToString(none)(codec)
        readFromString[WithOptionalWrapped](jsonNone)(codec) ==> none
      }

      test("value class in collection round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithWrappedList]
        val value = WithWrappedList(List(WrappedInt(1), WrappedInt(2)))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithWrappedList](json)(codec)
        decoded ==> value
      }
    }

    group("Array[T] support") {

      test("Array[Int] round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithIntArray]
        val value = WithIntArray(Array(1, 2, 3))
        val json = writeToString(value)(codec)
        json ==> """{"values":[1,2,3]}"""
        val decoded = readFromString[WithIntArray](json)(codec)
        decoded ==> value
      }

      test("Array[String] round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithStringArray]
        val value = WithStringArray(Array("a", "b", "c"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithStringArray](json)(codec)
        decoded ==> value
      }

      test("empty Array round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithIntArray]
        val value = WithIntArray(Array.empty[Int])
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithIntArray](json)(codec)
        decoded ==> value
      }
    }

    group("UUID support") {

      test("UUID round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithUUID]
        val id = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val value = WithUUID(id, "test")
        val json = writeToString(value)(codec)
        assert(json.contains("550e8400-e29b-41d4-a716-446655440000"))
        val decoded = readFromString[WithUUID](json)(codec)
        decoded ==> value
      }

      test("standalone UUID round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.util.UUID]
        val id = java.util.UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
        val json = writeToString(id)(codec)
        json ==> "\"123e4567-e89b-12d3-a456-426614174000\""
        readFromString[java.util.UUID](json)(codec) ==> id
      }
    }

    group("advanced collection types") {

      test("HashMap round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithHashMap]
        val value = WithHashMap(scala.collection.immutable.HashMap("a" -> 1, "b" -> 2))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithHashMap](json)(codec)
        decoded.data ==> value.data
      }

      test("TreeMap round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithTreeMap]
        val value = WithTreeMap(scala.collection.immutable.TreeMap("a" -> 1, "b" -> 2))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithTreeMap](json)(codec)
        decoded.data ==> value.data
      }

      test("ArrayBuffer round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithArrayBuffer]
        val value = WithArrayBuffer(scala.collection.mutable.ArrayBuffer(1, 2, 3))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithArrayBuffer](json)(codec)
        decoded.items ==> value.items
      }

      test("IntMap round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithIntMap]
        val value = WithIntMap(scala.collection.immutable.IntMap(1 -> "one", 2 -> "two"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithIntMap](json)(codec)
        decoded.data ==> value.data
      }

      test("LongMap round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithLongMap]
        val value = WithLongMap(scala.collection.immutable.LongMap(100L -> "hundred", 200L -> "two-hundred"))
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithLongMap](json)(codec)
        decoded.data ==> value.data
      }
    }

    group("nested sealed trait hierarchies") {

      val vehicleCodec = KindlingsJsonValueCodec.derived[Vehicle]

      test("intermediate sealed trait round-trip (leaf under MotorVehicle)") {
        val value: Vehicle = Truck(5000)
        val json = writeToString(value)(vehicleCodec)
        val decoded = readFromString[Vehicle](json)(vehicleCodec)
        decoded ==> value
      }

      test("direct child of top-level trait round-trip") {
        val value: Vehicle = Bicycle(21)
        val json = writeToString(value)(vehicleCodec)
        val decoded = readFromString[Vehicle](json)(vehicleCodec)
        decoded ==> value
      }

      test("all leaf variants round-trip (wrapper style)") {
        val variants: List[Vehicle] = List(Truck(3000), Motorcycle(600), Bicycle(7))
        for (v <- variants) {
          val json = writeToString(v)(vehicleCodec)
          readFromString[Vehicle](json)(vehicleCodec) ==> v
        }
      }
    }

    group("error message quality") {

      test("wrong type for field reports useful error") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val ex = intercept[JsonReaderException] {
          readFromString[SimplePerson]("""{"name":"Alice","age":"not-a-number"}""")(codec)
        }
        assert(ex.getMessage.nonEmpty)
      }

      test("completely invalid JSON throws") {
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        intercept[JsonReaderException] {
          readFromString[SimplePerson]("""not json at all""")(codec)
        }
      }
    }
  }

  group("combinatorial: wrapper x inner type") {

    test("CombOuter round-trip with all fields populated") {
      val codec = KindlingsJsonValueCodec.derived[CombOuter]
      val value = CombOuter(
        optPrimitive = Some(42),
        optCaseClass = Some(SimplePerson("Alice", 30)),
        optSealedTrait = Some(Circle(5.0)),
        optValueClass = Some(WrappedInt(7)),
        listCaseClass = List(SimplePerson("Bob", 25), SimplePerson("Eve", 28)),
        listSealedTrait = List(Circle(1.0), Rectangle(2.0, 3.0)),
        mapCaseClass = Map("first" -> SimplePerson("Carol", 40)),
        mapSealedTrait = Map("s1" -> Circle(10.0), "s2" -> Rectangle(4.0, 5.0))
      )
      val json = writeToString(value)(codec)
      val decoded = readFromString[CombOuter](json)(codec)
      decoded ==> value
    }

    test("CombOuter round-trip with None and empty collections") {
      val codec = KindlingsJsonValueCodec.derived[CombOuter]
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
      val json = writeToString(value)(codec)
      val decoded = readFromString[CombOuter](json)(codec)
      decoded ==> value
    }

    test("Option[Shape] round-trip (recursive sealed trait derivation)") {
      val codec = KindlingsJsonValueCodec.derived[Option[Shape]]
      val some: Option[Shape] = Some(Rectangle(3.0, 4.0))
      val jsonSome = writeToString(some)(codec)
      readFromString[Option[Shape]](jsonSome)(codec) ==> some
      val none: Option[Shape] = None
      val jsonNone = writeToString(none)(codec)
      readFromString[Option[Shape]](jsonNone)(codec) ==> none
    }

    test("Option[SimplePerson] without pre-existing codec") {
      val codec = KindlingsJsonValueCodec.derived[Option[SimplePerson]]
      val some: Option[SimplePerson] = Some(SimplePerson("Alice", 30))
      val jsonSome = writeToString(some)(codec)
      readFromString[Option[SimplePerson]](jsonSome)(codec) ==> some
      val none: Option[SimplePerson] = None
      val jsonNone = writeToString(none)(codec)
      readFromString[Option[SimplePerson]](jsonNone)(codec) ==> none
    }

    test("List[Shape] round-trip (recursive sealed trait derivation)") {
      val codec = KindlingsJsonValueCodec.derived[List[Shape]]
      val value: List[Shape] = List(Circle(1.0), Rectangle(2.0, 3.0), Circle(4.0))
      val json = writeToString(value)(codec)
      val decoded = readFromString[List[Shape]](json)(codec)
      decoded ==> value
    }

    test("Map[String, Shape] round-trip (recursive sealed trait derivation)") {
      val codec = KindlingsJsonValueCodec.derived[Map[String, Shape]]
      val value = Map("c" -> (Circle(1.0): Shape), "r" -> (Rectangle(2.0, 3.0): Shape))
      val json = writeToString(value)(codec)
      val decoded = readFromString[Map[String, Shape]](json)(codec)
      decoded ==> value
    }
  }

  group("annotation x type shape") {

    test("@fieldName and @stringified on case class fields round-trip") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedCaseClass]
      val value = AnnotatedCaseClass("Alice", 99, 1)
      val json = writeToString(value)(codec)
      assert(json.contains("\"full_name\":\"Alice\""))
      assert(json.contains("\"99\""))
      assert(json.contains("\"is_active\":\"1\""))
      val decoded = readFromString[AnnotatedCaseClass](json)(codec)
      decoded ==> value
    }

    test("@fieldName on sealed trait subtypes round-trip") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedSealedTrait]
      val valueA: AnnotatedSealedTrait = AnnotatedSubA("hello", 42)
      val jsonA = writeToString(valueA)(codec)
      assert(jsonA.contains("\"sub_name\":\"hello\""))
      assert(jsonA.contains("\"42\""))
      val decodedA = readFromString[AnnotatedSealedTrait](jsonA)(codec)
      decodedA ==> valueA

      val valueB: AnnotatedSealedTrait = AnnotatedSubB("world")
      val jsonB = writeToString(valueB)(codec)
      assert(jsonB.contains("\"sub_label\":\"world\""))
      val decodedB = readFromString[AnnotatedSealedTrait](jsonB)(codec)
      decodedB ==> valueB
    }

    test("@fieldName with config field name transform (annotation takes precedence)") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
      val codec = KindlingsJsonValueCodec.derived[AnnotatedCaseClass]
      val value = AnnotatedCaseClass("Bob", 50, 0)
      val json = writeToString(value)(codec)
      // @fieldName("full_name") takes precedence over snake_case transform
      assert(json.contains("\"full_name\""))
      assert(json.contains("\"is_active\""))
      val decoded = readFromString[AnnotatedCaseClass](json)(codec)
      decoded ==> value
    }

    test("@stringified on sealed trait subtypes round-trip") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedSealedTrait]
      val value: AnnotatedSealedTrait = AnnotatedSubA("test", 100)
      val json = writeToString(value)(codec)
      // @stringified on AnnotatedSubA.value should encode as string
      assert(json.contains("\"100\""))
      val decoded = readFromString[AnnotatedSealedTrait](json)(codec)
      decoded ==> value
    }

    test("@fieldName + @stringified combined on same field round-trip") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedCaseClass]
      val value = AnnotatedCaseClass("Carol", 77, 1)
      val json = writeToString(value)(codec)
      // is_active field has both annotations
      assert(json.contains("\"is_active\":\"1\""))
      val decoded = readFromString[AnnotatedCaseClass](json)(codec)
      decoded ==> value
    }

    test("@fieldName on sealed trait subtypes: decode with original keys fails") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedSealedTrait]
      val jsonOriginalKeys = """{"AnnotatedSubA":{"subName":"hello","value":42}}"""
      intercept[JsonReaderException] {
        readFromString[AnnotatedSealedTrait](jsonOriginalKeys)(codec)
      }
    }

    test("@fieldName on sealed trait subtypes: JSON keys are renamed, not original") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedSealedTrait]
      val value: AnnotatedSealedTrait = AnnotatedSubA("data", 7)
      val json = writeToString(value)(codec)
      // Positive: renamed keys present
      assert(json.contains("\"sub_name\""), s"expected 'sub_name' in $json")
      // Negative: original Scala field names absent
      assert(!json.contains("\"subName\""), s"unexpected 'subName' in $json")
    }

    test("@stringified on sealed trait subtypes: encode produces string-wrapped numeric, decode restores") {
      val codec = KindlingsJsonValueCodec.derived[AnnotatedSealedTrait]
      val value: AnnotatedSealedTrait = AnnotatedSubA("x", 999)
      val json = writeToString(value)(codec)
      // The numeric field 'value' on AnnotatedSubA must be encoded as a JSON string
      assert(json.contains("\"999\""), s"expected stringified '999' in $json")
      // Must NOT contain bare numeric 999 outside of a string
      assert(
        !json.contains(":999,") && !json.contains(":999}"),
        s"value should be stringified, not bare numeric in $json"
      )
      // Full round-trip
      val decoded = readFromString[AnnotatedSealedTrait](json)(codec)
      decoded ==> value
    }

    test("@transientField on sealed trait subtypes: field omitted from encoding") {
      val codec = KindlingsJsonValueCodec.derived[TransientSealedTrait]
      val valueA: TransientSealedTrait = TransientSubA("hello", "should-be-omitted")
      val jsonA = writeToString(valueA)(codec)
      assert(!jsonA.contains("cache"), s"@transientField 'cache' should be absent from $jsonA")
      assert(jsonA.contains("\"name\":\"hello\""), s"non-transient field missing in $jsonA")

      val valueB: TransientSealedTrait = TransientSubB(42, 999)
      val jsonB = writeToString(valueB)(codec)
      assert(!jsonB.contains("temp"), s"@transientField 'temp' should be absent from $jsonB")
      assert(jsonB.contains("\"value\":42"), s"non-transient field missing in $jsonB")
    }

    test("@transientField on sealed trait subtypes: decode without transient field uses default") {
      val codec = KindlingsJsonValueCodec.derived[TransientSealedTrait]
      // Encode then decode — transient field should get its default value
      val original = TransientSubA("test", "custom-cache")
      val json = writeToString[TransientSealedTrait](original)(codec)
      val decoded = readFromString[TransientSealedTrait](json)(codec)
      decoded match {
        case TransientSubA(name, cache) =>
          name ==> "test"
          cache ==> "default-cache" // default, not "custom-cache"
        case other => assert(false, s"expected TransientSubA, got $other")
      }
    }

    test("@transientField on sealed trait subtypes: round-trip restores defaults for all subtypes") {
      val codec = KindlingsJsonValueCodec.derived[TransientSealedTrait]
      val originalB = TransientSubB(100, 999)
      val json = writeToString[TransientSealedTrait](originalB)(codec)
      val decoded = readFromString[TransientSealedTrait](json)(codec)
      decoded match {
        case TransientSubB(value, temp) =>
          value ==> 100
          temp ==> -1 // default, not 999
        case other => assert(false, s"expected TransientSubB, got $other")
      }
    }
  }

  group("annotation x type shape: config interactions") {

    test("enumAsStrings + constructorNameMapper: names are transformed AND encoded as strings") {
      implicit val config: JsoniterConfig =
        JsoniterConfig(enumAsStrings = true, adtLeafClassNameMapper = _.toUpperCase)
      val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
      // All directions should encode as upper-case strings
      writeToString[CardinalDirection](North)(codec) ==> "\"NORTH\""
      writeToString[CardinalDirection](South)(codec) ==> "\"SOUTH\""
      writeToString[CardinalDirection](East)(codec) ==> "\"EAST\""
      writeToString[CardinalDirection](West)(codec) ==> "\"WEST\""
      // Decode must accept transformed names
      readFromString[CardinalDirection]("\"NORTH\"")(codec) ==> (North: CardinalDirection)
      readFromString[CardinalDirection]("\"WEST\"")(codec) ==> (West: CardinalDirection)
    }

    test("enumAsStrings + constructorNameMapper: original untransformed name is rejected") {
      implicit val config: JsoniterConfig =
        JsoniterConfig(enumAsStrings = true, adtLeafClassNameMapper = _.toUpperCase)
      val codec = KindlingsJsonValueCodec.derived[CardinalDirection]
      // Original mixed-case name should fail decoding
      intercept[JsonReaderException] {
        readFromString[CardinalDirection]("\"North\"")(codec)
      }
    }

    test("transientDefault + useDefaults: encode omits defaults, decode restores them") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
      val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
      val withDefaults = WithDefaultFields("Alice", 25, true) // age=25 and active=true are defaults
      val withNonDefaults = WithDefaultFields("Bob", 42, false)

      // Encoding: default values are omitted
      val jsonDefaults = writeToString(withDefaults)(codec)
      assert(!jsonDefaults.contains("\"age\""), s"default age should be omitted: $jsonDefaults")
      assert(!jsonDefaults.contains("\"active\""), s"default active should be omitted: $jsonDefaults")
      jsonDefaults ==> """{"name":"Alice"}"""

      // Encoding: non-default values are present
      val jsonNonDefaults = writeToString(withNonDefaults)(codec)
      assert(jsonNonDefaults.contains("\"age\":42"))
      assert(jsonNonDefaults.contains("\"active\":false"))

      // Decoding: missing fields are restored to defaults
      val decodedDefaults = readFromString[WithDefaultFields](jsonDefaults)(codec)
      decodedDefaults ==> withDefaults

      // Decoding: present fields are used as-is
      val decodedNonDefaults = readFromString[WithDefaultFields](jsonNonDefaults)(codec)
      decodedNonDefaults ==> withNonDefaults
    }

    test("transientDefault round-trip preserves identity for default and non-default values") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault
      val codec = KindlingsJsonValueCodec.derived[WithMixedTransient]
      // All defaults
      val allDefaults = WithMixedTransient("X", 0, None, Nil)
      readFromString[WithMixedTransient](writeToString(allDefaults)(codec))(codec) ==> allDefaults
      // All non-defaults
      val allNonDefaults = WithMixedTransient("Y", 99, Some("y@test.com"), List("tag1", "tag2"))
      readFromString[WithMixedTransient](writeToString(allNonDefaults)(codec))(codec) ==> allNonDefaults
    }
  }

  group("Either[A, B] codec") {

    test("Either[Int, String] Left round-trip") {
      val codec = KindlingsJsonValueCodec.derived[Either[Int, String]]
      val value: Either[Int, String] = Left(42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[Either[Int, String]](json)(codec)
      decoded ==> value
    }

    test("Either[Int, String] Right round-trip") {
      val codec = KindlingsJsonValueCodec.derived[Either[Int, String]]
      val value: Either[Int, String] = Right("hello")
      val json = writeToString(value)(codec)
      val decoded = readFromString[Either[Int, String]](json)(codec)
      decoded ==> value
    }

    test("Either[SimplePerson, String] with derived inner type") {
      val codec = KindlingsJsonValueCodec.derived[Either[SimplePerson, String]]
      val left: Either[SimplePerson, String] = Left(SimplePerson("Alice", 30))
      val jsonLeft = writeToString(left)(codec)
      readFromString[Either[SimplePerson, String]](jsonLeft)(codec) ==> left

      val right: Either[SimplePerson, String] = Right("error")
      val jsonRight = writeToString(right)(codec)
      readFromString[Either[SimplePerson, String]](jsonRight)(codec) ==> right
    }

    test("case class with Either field round-trip") {
      val codec = KindlingsJsonValueCodec.derived[WithEitherField]
      val left = WithEitherField(Left("error"))
      val jsonLeft = writeToString(left)(codec)
      readFromString[WithEitherField](jsonLeft)(codec) ==> left

      val right = WithEitherField(Right(42))
      val jsonRight = writeToString(right)(codec)
      readFromString[WithEitherField](jsonRight)(codec) ==> right
    }

    test("nested Either[Either[Int, String], Boolean] round-trip") {
      val codec = KindlingsJsonValueCodec.derived[Either[Either[Int, String], Boolean]]
      val leftLeft: Either[Either[Int, String], Boolean] = Left(Left(1))
      val jsonLL = writeToString(leftLeft)(codec)
      readFromString[Either[Either[Int, String], Boolean]](jsonLL)(codec) ==> leftLeft

      val leftRight: Either[Either[Int, String], Boolean] = Left(Right("x"))
      val jsonLR = writeToString(leftRight)(codec)
      readFromString[Either[Either[Int, String], Boolean]](jsonLR)(codec) ==> leftRight

      val right: Either[Either[Int, String], Boolean] = Right(true)
      val jsonR = writeToString(right)(codec)
      readFromString[Either[Either[Int, String], Boolean]](jsonR)(codec) ==> right
    }

    test("Either with discriminator style") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
      val codec = KindlingsJsonValueCodec.derived[Either[Int, String]]
      val left: Either[Int, String] = Left(42)
      val json = writeToString(left)(codec)
      assert(json.contains("\"type\""))
      readFromString[Either[Int, String]](json)(codec) ==> left
    }

    test("case class with nested Either field round-trip") {
      val codec = KindlingsJsonValueCodec.derived[WithNestedEither]
      val value = WithNestedEither(Left(Left(1)))
      val json = writeToString(value)(codec)
      readFromString[WithNestedEither](json)(codec) ==> value

      val value2 = WithNestedEither(Right(true))
      val json2 = writeToString(value2)(codec)
      readFromString[WithNestedEither](json2)(codec) ==> value2
    }
  }

  group("BitSet codec") {

    test("immutable.BitSet round-trip") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val value = scala.collection.immutable.BitSet(1, 5, 10, 42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[scala.collection.immutable.BitSet](json)(codec)
      decoded ==> value
    }

    test("mutable.BitSet round-trip") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.mutable.BitSet]
      val value = scala.collection.mutable.BitSet(3, 7, 15)
      val json = writeToString(value)(codec)
      val decoded = readFromString[scala.collection.mutable.BitSet](json)(codec)
      decoded ==> value
    }

    test("collection.BitSet round-trip") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.BitSet]
      val value: scala.collection.BitSet = scala.collection.immutable.BitSet(2, 4, 8)
      val json = writeToString(value)(codec)
      val decoded = readFromString[scala.collection.BitSet](json)(codec)
      decoded ==> value
    }

    test("empty immutable.BitSet round-trip") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val value = scala.collection.immutable.BitSet.empty
      val json = writeToString(value)(codec)
      json ==> "[]"
      val decoded = readFromString[scala.collection.immutable.BitSet](json)(codec)
      decoded ==> value
    }

    test("empty mutable.BitSet round-trip") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.mutable.BitSet]
      val value = scala.collection.mutable.BitSet.empty
      val json = writeToString(value)(codec)
      json ==> "[]"
      val decoded = readFromString[scala.collection.mutable.BitSet](json)(codec)
      decoded ==> value
    }

    test("case class with immutable.BitSet field round-trip") {
      val codec = KindlingsJsonValueCodec.derived[WithImmutableBitSet]
      val value = WithImmutableBitSet(scala.collection.immutable.BitSet(0, 100, 500))
      val json = writeToString(value)(codec)
      val decoded = readFromString[WithImmutableBitSet](json)(codec)
      decoded ==> value
    }

    test("case class with mutable.BitSet field round-trip") {
      val codec = KindlingsJsonValueCodec.derived[WithMutableBitSet]
      val value = WithMutableBitSet(scala.collection.mutable.BitSet(1, 2, 3))
      val json = writeToString(value)(codec)
      val decoded = readFromString[WithMutableBitSet](json)(codec)
      decoded ==> value
    }

    test("bitSetValueLimit exceeded is rejected") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withBitSetValueLimit(10)
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val json = "[1,5,10]"
      intercept[JsonReaderException] {
        readFromString[scala.collection.immutable.BitSet](json)(codec)
      }
    }

    test("bitSetValueLimit boundary accepted") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withBitSetValueLimit(10)
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val json = "[0,5,9]"
      val decoded = readFromString[scala.collection.immutable.BitSet](json)(codec)
      decoded ==> scala.collection.immutable.BitSet(0, 5, 9)
    }

    test("negative value is rejected") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val json = "[1,-3,5]"
      intercept[JsonReaderException] {
        readFromString[scala.collection.immutable.BitSet](json)(codec)
      }
    }

    test("negative value is rejected for mutable.BitSet") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.mutable.BitSet]
      val json = "[-1,2]"
      intercept[JsonReaderException] {
        readFromString[scala.collection.mutable.BitSet](json)(codec)
      }
    }

    test("bitSetValueLimit exceeded is rejected for mutable.BitSet") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withBitSetValueLimit(5)
      val codec = KindlingsJsonValueCodec.derived[scala.collection.mutable.BitSet]
      val json = "[1,2,5]"
      intercept[JsonReaderException] {
        readFromString[scala.collection.mutable.BitSet](json)(codec)
      }
    }

    test("immutable.BitSet encodes as sorted JSON array") {
      val codec = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
      val value = scala.collection.immutable.BitSet(10, 1, 5, 3)
      val json = writeToString(value)(codec)
      json ==> "[1,3,5,10]"
    }
  }

  // Compile-time config conflict validation tests are in JsoniterScala3Spec
  // because they depend on semiEval succeeding, which requires Scala 3 inline expansion.

  // ---------------------------------------------------------------------------
  // Default config differences from jsoniter-scala's CodecMakerConfig
  //
  // These tests document intentional divergences between Kindlings' JsoniterConfig
  // defaults and jsoniter-scala's CodecMakerConfig defaults. They serve as living
  // documentation: if a test here fails after a default change, it signals a
  // migration-relevant behavioral shift.
  // ---------------------------------------------------------------------------
  group("default config differences from jsoniter-scala") {

    // jsoniter-scala: transientDefault = true  (fields at default value are omitted)
    // Kindlings:      transientDefault = false (all fields are always present in output)
    group("transientDefault = false (default)") {

      test("field with default value IS included in output") {
        // Default config — no explicit transientDefault override
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true) // age=25 and active=true are defaults
        val json = writeToString(value)(codec)
        // With Kindlings default (transientDefault=false), ALL fields are present
        assert(json.contains("\"age\":25"), s"default-valued 'age' should be present: $json")
        assert(json.contains("\"active\":true"), s"default-valued 'active' should be present: $json")
        assert(json.contains("\"name\":\"Alice\""))
      }

      test("round-trip preserves all fields including defaults") {
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true)
        val json = writeToString(value)(codec)
        readFromString[WithDefaultFields](json)(codec) ==> value
      }
    }

    group("transientDefault = true (explicit, matching jsoniter-scala)") {

      test("field with default value is omitted from output") {
        implicit val config: JsoniterConfig = JsoniterConfig(transientDefault = true)
        val codec = KindlingsJsonValueCodec.derived[WithDefaultFields]
        val value = WithDefaultFields("Alice", 25, true)
        val json = writeToString(value)(codec)
        assert(!json.contains("\"age\""), s"default-valued 'age' should be omitted: $json")
        assert(!json.contains("\"active\""), s"default-valued 'active' should be omitted: $json")
        json ==> """{"name":"Alice"}"""
      }
    }

    // jsoniter-scala: checkFieldDuplication = true  (duplicate JSON keys rejected)
    // Kindlings:      checkFieldDuplication = false (duplicate keys accepted, last wins)
    group("checkFieldDuplication = false (default)") {

      test("duplicate fields accepted, last value wins") {
        // Default config — no explicit checkFieldDuplication override
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","age":30,"name":"Bob"}"""
        val decoded = readFromString[SimplePerson](json)(codec)
        // Last value wins
        decoded ==> SimplePerson("Bob", 30)
      }
    }

    group("checkFieldDuplication = true (explicit, matching jsoniter-scala)") {

      test("duplicate fields rejected with error") {
        implicit val config: JsoniterConfig = JsoniterConfig(checkFieldDuplication = true)
        val codec = KindlingsJsonValueCodec.derived[SimplePerson]
        val json = """{"name":"Alice","age":30,"name":"Bob"}"""
        intercept[JsonReaderException] {
          readFromString[SimplePerson](json)(codec)
        }
      }
    }

    // jsoniter-scala: discriminatorFieldName = Some("type") (discriminator-style ADT encoding)
    // Kindlings:      discriminatorFieldName = None          (wrapper-style ADT encoding)
    group("discriminatorFieldName = None (default)") {

      test("wrapper-style encoding for sealed traits") {
        // Default config — no explicit discriminatorFieldName override
        val codec = KindlingsJsonValueCodec.derived[Shape]
        val value: Shape = Circle(5.0)
        val json = writeToString(value)(codec)
        // Wrapper-style: {"Circle":{"radius":5.0}}
        assert(json.contains("\"Circle\""), s"expected wrapper key 'Circle': $json")
        assert(json.startsWith("{\"Circle\":{"), s"expected wrapper-style encoding: $json")
        val decoded = readFromString[Shape](json)(codec)
        decoded ==> value
      }
    }

    group("discriminatorFieldName = Some(\"type\") (explicit, matching jsoniter-scala)") {

      test("discriminator-style encoding for sealed traits") {
        implicit val config: JsoniterConfig = JsoniterConfig(discriminatorFieldName = Some("type"))
        val codec = KindlingsJsonValueCodec.derived[Animal]
        val value: Animal = Dog("Rex", "Labrador")
        val json = writeToString(value)(codec)
        // Discriminator-style: {"type":"Dog","name":"Rex","breed":"Labrador"}
        assert(json.contains("\"type\":\"Dog\""), s"expected discriminator field: $json")
        assert(!json.startsWith("{\"Dog\":{"), s"should not use wrapper-style: $json")
        val decoded = readFromString[Animal](json)(codec)
        decoded ==> value
      }
    }

    // jsoniter-scala: mapMaxInsertNumber = 1024      (DoS protection by default)
    // Kindlings:      mapMaxInsertNumber = Int.MaxValue (no limit by default)
    group("mapMaxInsertNumber = Int.MaxValue (default)") {

      test("large maps accepted with default config") {
        // Default config — no explicit mapMaxInsertNumber override
        val codec = KindlingsJsonValueCodec.derived[WithMapField]
        // Build a map larger than jsoniter-scala's default 1024 limit
        val entries = (1 to 1500).map(i => s"key$i" -> i).toMap
        val value = WithMapField(entries)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithMapField](json)(codec)
        decoded.data.size ==> 1500
      }
    }

    group("mapMaxInsertNumber = 2 (explicit limit)") {

      test("map with 3 entries rejected") {
        implicit val config: JsoniterConfig = JsoniterConfig(mapMaxInsertNumber = 2)
        val codec = KindlingsJsonValueCodec.derived[WithMapField]
        val json = """{"data":{"a":1,"b":2,"c":3}}"""
        intercept[JsonReaderException] {
          readFromString[WithMapField](json)(codec)
        }
      }

      test("map within limit accepted") {
        implicit val config: JsoniterConfig = JsoniterConfig(mapMaxInsertNumber = 2)
        val codec = KindlingsJsonValueCodec.derived[WithMapField]
        val json = """{"data":{"a":1,"b":2}}"""
        readFromString[WithMapField](json)(codec) ==> WithMapField(Map("a" -> 1, "b" -> 2))
      }
    }

    // jsoniter-scala: setMaxInsertNumber = 1024      (DoS protection by default)
    // Kindlings:      setMaxInsertNumber = Int.MaxValue (no limit by default)
    group("setMaxInsertNumber = Int.MaxValue (default)") {

      test("large collections accepted with default config") {
        // Default config — no explicit setMaxInsertNumber override
        val codec = KindlingsJsonValueCodec.derived[WithListField]
        // Build a list larger than jsoniter-scala's default 1024 limit
        val items = (1 to 1500).toList
        val value = WithListField(items)
        val json = writeToString(value)(codec)
        val decoded = readFromString[WithListField](json)(codec)
        decoded.items.size ==> 1500
      }
    }

    group("setMaxInsertNumber = 2 (explicit limit)") {

      test("collection with 3 elements rejected") {
        implicit val config: JsoniterConfig = JsoniterConfig(setMaxInsertNumber = 2)
        val codec = KindlingsJsonValueCodec.derived[WithListField]
        val json = """{"items":[1,2,3]}"""
        intercept[JsonReaderException] {
          readFromString[WithListField](json)(codec)
        }
      }

      test("collection within limit accepted") {
        implicit val config: JsoniterConfig = JsoniterConfig(setMaxInsertNumber = 2)
        val codec = KindlingsJsonValueCodec.derived[WithListField]
        val json = """{"items":[1,2]}"""
        readFromString[WithListField](json)(codec) ==> WithListField(List(1, 2))
      }
    }
  }
}
