package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Json, KeyDecoder}

final class KindlingsDecoderSpec extends MacroSuite {

  group("KindlingsDecoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        KindlingsDecoder.decode[Int](Json.fromInt(42)) ==> Right(42)
      }

      test("String") {
        KindlingsDecoder.decode[String](Json.fromString("hello")) ==> Right("hello")
      }

      test("Boolean") {
        KindlingsDecoder.decode[Boolean](Json.True) ==> Right(true)
      }

      test("Double") {
        KindlingsDecoder.decode[Double](Json.fromDoubleOrNull(3.14)) ==> Right(3.14)
      }

      test("Long") {
        KindlingsDecoder.decode[Long](Json.fromLong(42L)) ==> Right(42L)
      }
    }

    group("case classes") {

      test("simple case class") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[SimplePerson](json) ==> Right(SimplePerson("Alice", 30))
      }

      test("empty case class") {
        KindlingsDecoder.decode[EmptyClass](Json.obj()) ==> Right(EmptyClass())
      }

      test("single field case class") {
        val json = Json.obj("value" -> Json.fromInt(42))
        KindlingsDecoder.decode[SingleField](json) ==> Right(SingleField(42))
      }

      test("nested case class (auto-derived)") {
        val json = Json.obj(
          "name" -> Json.fromString("Bob"),
          "age" -> Json.fromInt(25),
          "address" -> Json.obj(
            "street" -> Json.fromString("123 Main St"),
            "city" -> Json.fromString("Springfield")
          )
        )
        KindlingsDecoder.decode[PersonWithAddress](json) ==>
          Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
      }

      test("case class with List of case classes") {
        val json = Json.obj(
          "name" -> Json.fromString("Dev"),
          "members" -> Json.arr(
            Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)),
            Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25))
          )
        )
        KindlingsDecoder.decode[TeamWithMembers](json) ==>
          Right(TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25))))
      }
    }

    group("options") {

      test("Some value") {
        KindlingsDecoder.decode[Option[Int]](Json.fromInt(42)) ==> Right(Some(42))
      }

      test("None from null") {
        KindlingsDecoder.decode[Option[Int]](Json.Null) ==> Right(None)
      }
    }

    group("collections") {

      test("List of ints") {
        val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
        KindlingsDecoder.decode[List[Int]](json) ==> Right(List(1, 2, 3))
      }

      test("empty list") {
        KindlingsDecoder.decode[List[Int]](Json.arr()) ==> Right(List.empty[Int])
      }

      test("Vector of strings") {
        val json = Json.arr(Json.fromString("a"), Json.fromString("b"))
        KindlingsDecoder.decode[Vector[String]](json) ==> Right(Vector("a", "b"))
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        KindlingsDecoder.decode[WrappedInt](Json.fromInt(42)) ==> Right(WrappedInt(42))
      }
    }

    group("sealed traits") {

      test("wrapper-style decoding (default)") {
        val json = Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(5.0): Shape)
      }

      test("wrapper-style decoding for second case") {
        val json = Json.obj(
          "Rectangle" -> Json.obj(
            "width" -> Json.fromDoubleOrNull(3.0),
            "height" -> Json.fromDoubleOrNull(4.0)
          )
        )
        KindlingsDecoder.decode[Shape](json) ==> Right(Rectangle(3.0, 4.0): Shape)
      }

      test("discriminator-style decoding") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Dog"),
          "name" -> Json.fromString("Rex"),
          "breed" -> Json.fromString("Labrador")
        )
        KindlingsDecoder.decode[Animal](json) ==> Right(Dog("Rex", "Labrador"): Animal)
      }

      test("unknown discriminator produces error") {
        val json = Json.obj("Unknown" -> Json.obj())
        val Left(error) = KindlingsDecoder.decode[Shape](json): @unchecked
        error.getMessage ==> "DecodingFailure at .Unknown: Unknown type discriminator: Unknown. Expected one of: Circle, Rectangle"
      }
    }

    group("sealed traits with case object singletons") {

      test("decode case object singleton (wrapper-style)") {
        val json = Json.obj("Yes" -> Json.obj())
        KindlingsDecoder.decode[SimpleEnumCirce](json) ==> Right(Yes: SimpleEnumCirce)
      }

      test("decode second case object singleton") {
        val json = Json.obj("No" -> Json.obj())
        KindlingsDecoder.decode[SimpleEnumCirce](json) ==> Right(No: SimpleEnumCirce)
      }
    }

    group("string enum decoding (enumAsStrings)") {

      test("decode case-object-only sealed trait from string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[CardinalDirection](Json.fromString("North")) ==> Right(North: CardinalDirection)
      }

      test("decode all cases from strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[CardinalDirection](Json.fromString("South")) ==> Right(South: CardinalDirection)
        KindlingsDecoder.decode[CardinalDirection](Json.fromString("East")) ==> Right(East: CardinalDirection)
        KindlingsDecoder.decode[CardinalDirection](Json.fromString("West")) ==> Right(West: CardinalDirection)
      }

      test("enum as string with constructor name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsDecoder.decode[CardinalDirection](Json.fromString("north")) ==> Right(North: CardinalDirection)
      }

      test("non-string input fails with enumAsStrings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        assert(KindlingsDecoder.decode[CardinalDirection](Json.obj("North" -> Json.obj())).isLeft)
      }

      test("unknown string value fails") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        assert(KindlingsDecoder.decode[CardinalDirection](Json.fromString("NorthWest")).isLeft)
      }
    }

    group("Scala Enumeration decoder (enumAsStrings)") {

      test("decode Scala Enumeration value from string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[ScalaColor.Value](Json.fromString("Red")) ==> Right(ScalaColor.Red: ScalaColor.Value)
      }

      test("decode all Scala Enumeration values from strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[ScalaColor.Value](Json.fromString("Green")) ==> Right(
          ScalaColor.Green: ScalaColor.Value
        )
        KindlingsDecoder.decode[ScalaColor.Value](Json.fromString("Blue")) ==> Right(ScalaColor.Blue: ScalaColor.Value)
      }

      test("Scala Enumeration with name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsDecoder.decode[ScalaColor.Value](Json.fromString("red")) ==> Right(ScalaColor.Red: ScalaColor.Value)
      }
    }

    // Java enum tests are in KindlingsDecoderJvmSpec (src/test/scalajvm)

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(5.0): Shape)
      }
    }

    group("derive") {

      test("explicit derive returns Decoder") {
        val decoder: Decoder[SimplePerson] = KindlingsDecoder.derive[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        decoder.decodeJson(json) ==> Right(SimplePerson("Alice", 30))
      }

      test("derived provides KindlingsDecoder") {
        val decoder: KindlingsDecoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        decoder.decodeJson(json) ==> Right(SimplePerson("Alice", 30))
      }
    }

    group("custom implicit priority") {

      test("user-provided implicit Decoder works with derived") {
        // Verifies that derived skips the self type during implicit search,
        // preventing infinite recursion when assigned to an implicit val.
        implicit val decoder: Decoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        decoder.decodeJson(json) ==> Right(SimplePerson("Alice", 30))
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val json = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
        KindlingsDecoder.decode[Map[String, Int]](json) ==> Right(Map("a" -> 1, "b" -> 2))
      }

      test("empty map") {
        KindlingsDecoder.decode[Map[String, Int]](Json.obj()) ==> Right(Map.empty[String, Int])
      }

      test("Map[Int, String]") {
        val json = Json.obj("1" -> Json.fromString("a"), "2" -> Json.fromString("b"))
        KindlingsDecoder.decode[Map[Int, String]](json) ==> Right(Map(1 -> "a", 2 -> "b"))
      }

      test("Map[Long, String]") {
        val json = Json.obj("100" -> Json.fromString("x"))
        KindlingsDecoder.decode[Map[Long, String]](json) ==> Right(Map(100L -> "x"))
      }

      test("empty Map[Int, String]") {
        KindlingsDecoder.decode[Map[Int, String]](Json.obj()) ==> Right(Map.empty[Int, String])
      }

      test("case class with Map[Int, String] field") {
        val json = Json.obj("data" -> Json.obj("1" -> Json.fromString("a")))
        KindlingsDecoder.decode[WithIntKeyMap](json) ==> Right(WithIntKeyMap(Map(1 -> "a")))
      }

      test("Map[Int, List[String]] nested") {
        val json = Json.obj("1" -> Json.arr(Json.fromString("a"), Json.fromString("b")))
        KindlingsDecoder.decode[Map[Int, List[String]]](json) ==> Right(Map(1 -> List("a", "b")))
      }

      test("value type key Map[UserId, String]") {
        val json = Json.obj("42" -> Json.fromString("alice"))
        KindlingsDecoder.decode[Map[UserId, String]](json) ==> Right(Map(UserId(42) -> "alice"))
      }

      test("enum key Map[CardinalDirection, String]") {
        val json = Json.obj("North" -> Json.fromString("up"), "South" -> Json.fromString("down"))
        KindlingsDecoder.decode[Map[CardinalDirection, String]](json) ==>
          Right(Map[CardinalDirection, String](North -> "up", South -> "down"))
      }

      test("Map[Int, String] invalid key returns Left") {
        val json = Json.obj("abc" -> Json.fromString("x"))
        assert(KindlingsDecoder.decode[Map[Int, String]](json).isLeft)
      }
    }

    group("key codec derivation") {

      test("Int key decodes") {
        val json = Json.obj("42" -> Json.fromString("a"))
        KindlingsDecoder.decode[Map[Int, String]](json) ==> Right(Map(42 -> "a"))
      }

      test("Int key matches KeyDecoder[Int]") {
        KeyDecoder[Int].apply("42") ==> Some(42)
        val json = Json.obj("42" -> Json.fromString("a"))
        KindlingsDecoder.decode[Map[Int, String]](json) ==> Right(Map(42 -> "a"))
      }

      test("invalid Int key returns Left") {
        val json = Json.obj("abc" -> Json.fromString("x"))
        assert(KindlingsDecoder.decode[Map[Int, String]](json).isLeft)
      }

      test("Long key decodes") {
        val json = Json.obj("100" -> Json.fromString("x"))
        KindlingsDecoder.decode[Map[Long, String]](json) ==> Right(Map(100L -> "x"))
      }

      test("invalid Long key returns Left") {
        val json = Json.obj("abc" -> Json.fromString("x"))
        assert(KindlingsDecoder.decode[Map[Long, String]](json).isLeft)
      }

      test("Double key decodes") {
        val json = Json.obj("3.14" -> Json.fromString("pi"))
        KindlingsDecoder.decode[Map[Double, String]](json) ==> Right(Map(3.14 -> "pi"))
      }

      test("Short key decodes") {
        val json = Json.obj("42" -> Json.fromString("a"))
        KindlingsDecoder.decode[Map[Short, String]](json) ==> Right(Map(42.toShort -> "a"))
      }

      test("Byte key decodes") {
        val json = Json.obj("7" -> Json.fromString("a"))
        KindlingsDecoder.decode[Map[Byte, String]](json) ==> Right(Map(7.toByte -> "a"))
      }

      test("user-provided KeyDecoder[UserId] is used") {
        implicit val userIdKeyDecoder: KeyDecoder[UserId] = KeyDecoder.instance { s =>
          if (s.startsWith("user-")) scala.util.Try(UserId(s.stripPrefix("user-").toInt)).toOption
          else None
        }
        val json = Json.obj("user-42" -> Json.fromString("alice"))
        KindlingsDecoder.decode[Map[UserId, String]](json) ==> Right(Map(UserId(42) -> "alice"))
      }

      test("value type key without user implicit unwraps") {
        val json = Json.obj("42" -> Json.fromString("alice"))
        KindlingsDecoder.decode[Map[UserId, String]](json) ==> Right(Map(UserId(42) -> "alice"))
      }

      test("enum key decodes") {
        val json = Json.obj("North" -> Json.fromString("up"))
        KindlingsDecoder.decode[Map[CardinalDirection, String]](json) ==>
          Right(Map[CardinalDirection, String](North -> "up"))
      }
    }

    group("recursive types") {

      test("recursive tree") {
        val json = Json.obj(
          "value" -> Json.fromInt(1),
          "children" -> Json.arr(
            Json.obj("value" -> Json.fromInt(2), "children" -> Json.arr()),
            Json.obj(
              "value" -> Json.fromInt(3),
              "children" -> Json.arr(
                Json.obj("value" -> Json.fromInt(4), "children" -> Json.arr())
              )
            )
          )
        )
        KindlingsDecoder.decode[RecursiveTree](json) ==>
          Right(RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil))))))
      }
    }

    group("sets") {

      test("Set of ints") {
        val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
        KindlingsDecoder.decode[Set[Int]](json) ==> Right(Set(1, 2, 3))
      }

      test("empty set") {
        KindlingsDecoder.decode[Set[Int]](Json.arr()) ==> Right(Set.empty[Int])
      }
    }

    group("configuration — member name transforms") {

      test("snake_case member names") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val json = Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }

      test("kebab-case member names") {
        implicit val config: Configuration = Configuration.default.withKebabCaseMemberNames
        val json = Json.obj("first-name" -> Json.fromString("Alice"), "last-name" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }

      test("PascalCase member names") {
        implicit val config: Configuration = Configuration.default.withPascalCaseMemberNames
        val json = Json.obj("FirstName" -> Json.fromString("Alice"), "LastName" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }

      test("SCREAMING_SNAKE_CASE member names") {
        implicit val config: Configuration = Configuration.default.withScreamingSnakeCaseMemberNames
        val json = Json.obj("FIRST_NAME" -> Json.fromString("Alice"), "LAST_NAME" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }
    }

    group("strictDecoding") {

      test("passes with exact fields") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[SimplePerson](json) ==> Right(SimplePerson("Alice", 30))
      }

      test("fails with unexpected fields") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj(
          "name" -> Json.fromString("Alice"),
          "age" -> Json.fromInt(30),
          "extra" -> Json.fromString("unexpected")
        )
        val Left(error) = KindlingsDecoder.decode[SimplePerson](json): @unchecked
        error.message ==> "Unexpected field(s): extra"
      }

      test("passes without strictDecoding even with extra fields") {
        val json = Json.obj(
          "name" -> Json.fromString("Alice"),
          "age" -> Json.fromInt(30),
          "extra" -> Json.fromString("ignored")
        )
        KindlingsDecoder.decode[SimplePerson](json) ==> Right(SimplePerson("Alice", 30))
      }

      test("empty case class strict rejects any fields") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj("foo" -> Json.fromInt(1))
        val Left(error) = KindlingsDecoder.decode[EmptyClass](json): @unchecked
        error.message ==> "Unexpected field(s): foo"
      }

      test("strictDecoding with name transform") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding.withSnakeCaseMemberNames
        val json = Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }
    }

    group("useDefaults") {

      test("uses default when field missing and useDefaults=true") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[PersonWithDefaults](json) ==> Right(PersonWithDefaults("Alice", 25))
      }

      test("uses provided value even with useDefaults=true") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[PersonWithDefaults](json) ==> Right(PersonWithDefaults("Alice", 30))
      }

      test("fails when field missing and useDefaults=false") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        val Left(error) = KindlingsDecoder.decode[PersonWithDefaults](json): @unchecked
        error.getMessage ==> "DecodingFailure at .age: Missing required field"
      }

      test("all defaults used when JSON is empty object") {
        implicit val config: Configuration = Configuration.default.withDefaults
        KindlingsDecoder.decode[AllDefaults](Json.obj()) ==> Right(AllDefaults())
      }

      test("field without default still required when useDefaults=true") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("age" -> Json.fromInt(30))
        val Left(error) = KindlingsDecoder.decode[PersonWithDefaults](json): @unchecked
        error.getMessage ==> "DecodingFailure at .name: Missing required field"
      }

      test("strictDecoding combined with useDefaults") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding.withDefaults
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[PersonWithDefaults](json) ==> Right(PersonWithDefaults("Alice", 25))
      }
    }

    group("tuples") {

      test("decode (Int, String) from JSON array") {
        val json = Json.arr(Json.fromInt(42), Json.fromString("hello"))
        KindlingsDecoder.decode[(Int, String)](json) ==> Right((42, "hello"))
      }

      test("decode (Int, String, Boolean) from JSON array") {
        val json = Json.arr(Json.fromInt(42), Json.fromString("hello"), Json.True)
        KindlingsDecoder.decode[(Int, String, Boolean)](json) ==> Right((42, "hello", true))
      }
    }

    group("generic case classes") {

      test("Box[Int]") {
        val json = Json.obj("value" -> Json.fromInt(42))
        KindlingsDecoder.decode[Box[Int]](json) ==> Right(Box(42))
      }

      test("Pair[String, Int]") {
        val json = Json.obj("first" -> Json.fromString("hello"), "second" -> Json.fromInt(42))
        KindlingsDecoder.decode[Pair[String, Int]](json) ==> Right(Pair("hello", 42))
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        val json = Json.obj(
          "name" -> Json.fromString("Alice"),
          "address" -> Json.obj(
            "street" -> Json.fromString("123 Main"),
            "city" -> Json.fromString("NYC"),
            "geo" -> Json.obj(
              "lat" -> Json.fromDoubleOrNull(40.7),
              "lon" -> Json.fromDoubleOrNull(-74.0)
            )
          )
        )
        KindlingsDecoder.decode[PersonFull](json) ==>
          Right(PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0))))
      }
    }

    group("type aliases") {

      test("WithAlias decodes type alias field") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[WithAlias](json) ==> Right(WithAlias("Alice", 30))
      }
    }

    group("combined configuration") {

      test("snake_case + discriminator + constructor transform") {
        implicit val config: Configuration = Configuration(
          transformMemberNames = Configuration.snakeCase,
          transformConstructorNames = _.toLowerCase,
          discriminator = Some("type")
        )
        val json = Json.obj(
          "type" -> Json.fromString("dog"),
          "name" -> Json.fromString("Rex"),
          "breed" -> Json.fromString("Labrador")
        )
        KindlingsDecoder.decode[Animal](json) ==> Right(Dog("Rex", "Labrador"): Animal)
      }

      test("useDefaults + strictDecoding + snake_case") {
        implicit val config: Configuration =
          Configuration.default.withDefaults.withStrictDecoding.withSnakeCaseMemberNames
        val json = Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[CamelCaseFields](json) ==> Right(CamelCaseFields("Alice", "Smith"))
      }
    }

    group("Option null vs absent key") {

      test("Option field present with null decodes to None") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "opt" -> Json.Null)
        KindlingsDecoder.decode[WithOptionalField](json) ==> Right(WithOptionalField("Alice", None))
      }

      test("Option field present with value decodes to Some") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "opt" -> Json.fromString("value"))
        KindlingsDecoder.decode[WithOptionalField](json) ==> Right(WithOptionalField("Alice", Some("value")))
      }

      test("Option field absent (no default) decodes to None") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[WithOptionalField](json) ==> Right(WithOptionalField("Alice", None))
      }

      test("Option field absent with useDefaults uses default") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[WithOptionalAndDefault](json) ==> Right(
          WithOptionalAndDefault("Alice", Some("default"))
        )
      }

      test("Option field present null with useDefaults decodes to None (not default)") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("Alice"), "opt" -> Json.Null)
        KindlingsDecoder.decode[WithOptionalAndDefault](json) ==> Right(WithOptionalAndDefault("Alice", None))
      }

      test("Option field absent without useDefaults still decodes to None") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[WithOptionalAndDefault](json) ==> Right(WithOptionalAndDefault("Alice", None))
      }
    }

    group("higher-kinded types") {

      test("HigherKindedType[List] decodes correctly") {
        val json = Json.obj("value" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
        KindlingsDecoder.decode[HigherKindedType[List]](json) ==> Right(HigherKindedType[List](List(1, 2, 3)))
      }

      test("HigherKindedType[Option] decodes correctly") {
        val json = Json.obj("value" -> Json.fromInt(42))
        KindlingsDecoder.decode[HigherKindedType[Option]](json) ==> Right(HigherKindedType[Option](Some(42)))
      }
    }

    group("empty class with non-object input") {

      test("decode Int as EmptyClass fails with Expected JSON object") {
        val Left(error) = KindlingsDecoder.decode[EmptyClass](Json.fromInt(42)): @unchecked
        error.message ==> "Expected JSON object"
      }

      test("decode empty object as EmptyClass succeeds") {
        KindlingsDecoder.decode[EmptyClass](Json.obj()) ==> Right(EmptyClass())
      }
    }

    group("error handling") {

      test("missing required field") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        val Left(error) = KindlingsDecoder.decode[SimplePerson](json): @unchecked
        error.getMessage ==> "DecodingFailure at .age: Missing required field"
      }

      test("wrong type for field") {
        val json = Json.obj("name" -> Json.fromInt(42), "age" -> Json.fromInt(30))
        val Left(error) = KindlingsDecoder.decode[SimplePerson](json): @unchecked
        error.getMessage ==> "DecodingFailure at .name: Got value '42' with wrong type, expecting string"
      }

      test("unknown discriminator in wrapper-style") {
        val json = Json.obj("Unknown" -> Json.obj())
        val Left(error) = KindlingsDecoder.decode[Shape](json): @unchecked
        error.getMessage ==> "DecodingFailure at .Unknown: Unknown type discriminator: Unknown. Expected one of: Circle, Rectangle"
      }
    }

    group("per-field annotations") {

      test("@fieldName overrides field name in decoding") {
        val json = Json.obj("user_name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[CirceWithFieldName](json) ==> Right(CirceWithFieldName("Alice", 30))
      }

      test("@fieldName takes precedence over config transform") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val json = Json.obj("user_name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsDecoder.decode[CirceWithFieldName](json) ==> Right(CirceWithFieldName("Alice", 30))
      }

      test("@transientField uses default value during decoding") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[CirceWithTransient](json) ==> Right(CirceWithTransient("Alice", None))
      }

      test("@transientField ignores field even if present in JSON") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "cache" -> Json.fromString("cached"))
        KindlingsDecoder.decode[CirceWithTransient](json) ==> Right(CirceWithTransient("Alice", None))
      }

      test("both annotations combined") {
        val json = Json.obj("display_name" -> Json.fromString("Alice"), "active" -> Json.True)
        KindlingsDecoder.decode[CirceWithBothAnnotations](json) ==>
          Right(CirceWithBothAnnotations("Alice", 0, true))
      }

      test("@transientField round-trip preserves non-transient fields") {
        val original = CirceWithTransient("Alice", Some("cached"))
        val json = KindlingsEncoder.encode(original)
        KindlingsDecoder.decode[CirceWithTransient](json) ==> Right(CirceWithTransient("Alice", None))
      }

      test("@transientField without default is compile error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsDecoder, annotations}
          import io.circe.Json
          case class BadTransient(@annotations.transientField x: Int)
          KindlingsDecoder.decode[BadTransient](Json.obj())
          """
        ).check(
          "@transientField on field 'x'",
          "requires a default value"
        )
      }
    }

    group("non-case-class sealed trait leaves") {

      test("sealed trait with non-case-class leaf using user-provided implicit") {
        implicit val plainLeafDecoder: Decoder[PlainLeaf] =
          Decoder.instance(c => c.downField("x").as[Int].map(new PlainLeaf(_)))
        val json = Json.obj("PlainLeaf" -> Json.obj("x" -> Json.fromInt(42)))
        KindlingsDecoder.decode[MixedADT](json) ==> Right(new PlainLeaf(42): MixedADT)
      }

      test("sealed trait case class leaf still derives normally") {
        implicit val plainLeafDecoder: Decoder[PlainLeaf] =
          Decoder.instance(c => c.downField("x").as[Int].map(new PlainLeaf(_)))
        val json = Json.obj("CaseLeaf" -> Json.obj("x" -> Json.fromInt(7)))
        KindlingsDecoder.decode[MixedADT](json) ==> Right(CaseLeaf(7): MixedADT)
      }
    }

    // java.time tests are in KindlingsDecoderJvmSpec (src/test/scalajvm)

    group("error accumulation (decodeAccumulating)") {

      test("accumulates errors across multiple fields") {
        val json = Json.obj("name" -> Json.fromInt(42), "age" -> Json.fromString("nope"))
        val decoder = KindlingsDecoder.derive[SimplePerson]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isInvalid)
        result.fold(
          errors => assert(errors.size >= 2),
          _ => fail("expected invalid")
        )
      }

      test("returns Valid on correct input") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        val decoder = KindlingsDecoder.derive[SimplePerson]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isValid)
        result.fold(
          _ => fail("expected valid"),
          person => person ==> SimplePerson("Alice", 30)
        )
      }

      test("accumulates errors in nested case classes") {
        val json = Json.obj(
          "name" -> Json.fromInt(42),
          "age" -> Json.fromString("nope"),
          "address" -> Json.obj("street" -> Json.fromInt(0), "city" -> Json.fromInt(0))
        )
        val decoder = KindlingsDecoder.derive[PersonWithAddress]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isInvalid)
        result.fold(
          errors =>
            assert(errors.size >= 3), // name, age, address (nested inline-derived types use default decodeAccumulating)
          _ => fail("expected invalid")
        )
      }

      test("single field error gives one error") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromString("nope"))
        val decoder = KindlingsDecoder.derive[SimplePerson]
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isInvalid)
        result.fold(
          errors => errors.size ==> 1,
          _ => fail("expected invalid")
        )
      }

      test("derived (implicit) decoder has decodeAccumulating override") {
        val decoder: KindlingsDecoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromInt(42), "age" -> Json.fromString("nope"))
        val result = decoder.decodeAccumulating(json.hcursor)
        assert(result.isInvalid)
        result.fold(
          errors => assert(errors.size >= 2),
          _ => fail("expected invalid")
        )
      }

      test("empty case class accumulating returns Valid") {
        val decoder = KindlingsDecoder.derive[EmptyClass]
        val result = decoder.decodeAccumulating(Json.obj().hcursor)
        assert(result.isValid)
      }
    }

    group("UTF-8 field names") {

      test("@fieldName with non-ASCII characters decodes correctly") {
        val json = Json.obj(
          "名前" -> Json.fromString("Alice"),
          "données" -> Json.fromInt(30),
          "field with spaces" -> Json.fromBoolean(true)
        )
        KindlingsDecoder.decode[CirceWithUtf8FieldNames](json) ==>
          Right(CirceWithUtf8FieldNames("Alice", 30, true))
      }
    }

    group("compile-time errors") {

      test("decode with unhandled type produces error message") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsDecoder, NotACirceType}
          import io.circe.Json
          KindlingsDecoder.decode[NotACirceType](Json.obj())
          """
        ).check(
          "Macro derivation failed with the following errors:",
          "  - The type hearth.kindlings.circederivation.NotACirceType was not handled by any decoder derivation rule:",
          "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
        )
      }

      test("decode with Nothing type parameter produces clear error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.KindlingsDecoder
          import io.circe.Json
          val result = KindlingsDecoder.decode(Json.obj())
          """
        ).check(
          "type parameter was inferred as"
        )
      }
    }

    group("edge cases") {

      test("null for non-Option field fails decode") {
        val json = io.circe.parser.parse("""{"name":null,"age":30}""").getOrElse(Json.Null)
        assert(KindlingsDecoder.decode[SimplePerson](json).isLeft)
      }

      test("fields in different order than case class") {
        val json = io.circe.parser.parse("""{"age":30,"name":"Alice"}""").getOrElse(Json.Null)
        KindlingsDecoder.decode[SimplePerson](json) ==> Right(SimplePerson("Alice", 30))
      }
    }

    group("Option + default interaction") {

      test("null overrides default for Option[String]") {
        val json = Json.obj("name" -> Json.fromString("test"), "opt" -> Json.Null)
        KindlingsDecoder.decode[WithOptionAndDefault](json) ==> Right(WithOptionAndDefault("test", None))
      }

      test("missing field uses default for Option[String]") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("test"))
        KindlingsDecoder.decode[WithOptionAndDefault](json) ==> Right(WithOptionAndDefault("test", Some("default")))
      }

      test("null for non-Option with default fails decode") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("test"), "age" -> Json.Null)
        assert(KindlingsDecoder.decode[PersonWithDefaults](json).isLeft)
      }

      test("missing key for Option without default") {
        val json = Json.obj("name" -> Json.fromString("test"))
        KindlingsDecoder.decode[WithOptionNoDefault](json) ==> Right(WithOptionNoDefault("test", None))
      }

      test("provided value overrides default") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("test"), "opt" -> Json.fromString("explicit"))
        KindlingsDecoder.decode[WithOptionAndDefault](json) ==> Right(WithOptionAndDefault("test", Some("explicit")))
      }

      test("missing non-Option without default fails") {
        val json = Json.obj("name" -> Json.fromString("test"))
        val Left(error) = KindlingsDecoder.decode[PersonWithDefaults](json): @unchecked
        error.getMessage ==> "DecodingFailure at .age: Missing required field"
      }
    }

    group("constructor name transforms extended") {

      test("PascalCase constructors") {
        implicit val config: Configuration = Configuration.default.withPascalCaseConstructorNames
        val encoded = KindlingsEncoder.encode[Shape](Circle(5.0))
        val obj = encoded.asObject.get
        assert(obj.contains("Circle"))
        KindlingsDecoder.decode[Shape](encoded) ==> Right(Circle(5.0): Shape)
      }

      test("SCREAMING_SNAKE_CASE constructors") {
        implicit val config: Configuration = Configuration.default.withScreamingSnakeCaseConstructorNames
        val encoded = KindlingsEncoder.encode[Shape](Circle(5.0))
        val obj = encoded.asObject.get
        assert(obj.contains("CIRCLE"))
        KindlingsDecoder.decode[Shape](encoded) ==> Right(Circle(5.0): Shape)
      }
    }

    group("snake_case consecutive capitals") {

      test("HTMLParser becomes html_parser") {
        Configuration.default.withSnakeCaseMemberNames.transformMemberNames("HTMLParser") ==> "html_parser"
      }

      test("firstName becomes first_name") {
        Configuration.default.withSnakeCaseMemberNames.transformMemberNames("firstName") ==> "first_name"
      }

      test("getHTTPSResponse becomes get_https_response") {
        Configuration.default.withSnakeCaseMemberNames.transformMemberNames("getHTTPSResponse") ==>
          "get_https_response"
      }

      test("kebab-case HTMLParser becomes html-parser") {
        Configuration.default.withKebabCaseMemberNames.transformMemberNames("HTMLParser") ==> "html-parser"
      }
    }

    group("sealed trait hierarchy") {

      test("multi-level hierarchy encodes/decodes") {
        // Uncle is a direct child of GrandParent on both Scala 2 and 3
        val original: GrandParent = Uncle("Bob")
        val encoded = KindlingsEncoder.encode[GrandParent](original)
        val obj = encoded.asObject.get
        assert(obj.contains("Uncle"))
        KindlingsDecoder.decode[GrandParent](encoded) ==> Right(Uncle("Bob"): GrandParent)
      }
    }

    group("strict decoding extended") {

      test("strict decoding rejects unknown fields with field list in error") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj(
          "name" -> Json.fromString("Alice"),
          "age" -> Json.fromInt(30),
          "foo" -> Json.fromString("bar"),
          "baz" -> Json.fromInt(99)
        )
        val Left(error) = KindlingsDecoder.decode[SimplePerson](json): @unchecked
        assert(error.message.contains("Unexpected field(s):"))
        assert(error.message.contains("baz"))
        assert(error.message.contains("foo"))
      }
    }
  }
}
