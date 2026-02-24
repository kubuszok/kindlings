package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Json}

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
    }
  }
}
