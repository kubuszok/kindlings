package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json, KeyEncoder}

final class KindlingsEncoderSpec extends MacroSuite {

  group("KindlingsEncoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        KindlingsEncoder.encode(42) ==> Json.fromInt(42)
      }

      test("String") {
        KindlingsEncoder.encode("hello") ==> Json.fromString("hello")
      }

      test("Boolean") {
        KindlingsEncoder.encode(true) ==> Json.True
      }

      test("Double") {
        KindlingsEncoder.encode(3.14) ==> Json.fromDoubleOrNull(3.14)
      }

      test("Long") {
        KindlingsEncoder.encode(42L) ==> Json.fromLong(42L)
      }
    }

    group("case classes") {

      test("simple case class") {
        KindlingsEncoder.encode(SimplePerson("Alice", 30)) ==>
          Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }

      test("empty case class") {
        KindlingsEncoder.encode(EmptyClass()) ==> Json.obj()
      }

      test("single field case class") {
        KindlingsEncoder.encode(SingleField(42)) ==> Json.obj("value" -> Json.fromInt(42))
      }

      test("nested case class") {
        KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))) ==>
          Json.obj(
            "name" -> Json.fromString("Bob"),
            "age" -> Json.fromInt(25),
            "address" -> Json.obj(
              "street" -> Json.fromString("123 Main St"),
              "city" -> Json.fromString("Springfield")
            )
          )
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        KindlingsEncoder.encode(WrappedInt(42)) ==> Json.fromInt(42)
      }
    }

    group("options") {

      test("Some value") {
        KindlingsEncoder.encode(Option(42)) ==> Json.fromInt(42)
      }

      test("None") {
        KindlingsEncoder.encode(Option.empty[Int]) ==> Json.Null
      }
    }

    group("collections") {

      test("List of ints") {
        KindlingsEncoder.encode(List(1, 2, 3)) ==>
          Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
      }

      test("empty list") {
        KindlingsEncoder.encode(List.empty[Int]) ==> Json.arr()
      }

      test("Vector of strings") {
        KindlingsEncoder.encode(Vector("a", "b")) ==>
          Json.arr(Json.fromString("a"), Json.fromString("b"))
      }

      test("List of case classes") {
        KindlingsEncoder.encode(
          TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        ) ==>
          Json.obj(
            "name" -> Json.fromString("Dev"),
            "members" -> Json.arr(
              Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)),
              Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25))
            )
          )
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val json = KindlingsEncoder.encode(Map("a" -> 1, "b" -> 2))
        val obj = json.asObject.get
        obj("a") ==> Some(Json.fromInt(1))
        obj("b") ==> Some(Json.fromInt(2))
      }

      test("empty map") {
        KindlingsEncoder.encode(Map.empty[String, Int]) ==> Json.obj()
      }

      test("Map[Int, String]") {
        val json = KindlingsEncoder.encode(Map(1 -> "a", 2 -> "b"))
        val obj = json.asObject.get
        obj("1") ==> Some(Json.fromString("a"))
        obj("2") ==> Some(Json.fromString("b"))
      }

      test("Map[Long, String]") {
        val json = KindlingsEncoder.encode(Map(100L -> "x"))
        val obj = json.asObject.get
        obj("100") ==> Some(Json.fromString("x"))
      }

      test("empty Map[Int, String]") {
        KindlingsEncoder.encode(Map.empty[Int, String]) ==> Json.obj()
      }

      test("case class with Map[Int, String] field") {
        val json = KindlingsEncoder.encode(WithIntKeyMap(Map(1 -> "a")))
        val obj = json.asObject.get
        val data = obj("data").get.asObject.get
        data("1") ==> Some(Json.fromString("a"))
      }

      test("Map[Int, List[String]] nested") {
        val json = KindlingsEncoder.encode(Map(1 -> List("a", "b")))
        val obj = json.asObject.get
        obj("1") ==> Some(Json.arr(Json.fromString("a"), Json.fromString("b")))
      }

      test("value type key Map[UserId, String]") {
        val json = KindlingsEncoder.encode(Map(UserId(42) -> "alice"))
        val obj = json.asObject.get
        obj("42") ==> Some(Json.fromString("alice"))
      }

      test("enum key Map[CardinalDirection, String]") {
        val json = KindlingsEncoder.encode(Map[CardinalDirection, String](North -> "up", South -> "down"))
        val obj = json.asObject.get
        obj("North") ==> Some(Json.fromString("up"))
        obj("South") ==> Some(Json.fromString("down"))
      }
    }

    group("key codec derivation") {

      test("Int key encodes as toString") {
        val json = KindlingsEncoder.encode(Map(42 -> "a", -1 -> "b"))
        val obj = json.asObject.get
        obj("42") ==> Some(Json.fromString("a"))
        obj("-1") ==> Some(Json.fromString("b"))
      }

      test("Int key matches KeyEncoder[Int]") {
        val json = KindlingsEncoder.encode(Map(42 -> "a"))
        val jsonKey = json.asObject.get.keys.head
        jsonKey ==> KeyEncoder[Int].apply(42)
      }

      test("Long key matches KeyEncoder[Long]") {
        val json = KindlingsEncoder.encode(Map(42L -> "a"))
        val jsonKey = json.asObject.get.keys.head
        jsonKey ==> KeyEncoder[Long].apply(42L)
      }

      test("Double key matches KeyEncoder[Double]") {
        val json = KindlingsEncoder.encode(Map(3.14 -> "pi"))
        val jsonKey = json.asObject.get.keys.head
        jsonKey ==> KeyEncoder[Double].apply(3.14)
      }

      test("Short key encodes as toString") {
        val json = KindlingsEncoder.encode(Map(42.toShort -> "a"))
        val obj = json.asObject.get
        obj("42") ==> Some(Json.fromString("a"))
      }

      test("Byte key encodes as toString") {
        val json = KindlingsEncoder.encode(Map(7.toByte -> "a"))
        val obj = json.asObject.get
        obj("7") ==> Some(Json.fromString("a"))
      }

      test("user-provided KeyEncoder[UserId] is used") {
        implicit val userIdKeyEncoder: KeyEncoder[UserId] = KeyEncoder.instance(uid => s"user-${uid.value}")
        val json = KindlingsEncoder.encode(Map(UserId(42) -> "alice"))
        val obj = json.asObject.get
        obj("user-42") ==> Some(Json.fromString("alice"))
      }

      test("value type key without user implicit uses unwrap") {
        val json = KindlingsEncoder.encode(Map(UserId(42) -> "alice"))
        val obj = json.asObject.get
        obj("42") ==> Some(Json.fromString("alice"))
      }

      test("enum key encodes as toString") {
        val json = KindlingsEncoder.encode(Map[CardinalDirection, String](North -> "up"))
        val obj = json.asObject.get
        obj("North") ==> Some(Json.fromString("up"))
      }
    }

    group("sealed traits") {

      test("wrapper-style encoding (default)") {
        KindlingsEncoder.encode[Shape](Circle(5.0)) ==>
          Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
      }

      test("wrapper-style encoding for second case") {
        KindlingsEncoder.encode[Shape](Rectangle(3.0, 4.0)) ==>
          Json.obj(
            "Rectangle" -> Json.obj(
              "width" -> Json.fromDoubleOrNull(3.0),
              "height" -> Json.fromDoubleOrNull(4.0)
            )
          )
      }

      test("discriminator-style encoding") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        KindlingsEncoder.encode[Animal](Dog("Rex", "Labrador")) ==>
          Json.obj(
            "type" -> Json.fromString("Dog"),
            "name" -> Json.fromString("Rex"),
            "breed" -> Json.fromString("Labrador")
          )
      }
    }

    group("recursive types") {

      test("recursive tree") {
        val tree = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        KindlingsEncoder.encode(tree) ==>
          Json.obj(
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
      }
    }

    group("sets") {

      test("Set of ints") {
        KindlingsEncoder.encode(Set(1)) ==> Json.arr(Json.fromInt(1))
      }

      test("empty set") {
        KindlingsEncoder.encode(Set.empty[Int]) ==> Json.arr()
      }
    }

    group("sealed traits with case object singletons") {

      test("case object singleton (wrapper-style)") {
        KindlingsEncoder.encode[SimpleEnumCirce](Yes) ==> Json.obj("Yes" -> Json.obj())
      }

      test("second case object singleton (wrapper-style)") {
        KindlingsEncoder.encode[SimpleEnumCirce](No) ==> Json.obj("No" -> Json.obj())
      }
    }

    group("string enum encoding (enumAsStrings)") {

      test("encode case-object-only sealed trait as string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[CardinalDirection](North) ==> Json.fromString("North")
      }

      test("encode all cases as strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[CardinalDirection](South) ==> Json.fromString("South")
        KindlingsEncoder.encode[CardinalDirection](East) ==> Json.fromString("East")
        KindlingsEncoder.encode[CardinalDirection](West) ==> Json.fromString("West")
      }

      test("enum as string with constructor name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[CardinalDirection](North) ==> Json.fromString("north")
      }

      test("enumAsStrings=false still uses wrapper-style") {
        implicit val config: Configuration = Configuration(enumAsStrings = false)
        KindlingsEncoder.encode[CardinalDirection](North) ==> Json.obj("North" -> Json.obj())
      }
    }

    group("Scala Enumeration (enumAsStrings)") {

      test("encode Scala Enumeration value as string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[ScalaColor.Value](ScalaColor.Red) ==> Json.fromString("Red")
      }

      test("encode all Scala Enumeration values as strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[ScalaColor.Value](ScalaColor.Green) ==> Json.fromString("Green")
        KindlingsEncoder.encode[ScalaColor.Value](ScalaColor.Blue) ==> Json.fromString("Blue")
      }

      test("Scala Enumeration with name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[ScalaColor.Value](ScalaColor.Red) ==> Json.fromString("red")
      }
    }

    // Java enum tests are in KindlingsEncoderJvmSpec (src/test/scalajvm)

    group("configuration") {

      test("snake_case member names") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        KindlingsEncoder.encode(CamelCaseFields("Alice", "Smith")) ==>
          Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
      }

      test("kebab-case member names") {
        implicit val config: Configuration = Configuration.default.withKebabCaseMemberNames
        KindlingsEncoder.encode(CamelCaseFields("Alice", "Smith")) ==>
          Json.obj("first-name" -> Json.fromString("Alice"), "last-name" -> Json.fromString("Smith"))
      }

      test("PascalCase member names") {
        implicit val config: Configuration = Configuration.default.withPascalCaseMemberNames
        KindlingsEncoder.encode(CamelCaseFields("Alice", "Smith")) ==>
          Json.obj("FirstName" -> Json.fromString("Alice"), "LastName" -> Json.fromString("Smith"))
      }

      test("SCREAMING_SNAKE_CASE member names") {
        implicit val config: Configuration = Configuration.default.withScreamingSnakeCaseMemberNames
        KindlingsEncoder.encode(CamelCaseFields("Alice", "Smith")) ==>
          Json.obj("FIRST_NAME" -> Json.fromString("Alice"), "LAST_NAME" -> Json.fromString("Smith"))
      }

      test("custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[Shape](Circle(5.0)) ==>
          Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
      }

      test("snake_case constructor names") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = Configuration.snakeCase)
        KindlingsEncoder.encode[Shape](Circle(5.0)) ==>
          Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
      }
    }

    group("derive") {

      test("explicit derive returns Encoder") {
        val encoder: Encoder[SimplePerson] = KindlingsEncoder.derive[SimplePerson]
        encoder(SimplePerson("Alice", 30)) ==>
          Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }

      test("derived provides KindlingsEncoder") {
        val encoder: KindlingsEncoder[SimplePerson] = KindlingsEncoder.derived[SimplePerson]
        encoder.apply(SimplePerson("Alice", 30)) ==>
          Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }
    }

    group("custom implicit priority") {

      test("user-provided Encoder is used over derivation") {
        implicit val customEncoder: Encoder[SingleField] = Encoder.instance { sf =>
          Json.fromInt(sf.value * 10)
        }
        KindlingsEncoder.encode(SingleField(5)) ==> Json.fromInt(50)
      }
    }

    group("tuples") {

      test("encode (Int, String) as JSON array") {
        KindlingsEncoder.encode((42, "hello")) ==>
          Json.arr(Json.fromInt(42), Json.fromString("hello"))
      }

      test("encode (Int, String, Boolean) as JSON array") {
        KindlingsEncoder.encode((42, "hello", true)) ==>
          Json.arr(Json.fromInt(42), Json.fromString("hello"), Json.True)
      }
    }

    group("generic case classes") {

      test("Box[Int]") {
        KindlingsEncoder.encode(Box(42)) ==> Json.obj("value" -> Json.fromInt(42))
      }

      test("Pair[String, Int]") {
        KindlingsEncoder.encode(Pair("hello", 42)) ==>
          Json.obj("first" -> Json.fromString("hello"), "second" -> Json.fromInt(42))
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        KindlingsEncoder.encode(PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))) ==>
          Json.obj(
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
      }
    }

    group("type aliases") {

      test("WithAlias round-trips type alias field") {
        KindlingsEncoder.encode(WithAlias("Alice", 30)) ==>
          Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }
    }

    group("combined configuration") {

      test("snake_case members + discriminator + constructor transform") {
        implicit val config: Configuration = Configuration(
          transformMemberNames = Configuration.snakeCase,
          transformConstructorNames = _.toLowerCase,
          discriminator = Some("type")
        )
        KindlingsEncoder.encode[Animal](Dog("Rex", "Labrador")) ==>
          Json.obj(
            "type" -> Json.fromString("dog"),
            "name" -> Json.fromString("Rex"),
            "breed" -> Json.fromString("Labrador")
          )
      }
    }

    group("per-field annotations") {

      test("@fieldName overrides field name in encoding") {
        KindlingsEncoder.encode(CirceWithFieldName("Alice", 30)) ==>
          Json.obj("user_name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }

      test("@fieldName takes precedence over config transform") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        KindlingsEncoder.encode(CirceWithFieldName("Alice", 30)) ==>
          Json.obj("user_name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
      }

      test("@transientField excludes field from encoding") {
        KindlingsEncoder.encode(CirceWithTransient("Alice", Some("cached"))) ==>
          Json.obj("name" -> Json.fromString("Alice"))
      }

      test("both annotations combined") {
        KindlingsEncoder.encode(CirceWithBothAnnotations("Alice", 42, true)) ==>
          Json.obj("display_name" -> Json.fromString("Alice"), "active" -> Json.True)
      }

      test("@transientField without default is compile error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsEncoder, annotations}
          case class BadTransient(@annotations.transientField x: Int)
          KindlingsEncoder.encode(BadTransient(1))
          """
        ).check(
          "@transientField on field 'x'",
          "requires a default value"
        )
      }
    }

    group("non-case-class sealed trait leaves") {

      test("sealed trait with non-case-class leaf using user-provided implicit") {
        implicit val plainLeafEncoder: Encoder[PlainLeaf] =
          Encoder.instance(leaf => Json.obj("x" -> Json.fromInt(leaf.x)))
        KindlingsEncoder.encode[MixedADT](new PlainLeaf(42)) ==>
          Json.obj("PlainLeaf" -> Json.obj("x" -> Json.fromInt(42)))
      }

      test("sealed trait case class leaf still derives normally") {
        implicit val plainLeafEncoder: Encoder[PlainLeaf] =
          Encoder.instance(leaf => Json.obj("x" -> Json.fromInt(leaf.x)))
        KindlingsEncoder.encode[MixedADT](CaseLeaf(7)) ==>
          Json.obj("CaseLeaf" -> Json.obj("x" -> Json.fromInt(7)))
      }
    }

    // java.time tests are in KindlingsEncoderJvmSpec (src/test/scalajvm)

    group("higher-kinded types") {

      test("HigherKindedType[List] encodes correctly") {
        KindlingsEncoder.encode(HigherKindedType[List](List(1, 2, 3))) ==>
          Json.obj("value" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
      }

      test("HigherKindedType[Option] encodes correctly") {
        KindlingsEncoder.encode(HigherKindedType[Option](Some(42))) ==>
          Json.obj("value" -> Json.fromInt(42))
      }
    }

    group("compile-time errors") {

      test("encode with unhandled type produces error message") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsEncoder, NotACirceType}
          KindlingsEncoder.encode(new NotACirceType)
          """
        ).check(
          "Macro derivation failed with the following errors:",
          "  - The type hearth.kindlings.circederivation.NotACirceType was not handled by any encoder derivation rule:",
          "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
        )
      }
    }
  }
}
