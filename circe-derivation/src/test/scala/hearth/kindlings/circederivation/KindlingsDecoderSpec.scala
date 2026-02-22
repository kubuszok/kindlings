package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Json}

final class KindlingsDecoderSpec extends MacroSuite {

  group("KindlingsDecoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        assertEquals(KindlingsDecoder.decode[Int](Json.fromInt(42)), Right(42))
      }

      test("String") {
        assertEquals(KindlingsDecoder.decode[String](Json.fromString("hello")), Right("hello"))
      }

      test("Boolean") {
        assertEquals(KindlingsDecoder.decode[Boolean](Json.True), Right(true))
      }

      test("Double") {
        assertEquals(KindlingsDecoder.decode[Double](Json.fromDoubleOrNull(3.14)), Right(3.14))
      }

      test("Long") {
        assertEquals(KindlingsDecoder.decode[Long](Json.fromLong(42L)), Right(42L))
      }
    }

    group("case classes") {

      test("simple case class") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        assertEquals(KindlingsDecoder.decode[SimplePerson](json), Right(SimplePerson("Alice", 30)))
      }

      test("empty case class") {
        val json = Json.obj()
        assertEquals(KindlingsDecoder.decode[EmptyClass](json), Right(EmptyClass()))
      }

      test("single field case class") {
        val json = Json.obj("value" -> Json.fromInt(42))
        assertEquals(KindlingsDecoder.decode[SingleField](json), Right(SingleField(42)))
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
        assertEquals(
          KindlingsDecoder.decode[PersonWithAddress](json),
          Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
        )
      }

      test("case class with List of case classes") {
        val json = Json.obj(
          "name" -> Json.fromString("Dev"),
          "members" -> Json.arr(
            Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)),
            Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25))
          )
        )
        assertEquals(
          KindlingsDecoder.decode[TeamWithMembers](json),
          Right(TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25))))
        )
      }
    }

    group("options") {

      test("Some value") {
        val json = Json.fromInt(42)
        assertEquals(KindlingsDecoder.decode[Option[Int]](json), Right(Some(42)))
      }

      test("None from null") {
        val json = Json.Null
        assertEquals(KindlingsDecoder.decode[Option[Int]](json), Right(None))
      }
    }

    group("collections") {

      test("List of ints") {
        val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
        assertEquals(KindlingsDecoder.decode[List[Int]](json), Right(List(1, 2, 3)))
      }

      test("empty list") {
        val json = Json.arr()
        assertEquals(KindlingsDecoder.decode[List[Int]](json), Right(List.empty[Int]))
      }

      test("Vector of strings") {
        val json = Json.arr(Json.fromString("a"), Json.fromString("b"))
        assertEquals(KindlingsDecoder.decode[Vector[String]](json), Right(Vector("a", "b")))
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val json = Json.fromInt(42)
        assertEquals(KindlingsDecoder.decode[WrappedInt](json), Right(WrappedInt(42)))
      }
    }

    group("sealed traits") {

      test("wrapper-style decoding (default)") {
        val json = Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        assertEquals(KindlingsDecoder.decode[Shape](json), Right(Circle(5.0): Shape))
      }

      test("wrapper-style decoding for second case") {
        val json = Json.obj(
          "Rectangle" -> Json.obj(
            "width" -> Json.fromDoubleOrNull(3.0),
            "height" -> Json.fromDoubleOrNull(4.0)
          )
        )
        assertEquals(KindlingsDecoder.decode[Shape](json), Right(Rectangle(3.0, 4.0): Shape))
      }

      test("discriminator-style decoding") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Dog"),
          "name" -> Json.fromString("Rex"),
          "breed" -> Json.fromString("Labrador")
        )
        assertEquals(KindlingsDecoder.decode[Animal](json), Right(Dog("Rex", "Labrador"): Animal))
      }

      test("unknown discriminator produces error") {
        val json = Json.obj("Unknown" -> Json.obj())
        val result = KindlingsDecoder.decode[Shape](json)
        assert(result.isLeft)
      }
    }

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        assertEquals(KindlingsDecoder.decode[Shape](json), Right(Circle(5.0): Shape))
      }
    }

    group("derive") {

      test("explicit derive returns Decoder") {
        val decoder: Decoder[SimplePerson] = KindlingsDecoder.derive[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        assertEquals(decoder.decodeJson(json), Right(SimplePerson("Alice", 30)))
      }

      test("derived provides KindlingsDecoder") {
        val decoder: KindlingsDecoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        assertEquals(decoder.decodeJson(json), Right(SimplePerson("Alice", 30)))
      }
    }

    group("custom implicit priority") {

      test("user-provided implicit Decoder works with derived") {
        // Verifies that derived skips the self type during implicit search,
        // preventing infinite recursion when assigned to an implicit val.
        implicit val decoder: Decoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        assertEquals(decoder.decodeJson(json), Right(SimplePerson("Alice", 30)))
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val json = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
        assertEquals(KindlingsDecoder.decode[Map[String, Int]](json), Right(Map("a" -> 1, "b" -> 2)))
      }

      test("empty map") {
        assertEquals(KindlingsDecoder.decode[Map[String, Int]](Json.obj()), Right(Map.empty[String, Int]))
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
        assertEquals(
          KindlingsDecoder.decode[RecursiveTree](json),
          Right(RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil))))))
        )
      }
    }

    group("error handling") {

      test("missing required field") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        val result = KindlingsDecoder.decode[SimplePerson](json)
        assert(result.isLeft)
      }

      test("wrong type for field") {
        val json = Json.obj("name" -> Json.fromInt(42), "age" -> Json.fromInt(30))
        val result = KindlingsDecoder.decode[SimplePerson](json)
        assert(result.isLeft)
      }
    }
  }
}
