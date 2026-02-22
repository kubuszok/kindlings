package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Json}

@scala.annotation.nowarn("msg=is never used")
final class KindlingsDecoderSpec extends MacroSuite {

  group("KindlingsDecoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        assertEquals(KindlingsDecoder.decode[Int](Json.fromInt(42).hcursor), Right(42))
      }

      test("String") {
        assertEquals(KindlingsDecoder.decode[String](Json.fromString("hello").hcursor), Right("hello"))
      }

      test("Boolean") {
        assertEquals(KindlingsDecoder.decode[Boolean](Json.True.hcursor), Right(true))
      }

      test("Double") {
        assertEquals(KindlingsDecoder.decode[Double](Json.fromDoubleOrNull(3.14).hcursor), Right(3.14))
      }

      test("Long") {
        assertEquals(KindlingsDecoder.decode[Long](Json.fromLong(42L).hcursor), Right(42L))
      }
    }

    group("case classes") {

      test("simple case class") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        assertEquals(KindlingsDecoder.decode[SimplePerson](json.hcursor), Right(SimplePerson("Alice", 30)))
      }

      test("empty case class") {
        val json = Json.obj()
        assertEquals(KindlingsDecoder.decode[EmptyClass](json.hcursor), Right(EmptyClass()))
      }

      test("single field case class") {
        val json = Json.obj("value" -> Json.fromInt(42))
        assertEquals(KindlingsDecoder.decode[SingleField](json.hcursor), Right(SingleField(42)))
      }

      test("nested case class with explicit inner decoder") {
        // Derive Address decoder first (no implicit Decoder[Address] in scope here),
        // then make it implicit in a nested scope for PersonWithAddress derivation.
        val addressDec: Decoder[Address] = KindlingsDecoder.derived[Address]
        val personDec: Decoder[PersonWithAddress] = {
          implicit val ad: Decoder[Address] = addressDec
          KindlingsDecoder.derived[PersonWithAddress]
        }
        val json = Json.obj(
          "name" -> Json.fromString("Bob"),
          "age" -> Json.fromInt(25),
          "address" -> Json.obj(
            "street" -> Json.fromString("123 Main St"),
            "city" -> Json.fromString("Springfield")
          )
        )
        assertEquals(
          personDec.decodeJson(json),
          Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
        )
      }
    }

    group("options") {

      test("Some value") {
        val json = Json.fromInt(42)
        assertEquals(KindlingsDecoder.decode[Option[Int]](json.hcursor), Right(Some(42)))
      }

      test("None from null") {
        val json = Json.Null
        assertEquals(KindlingsDecoder.decode[Option[Int]](json.hcursor), Right(None))
      }
    }

    group("collections") {

      test("List of ints") {
        val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
        assertEquals(KindlingsDecoder.decode[List[Int]](json.hcursor), Right(List(1, 2, 3)))
      }

      test("empty list") {
        val json = Json.arr()
        assertEquals(KindlingsDecoder.decode[List[Int]](json.hcursor), Right(List.empty[Int]))
      }

      test("Vector of strings") {
        val json = Json.arr(Json.fromString("a"), Json.fromString("b"))
        assertEquals(KindlingsDecoder.decode[Vector[String]](json.hcursor), Right(Vector("a", "b")))
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val json = Json.fromInt(42)
        assertEquals(KindlingsDecoder.decode[WrappedInt](json.hcursor), Right(WrappedInt(42)))
      }
    }

    group("sealed traits") {

      test("wrapper-style decoding (default)") {
        val json = Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        assertEquals(KindlingsDecoder.decode[Shape](json.hcursor), Right(Circle(5.0): Shape))
      }

      test("wrapper-style decoding for second case") {
        val json = Json.obj(
          "Rectangle" -> Json.obj(
            "width" -> Json.fromDoubleOrNull(3.0),
            "height" -> Json.fromDoubleOrNull(4.0)
          )
        )
        assertEquals(KindlingsDecoder.decode[Shape](json.hcursor), Right(Rectangle(3.0, 4.0): Shape))
      }

      test("discriminator-style decoding") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Dog"),
          "name" -> Json.fromString("Rex"),
          "breed" -> Json.fromString("Labrador")
        )
        assertEquals(KindlingsDecoder.decode[Animal](json.hcursor), Right(Dog("Rex", "Labrador"): Animal))
      }

      test("unknown discriminator produces error") {
        val json = Json.obj("Unknown" -> Json.obj())
        val result = KindlingsDecoder.decode[Shape](json.hcursor)
        assert(result.isLeft)
      }
    }

    group("configuration") {

      test("custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        assertEquals(KindlingsDecoder.decode[Shape](json.hcursor), Right(Circle(5.0): Shape))
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

    group("error handling") {

      test("missing required field") {
        val json = Json.obj("name" -> Json.fromString("Alice"))
        val result = KindlingsDecoder.decode[SimplePerson](json.hcursor)
        assert(result.isLeft)
      }

      test("wrong type for field") {
        val json = Json.obj("name" -> Json.fromInt(42), "age" -> Json.fromInt(30))
        val result = KindlingsDecoder.decode[SimplePerson](json.hcursor)
        assert(result.isLeft)
      }
    }
  }
}
