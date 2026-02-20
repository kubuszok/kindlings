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

      // Note: nested case class test requires recursive field derivation (TODO)
      // For now, nested case classes work when an implicit Decoder is provided
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
