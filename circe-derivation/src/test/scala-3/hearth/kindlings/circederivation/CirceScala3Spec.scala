package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

// Scala 3 enum with parameterized cases (each case is a case class)
enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

@scala.annotation.nowarn("msg=is never used")
final class CirceScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    group("encoding") {

      test("enum variant with fields (wrapper-style)") {
        val json = KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5))
        assertEquals(json, Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5))))
      }

      test("second enum variant (wrapper-style)") {
        val json = KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0))
        assertEquals(json, Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0))))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0))
        assertEquals(
          json,
          Json.obj(
            "type" -> Json.fromString("Banana"),
            "length" -> Json.fromDoubleOrNull(20.0)
          )
        )
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5))
        assertEquals(json, Json.obj("apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5))))
      }
    }

    group("decoding") {

      test("enum variant with fields (wrapper-style)") {
        val json = Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        assertEquals(KindlingsDecoder.decode[Fruit](json.hcursor), Right(Fruit.Banana(20.0)))
      }

      test("second enum variant (wrapper-style)") {
        val json = Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
        assertEquals(KindlingsDecoder.decode[Fruit](json.hcursor), Right(Fruit.Apple(1.5)))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Apple"),
          "weight" -> Json.fromDoubleOrNull(1.5)
        )
        assertEquals(KindlingsDecoder.decode[Fruit](json.hcursor), Right(Fruit.Apple(1.5)))
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        assertEquals(KindlingsDecoder.decode[Fruit](json.hcursor), Right(Fruit.Banana(20.0)))
      }
    }
  }

  group("auto-derivation isolation") {

    group("encoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = KindlingsEncoder.encode[Shape](Circle(5.0))
        // If circe's auto-derivation were used, the constructor name would remain "Circle"
        assertEquals(json, Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0))))
      }

      test("member name transforms are applied") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val json = KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main", "SF")))
        val keys = json.asObject.get.keys.toList
        assert(keys.contains("name"))
        assert(keys.contains("age"))
        assert(keys.contains("address"))
      }
    }

    group("decoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        // If circe's auto-derivation were used, it would look for "Circle" not "circle"
        assertEquals(KindlingsDecoder.decode[Shape](json.hcursor), Right(Circle(5.0): Shape))
      }
    }
  }
}
