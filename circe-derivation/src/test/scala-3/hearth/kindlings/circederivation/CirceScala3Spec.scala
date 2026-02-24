package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

// Scala 3 enum with parameterized cases (each case is a case class)
enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

enum Color {
  case Red, Green, Blue
}

object OpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class UserWithOpaque(id: OpaqueTypes.UserId, name: String)

final class CirceScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    group("encoding") {

      test("enum variant with fields (wrapper-style)") {
        KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5)) ==>
          Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
      }

      test("second enum variant (wrapper-style)") {
        KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0)) ==>
          Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0)) ==>
          Json.obj(
            "type" -> Json.fromString("Banana"),
            "length" -> Json.fromDoubleOrNull(20.0)
          )
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5)) ==>
          Json.obj("apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
      }
    }

    group("decoding") {

      test("enum variant with fields (wrapper-style)") {
        val json = Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Banana(20.0))
      }

      test("second enum variant (wrapper-style)") {
        val json = Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Apple"),
          "weight" -> Json.fromDoubleOrNull(1.5)
        )
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Banana(20.0))
      }
    }
  }

  group("opaque types") {

    test("encode standalone opaque type") {
      import OpaqueTypes.*
      KindlingsEncoder.encode(UserId(42)) ==> Json.fromInt(42)
    }

    test("decode standalone opaque type") {
      import OpaqueTypes.*
      KindlingsDecoder.decode[UserId](Json.fromInt(42)) ==> Right(UserId(42))
    }

    test("encode case class with opaque type field") {
      import OpaqueTypes.*
      KindlingsEncoder.encode(UserWithOpaque(UserId(42), "Alice")) ==>
        Json.obj("id" -> Json.fromInt(42), "name" -> Json.fromString("Alice"))
    }

    test("decode case class with opaque type field") {
      import OpaqueTypes.*
      val json = Json.obj("id" -> Json.fromInt(42), "name" -> Json.fromString("Alice"))
      KindlingsDecoder.decode[UserWithOpaque](json) ==> Right(UserWithOpaque(UserId(42), "Alice"))
    }
  }

  group("parameterless Scala 3 enums") {

    test("encode parameterless enum") {
      KindlingsEncoder.encode[Color](Color.Red) ==> Json.obj("Red" -> Json.obj())
    }

    test("decode parameterless enum") {
      val json = Json.obj("Green" -> Json.obj())
      KindlingsDecoder.decode[Color](json) ==> Right(Color.Green)
    }
  }

  group("auto-derivation isolation") {

    group("encoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        // If circe's auto-derivation were used, the constructor name would remain "Circle"
        KindlingsEncoder.encode[Shape](Circle(5.0)) ==>
          Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
      }

      test("member name transforms are applied") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main", "SF"))) ==>
          Json.obj(
            "name" -> Json.fromString("Bob"),
            "age" -> Json.fromInt(25),
            "address" -> Json.obj("street" -> Json.fromString("123 Main"), "city" -> Json.fromString("SF"))
          )
      }
    }

    group("decoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        // If circe's auto-derivation were used, it would look for "Circle" not "circle"
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(5.0): Shape)
      }
    }
  }
}
