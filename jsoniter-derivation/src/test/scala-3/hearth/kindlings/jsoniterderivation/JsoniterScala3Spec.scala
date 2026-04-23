package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

final class JsoniterScala3Spec extends MacroSuite {

  group("Scala 3 opaque types") {

    test("opaque type in case class round-trip") {
      import JsoniterOpaqueTypes.*
      val codec = KindlingsJsonValueCodec.derive[JsoniterUserWithOpaque]
      val value = JsoniterUserWithOpaque(UserId(42), "Alice")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterUserWithOpaque](json)(codec)
      decoded.name ==> "Alice"
      decoded.id.value ==> 42
    }
  }

  group("Scala 3 literal types") {

    test("literal String field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralString]
      val value = JsoniterWithLiteralString("hello", "Alice")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterWithLiteralString](json)(codec)
      decoded ==> value
    }

    test("literal Int field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralInt]
      val value = JsoniterWithLiteralInt(42, "Bob")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterWithLiteralInt](json)(codec)
      decoded ==> value
    }

    test("literal Boolean field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralBoolean]
      val value = JsoniterWithLiteralBoolean(true, "Carol")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterWithLiteralBoolean](json)(codec)
      decoded ==> value
    }
  }

  group("Scala 3 union types") {

    test("case class union round-trip (first variant)") {
      val codec = KindlingsJsonValueCodec.derive[ParrotOrHamster]
      val value: ParrotOrHamster = Parrot("Polly", 100)
      val json = writeToString(value)(codec)
      val decoded = readFromString[ParrotOrHamster](json)(codec)
      decoded ==> value
    }

    test("case class union round-trip (second variant)") {
      val codec = KindlingsJsonValueCodec.derive[ParrotOrHamster]
      val value: ParrotOrHamster = Hamster("Biscuit", 7.5)
      val json = writeToString(value)(codec)
      val decoded = readFromString[ParrotOrHamster](json)(codec)
      decoded ==> value
    }
  }

  group("Scala 3 enums") {

    test("simple enum (case objects) round-trip with enumAsStrings") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
      val codec = KindlingsJsonValueCodec.derive[Color]
      val json = writeToString(Color.Red: Color)(codec)
      json ==> "\"Red\""
      readFromString[Color](json)(codec) ==> Color.Red
    }

    test("simple enum all values round-trip") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
      val codec = KindlingsJsonValueCodec.derive[Color]
      for c <- Color.values do {
        val json = writeToString(c: Color)(codec)
        readFromString[Color](json)(codec) ==> c
      }
    }

    test("parameterized enum (mixed cases) round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterVehicle]
      val car: JsoniterVehicle = JsoniterVehicle.Car("Toyota", 2024)
      val bike: JsoniterVehicle = JsoniterVehicle.Bike(21)
      val carJson = writeToString(car)(codec)
      val bikeJson = writeToString(bike)(codec)
      readFromString[JsoniterVehicle](carJson)(codec) ==> car
      readFromString[JsoniterVehicle](bikeJson)(codec) ==> bike
    }

    test("enum used as field in case class") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
      val codec = KindlingsJsonValueCodec.derive[WithColor]
      val value = WithColor("sky", Color.Blue)
      val json = writeToString(value)(codec)
      readFromString[WithColor](json)(codec) ==> value
    }

    test("parameterized enum used as field in case class") {
      val codec = KindlingsJsonValueCodec.derive[WithVehicle]
      val value = WithVehicle("Alice", JsoniterVehicle.Car("Honda", 2023))
      val json = writeToString(value)(codec)
      readFromString[WithVehicle](json)(codec) ==> value
    }

    test("parameterized enum with discriminator") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withDiscriminator("_type")
      val codec = KindlingsJsonValueCodec.derive[JsoniterVehicle]
      val car: JsoniterVehicle = JsoniterVehicle.Car("Tesla", 2025)
      val json = writeToString(car)(codec)
      assert(json.contains("\"_type\""))
      readFromString[JsoniterVehicle](json)(codec) ==> car
    }
  }

  group("companion object given derivation") {

    test("CompModel codec derived in companion works") {
      val codec = CompModel.codec
      val model = CompModel(
        version = List(0, 1),
        id = "test",
        meshes = Nil,
        materials = Nil,
        nodes = Nil,
        animations = Nil
      )
      val json = writeToString(model)(codec)
      val decoded = readFromString[CompModel](json)(codec)
      decoded.id ==> "test"
      decoded.version ==> List[Short](0, 1)
    }
  }
}
