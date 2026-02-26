package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

final class JsoniterScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    test("wrapper-style round-trip") {
      val codec = KindlingsJsonValueCodec.derive[Fruit]
      val value: Fruit = Fruit.Apple(1.5)
      val json = writeToString(value)(codec)
      json.contains("\"Apple\"") ==> true
      val decoded = readFromString[Fruit](json)(codec)
      decoded ==> value
    }

    test("second variant wrapper-style round-trip") {
      val codec = KindlingsJsonValueCodec.derive[Fruit]
      val value: Fruit = Fruit.Banana(20.0)
      val json = writeToString(value)(codec)
      json.contains("\"Banana\"") ==> true
      val decoded = readFromString[Fruit](json)(codec)
      decoded ==> value
    }

    test("discriminator-style round-trip") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
      val codec = KindlingsJsonValueCodec.derive[Fruit]
      val value: Fruit = Fruit.Banana(20.0)
      val json = writeToString(value)(codec)
      json.contains("\"type\":\"Banana\"") ==> true
      val decoded = readFromString[Fruit](json)(codec)
      decoded ==> value
    }

    test("custom name transform round-trip") {
      implicit val config: JsoniterConfig =
        JsoniterConfig(adtLeafClassNameMapper = _.toLowerCase)
      val codec = KindlingsJsonValueCodec.derive[Fruit]
      val value: Fruit = Fruit.Apple(1.5)
      val json = writeToString(value)(codec)
      json.contains("\"apple\"") ==> true
      val decoded = readFromString[Fruit](json)(codec)
      decoded ==> value
    }
  }

  group("opaque types") {

    test("standalone opaque type round-trip") {
      import JsoniterOpaqueTypes.*
      val codec = KindlingsJsonValueCodec.derive[UserId]
      val value = UserId(42)
      val json = writeToString(value)(codec)
      val decoded = readFromString[UserId](json)(codec)
      decoded ==> value
    }

    test("case class with opaque type field round-trip") {
      import JsoniterOpaqueTypes.*
      val codec = KindlingsJsonValueCodec.derive[JsoniterUserWithOpaque]
      val value = JsoniterUserWithOpaque(UserId(42), "Alice")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterUserWithOpaque](json)(codec)
      decoded ==> value
    }
  }

  group("literal types") {

    test("case class with literal String field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralString]
      val value = JsoniterWithLiteralString("hello", "Alice")
      val json = writeToString(value)(codec)
      json.contains("\"hello\"") ==> true
      val decoded = readFromString[JsoniterWithLiteralString](json)(codec)
      decoded ==> value
    }

    test("case class with literal Int field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralInt]
      val value = JsoniterWithLiteralInt(42, "Bob")
      val json = writeToString(value)(codec)
      json.contains("42") ==> true
      val decoded = readFromString[JsoniterWithLiteralInt](json)(codec)
      decoded ==> value
    }

    test("case class with literal Boolean field round-trip") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralBoolean]
      val value = JsoniterWithLiteralBoolean(true, "Carol")
      val json = writeToString(value)(codec)
      val decoded = readFromString[JsoniterWithLiteralBoolean](json)(codec)
      decoded ==> value
    }

    test("decode literal String with wrong value fails") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralString]
      intercept[com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException] {
        readFromString[JsoniterWithLiteralString]("""{"tag":"wrong","name":"Alice"}""")(codec)
      }
    }

    test("decode literal Int with wrong value fails") {
      val codec = KindlingsJsonValueCodec.derive[JsoniterWithLiteralInt]
      intercept[com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException] {
        readFromString[JsoniterWithLiteralInt]("""{"code":99,"name":"Bob"}""")(codec)
      }
    }
  }

  group("named tuples (Scala 3.7+)") {

    test("simple named tuple round-trip") {
      val codec = KindlingsJsonValueCodec.derive[(name: String, age: Int)]
      val value: (name: String, age: Int) = ("Alice", 42)
      val json = writeToString(value)(codec)
      json.contains("\"name\"") ==> true
      json.contains("\"age\"") ==> true
      val decoded = readFromString[(name: String, age: Int)](json)(codec)
      decoded ==> value
    }

    test("named tuple with nested case class round-trip") {
      val codec = KindlingsJsonValueCodec.derive[(person: SimplePerson, score: Int)]
      val value: (person: SimplePerson, score: Int) = (SimplePerson("Bob", 25), 100)
      val json = writeToString(value)(codec)
      val decoded = readFromString[(person: SimplePerson, score: Int)](json)(codec)
      decoded ==> value
    }

    test("named tuple with field name transform") {
      implicit val config: JsoniterConfig =
        JsoniterConfig(fieldNameMapper = JsoniterConfig.default.withSnakeCaseFieldNames.fieldNameMapper)
      val codec = KindlingsJsonValueCodec.derive[(firstName: String, lastName: String)]
      val value: (firstName: String, lastName: String) = ("Alice", "Smith")
      val json = writeToString(value)(codec)
      json.contains("\"first_name\"") ==> true
      json.contains("\"last_name\"") ==> true
      val decoded = readFromString[(firstName: String, lastName: String)](json)(codec)
      decoded ==> value
    }
  }

  group("union types (Scala 3)") {

    // Union type member names use fully-qualified names from Hearth's directChildren
    val StringFQN = "java.lang.String"
    val IntFQN = "scala.Int"
    val ParrotFQN = "hearth.kindlings.jsoniterderivation.Parrot"
    val HamsterFQN = "hearth.kindlings.jsoniterderivation.Hamster"

    test("String | Int round-trip with String") {
      val codec = KindlingsJsonValueCodec.derive[StringOrInt]
      val value: StringOrInt = "hello"
      val json = writeToString(value)(codec)
      json.contains("\"" + StringFQN + "\"") ==> true
      val decoded = readFromString[StringOrInt](json)(codec)
      decoded ==> value
    }

    test("String | Int round-trip with Int") {
      val codec = KindlingsJsonValueCodec.derive[StringOrInt]
      val value: StringOrInt = 42
      val json = writeToString(value)(codec)
      json.contains("\"" + IntFQN + "\"") ==> true
      val decoded = readFromString[StringOrInt](json)(codec)
      decoded ==> value
    }

    test("case class union round-trip") {
      val codec = KindlingsJsonValueCodec.derive[ParrotOrHamster]
      val value: ParrotOrHamster = Parrot("Polly", 100)
      val json = writeToString(value)(codec)
      json.contains("\"" + ParrotFQN + "\"") ==> true
      val decoded = readFromString[ParrotOrHamster](json)(codec)
      decoded ==> value
    }

    test("second case class union member round-trip") {
      val codec = KindlingsJsonValueCodec.derive[ParrotOrHamster]
      val value: ParrotOrHamster = Hamster("Biscuit", 7.5)
      val json = writeToString(value)(codec)
      json.contains("\"" + HamsterFQN + "\"") ==> true
      val decoded = readFromString[ParrotOrHamster](json)(codec)
      decoded ==> value
    }
  }
}
