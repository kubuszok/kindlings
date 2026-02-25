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
}
