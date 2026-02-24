package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

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

}
