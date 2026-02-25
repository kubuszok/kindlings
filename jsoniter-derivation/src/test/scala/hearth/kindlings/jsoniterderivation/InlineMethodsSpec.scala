package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

class NotAJsonType

final class InlineMethodsSpec extends MacroSuite {

  group("KindlingsJsonValueCodec.writeToString") {

    test("simple case class") {
      val json = KindlingsJsonValueCodec.writeToString(SimplePerson("Alice", 30))
      json ==> """{"name":"Alice","age":30}"""
    }

    test("uses implicit codec when available") {
      @scala.annotation.nowarn("msg=is never used")
      implicit val customIntCodec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
        def nullValue: Int = 0
        def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Int): Int =
          in.readInt()
        def encodeValue(x: Int, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
          out.writeVal(x * 100)
      }
      val json = KindlingsJsonValueCodec.writeToString(42)
      json ==> "4200"
    }

    test("with custom config") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
      val json = KindlingsJsonValueCodec.writeToString(CamelCasePerson("Alice", "Smith"))
      json.contains("\"first-name\"") ==> true
      json.contains("\"last-name\"") ==> true
    }
  }

  group("KindlingsJsonValueCodec.readFromString") {

    test("simple case class success") {
      val result = KindlingsJsonValueCodec.readFromString[SimplePerson]("""{"name":"Alice","age":30}""")
      result ==> Right(SimplePerson("Alice", 30))
    }

    test("invalid JSON returns Left with error message") {
      val result = KindlingsJsonValueCodec.readFromString[SimplePerson]("not valid json")
      result.isLeft ==> true
      val Left(error) = result: @unchecked
      assert(error.isInstanceOf[JsonReaderException])
      assert(error.getMessage.contains("expected"))
    }

    test("with custom config") {
      implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
      val result = KindlingsJsonValueCodec.readFromString[CamelCasePerson](
        """{"first-name":"Alice","last-name":"Smith"}"""
      )
      result ==> Right(CamelCasePerson("Alice", "Smith"))
    }

    test("uses implicit codec when available") {
      @scala.annotation.nowarn("msg=is never used")
      implicit val customIntCodec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
        def nullValue: Int = 0
        def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Int): Int =
          in.readInt() * 10
        def encodeValue(x: Int, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
          out.writeVal(x)
      }
      val result = KindlingsJsonValueCodec.readFromString[Int]("5")
      result ==> Right(50)
    }
  }

  group("syntax.toJsonString") {

    test("simple case class") {
      import syntax.*
      val json = SimplePerson("Alice", 30).toJsonString
      json ==> """{"name":"Alice","age":30}"""
    }

    test("with custom config") {
      import syntax.*
      implicit val config: JsoniterConfig = JsoniterConfig.default.withKebabCaseFieldNames
      val json = CamelCasePerson("Alice", "Smith").toJsonString
      json.contains("\"first-name\"") ==> true
      json.contains("\"last-name\"") ==> true
    }
  }

  group("syntax.fromJsonString") {

    test("simple case class success") {
      import syntax.*
      val result = """{"name":"Alice","age":30}""".fromJsonString[SimplePerson]
      result ==> Right(SimplePerson("Alice", 30))
    }

    test("invalid JSON returns Left with error message") {
      import syntax.*
      val Left(error) = "not valid json".fromJsonString[SimplePerson]: @unchecked
      assert(error.isInstanceOf[JsonReaderException])
      assert(error.getMessage.contains("expected"))
    }
  }

  group("round-trip") {

    test("writeToString then readFromString") {
      val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
      val json = KindlingsJsonValueCodec.writeToString(value)
      val result = KindlingsJsonValueCodec.readFromString[PersonWithAddress](json)
      result ==> Right(value)
    }

    test("toJsonString then fromJsonString") {
      import syntax.*
      val value = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
      val json = value.toJsonString
      val result = json.fromJsonString[PersonWithAddress]
      result ==> Right(value)
    }

    test("sealed trait round-trip") {
      val value: Shape = Circle(5.0)
      val json = KindlingsJsonValueCodec.writeToString(value)
      val result = KindlingsJsonValueCodec.readFromString[Shape](json)
      result ==> Right(value)
    }

    test("sealed trait with discriminator round-trip") {
      implicit val config: JsoniterConfig = JsoniterConfig(discriminatorFieldName = Some("type"))
      val value: Animal = Dog("Rex", "Labrador")
      val json = KindlingsJsonValueCodec.writeToString(value)
      val result = KindlingsJsonValueCodec.readFromString[Animal](json)
      result ==> Right(value)
    }
  }

  group("compile-time errors") {

    test("writeToString with unhandled type produces error message") {
      compileErrors(
        """
        import hearth.kindlings.jsoniterderivation.{KindlingsJsonValueCodec, NotAJsonType}
        KindlingsJsonValueCodec.writeToString(new NotAJsonType)
        """
      ).check(
        "Macro derivation failed with the following errors:",
        "  - The type hearth.kindlings.jsoniterderivation.NotAJsonType was not handled by any codec derivation rule:",
        "Enable debug logging with: import hearth.kindlings.jsoniterderivation.debug.logDerivationForKindlingsJsonValueCodec or scalac option -Xmacro-settings:jsoniterDerivation.logDerivation=true"
      )
    }

    test("readFromString with unhandled type produces error message") {
      compileErrors(
        """
        import hearth.kindlings.jsoniterderivation.{KindlingsJsonValueCodec, NotAJsonType}
        KindlingsJsonValueCodec.readFromString[NotAJsonType]("{}")
        """
      ).check(
        "Macro derivation failed with the following errors:",
        "  - The type hearth.kindlings.jsoniterderivation.NotAJsonType was not handled by any codec derivation rule:",
        "Enable debug logging with: import hearth.kindlings.jsoniterderivation.debug.logDerivationForKindlingsJsonValueCodec or scalac option -Xmacro-settings:jsoniterDerivation.logDerivation=true"
      )
    }

    test("readFromString with Nothing type parameter produces clear error") {
      compileErrors(
        """
        import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
        val result = KindlingsJsonValueCodec.readFromString("{}")
        """
      ).check(
        "type parameter was inferred as"
      )
    }
  }
}
