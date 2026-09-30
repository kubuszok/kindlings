package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException}

final class NeotypeJsoniterSpec extends MacroSuite {

  private val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))

  group("Neotype + Jsoniter") {

    test("case class with neotype fields encodes correctly") {
      val codec = KindlingsJsonValueCodec.derived[NeotypePerson]
      writeToString(person)(codec) ==> """{"email":"alice@example.com","age":30,"nickname":"ally"}"""
    }

    test("case class with neotype fields decodes valid JSON") {
      val codec = KindlingsJsonValueCodec.derived[NeotypePerson]
      readFromString("""{"email":"alice@example.com","age":30,"nickname":"ally"}""")(codec) ==> person
    }

    test("neotype validation rejects invalid value with error") {
      val codec = KindlingsJsonValueCodec.derived[NeotypePerson]
      intercept[JsonReaderException] {
        readFromString("""{"email":"not-an-email","age":30,"nickname":"ally"}""")(codec)
      }
    }

    test("encode then decode preserves value") {
      val codec = KindlingsJsonValueCodec.derived[NeotypePerson]
      readFromString(writeToString(person)(codec))(codec) ==> person
    }
  }
}
