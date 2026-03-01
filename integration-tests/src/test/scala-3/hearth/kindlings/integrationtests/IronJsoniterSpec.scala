package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec

final class IronJsoniterSpec extends MacroSuite {

  group("Iron + Jsoniter") {

    group("encoding") {

      test("case class with iron fields encodes correctly") {
        val person = IronPerson("Alice", 30)
        val codec = KindlingsJsonValueCodec.derived[IronPerson]
        val json = new String(
          com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(person)(codec),
          "UTF-8"
        )
        assert(json.contains("\"name\":\"Alice\""))
        assert(json.contains("\"age\":30"))
      }
    }

    group("decoding valid") {

      test("case class with iron fields decodes valid JSON") {
        val codec = KindlingsJsonValueCodec.derived[IronPerson]
        val result = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
          """{"name":"Alice","age":30}"""
        )(codec)
        assertEquals(result.name: String, "Alice")
        assertEquals(result.age: Int, 30)
      }
    }

    group("decoding invalid") {

      test("iron positive rejects negative with error") {
        val codec = KindlingsJsonValueCodec.derived[IronPerson]
        intercept[com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException] {
          com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
            """{"name":"Alice","age":-1}"""
          )(codec)
        }
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val codec = KindlingsJsonValueCodec.derived[IronPerson]
        val person = IronPerson("Alice", 30)
        val bytes = com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(person)(codec)
        val decoded = com.github.plokhotnyuk.jsoniter_scala.core.readFromArray(bytes)(codec)
        assertEquals(decoded.name: String, "Alice")
        assertEquals(decoded.age: Int, 30)
      }
    }
  }
}
