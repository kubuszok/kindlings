package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec

final class RefinedJsoniterSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + Jsoniter") {

    group("encoding") {

      test("case class with refined fields encodes correctly") {
        val person = RefinedPerson(alice, thirty)
        val json = new String(
          com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(person)(
            KindlingsJsonValueCodec.derived[RefinedPerson]
          ),
          "UTF-8"
        )
        assert(json.contains("\"name\":\"Alice\""))
        assert(json.contains("\"age\":30"))
      }
    }

    group("decoding valid") {

      test("case class with refined fields decodes valid JSON") {
        val codec = KindlingsJsonValueCodec.derived[RefinedPerson]
        val result = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
          """{"name":"Alice","age":30}"""
        )(codec)
        assertEquals(result.name.value, "Alice")
        assertEquals(result.age.value, 30)
      }
    }

    group("decoding invalid") {

      test("refined positive rejects negative with error") {
        val codec = KindlingsJsonValueCodec.derived[RefinedPerson]
        intercept[com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException] {
          com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
            """{"name":"Alice","age":-1}"""
          )(codec)
        }
      }

      test("refined non-empty rejects empty string with error") {
        val codec = KindlingsJsonValueCodec.derived[RefinedPerson]
        intercept[com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException] {
          com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
            """{"name":"","age":30}"""
          )(codec)
        }
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val codec = KindlingsJsonValueCodec.derived[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val bytes = com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(person)(codec)
        val decoded = com.github.plokhotnyuk.jsoniter_scala.core.readFromArray(bytes)(codec)
        assertEquals(decoded.name.value, "Alice")
        assertEquals(decoded.age.value, 30)
      }
    }
  }
}
