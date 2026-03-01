package hearth.kindlings.integrationtests

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import io.circe.Json

final class RefinedCirceSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + Circe") {

    group("encoding") {

      test("case class with refined fields encodes normally") {
        val person = RefinedPerson(alice, thirty)
        val expected = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsEncoder.encode(person) ==> expected
      }
    }

    group("decoding valid") {

      test("refined value decodes from valid underlying") {
        val result = KindlingsDecoder.decode[String Refined NonEmpty](Json.fromString("hello"))
        assert(result.isRight)
        result.foreach(v => assertEquals(v.value, "hello"))
      }

      test("case class with refined fields decodes valid JSON") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        val result = KindlingsDecoder.decode[RefinedPerson](json)
        assert(result.isRight)
        result.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }

    group("decoding invalid") {

      test("refined value rejects invalid underlying with error") {
        val result = KindlingsDecoder.decode[String Refined NonEmpty](Json.fromString(""))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("refined positive rejects negative") {
        val result = KindlingsDecoder.decode[Int Refined Positive](Json.fromInt(-1))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("case class with invalid refined field produces error") {
        val json = Json.obj("name" -> Json.fromString(""), "age" -> Json.fromInt(30))
        val result = KindlingsDecoder.decode[RefinedPerson](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val person = RefinedPerson(alice, thirty)
        val json = KindlingsEncoder.encode(person)
        val decoded = KindlingsDecoder.decode[RefinedPerson](json)
        assert(decoded.isRight)
        decoded.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }
  }
}
