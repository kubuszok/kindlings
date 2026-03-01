package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import io.circe.Json

final class IronCirceSpec extends MacroSuite {

  group("Iron + Circe") {

    group("encoding") {

      test("iron value encodes as underlying type") {
        val age: Int :| Positive = 30
        KindlingsEncoder.encode(age) ==> Json.fromInt(30)
      }

      test("case class with iron fields encodes normally") {
        val person = IronPerson("Alice", 30)
        val expected = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        KindlingsEncoder.encode(person) ==> expected
      }
    }

    group("decoding valid") {

      test("iron value decodes from valid underlying") {
        val result = KindlingsDecoder.decode[Int :| Positive](Json.fromInt(42))
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("case class with iron fields decodes valid JSON") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30))
        val result = KindlingsDecoder.decode[IronPerson](json)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("decoding invalid") {

      test("iron positive rejects negative") {
        val result = KindlingsDecoder.decode[Int :| Positive](Json.fromInt(-1))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("case class with invalid iron field produces error") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(-1))
        val result = KindlingsDecoder.decode[IronPerson](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val person = IronPerson("Alice", 30)
        val json = KindlingsEncoder.encode(person)
        val decoded = KindlingsDecoder.decode[IronPerson](json)
        assert(decoded.isRight, s"Expected Right but got $decoded")
      }
    }
  }
}
