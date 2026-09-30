package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import io.circe.Json

final class NeotypeCirceSpec extends MacroSuite {

  private val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))

  private def personJson(email: String, age: Int): Json = Json.obj(
    "email" -> Json.fromString(email),
    "age" -> Json.fromInt(age),
    "nickname" -> Json.fromString("ally")
  )

  group("Neotype + Circe") {

    group("encoding") {

      test("newtype value encodes as underlying type") {
        KindlingsEncoder.encode(NeotypeEmail("alice@example.com")) ==> Json.fromString("alice@example.com")
      }

      test("subtype value encodes as underlying type") {
        KindlingsEncoder.encode(NeotypeAge(30)) ==> Json.fromInt(30)
      }

      test("case class with neotype fields encodes normally") {
        KindlingsEncoder.encode(person) ==> personJson("alice@example.com", 30)
      }

      test("optional neotype field encodes as underlying type") {
        KindlingsEncoder.encode(WithNeotypeOption(Some(NeotypeAge(1)))) ==> Json.obj("value" -> Json.fromInt(1))
      }
    }

    group("decoding valid") {

      test("newtype value decodes from valid underlying") {
        KindlingsDecoder.decode[NeotypeEmail](Json.fromString("alice@example.com")) ==>
          Right(NeotypeEmail("alice@example.com"))
      }

      test("newtype without validation accepts any value") {
        KindlingsDecoder.decode[NeotypeNickname](Json.fromString("")) ==> Right(NeotypeNickname(""))
      }

      test("case class with neotype fields decodes valid JSON") {
        KindlingsDecoder.decode[NeotypePerson](personJson("alice@example.com", 30)) ==> Right(person)
      }
    }

    group("decoding invalid") {

      test("newtype validation rejects invalid value") {
        val result = KindlingsDecoder.decode[NeotypeEmail](Json.fromString("not-an-email"))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("subtype validation rejects invalid value") {
        val result = KindlingsDecoder.decode[NeotypeAge](Json.fromInt(-1))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("case class with invalid neotype field produces error") {
        val result = KindlingsDecoder.decode[NeotypePerson](personJson("alice@example.com", -1))
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("optional neotype field is validated") {
        val result = KindlingsDecoder.decode[WithNeotypeOption](Json.obj("value" -> Json.fromInt(-1)))
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("user-provided instances") {

      test("companion-provided Encoder is used instead of unwrapping") {
        KindlingsEncoder.encode(WithNeotypeCustomId(NeotypeCustomId(7))) ==> Json.obj("id" -> Json.fromString("id-7"))
      }

      test("companion-provided Decoder is used instead of make") {
        KindlingsDecoder.decode[WithNeotypeCustomId](Json.obj("id" -> Json.fromString("id-7"))) ==>
          Right(WithNeotypeCustomId(NeotypeCustomId(7)))
      }

      test("implicit in local scope is used instead of unwrapping") {
        implicit val ageEncoder: io.circe.Encoder[NeotypeAge] =
          io.circe.Encoder.encodeString.contramap(age => s"age-${age: Int}")
        KindlingsEncoder.encode(WithNeotypeOption(Some(NeotypeAge(1)))) ==>
          Json.obj("value" -> Json.fromString("age-1"))
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        KindlingsDecoder.decode[NeotypePerson](KindlingsEncoder.encode(person)) ==> Right(person)
      }
    }
  }
}
