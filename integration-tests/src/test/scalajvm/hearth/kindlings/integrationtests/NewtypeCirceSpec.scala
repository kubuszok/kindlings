package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.integrationtests.newtypeExamples.*
import io.circe.Json

final class NewtypeCirceSpec extends MacroSuite {

  private val userJson = Json.obj(
    "id" -> Json.fromInt(1),
    "name" -> Json.fromString("alice"),
    "score" -> Json.fromInt(42),
    "tags" -> Json.arr(Json.fromString("a"), Json.fromString("b"))
  )

  group("scala-newtype + Circe") {

    group("encoding") {

      test("newtype value encodes as underlying type") {
        KindlingsEncoder.encode(NewtypeUserId(1)) ==> Json.fromInt(1)
      }

      test("newsubtype value encodes as underlying type") {
        KindlingsEncoder.encode(NewtypeScore(42)) ==> Json.fromInt(42)
      }

      test("parameterized newtype encodes as underlying type") {
        KindlingsEncoder.encode(NewtypeTags(List("a", "b"))) ==> Json.arr(Json.fromString("a"), Json.fromString("b"))
      }

      test("case class with newtype fields encodes normally") {
        KindlingsEncoder.encode(NewtypeUser.example) ==> userJson
      }

      test("optional newtype field encodes as underlying type") {
        KindlingsEncoder.encode(WithNewtypeOption(Some(NewtypeUserId(1)))) ==> Json.obj("value" -> Json.fromInt(1))
      }
    }

    group("decoding") {

      test("newtype value decodes from underlying") {
        KindlingsDecoder.decode[NewtypeUserId](Json.fromInt(1)) ==> Right(NewtypeUserId(1))
      }

      test("case class with newtype fields decodes") {
        KindlingsDecoder.decode[NewtypeUser](userJson) ==> Right(NewtypeUser.example)
      }

      test("newtype decoding fails on wrong underlying type") {
        val result = KindlingsDecoder.decode[NewtypeUserId](Json.fromString("not-a-number"))
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("user-provided instances") {

      test("companion-provided Encoder is used instead of unwrapping") {
        KindlingsEncoder.encode(WithNewtypeCustomId(NewtypeCustomId(7))) ==> Json.obj("id" -> Json.fromString("id-7"))
      }

      test("companion-provided Decoder is used instead of wrapping") {
        KindlingsDecoder.decode[WithNewtypeCustomId](Json.obj("id" -> Json.fromString("id-7"))) ==>
          Right(WithNewtypeCustomId(NewtypeCustomId(7)))
      }

      test("implicit in local scope is used instead of unwrapping") {
        implicit val userIdEncoder: io.circe.Encoder[NewtypeUserId] =
          io.circe.Encoder.encodeString.contramap(id => s"user-${id.value}")
        KindlingsEncoder.encode(WithNewtypeOption(Some(NewtypeUserId(1)))) ==>
          Json.obj("value" -> Json.fromString("user-1"))
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        KindlingsDecoder.decode[NewtypeUser](KindlingsEncoder.encode(NewtypeUser.example)) ==> Right(
          NewtypeUser.example
        )
      }
    }
  }
}
