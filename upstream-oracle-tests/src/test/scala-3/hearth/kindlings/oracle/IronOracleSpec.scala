package hearth.kindlings.oracle

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration as CirceConfig, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import io.circe.{Decoder, Encoder, Json}
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration as TapirConfig

import scala.util.Try

type IUserName = IUserName.T
object IUserName extends RefinedType[String, MinLength[3]]

final case class IAccount(
    id: Long :| Positive,
    name: String :| (MinLength[3] & MaxLength[20]),
    email: String :| Match["^.+@.+$"],
    score: Int :| (GreaterEqual[0] & LessEqual[100]),
    tags: List[String] :| MinLength[1],
    userName: IUserName,
    nickname: Option[String :| MinLength[1]]
)

/** Kindlings with `kindlings-iron-integration` vs Iron's own integrations: tapir-iron (schemas with validators),
  * iron-circe and iron-jsoniter (codecs that refine on decoding).
  */
final class IronOracleSpec extends MacroSuite {

  private val valid = IAccount(
    id = 1L.refineUnsafe,
    name = "Alice".refineUnsafe,
    email = "alice@example.com".refineUnsafe,
    score = 42.refineUnsafe,
    tags = List("a").refineUnsafe,
    userName = IUserName("alice"),
    nickname = Some("Al".refineUnsafe)
  )

  private val validJson =
    """{"id":1,"name":"Alice","email":"alice@example.com","score":42,"tags":["a"],"userName":"alice","nickname":"Al"}"""

  /** Inputs violating one constraint each (and a valid one, to check both sides accept it). */
  private val inputs: List[(String, String)] = List(
    "valid" -> validJson,
    "id not positive" -> validJson.replace("\"id\":1", "\"id\":0"),
    "name too short" -> validJson.replace("\"Alice\"", "\"Al\""),
    "name too long" -> validJson.replace("\"Alice\"", "\"" + ("A" * 21) + "\""),
    "email not matching" -> validJson.replace("alice@example.com", "alice"),
    "score above maximum" -> validJson.replace("\"score\":42", "\"score\":101"),
    "tags empty" -> validJson.replace("[\"a\"]", "[]"),
    "refined type invalid" -> validJson.replace("\"userName\":\"alice\"", "\"userName\":\"al\""),
    "optional refined value invalid" -> validJson.replace("\"nickname\":\"Al\"", "\"nickname\":\"\""),
    "non-empty refined collection missing" -> validJson.replace(",\"tags\":[\"a\"]", "")
  )

  private object KindlingsSide {
    given circe: CirceConfig = CirceConfig.default
    given jsoniter: JsoniterConfig = JsoniterConfig.default
    def schema: Schema[IAccount] = KindlingsSchema.derived[IAccount].schema
    val encoder: Encoder[IAccount] = KindlingsEncoder.derived[IAccount]
    val decoder: Decoder[IAccount] = KindlingsDecoder.derived[IAccount]
    val codec: JsonValueCodec[IAccount] = KindlingsJsonValueCodec.derived[IAccount]
  }

  private object KindlingsWithTapirIron {
    given circe: CirceConfig = CirceConfig.default
    import sttp.tapir.codec.iron.given
    def schema: Schema[IAccount] = KindlingsSchema.derived[IAccount].schema
  }

  private object UpstreamSchema {
    given TapirConfig = TapirConfig.default
    import sttp.tapir.codec.iron.given
    import sttp.tapir.generic.auto.*
    def schema: Schema[IAccount] = summon[Schema[IAccount]]
  }

  private object UpstreamCirce {
    import io.github.iltotore.iron.circe.given
    val encoder: Encoder[IAccount] = Encoder.AsObject.derived[IAccount]
    val decoder: Decoder[IAccount] = Decoder.derived[IAccount]
  }

  private object UpstreamJsoniter {
    import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
    import io.github.iltotore.iron.jsoniter.given
    val codec: JsonValueCodec[IAccount] = JsonCodecMaker.make[IAccount]
  }

  private def outcome(result: Option[IAccount]): String = result.fold("rejected")(a => s"accepted $a")

  group("Iron oracle") {

    test("tapir schema without tapir-iron: same shape, constraints become validators only with tapir-iron") {
      // Documented (Iron integration guide): tapir alone cannot derive schemas for Iron types at all; Kindlings derives
      // them as the underlying type, and uses tapir-iron's schemas (with validators) when they are imported.
      val differences = Oracle.diff(Oracle.jsonSchema(KindlingsSide.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(
        differences,
        List(
          "$.properties[name].maxLength: kindlings=None upstream=Some(20)",
          "$.properties[name].minLength: kindlings=None upstream=Some(3)",
          "$.properties[email].pattern: kindlings=None upstream=Some(Pattern(^.+@.+$))",
          "$.properties[score].minimum: kindlings=None upstream=Some(0)",
          "$.properties[score].maximum: kindlings=None upstream=Some(100)",
          "$.properties[tags].minItems: kindlings=None upstream=Some(1)",
          "$.properties[userName].minLength: kindlings=None upstream=Some(3)",
          "$.properties[nickname].minLength: kindlings=None upstream=Some(1)"
        ),
        s"Kindlings vs tapir-iron differences:\n${differences.mkString("\n")}"
      )
    }

    test("tapir schema with tapir-iron imported vs tapir-iron: identical") {
      val differences =
        Oracle.diff(Oracle.jsonSchema(KindlingsWithTapirIron.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(
        differences,
        Nil,
        s"Kindlings + tapir-iron vs tapir-iron differences:\n${differences.mkString("\n")}"
      )
    }

    test("circe encoding vs iron-circe") {
      assertEquals(KindlingsSide.encoder(valid), UpstreamCirce.encoder(valid))
    }

    test("circe decoding vs iron-circe: the same inputs are accepted and rejected") {
      val differences = inputs.flatMap { case (label, json) =>
        val parsed: Json = io.circe.parser.parse(json).toOption.get
        val k = KindlingsSide.decoder.decodeJson(parsed).toOption
        val u = UpstreamCirce.decoder.decodeJson(parsed).toOption
        if k == u then Nil else List(s"$label: kindlings=${outcome(k)} upstream=${outcome(u)}")
      }
      assertEquals(differences, Nil, differences.mkString("\n"))
    }

    test("jsoniter encoding vs iron-jsoniter") {
      assertEquals(writeToString(valid)(using KindlingsSide.codec), writeToString(valid)(using UpstreamJsoniter.codec))
    }

    test("jsoniter decoding vs iron-jsoniter: the same inputs are accepted and rejected") {
      val differences = inputs.flatMap { case (label, json) =>
        val k = Try(readFromString[IAccount](json)(using KindlingsSide.codec)).toOption
        val u = Try(readFromString[IAccount](json)(using UpstreamJsoniter.codec)).toOption
        if k == u then Nil else List(s"$label: kindlings=${outcome(k)} upstream=${outcome(u)}")
      }
      assertEquals(
        differences,
        // Documented improvement: jsoniter-scala fills an absent field of a non-collection type with `null`, which
        // here bypasses the `MinLength[1]` constraint; Kindlings reports the missing required field.
        List(
          "non-empty refined collection missing: kindlings=rejected " +
            "upstream=accepted IAccount(1,Alice,alice@example.com,42,null,alice,Some(Al))"
        ),
        differences.mkString("\n")
      )
    }
  }
}
