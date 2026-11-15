package hearth.kindlings.oracle.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{MaxSize, MinSize, NonEmpty}
import eu.timepit.refined.numeric.{Interval, Positive}
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration as CirceConfig, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import io.circe.{Decoder, Encoder, Json}
import sttp.tapir.{Schema, SchemaType}
import sttp.tapir.generic.Configuration as TapirConfig

final case class RfAccount(
    name: Refined[String, NonEmpty],
    age: Refined[Int, Positive],
    score: Refined[Int, Interval.Closed[0, 100]],
    code: Refined[String, And[MinSize[3], MaxSize[5]]],
    email: Refined[String, MatchesRegex["^.+@.+$"]],
    nickname: Option[Refined[String, NonEmpty]]
)

final case class RfKeyed(byName: Map[Refined[String, NonEmpty], Int])

/** Kindlings with `kindlings-refined-integration` vs refined's own integrations: tapir-refined (schemas with
  * validators) and circe-refined (codecs that refine on decoding). jsoniter-scala has no refined integration.
  */
final class RefinedOracleSpec extends MacroSuite {

  private def refined[A, P](value: A)(implicit v: eu.timepit.refined.api.Validate[A, P]): Refined[A, P] =
    refineV[P](value).fold(e => throw new IllegalArgumentException(e), identity)

  private val valid = RfAccount(
    name = refined[String, NonEmpty]("Alice"),
    age = refined[Int, Positive](30),
    score = refined[Int, Interval.Closed[0, 100]](42),
    code = refined[String, And[MinSize[3], MaxSize[5]]]("abcd"),
    email = refined[String, MatchesRegex["^.+@.+$"]]("alice@example.com"),
    nickname = Some(refined[String, NonEmpty]("Al"))
  )

  private val validJson =
    """{"name":"Alice","age":30,"score":42,"code":"abcd","email":"alice@example.com","nickname":"Al"}"""

  /** Inputs violating one constraint each (and a valid one, to check both sides accept it). */
  private val inputs: List[(String, String)] = List(
    "valid" -> validJson,
    "name empty" -> validJson.replace("\"Alice\"", "\"\""),
    "age not positive" -> validJson.replace("\"age\":30", "\"age\":0"),
    "score above maximum" -> validJson.replace("\"score\":42", "\"score\":101"),
    "code too short" -> validJson.replace("\"abcd\"", "\"ab\""),
    "code too long" -> validJson.replace("\"abcd\"", "\"abcdef\""),
    "email not matching" -> validJson.replace("alice@example.com", "alice"),
    "optional refined value invalid" -> validJson.replace("\"nickname\":\"Al\"", "\"nickname\":\"\""),
    "optional refined value missing" -> validJson.replace(",\"nickname\":\"Al\"", "")
  )

  private object KindlingsSide {
    implicit val circe: CirceConfig = CirceConfig.default
    def schema: Schema[RfAccount] = KindlingsSchema.derived[RfAccount].schema
    def keyed: Schema[RfKeyed] = KindlingsSchema.derived[RfKeyed].schema
    val encoder: Encoder[RfAccount] = KindlingsEncoder.derived[RfAccount]
    val decoder: Decoder[RfAccount] = KindlingsDecoder.derived[RfAccount]
  }

  private object KindlingsWithTapirRefined {
    implicit val circe: CirceConfig = CirceConfig.default
    import sttp.tapir.codec.refined.*
    def schema: Schema[RfAccount] = KindlingsSchema.derived[RfAccount].schema
  }

  private object UpstreamSchema {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.codec.refined.*
    import sttp.tapir.generic.auto.*
    def schema: Schema[RfAccount] = implicitly[Schema[RfAccount]]
    // tapir derives no schema for maps with non-String keys: its users declare `Schema.schemaForMap[K, V]`
    def keyedMap: Schema[Map[Refined[String, NonEmpty], Int]] =
      Schema.schemaForMap[Refined[String, NonEmpty], Int](_.value)
  }

  private object UpstreamCirce {
    import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
    import io.circe.refined.*
    val encoder: Encoder[RfAccount] = deriveEncoder[RfAccount]
    val decoder: Decoder[RfAccount] = deriveDecoder[RfAccount]
  }

  private def outcome(result: Option[RfAccount]): String = result.fold("rejected")(a => s"accepted $a")

  group("refined oracle") {

    test("tapir schema without tapir-refined: same shape, predicates become validators only with tapir-refined") {
      // Documented (refined integration guide, "Tapir schemas"): Kindlings derives a refined type's schema as the
      // underlying type's, and uses tapir-refined's schemas (with validators) when they are imported.
      val differences = Oracle.diff(Oracle.jsonSchema(KindlingsSide.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(
        differences,
        List(
          "$.properties[name].minLength: kindlings=None upstream=Some(1)",
          "$.properties[age].exclusiveMinimum: kindlings=None upstream=Some(0)",
          "$.properties[score].minimum: kindlings=None upstream=Some(0)",
          "$.properties[score].maximum: kindlings=None upstream=Some(100)",
          "$.properties[code].maxLength: kindlings=None upstream=Some(5)",
          "$.properties[code].minLength: kindlings=None upstream=Some(3)",
          "$.properties[email].pattern: kindlings=None upstream=Some(Pattern(^.+@.+$))",
          "$.properties[nickname].minLength: kindlings=None upstream=Some(1)"
        ),
        s"Kindlings vs tapir-refined differences:\n${differences.mkString("\n")}"
      )
    }

    test("tapir schema with tapir-refined imported vs tapir-refined: identical") {
      val differences =
        Oracle.diff(Oracle.jsonSchema(KindlingsWithTapirRefined.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(
        differences,
        Nil,
        s"Kindlings + tapir-refined vs tapir-refined differences:\n${differences.mkString("\n")}"
      )
    }

    test("the name of a map keyed by a refined type vs tapir's Schema.schemaForMap") {
      val kindlingsName = KindlingsSide.keyed.schemaType match {
        case p: SchemaType.SProduct[?] => p.fields.head.schema.name
        case other                     => fail(s"Expected SProduct, got: $other")
      }
      def shortNames(name: Option[Schema.SName]): List[String] =
        name.toList.flatMap(n => n.fullName :: n.typeParameterShortNames).map(_.split('.').last)
      // Documented (tapir guide, "Maps, collections and other built-in types"): Kindlings prints type arguments
      // dealiased (refined's `NonEmpty` is an alias of `Not[Empty]`); tapir names the predicate differently on each
      // Scala version (Scala 3: the alias `NonEmpty`; Scala 2: `Not`, without its argument).
      assertEquals(shortNames(kindlingsName), List("Map", "Refined", "String", "Not", "Empty", "Int"))
      assertEquals(
        shortNames(UpstreamSchema.keyedMap.name).filterNot(Set("NonEmpty", "Not")),
        List("Map", "Refined", "String", "Int")
      )
    }

    test("circe encoding vs circe-refined") {
      assertEquals(KindlingsSide.encoder(valid), UpstreamCirce.encoder(valid))
    }

    test("circe decoding vs circe-refined: the same inputs are accepted and rejected") {
      val differences = inputs.flatMap { case (label, json) =>
        val parsed: Json = io.circe.parser.parse(json).toOption.get
        val k = KindlingsSide.decoder.decodeJson(parsed).toOption
        val u = UpstreamCirce.decoder.decodeJson(parsed).toOption
        if (k == u) Nil else List(s"$label: kindlings=${outcome(k)} upstream=${outcome(u)}")
      }
      assertEquals(differences, Nil, differences.mkString("\n"))
    }
  }
}
