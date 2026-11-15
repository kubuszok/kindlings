package hearth.kindlings.oracle.neotype

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration as CirceConfig, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import io.circe.{Decoder, Encoder, Json}
import neotype.{unwrap, Newtype, Subtype}
import sttp.tapir.{Schema, SchemaType}
import sttp.tapir.generic.Configuration as TapirConfig

import scala.util.Try

object NoName extends Newtype[String] {
  override inline def validate(input: String): Boolean | String =
    if input.nonEmpty then true else "must not be empty"
}
type NoName = NoName.Type

object NoAge extends Subtype[Int] {
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true else "must be positive"
}
type NoAge = NoAge.Type

object NoTag extends Newtype[String]
type NoTag = NoTag.Type

final case class NoAccount(name: NoName, age: NoAge, tag: NoTag, nickname: Option[NoName])

final case class NoKeyed(byName: Map[NoName, Int], byTag: Map[NoTag, NoName])

/** Kindlings with `kindlings-neotype-integration` vs neotype's own integrations: neotype-tapir (schemas validating with
  * the newtype's `validate`), neotype-circe and neotype-jsoniter (codecs that validate on decoding).
  */
final class NeotypeOracleSpec extends MacroSuite {

  private val valid = NoAccount(NoName("Alice"), NoAge(30), NoTag("t"), Some(NoName("Al")))

  private val validJson = """{"name":"Alice","age":30,"tag":"t","nickname":"Al"}"""

  /** Inputs violating one constraint each (and a valid one, to check both sides accept it). */
  private val inputs: List[(String, String)] = List(
    "valid" -> validJson,
    "name empty" -> validJson.replace("\"Alice\"", "\"\""),
    "age not positive" -> validJson.replace("\"age\":30", "\"age\":0"),
    "optional newtype invalid" -> validJson.replace("\"nickname\":\"Al\"", "\"nickname\":\"\""),
    "optional newtype missing" -> validJson.replace(",\"nickname\":\"Al\"", "")
  )

  private object KindlingsSide {
    given circe: CirceConfig = CirceConfig.default
    given jsoniter: JsoniterConfig = JsoniterConfig.default
    def schema: Schema[NoAccount] = KindlingsSchema.derived[NoAccount].schema
    def keyed: Schema[NoKeyed] = KindlingsSchema.derived[NoKeyed].schema
    val encoder: Encoder[NoAccount] = KindlingsEncoder.derived[NoAccount]
    val decoder: Decoder[NoAccount] = KindlingsDecoder.derived[NoAccount]
    val codec: JsonValueCodec[NoAccount] = KindlingsJsonValueCodec.derived[NoAccount]
  }

  private object KindlingsWithNeotypeTapir {
    given circe: CirceConfig = CirceConfig.default
    import neotype.interop.tapir.given
    def schema: Schema[NoAccount] = KindlingsSchema.derived[NoAccount].schema
  }

  private object UpstreamSchema {
    given TapirConfig = TapirConfig.default
    import neotype.interop.tapir.given
    import sttp.tapir.generic.auto.*
    def schema: Schema[NoAccount] = summon[Schema[NoAccount]]
    // tapir derives no schema for maps with non-String keys: its users declare `Schema.schemaForMap[K, V]`
    def byName: Schema[Map[NoName, Int]] = Schema.schemaForMap[NoName, Int](_.unwrap)
    def byTag: Schema[Map[NoTag, NoName]] = Schema.schemaForMap[NoTag, NoName](_.unwrap)
  }

  private object UpstreamCirce {
    import neotype.interop.circe.given
    val encoder: Encoder[NoAccount] = Encoder.AsObject.derived[NoAccount]
    val decoder: Decoder[NoAccount] = Decoder.derived[NoAccount]
  }

  private object UpstreamJsoniter {
    import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
    import neotype.interop.jsoniter.given
    val codec: JsonValueCodec[NoAccount] = JsonCodecMaker.make[NoAccount]
  }

  private def outcome(result: Option[NoAccount]): String = result.fold("rejected")(a => s"accepted $a")

  group("neotype oracle") {

    test("tapir schema without neotype-tapir vs neotype-tapir") {
      val differences = Oracle.diff(Oracle.jsonSchema(KindlingsSide.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(differences, Nil, s"Kindlings vs neotype-tapir differences:\n${differences.mkString("\n")}")
    }

    test("tapir schema with neotype-tapir imported vs neotype-tapir: identical") {
      val differences =
        Oracle.diff(Oracle.jsonSchema(KindlingsWithNeotypeTapir.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(differences, Nil, s"Kindlings + neotype-tapir vs neotype-tapir:\n${differences.mkString("\n")}")
    }

    test("tapir schema with neotype-tapir imported: validates with the newtype's validate, as neotype-tapir does") {
      val kindlings = KindlingsWithNeotypeTapir.schema
      val upstream = UpstreamSchema.schema
      val invalid = NoAccount(NoName.unsafeMake(""), NoAge.unsafeMake(0), NoTag("t"), None)
      assertEquals(kindlings.applyValidation(invalid).size, upstream.applyValidation(invalid).size)
      assertEquals(kindlings.applyValidation(valid), Nil)
    }

    test("the names of maps keyed by newtypes vs tapir's Schema.schemaForMap") {
      val names = KindlingsSide.keyed.schemaType match {
        case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.name -> f.schema.name).toMap
        case other                     => fail(s"Expected SProduct, got: $other")
      }
      // Named after the companions (`Map_NoName_Int`, not `Map_Type_Int`), as tapir names them through the aliases
      assertEquals(names("byName"), UpstreamSchema.byName.name)
      assertEquals(names("byTag"), UpstreamSchema.byTag.name)
      assertNotEquals(names("byName"), names("byTag"))
    }

    test("circe encoding vs neotype-circe") {
      assertEquals(KindlingsSide.encoder(valid), UpstreamCirce.encoder(valid))
    }

    test("circe decoding vs neotype-circe: the same inputs are accepted and rejected") {
      val differences = inputs.flatMap { case (label, json) =>
        val parsed: Json = io.circe.parser.parse(json).toOption.get
        val k = KindlingsSide.decoder.decodeJson(parsed).toOption
        val u = UpstreamCirce.decoder.decodeJson(parsed).toOption
        if k == u then Nil else List(s"$label: kindlings=${outcome(k)} upstream=${outcome(u)}")
      }
      assertEquals(differences, Nil, differences.mkString("\n"))
    }

    test("jsoniter encoding vs neotype-jsoniter") {
      assertEquals(writeToString(valid)(using KindlingsSide.codec), writeToString(valid)(using UpstreamJsoniter.codec))
    }

    test("jsoniter decoding vs neotype-jsoniter: the same inputs are accepted and rejected") {
      val differences = inputs.flatMap { case (label, json) =>
        val k = Try(readFromString[NoAccount](json)(using KindlingsSide.codec)).toOption
        val u = Try(readFromString[NoAccount](json)(using UpstreamJsoniter.codec)).toOption
        if k == u then Nil else List(s"$label: kindlings=${outcome(k)} upstream=${outcome(u)}")
      }
      assertEquals(differences, Nil, differences.mkString("\n"))
    }
  }
}
