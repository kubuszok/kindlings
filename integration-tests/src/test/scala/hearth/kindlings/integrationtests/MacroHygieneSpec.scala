package hearth.kindlings.integrationtests

import cats.data.NonEmptyList
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import io.circe.{Decoder, Encoder}
import sttp.tapir.Schema

sealed trait HygieneShape
final case class HygieneCircle(radius: Double) extends HygieneShape
case object HygieneEmpty extends HygieneShape

final case class HygieneAll(
    id: java.util.UUID,
    name: String,
    nickname: Option[String],
    tags: List[String],
    counts: Map[String, Int],
    big: BigDecimal,
    shape: HygieneShape,
    nel: NonEmptyList[Int]
)

/** Derivations expanded where members named like the top-level packages that generated code refers to (`scala`, `java`,
  * `hearth`, `io` (circe), `com` (jsoniter-scala), `cats`, `sttp`, `math`) shadow those packages. Generated code must
  * refer to them through `_root_` (on Scala 2, Hearth's cross-quotes print and re-parse the quoted code, see
  * docs/research/hearth-gap-scala2-quote-hygiene.md).
  */
object HygieneDerivations {
  def scala: Int = 1
  def java: Int = 2
  def hearth: Int = 3
  def io: Int = 4
  def com: Int = 5
  def cats: Int = 6
  def sttp: Int = 7
  def math: Int = 8

  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  val encoder: Encoder[HygieneAll] = KindlingsEncoder.derived[HygieneAll]
  val decoder: Decoder[HygieneAll] = KindlingsDecoder.derived[HygieneAll]
  val codec: JsonValueCodec[HygieneAll] = KindlingsJsonValueCodec.derived[HygieneAll]
  val schema: Schema[HygieneAll] = KindlingsSchema.derived[HygieneAll].schema
}

final class MacroHygieneSpec extends MacroSuite {

  private val value = HygieneAll(
    id = java.util.UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
    name = "n",
    nickname = Some("k"),
    tags = List("a"),
    counts = Map("c" -> 1),
    big = BigDecimal("1.5"),
    shape = HygieneCircle(2.0),
    nel = NonEmptyList.of(1, 2)
  )

  group("derivations compile and work where package names are shadowed") {

    test("circe round trip") {
      val json = HygieneDerivations.encoder(value)
      assertEquals(HygieneDerivations.decoder.decodeJson(json), Right(value))
    }

    test("jsoniter round trip") {
      val json = writeToString(value)(HygieneDerivations.codec)
      assertEquals(readFromString[HygieneAll](json)(HygieneDerivations.codec), value)
    }

    test("tapir schema") {
      assertEquals(HygieneDerivations.schema.name.map(_.fullName), Some(classOf[HygieneAll].getName))
    }
  }
}
