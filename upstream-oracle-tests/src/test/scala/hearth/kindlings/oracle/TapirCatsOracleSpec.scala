package hearth.kindlings.oracle

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration as CirceConfig
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration as TapirConfig

final case class OCats(
    nel: NonEmptyList[Int],
    nev: NonEmptyVector[String],
    nec: NonEmptyChain[Long],
    nes: NonEmptySet[Int],
    chain: Chain[String]
)

/** Kindlings' schemas for cats data types (via `kindlings-cats-integration`) vs tapir-cats' `TapirCodecCats`. */
final class TapirCatsOracleSpec extends MacroSuite {

  private object KindlingsSide {
    implicit val circe: CirceConfig = CirceConfig.default
    def catsSchema: Schema[OCats] = KindlingsSchema.derived[OCats].schema
  }

  private object UpstreamSide {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.generic.auto.*
    import sttp.tapir.integ.cats.codec.*
    def catsSchema: Schema[OCats] = implicitly[Schema[OCats]]
  }

  test("cats non-empty collections and Chain") {
    val differences =
      Oracle.diff(Oracle.jsonSchema(KindlingsSide.catsSchema), Oracle.jsonSchema(UpstreamSide.catsSchema))
    assertEquals(
      differences,
      // Documented (tapir guide, "Required and optional fields"): circe's decoder rejects a missing `Chain`, tapir-cats
      // derives its schema from `List`'s, which is optional.
      List("$.required: kindlings=List(nel, nev, nec, nes, chain) upstream=List(nel, nev, nec, nes)"),
      s"Kindlings vs tapir-cats JSON Schema differences:\n${differences.mkString("\n")}"
    )
  }
}
