package hearth.kindlings.oracle.tapirparity

import sttp.tapir.Schema
import sttp.tapir.generic.auto.*

object TpVersion {
  val isScala3: Boolean = false

  /** Recursive schemas declared the way tapir's docs say for Scala 2: `implicit lazy val`s. */
  object Recursive {
    implicit lazy val fSchema: Schema[PtF] = Schema.derived[PtF]
    implicit lazy val nodeSchema: Schema[PtNode] = Schema.derived[PtNode]
    implicit lazy val iOptSchema: Schema[PtIOpt] = Schema.derived[PtIOpt]
    implicit lazy val iListSchema: Schema[PtIList] = Schema.derived[PtIList]
  }

  /** As [[Recursive]], with a discriminator. */
  object RecursiveWithDiscriminator {
    implicit val tapir: sttp.tapir.generic.Configuration =
      sttp.tapir.generic.Configuration.default.withDiscriminator("type")
    implicit lazy val nodeSchema: Schema[PtNode] = Schema.derived[PtNode]
  }
}

/** Value classes derived by tapir (supported by its Scala 2 derivation only). */
object TpValueClasses {
  def upstream: Schema[PtUsesValueClasses] = implicitly[Schema[PtUsesValueClasses]]
}
