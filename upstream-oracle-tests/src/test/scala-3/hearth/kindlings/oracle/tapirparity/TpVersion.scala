package hearth.kindlings.oracle.tapirparity

import sttp.tapir.Schema
import sttp.tapir.generic.auto.*

object TpVersion {
  val isScala3: Boolean = true

  /** Recursive schemas declared the way tapir's docs say for Scala 3: `implicit def`s. */
  object Recursive {
    implicit def fSchema: Schema[PtF] = Schema.derived[PtF]
    implicit def nodeSchema: Schema[PtNode] = Schema.derived[PtNode]
    implicit def iOptSchema: Schema[PtIOpt] = Schema.derived[PtIOpt]
    implicit def iListSchema: Schema[PtIList] = Schema.derived[PtIList]
  }

  /** As [[Recursive]], with a discriminator. */
  object RecursiveWithDiscriminator {
    implicit val tapir: sttp.tapir.generic.Configuration =
      sttp.tapir.generic.Configuration.default.withDiscriminator("type")
    implicit def nodeSchema: Schema[PtNode] = Schema.derived[PtNode]
  }
}

/** Value classes: tapir's Scala 3 derivation does not support them, its users write the schemas by hand. */
object TpValueClasses {
  def upstream: Schema[PtUsesValueClasses] = {
    implicit val s: Schema[PtStringValueClass] = Schema.schemaForString.as[PtStringValueClass]
    implicit val i: Schema[PtIntValueClass] = Schema.schemaForInt.as[PtIntValueClass]
    implicit val d: Schema[PtDoubleValueClass] = Schema.schemaForDouble.as[PtDoubleValueClass]
    implicitly[Schema[PtUsesValueClasses]]
  }
}
