package hearth.kindlings.tapirschemaderivation

import sttp.tapir.Schema

trait KindlingsSchema[A] {
  def schema: Schema[A]
}
object KindlingsSchema extends KindlingsSchemaCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
