package hearth.kindlings.tapirschemaderivation

import sttp.tapir.Schema
import scala.language.experimental.macros

private[tapirschemaderivation] trait KindlingsSchemaCompanionCompat { this: KindlingsSchema.type =>

  def derive[A]: Schema[A] =
    macro internal.compiletime.SchemaMacros.deriveSchemaImpl[A]

  implicit def derived[A]: KindlingsSchema[A] =
    macro internal.compiletime.SchemaMacros.deriveKindlingsSchemaImpl[A]
}
