package hearth.kindlings.tapirschemaderivation

import scala.language.experimental.macros

private[tapirschemaderivation] trait KindlingsSchemaCompanionCompat { this: KindlingsSchema.type =>

  implicit def derived[A]: KindlingsSchema[A] =
    macro internal.compiletime.SchemaMacros.deriveKindlingsSchemaImpl[A]
}
