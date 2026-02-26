package hearth.kindlings.tapirschemaderivation

import sttp.tapir.Schema

private[tapirschemaderivation] trait KindlingsSchemaCompanionCompat { this: KindlingsSchema.type =>

  inline def derive[A]: Schema[A] = ${
    internal.compiletime.SchemaMacros.deriveSchemaImpl[A]
  }

  inline given derived[A]: KindlingsSchema[A] = ${
    internal.compiletime.SchemaMacros.deriveKindlingsSchemaImpl[A]
  }
}
