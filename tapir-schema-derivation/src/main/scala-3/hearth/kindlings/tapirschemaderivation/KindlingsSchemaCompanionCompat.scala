package hearth.kindlings.tapirschemaderivation

private[tapirschemaderivation] trait KindlingsSchemaCompanionCompat { this: KindlingsSchema.type =>

  inline given derived[A]: KindlingsSchema[A] = ${
    internal.compiletime.SchemaMacros.deriveKindlingsSchemaImpl[A]
  }

  inline def derivedEnumeration[A]: KindlingsSchema[A] = ${
    internal.compiletime.SchemaMacros.deriveKindlingsSchemaEnumerationImpl[A]
  }
}
