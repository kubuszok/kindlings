package hearth.kindlings.avroderivation

import org.apache.avro.Schema

private[avroderivation] trait AvroSchemaForCompanionCompat { this: AvroSchemaFor.type =>

  @deprecated("Use .derived instead", "next")
  inline def derive[A](using config: AvroConfig): AvroSchemaFor[A] = ${
    internal.compiletime.SchemaForMacros.deriveSchemaForImpl[A]('config)
  }

  inline def schemaOf[A](using config: AvroConfig): Schema = ${
    internal.compiletime.SchemaForMacros.deriveInlineSchemaImpl[A]('config)
  }

  inline given derived[A](using config: AvroConfig): AvroSchemaFor[A] = ${
    internal.compiletime.SchemaForMacros.deriveSchemaForImpl[A]('config)
  }
}
