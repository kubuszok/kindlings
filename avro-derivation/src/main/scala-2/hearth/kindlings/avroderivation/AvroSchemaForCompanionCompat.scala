package hearth.kindlings.avroderivation

import org.apache.avro.Schema
import scala.language.experimental.macros

private[avroderivation] trait AvroSchemaForCompanionCompat { this: AvroSchemaFor.type =>

  @deprecated("Use .derived instead", "next")
  def derive[A](implicit config: AvroConfig): AvroSchemaFor[A] =
    macro internal.compiletime.SchemaForMacros.deriveSchemaForImpl[A]

  def schemaOf[A](implicit config: AvroConfig): Schema =
    macro internal.compiletime.SchemaForMacros.deriveInlineSchemaImpl[A]

  implicit def derived[A](implicit config: AvroConfig): AvroSchemaFor[A] =
    macro internal.compiletime.SchemaForMacros.deriveSchemaForImpl[A]
}
