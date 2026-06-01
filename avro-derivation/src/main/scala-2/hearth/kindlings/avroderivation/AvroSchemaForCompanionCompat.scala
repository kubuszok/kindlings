package hearth.kindlings.avroderivation

import org.apache.avro.Schema
import scala.language.experimental.macros

private[avroderivation] trait AvroSchemaForCompanionCompat { this: AvroSchemaFor.type =>

  def schemaOf[A](implicit config: AvroConfig): Schema =
    macro internal.compiletime.SchemaForMacros.deriveInlineSchemaImpl[A]

  implicit def derived[A](implicit config: AvroConfig): AvroSchemaFor[A] =
    macro internal.compiletime.SchemaForMacros.deriveSchemaForImpl[A]
}
