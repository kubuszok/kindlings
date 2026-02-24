package hearth.kindlings.avroderivation

import org.apache.avro.Schema

trait AvroSchemaFor[A] {
  def schema: Schema
}
object AvroSchemaFor extends AvroSchemaForCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
