package hearth.kindlings.avroderivation

trait AvroEncoder[A] extends AvroSchemaFor[A] {
  def encode(value: A): Any
}
object AvroEncoder extends AvroEncoderCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
