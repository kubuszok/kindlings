package hearth.kindlings.avroderivation

trait AvroDecoder[A] extends AvroSchemaFor[A] {
  def decode(value: Any): A
}
object AvroDecoder extends AvroDecoderCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
