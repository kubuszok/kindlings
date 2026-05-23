package hearth.kindlings.avroderivation

trait AvroDecoder[A] {
  def schema: org.apache.avro.Schema
  def decode(value: Any): A
}
object AvroDecoder extends AvroDecoderCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
