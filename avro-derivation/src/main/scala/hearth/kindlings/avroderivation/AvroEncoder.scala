package hearth.kindlings.avroderivation

trait AvroEncoder[A] {
  def schema: org.apache.avro.Schema
  def encode(value: A): Any
}
object AvroEncoder extends AvroEncoderCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
