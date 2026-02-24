package hearth.kindlings.avroderivation

import scala.language.experimental.macros

private[avroderivation] trait AvroEncoderCompanionCompat { this: AvroEncoder.type =>

  def derive[A](implicit config: AvroConfig): AvroEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderImpl[A]

  def encode[A](value: A)(implicit config: AvroConfig): Any =
    macro internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]

  implicit def derived[A](implicit config: AvroConfig): AvroEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderImpl[A]
}
