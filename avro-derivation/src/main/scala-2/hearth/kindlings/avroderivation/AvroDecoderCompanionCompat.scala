package hearth.kindlings.avroderivation

import scala.language.experimental.macros

private[avroderivation] trait AvroDecoderCompanionCompat { this: AvroDecoder.type =>

  def derive[A](implicit config: AvroConfig): AvroDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveDecoderImpl[A]

  def decode[A](value: Any)(implicit config: AvroConfig): A =
    macro internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]

  implicit def derived[A](implicit config: AvroConfig): AvroDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveDecoderImpl[A]
}
