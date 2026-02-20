package hearth.kindlings.circederivation

import io.circe.{Encoder, Json}
import scala.language.experimental.macros

private[circederivation] trait KindlingsEncoderCompanionCompat { this: KindlingsEncoder.type =>

  def derive[A](implicit config: Configuration): Encoder[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderImpl[A]

  def encode[A](value: A)(implicit config: Configuration): Json =
    macro internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]

  implicit def derived[A](implicit config: Configuration): KindlingsEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]
}
