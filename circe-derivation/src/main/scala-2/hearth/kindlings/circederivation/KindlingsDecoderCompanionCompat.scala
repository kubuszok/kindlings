package hearth.kindlings.circederivation

import io.circe.{Decoder, DecodingFailure, Json}
import scala.language.experimental.macros

private[circederivation] trait KindlingsDecoderCompanionCompat { this: KindlingsDecoder.type =>

  def derive[A](implicit config: Configuration): Decoder[A] =
    macro internal.compiletime.DecoderMacros.deriveDecoderImpl[A]

  def decode[A](json: Json)(implicit config: Configuration): Either[DecodingFailure, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]

  implicit def derived[A](implicit config: Configuration): KindlingsDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]
}
