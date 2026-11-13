package hearth.kindlings.circederivation

import io.circe.{Encoder, Json}
import scala.language.experimental.macros

private[circederivation] trait KindlingsEncoderCompanionCompat { this: KindlingsEncoder.type =>

  def deriveAsObject[A](implicit config: Configuration): Encoder.AsObject[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderAsObjectImpl[A]

  /** Explicit unwrapping (generic-extras' `deriveUnwrapped*`): a case class with exactly one field is coded as that
    * field's JSON. Automatic unwrapping only applies to `AnyVal` value classes.
    */
  def deriveUnwrapped[A](implicit config: Configuration): KindlingsEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveUnwrappedEncoderImpl[A]

  def encode[A](value: A)(implicit config: Configuration): Json =
    macro internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]

  implicit def derived[A](implicit config: Configuration): KindlingsEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]
}
