package hearth.kindlings.circederivation

import io.circe.{Decoder, DecodingFailure, Json}
import scala.language.experimental.macros

private[circederivation] trait KindlingsDecoderCompanionCompat extends KindlingsDecoderBinaryCompat {
  this: KindlingsDecoder.type =>

  def decode[A](json: Json)(implicit config: Configuration): Either[DecodingFailure, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]

  /** A decoder of patches for a case class, like circe-generic's `deriveFor[A].patch`: each field is optional, an
    * absent or `null` key keeps the original value, and a value of the wrong type fails at decode time.
    */
  def patch[A](implicit config: Configuration): Decoder[A => A] =
    macro internal.compiletime.DecoderMacros.derivePatchDecoderImpl[A]

  /** A decoder of a case class with some fields supplied later, like circe-generic's `deriveFor[F].incomplete` and
    * generic-extras' `deriveConfiguredFor[F].incomplete`: `F` is a function type `(P1, ..., Pn) => C`, each `Pi` stands
    * for the first remaining field of `C` of that type, and the other fields are decoded from the JSON object.
    * {{{
    * case class Qux(i: Int, a: String, j: Int)
    * val decoder: Decoder[Int => Qux] = KindlingsDecoder.incomplete[Int => Qux] // `i` is supplied later
    * }}}
    */
  def incomplete[F](implicit config: Configuration): Decoder[F] =
    macro internal.compiletime.DecoderMacros.deriveIncompleteDecoderImpl[F]

  /** Explicit unwrapping (generic-extras' `deriveUnwrapped*`): a case class with exactly one field is coded as that
    * field's JSON. Automatic unwrapping only applies to `AnyVal` value classes.
    */
  def deriveUnwrapped[A](implicit config: Configuration): KindlingsDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveUnwrappedDecoderImpl[A]

  implicit def derived[A](implicit config: Configuration): KindlingsDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]
}
