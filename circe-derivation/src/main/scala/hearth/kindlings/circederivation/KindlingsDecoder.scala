package hearth.kindlings.circederivation

import io.circe.{Decoder, HCursor}

trait KindlingsDecoder[A] extends Decoder[A] {
  def apply(c: HCursor): Decoder.Result[A]
  def expectedFields: Option[Set[String]] = None
}
object KindlingsDecoder extends KindlingsDecoderCompanionCompat {

  def patch[A](implicit decoder: Decoder[A], encoder: io.circe.Encoder[A]): Decoder[A => A] =
    internal.runtime.CirceDerivationUtils.patchDecoder(decoder, encoder)

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
