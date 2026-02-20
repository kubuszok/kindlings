package hearth.kindlings.circederivation

import io.circe.{Decoder, HCursor}

trait KindlingsDecoder[A] extends Decoder[A] {
  def apply(c: HCursor): Decoder.Result[A]
}
object KindlingsDecoder extends KindlingsDecoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
