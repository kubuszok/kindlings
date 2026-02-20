package hearth.kindlings.circederivation

import io.circe.{Encoder, Json}

trait KindlingsEncoder[A] extends Encoder[A] {
  def apply(a: A): Json
}
object KindlingsEncoder extends KindlingsEncoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
