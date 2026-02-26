package hearth.kindlings.circederivation

import io.circe.Codec

trait KindlingsCodecAsObject[A] extends Codec.AsObject[A]

object KindlingsCodecAsObject extends KindlingsCodecAsObjectCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder]] and
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
